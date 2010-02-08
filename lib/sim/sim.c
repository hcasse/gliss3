/*
 * $Id: sim.c,v 1.9 2009/11/26 09:01:18 casse Exp $
 * Copyright (c) 2009, IRIT - UPS <casse@irit.fr>
 *
 * This file is part of OGliss.
 *
 * OGliss is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * OGliss is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OGliss; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gliss/api.h>
#include <loader.h>





void syntax(char *prog_name)
{
	fprintf(stderr, "\nSyntax is: %s <exec_name> [-start=<hexa_address>] [-exit=<hexa_address>]\n"
			"\n"
			"if args or env strings must be passed to the simulated program,\n"
			"put them in <exec_name>.argv or <exec_name>.envp,\n"
			"one arg or env string on each line, whitespaces will not be ignored,\n"
			"a single '\\n' must be added on the last line of these files\n", prog_name);
}

extern char **environ;

typedef struct init_options_t
{
	int argc;
	char **argv;
	char **envp;
} init_options;

void copy_options_to_gliss_env(gliss_env_t *env, init_options *opt)
{
	env->argc = opt->argc;

	env->argv = opt->argv;
	env->argv_addr = 0;

	env->envp = opt->envp;
	env->envp_addr = 0;

	env->auxv = 0;
	env->auxv_addr = 0;

	env->stack_pointer = 0;
}


/* replace every '\n' by '\0'
   after we read argv or envp file
   => there will be 2 '\0' after the last string
   return the number of '\n' replaced, ie the number of "substrings" */
int cut_multi_string(char *s)
{
	int i = 0;
	while (*s)
	{
		if (*s == '\n')
		{
			*s = '\0';
			i++;
		}
		s++;
	}
	return i;
}

/* find the beginning of the next "substring", 0 if none (ie the next '\0' is followed by another '\0')
   WARNING: be sure not to call this if no '\0' is after s */
char *next_multi_string(char *s)
{
	while (*s)
		s++;

	printf("next multi, s=%c[%02X], s+1=%c[%02X]\n", *s, *s, *(s+1), *(s+1));fflush(stdout);
	return s + 1;
}


int main(int argc, char **argv)
{
	gliss_sim_t *sim = 0;
	gliss_state_t *state = 0;
	gliss_platform_t *platform = 0;
	gliss_loader_t *loader = 0;
	gliss_address_t addr_start = 0;
	gliss_address_t addr_exit = 0;
	char *c_ptr = 0;
	int is_start_given = 0;
	int is_exit_given = 0;
	int prog_index = 0;
	/*Elf32_Sym *Elf_sym = 0;*/
	int sym_exit = 0, sym_start = 0;
	FILE *f = 0;
	/* this buffer should be big enough to hold an executable's name + 5 chars */
	char buffer[256];
	char *argv_str = 0;
	char *envp_str = 0;
	init_options options = {0, 0, 0};
	int i = 0;
	int nb, nb_bis = 0;
	long file_size = 0;


	/* test argument count */
	if ((argc < 2) || (argc > 4)) {
		fprintf(stderr, "ERROR: too many or too few arguments\n");
		syntax(argv[0]);
		return 1;
	}


	/* get the start address */
	i = 0;
	for ( ; i<argc; i++)
		if (strncmp(argv[i], "-start=", 7) == 0)
			break;

	is_start_given = i;
	if (i == argc)
		is_start_given = 0;

	if (is_start_given)
	{
		addr_start = strtoul(argv[is_start_given] + 7, &c_ptr, 16);
		if (addr_start == 0) {
			fprintf(stderr, "ERROR: bad start address specified : %s, only hexadecimal address accepted\n", argv[is_start_given]);
			syntax(argv[0]);
			return 2;
		}
		// !!DEBUG!!
		printf("start given: %08X\n", addr_start);
	}
	else
		addr_start = 0;


	/* get the exit address */
	i = 0;
	for ( ; i<argc; i++)
		if (strncmp(argv[i], "-exit=", 6) == 0)
			break;

	is_exit_given = i;
	if (i == argc)
		is_exit_given = 0;

	if (is_exit_given)
	{
		addr_exit = strtoul(argv[is_exit_given] + 6, &c_ptr, 16);
		if (addr_exit == 0) {
			fprintf(stderr, "ERROR: bad exit address specified : %s, only hexadecimal address accepted\n", argv[is_exit_given]);
			syntax(argv[0]);
			return 2;
		}
		// !!DEBUG!!
		printf("exit given: %08X\n", addr_exit);
	}
	else
		addr_exit = 0;

	/* find the program name */
	prog_index = 0;
	for (i=1; i<argc; i++)
		/* the program name is the only remaining argument */
		if ((i != is_start_given) && (i != is_exit_given))
			prog_index = i;
	if (prog_index == 0) {
		fprintf(stderr, "ERROR: no program name given\n");
		syntax(argv[0]);
		return 2;
	}

	/* open the exec file */
	loader = gliss_loader_open(argv[prog_index]);
	if(loader == NULL) {
		fprintf(stderr, "ERROR: no more resources (1)\n");
		return 2;
	}

	/* find the _start symbol if no start address is given */
	if ( ! is_start_given)
	{
		addr_start = 0;

		/* search symbol _start */
		for(sym_start = 0; sym_start < gliss_loader_count_syms(loader); sym_start++)
		{
			gliss_loader_sym_t data;
			gliss_loader_sym(loader, sym_start, &data);
			if (strcmp(data.name, "_start") == 0)
			{
				/* we found _start */
				addr_start = data.value;
				break;
			}
		}

		/* check for error */
		if (addr_start == 0) {
			fprintf(stderr, "ERROR: cannot find the \"_start\" symbol and no start address is given.\n");
			syntax(argv[0]);
			return 2;
		}

		// !!DEBUG!!
		printf("_start found at %08X\n", addr_start);
	}

	/* find the _exit symbol if no exit address is given */
	if ( ! is_exit_given)
	{
		addr_exit = 0;

		/* search symbol _exit */
		for(sym_exit = 0; sym_exit < gliss_loader_count_syms(loader); sym_exit++)
		{
			gliss_loader_sym_t data;
			gliss_loader_sym(loader, sym_exit, &data);
			if (strcmp(data.name, "_exit") == 0)
			{
				/* we found _exit */
				addr_exit = data.value;
				break;
			}
		}

		/* check for error */
		if (addr_exit == 0) {
			fprintf(stderr, "ERROR: cannot find the \"_exit\" symbol and no exit address is given.\n");
			syntax(argv[0]);
			return 2;
		}

		// !!DEBUG!!
		printf("_exit found at %08X\n", addr_exit);
	}

	options.argv = options.envp = 0;

	/* get the argv file and copy it into a buffer */
	argv_str = 0;
	strcpy(buffer, argv[prog_index]);
	strcat(buffer, ".argv");
	f = fopen(buffer, "r");
	if (f == 0)
	{
		/* no argv specifed */
		argv_str = 0;
	}
	else
	{
		/* get file size */
		fseek(f, 0, SEEK_END);
		file_size = ftell(f);
		//!!DEBUG!!
		/*printf("argv file size %d\n", file_size);*/
		rewind(f);
		/* allocate buffer */
		argv_str = malloc((file_size + 1) * sizeof(char));
		if (argv_str == 0) {
			fprintf(stderr, "ERROR: cannot allocate memory\n");
			syntax(argv[0]);
			return 1;
		}
		/* copy the file */
		if (fread(argv_str, sizeof(char), file_size, f) != file_size) {
			fprintf(stderr, "ERROR: cannot read the whole option file\n");
			syntax(argv[0]);
			return 1;
		}
		argv_str[file_size] = '\0';
		//!!debug!!
		printf("argv file read [%s]\n", argv_str);
		/* close the file */
		if (fclose(f)) {
			fprintf(stderr, "ERROR: cannot close the option file\n");
			syntax(argv[0]);
			return 1;
		}
	}
printf("B!\n"); fflush(stdout);
	/* get the envp file and copy it into a buffer */
	envp_str = 0;
	strcpy(buffer, argv[prog_index]);
	strcat(buffer, ".envp");
	f = fopen(buffer, "r");
	if (f != 0)
	{
		/* get file size */
		fseek(f, 0, SEEK_END);
		file_size = ftell(f);
		rewind(f);
		/* allocate buffer */
		envp_str = malloc(file_size * sizeof(char));
		if (envp_str == 0) {
			fprintf(stderr, "ERROR: cannot allocate memory\n");
			syntax(argv[0]);
			return 1;
		}
		/* copy the file */
		if (fread(envp_str, sizeof(char), file_size, f) != file_size) {
			fprintf(stderr, "ERROR: cannot read the whole option file\n");
			syntax(argv[0]);
			return 1;
		}
		envp_str[file_size] = '\0';
		/* close the file */
		if (fclose(f)) {
			fprintf(stderr, "ERROR: cannot close the option file\n");
			syntax(argv[0]);
			return 1;
		}
	}
	/* else : no envp specifed */
	/* we have to store the default env */

	/* copy argv */
	nb = 0;
	if (argv_str)
	{
		int toto = strlen(argv_str);
		nb = cut_multi_string(argv_str);
		// !!DEBUG!!
		printf("%d args found\n", nb);
		for (i=0; i<toto; i++)
			printf("{%c|%02X}\n", argv_str[i], argv_str[i]);
	}
	options.argv = malloc((nb + 2) * sizeof(char *));
	if (options.argv == 0) {
		fprintf(stderr, "ERROR: cannot allocate memory\n");
		syntax(argv[0]);
		return 1;
	}
	c_ptr = argv_str;
	options.argv[0] = malloc(strlen(argv[0]) + 1);
	strcpy(options.argv[0], argv[0]);
	for (i = 1; i <= nb; i++)
	{
		options.argv[i] = malloc(sizeof(char) * (strlen(c_ptr) + 1));
		if (options.argv[i] == 0) {
			fprintf(stderr, "ERROR: cannot allocate memory\n");
			syntax(argv[0]);
			return 1;
		}
		strcpy(options.argv[i], c_ptr);

		//!!DEBUG!!
		printf("argv_str[%d]=(%d)[%s]\n", i, strlen(c_ptr), c_ptr);
		c_ptr = next_multi_string(c_ptr);
	}
	options.argv[nb + 1] = 0;
	options.argc = nb + 1;
	if (argv_str)
		free(argv_str);
printf("C!\n");fflush(stdout);

	/* copy default env and add envp */

	/* find default env size and added env size*/
	nb = 0;
	while (environ[nb++]);
	// !!DEBUG!!
	//printf("%d default env strings\n", nb);

	nb_bis = 0;
	if (envp_str)
	{
		nb_bis = cut_multi_string(envp_str);
		// !!DEBUG!!
		//printf("%d added env strings\n", nb_bis);
	}
	//!!DEBUG!!
	nb = 2;
	/* copy envs */
	options.envp = malloc((nb + nb_bis + 1) * sizeof(char *));
	if (options.envp == 0) {
		fprintf(stderr, "ERROR: cannot allocate memory\n");
		syntax(argv[0]);
		return 1;
	}
	/* 1st default env */
	for (i = 0; i < nb; i++)
		options.envp[i] = environ[i];
	/* then added env */
	c_ptr = envp_str;
	for (i = nb; i < nb + nb_bis; i++)
	{
		options.envp[i] = c_ptr;
		c_ptr = next_multi_string(c_ptr);
	}

	options.envp[nb + nb_bis + 1] = 0;
	if (envp_str)
		free(envp_str);
printf("D!\n");fflush(stdout);

gliss_loader_close(loader);

	/* make the platform */
	platform = gliss_new_platform();
	if(platform == NULL)  {
		fprintf(stderr, "ERROR: no more resources (2)\n");
		return 2;
	}
	/* initialize system options */
	copy_options_to_gliss_env(ppc_get_sys_env(platform), &options);

	/* load the image in the platform */
	if (gliss_load_platform(platform, argv[prog_index]) == -1) {
		fprintf(stderr, "ERROR: cannot load the given executable : %s.\n", argv[i]);
		syntax(argv[0]);
		return 2;
	}

	/* make the state depending on the platform */
	state = gliss_new_state(platform);
	if (state == NULL)  {
		fprintf(stderr, "ERROR: no more resources (3)\n");
		return 2;
	}

	/* make the simulator */
	sim = gliss_new_sim(state, addr_start, addr_exit);
	if (sim == NULL) {
		fprintf(stderr, "ERROR: no more resources (4)\n");
		return 2;
	}


// !!DEBUG BEGIN!!
//state->GPR[1] = 0x7ffdfeb0;
//	printf("entry point given by the loader to the platform then to the state NIA: %08X\n", state->NIA);
	printf("state before simulation\n");
	gliss_dump_state(sim->state, stdout);
	fflush(stdout);
	int cpt=0;

// !!DEBUG END!!

	/* perform the simulation */
	while(1)
	{
		if (gliss_is_sim_ended(sim))
			break;
		gliss_step(sim);
// !!DEBUG BEGIN!!
//	cpt++;
//	printf("state after instr %d\n", cpt);
//	gliss_dump_state(sim->state, stdout);
//	fflush(stdout);
//	getchar();
	//if (cpt > 500)
		//break;
// !!DEBUG END!!
	}

// !!DEBUG BEGIN!!
	printf("state after simulation\n");
	gliss_dump_state(sim->state, stdout);
	fflush(stdout);
// !!DEBUG END!!

	/* cleanup */
	/* this will also delete the associated state and the platform (if no one is locked on it) */
	gliss_delete_sim(sim);
	return 0;
}
