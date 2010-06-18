/*
 * Simulator base file.
 * Copyright (c) 2010, IRIT - UPS <casse@irit.fr>
 *
 * This file is part of GLISS V2.
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

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <gliss/api.h>
#include <gliss/macros.h>
#include <gliss/loader.h>





/**
 * Display usage of the command.
 * @param prog_name	Program name.
 */
void usage(const char *prog_name) {
	fprintf(stderr, "SYNTAX: %s <exec_name> OPTIONS\n\n"
			"OPTIONS may be a combination of \n"
			"  -exit=<hexa_address>] : simulation exit address (default symbol _exit)\n"
			"  -h, -help : display usage message\n"
			"	-s : display statistics\n"
			"  -start=<hexa_address> : simulation start address (default symbol _start)\n"
			"  -v, -verbose : display simulated instructions\n"
			"\n"
			"if args or env strings must be passed to the simulated program,\n"
			"put them in <exec_name>.argv or <exec_name>.envp,\n"
			"one arg or env string on each line, whitespaces will not be ignored,\n"
			"a single '\\n' must be added on the last line of these files\n\n", prog_name);
}


/**
 * Display error.
 * @param fmt		Format string.
 * @param args		Format arguments.
 */
void error_args(const char *fmt, va_list args) {
	fprintf(stderr, "ERROR: ");
	vfprintf(stderr, fmt, args);
}


/**
 * Display error.
 * @param fmt		Format string.
 * @param ...		Format arguments.
 */
void error(const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	error_args(fmt, args);
	va_end(args);
}


/**
 * Display a syntax error.
 * @param prog_name		Program name.
 * @param fmt			Format string.
 * @param ...			Format arguments.
 */
void syntax_error(char *prog_name, const char *fmt, ...) {
	va_list args;
	usage(prog_name);
	va_start(args, fmt);
	error_args(fmt, args);
	va_end(args);
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

void free_options(init_options *opt)
{
	int i = 0;

	if (opt->argv)
	{
		for (i = 0; opt->argv[i]; i++)
			free(opt->argv[i]);
		free(opt->argv);
	}
	if (opt->envp)
	{
		for (i = 0; opt->envp[i]; i++)
			free(opt->envp[i]);
		free(opt->envp);
	}
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

	/*printf("next multi, s=%c[%02X], s+1=%c[%02X]\n", *s, *s, *(s+1), *(s+1));fflush(stdout);*/
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
	int verbose = 0;
	int stats = 0;
	int inst_cnt = 0;
	uint64_t start_time, end_time, delay;

	/* scan arguments */
	for(i = 1; i < argc; i++) {

		/* -h or -help options */
		if(strcmp(argv[i], "-help") == 0 || strcmp(argv[i], "-h") == 0)  {
			usage(argv[0]);
			return 0;
		}

		/* -v or -verbose option */
		else if(strcmp(argv[i], "-verbose") == 0 || strcmp(argv[i], "-v") == 0)
			verbose = 1;

		/* -start= option */
		else if(strncmp(argv[i], "-start=", 7) == 0) {
			is_start_given = i;
			addr_start = strtoul(argv[i] + 7, &c_ptr, 16);
			if(*c_ptr != '\0') {
				syntax_error(argv[0],  "bad start address specified : %s, only hexadecimal address accepted\n", argv[i]);
				return 2;
			}
		}

		/* -exit= option */
		else if(strncmp(argv[i], "-exit=", 6) == 0) {
			is_exit_given = i;
			addr_exit = strtoul(argv[is_exit_given] + 6, &c_ptr, 16);
			if(*c_ptr == '\0') {
				syntax_error(argv[0], "bad exit address specified : %s, only hexadecimal address accepted\n", argv[i]);
				return 2;
			}
		}

		/* -s option */
		else if(strcmp(argv[i], "-s") == 0)
			stats = 1;

		/* option ? */
		else if(argv[i][0] == '-') {
			syntax_error(argv[0], "unknown option: %s\n", argv[i]);
			return 2;
		}

		/* free argument */
		else if(prog_index != 0) {
			syntax_error(argv[0], "garbage argument: %s\n", argv[i]);
			return 2;
		}
		else
			prog_index = i;
	}

	/* exec available ? */
	if(prog_index == 0) {
		syntax_error(argv[0], "no executable given !");
		return 2;
	}

	/* open the exec file */
	loader = gliss_loader_open(argv[prog_index]);
	if(loader == NULL) {
		fprintf(stderr, "ERROR: cannot open program %s\n", argv[prog_index]);
		return 2;
	}

	/* find the _start symbol if no start address is given */
	if(!is_start_given)
		addr_start = gliss_loader_start(loader);
	if(verbose)
		printf("START=%08x\n", addr_start);

	/* find the _exit symbol if no exit address is given */
	if (!is_exit_given) {
		int found = 0;

		/* search symbol _exit */
		for(sym_exit = 0; sym_exit < gliss_loader_count_syms(loader); sym_exit++) {
			gliss_loader_sym_t data;
			gliss_loader_sym(loader, sym_exit, &data);
			if(strcmp(data.name, "_exit") == 0) {
				/* we found _exit */
				addr_exit = data.value;
				found = 1;
				break;
			}
		}

		/* check for error */
		if(!found) {
			syntax_error(argv[0], "ERROR: cannot find the \"_exit\" symbol and no exit address is given.\n");
			return 2;
		}
	}
	if(verbose)
		printf("EXIT=%08x\n", addr_exit);

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
			error("ERROR: cannot allocate memory\n");
			return 1;
		}
		/* copy the file */
		if (fread(argv_str, sizeof(char), file_size, f) != file_size) {
			error("ERROR: cannot read the whole option file\n");
			return 1;
		}
		argv_str[file_size] = '\0';
		//!!debug!!
		//printf("argv file read [%s]\n", argv_str);
		/* close the file */
		if (fclose(f)) {
			error("ERROR: cannot close the option file\n");
			return 1;
		}
	}


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
		envp_str = malloc((file_size + 1) * sizeof(char));
		if (envp_str == 0) {
			error("ERROR: cannot allocate memory\n");
			return 1;
		}
		/* copy the file */
		if (fread(envp_str, sizeof(char), file_size, f) != file_size) {
			error("ERROR: cannot read the whole option file\n");
			return 1;
		}
		envp_str[file_size] = '\0';
		/* close the file */
		if (fclose(f)) {
			error("ERROR: cannot close the option file\n");
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
		/*printf("%d args found\n", nb);
		for (i=0; i<toto; i++)
			printf("{%c|%02X}\n", argv_str[i], argv_str[i]);*/
	}
	options.argv = malloc((nb + 2) * sizeof(char *));
	if (options.argv == 0) {
		error("ERROR: cannot allocate memory\n");
		return 1;
	}
	c_ptr = argv_str;
	options.argv[0] = malloc(strlen(argv[0]) + 1);
	strcpy(options.argv[0], argv[0]);
	for (i = 1; i <= nb; i++)
	{
		options.argv[i] = malloc(sizeof(char) * (strlen(c_ptr) + 1));
		if (options.argv[i] == 0) {
			error("ERROR: cannot allocate memory\n");
			return 1;
		}
		strcpy(options.argv[i], c_ptr);

		//!!DEBUG!!
		/*printf("argv_str[%d]=(%d)[%s]\n", i, strlen(c_ptr), c_ptr); */
		c_ptr = next_multi_string(c_ptr);
	}
	options.argv[nb + 1] = 0;
	options.argc = nb + 1;
	if (argv_str)
		free(argv_str);


	/* copy default env and add envp */

	/* find default env size and added env size*/
	nb = 0;
	while (environ[nb])
		nb++;
	// !!DEBUG!!
	//printf("%d default env strings\n", nb);

	nb_bis = 0;
	if (envp_str)
	{
		nb_bis = cut_multi_string(envp_str);
		// !!DEBUG!!
		//printf("%d added env strings\n", nb_bis);
	}

	/* copy envs */
	options.envp = malloc((nb + nb_bis + 1) * sizeof(char *));
	if (options.envp == 0) {
		error("ERROR: cannot allocate memory\n");
		return 1;
	}

	/* 1st default env */
	for (i = 0; i < nb; i++)
	{
		options.envp[i] = malloc(sizeof(char) * (strlen(environ[i]) + 1));
		if (options.envp[i] == 0) {
			error("ERROR: cannot allocate memory\n");
			return 1;
		}
		strcpy(options.envp[i], environ[i]);
	}

	/* then added env */
	c_ptr = envp_str;
	for (i = nb; i < nb + nb_bis; i++)
	{
		options.envp[i] = malloc(sizeof(char) * (strlen(c_ptr) + 1));
		if (options.envp[i] == 0) {
			error("ERROR: cannot allocate memory\n");
			return 1;
		}
		strcpy(options.envp[i], c_ptr);
		c_ptr = next_multi_string(c_ptr);
	}

	options.envp[nb + nb_bis] = 0;
	if (envp_str)
		free(envp_str);

	gliss_loader_close(loader);


	/* make the platform */
	platform = gliss_new_platform();
	if (platform == NULL)  {
		fprintf(stderr, "ERROR: cannot create platform\n");
		return 2;
	}
	/* initialize system options */
	copy_options_to_gliss_env(gliss_get_sys_env(platform), &options);

	/* load the image in the platform */
	if (gliss_load_platform(platform, argv[prog_index]) == -1) {
		error("ERROR: cannot load the given executable : %s.\n", argv[i]);
		return 2;
	}

	/* free argv and envp once copied to simulator's memory */
	free_options(&options);

	/* make the state depending on the platform */
	state = gliss_new_state(platform);
	if (state == NULL)  {
		fprintf(stderr, "ERROR: cannot create state\n");
		return 2;
	}

	/* make the simulator */
	sim = gliss_new_sim(state, addr_start, addr_exit);
	if (sim == NULL) {
		fprintf(stderr, "ERROR: cannot create simulator\n");
		return 2;
	}


// !!DEBUG BEGIN!!
//state->GPR[1] = 0x7ffdfeb0;
//	printf("entry point given by the loader to the platform then to the state NIA: %08X\n", state->NIA);
//	printf("state before simulation\n");
//	gliss_dump_state(sim->state, stdout);
	fflush(stdout);
	int cpt=0;

// !!DEBUG END!!

	/* measure time */
	if(stats) 
	{
		struct rusage buf;
		getrusage(RUSAGE_SELF, &buf);
		start_time = (uint64_t)buf.ru_utime.tv_sec*1000000.00 + buf.ru_utime.tv_usec;
	}

	/* full speed simulation */
	if(!verbose) {
		int a = 40;
		while(a)
		{
			sim->addr_exit = addr_exit;
			sim->state->NIA = addr_start;
			inst_cnt += ppc_run_and_count_inst(sim);
			a--;
		}
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

	/* verbose simulation */
	else {
		gliss_inst_t *inst;
		while(!gliss_is_sim_ended(sim)) 
		{
			inst = gliss_next_inst(sim);
			gliss_disasm(buffer, inst);
			fprintf(stderr, "%08x: %s\n", gliss_next_addr(sim),  buffer);
			gliss_free_inst(inst);
			gliss_step(sim);
			inst_cnt++;
		}
	}

// !!DEBUG BEGIN!!
//	printf("state after simulation\n");
//	gliss_dump_state(sim->state, stdout);
//	fflush(stdout);
// !!DEBUG END!!

	/* produce statistics */
	if(stats) {
		struct rusage buf;
		getrusage(RUSAGE_SELF, &buf);
		end_time = (uint64_t)buf.ru_utime.tv_sec*1000000.00 + buf.ru_utime.tv_usec;
		delay = end_time - start_time;
		printf("\n Execution of : %s\n", argv[prog_index]);
		printf("Simulated instructions = %d\n", inst_cnt);
        printf("Time = %f ms\n", (double)delay / 1000.00);
		printf("Rate = %f Mhz\n", ((double)inst_cnt / ((double)delay / 1000000.00)) / 1000000.00);
	}

	/* cleanup */
//	free_options(&options);
	/* this will also delete the associated state and the platform (if no one is locked on it) */
	gliss_delete_sim(sim);
	return 0;
}
