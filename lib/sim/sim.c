/*
 * $Id: sim.c,v 1.6 2009/04/30 13:36:50 barre Exp $
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
	fprintf(stderr, "Syntax is: %s [-start=<start_address>] [-exit=<exit_address>] <executable_name>\n", prog_name);
}


int main(int argc, char **argv) {
	gliss_sim_t *sim;
	gliss_state_t *state;
	gliss_platform_t *platform;
	gliss_loader_t *loader;
	gliss_address_t addr_start;
	gliss_address_t addr_exit;
	char *c_ptr;
	int is_start_given;
	int is_exit_given;
	int prog_index;
	Elf32_Sym *Elf_sym;
	gliss_sym_t sym_exit;
	int i;

	
	/* test argument count */
	if ((argc < 2) || (argc > 4)) {
		fprintf(stderr, "Error: too many or too few arguments\n");
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
		if (addr_start == 0)
		{
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
		if (addr_exit == 0)
		{
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
	if (prog_index == 0)
	{
		fprintf(stderr, "ERROR: no program name given\n");
		syntax(argv[0]);
		return 2;
	}


	/* find the _exit symbol if no exit address is given */
	if ( ! is_exit_given)
	{	
		addr_exit = 0;

		/* open the file */
		loader = gliss_loader_open(argv[prog_index]);
		if(loader == NULL) {
			fprintf(stderr, "ERROR: no more resources\n");
			return 2;
		}
		
		/* search symbol _exit */
		Elf_sym = gliss_loader_first_sym(loader, &sym_exit);
		while (sym_exit >= 0)
		{
			Elf_sym = gliss_loader_next_sym(loader, &sym_exit);
			if (Elf_sym)
			{
				c_ptr = gliss_loader_name_of_sym(loader, sym_exit);
				if (strcmp(c_ptr, "_exit") == 0)
				{
					/* we found _exit */
					addr_exit = Elf_sym->st_value;
					break;
				}
			}
		}
		
		/* check for error */
		if (addr_exit == 0)
		{
			fprintf(stderr, "ERROR: cannot find the \"_exit\" symbol and no exit address is given.\n");
			syntax(argv[0]);
			return 2;
		}

		/* close the file */
		gliss_loader_close(loader);
		
		// !!DEBUG!!
		printf("_exit found at %08X\n", addr_exit);
	}
	
	
	/* make the platform */
	platform = gliss_new_platform();
	if(platform == NULL)  {
		fprintf(stderr, "ERROR: no more resources\n");
		return 2;
	}
	
	/* load the image in the platform */
	if (gliss_load_platform(platform, argv[prog_index]) == -1) {
		fprintf(stderr, "ERROR: cannot load the given executable : %s.\n", argv[i]);
		syntax(argv[0]);
		return 2;
	}

	/* make the state depending on the platform */
	state = gliss_new_state(platform);
	if (state == NULL)  {
		fprintf(stderr, "ERROR: no more resources\n");
		return 2;
	}
	
	/* make the simulator */
	sim = gliss_new_sim(state, addr_start, addr_exit);
	if (sim == NULL) {
		fprintf(stderr, "ERROR: no more resources\n");
		return 2;
	}
	
// !!DEBUG BEGIN!!
	printf("entry point given by the loader to the platform then to the state NIA: %08X\n", state->NIA);
//	printf("state before simulation\n");
//	gliss_dump_state(sim->state, stdout);
	int cpt=0;
// !!DEBUG END!!

	/* perform the simulation */
	while(1)
	{
		if (gliss_is_sim_ended(sim))
			break;
		gliss_step(sim);
// !!DEBUG BEGIN!!	
	cpt++;
	printf("\nstate after instr %d\n", cpt);
	gliss_dump_state(sim->state, stdout);
// !!DEBUG END!!	
	}
	
// !!DEBUG BEGIN!!	
//	printf("\nstate after simulation\n");
//	gliss_dump_state(sim->state, stdout);
// !!DEBUG END!!	
	
	/* cleanup */
	/* this will also delete the associated state and the platform (if no one is locked on it) */
	gliss_delete_sim(sim);
	return 0;
}

