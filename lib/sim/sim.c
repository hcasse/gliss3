/*
 * $Id: sim.c,v 1.5 2009/04/28 12:40:43 barre Exp $
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
#include <gliss/api.h>
#include <loader.h>


int main(int argc, char **argv) {
	gliss_sim_t *sim;
	gliss_state_t *state;
	gliss_platform_t *platform;
	int i;

	
	/* test arguments */
	if(argc != 2) {
		fprintf(stderr, "ERROR: one argument required: the simulated program !\n");
		return 1;
	}
	
	/* make the platform */
	platform = gliss_new_platform();
	if(platform == NULL)  {
		fprintf(stderr, "ERROR: no more resources\n");
		return 2;
	}
	
	/* load the image in the platform */
	if(gliss_load_platform(platform, argv[1]) == -1) {
		fprintf(stderr, "ERROR: cannot load the given executable : %s.\n", argv[1]);
		return 2;
	}
	
	/* make the state depending on the platform */
	state = gliss_new_state(platform);
	if(state == NULL)  {
		fprintf(stderr, "ERROR: no more resources\n");
		return 2;
	}
	
	/* make the simulator */
	sim = gliss_new_sim(state);
	if(sim == NULL) {
		fprintf(stderr, "ERROR: no more resources\n");
		return 2;
	}
	
	printf("state before simulation\n");
	gliss_dump_state(sim->state, stdout);

int cpt=0;

	/* perform the simulation */
	while(1)
	{
		if (gliss_is_sim_ended(sim))
			break;
		gliss_step(sim);
	
	cpt++;
	printf("\nstate after instr %d\n", cpt);
	gliss_dump_state(sim->state, stdout);
	}
	
	printf("\nstate after simulation\n");
	gliss_dump_state(sim->state, stdout);
	
	/* cleanup */
	/* this will also delete the associated state and the platform (if no one is locked on it) */
	gliss_delete_sim(sim);
	/*gliss_delete_state(state);*/
	return 0;
}

