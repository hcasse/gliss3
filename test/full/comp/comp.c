#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ppc/api.h>
#include <loader.h>
#include "iss_include.h"

extern char **environ;

/*** GLISS2 simulator ***/
ppc_sim_t *sim = 0;
ppc_state_t *state = 0;
ppc_platform_t *platform = 0;
ppc_loader_t *loader = 0;

int gliss2_prepare(int argc, char **argv) {
	ppc_address_t addr_start = 0;
	ppc_address_t addr_exit = 0;
	char *c_ptr = 0;
	Elf32_Sym *Elf_sym = 0;
	ppc_sym_t sym_exit = 0, sym_start = 0;

	/* open the exec file */
	loader = ppc_loader_open(argv[1]);
	if(loader == NULL) {
		fprintf(stderr, "ERROR: no more resources\n");
		return 2;
	}

	/* find the _start symbol if no start address is given */
	addr_start = 0;
	Elf_sym = ppc_loader_first_sym(loader, &sym_start);
	while (sym_start >= 0)
	{
		Elf_sym = ppc_loader_next_sym(loader, &sym_start);
		if (Elf_sym)
		{
			c_ptr = ppc_loader_name_of_sym(loader, sym_start);
			if (strcmp(c_ptr, "_start") == 0)
			{
				/* we found _start */
				addr_start = Elf_sym->st_value;
				break;
			}
		}
	}

	/* start address found ? */
	if (addr_start == 0) {
		fprintf(stderr, "ERROR: cannot find the \"_start\" symbol and no start address is given.\n");
		return 2;
	}
	else
		printf("_start found at %08X\n", addr_start);

	/* find the _exit symbol if no exit address is given */
	addr_exit = 0;
	Elf_sym = ppc_loader_first_sym(loader, &sym_exit);
	while (sym_exit >= 0)
	{
		Elf_sym = ppc_loader_next_sym(loader, &sym_exit);
		if (Elf_sym)
		{
			c_ptr = ppc_loader_name_of_sym(loader, sym_exit);
			if (strcmp(c_ptr, "_exit") == 0)
			{
				/* we found _exit */
				addr_exit = Elf_sym->st_value;
				break;
			}
		}
	}

	/* exit address found ? */
	if (addr_exit == 0) {
		fprintf(stderr, "ERROR: cannot find the \"_exit\" symbol and no exit address is given.\n");
		return 2;
	}
	else
		printf("_exit found at %08X\n", addr_exit);

	/* cleanup first opening */
	/*ppc_loader_close(loader);*/

	/* make the platform */
	platform = ppc_new_platform();
	if(platform == NULL)  {
		fprintf(stderr, "ERROR: no more resources\n");
		return 2;
	}

	/* load the image in the platform */
	ppc_loader_load(loader, ppc_get_memory(platform, PPC_MAIN_MEMORY));

	/* make the state depending on the platform */
	state = ppc_new_state(platform);
	if (state == NULL)  {
		fprintf(stderr, "ERROR: no more resources\n");
		return 2;
	}

	/* make the simulator */
	sim = ppc_new_sim(state, addr_start, addr_exit);
	if (sim == NULL) {
		fprintf(stderr, "ERROR: no more resources\n");
		return 2;
	}

	/* prepare process configuration */
	ppc_env_t env;
	env.argc = argc - 1;
	env.argv = argv + 1;
	env.argv_addr = 0;
	env.envp = environ;
	env.envp_addr = 0;
	env.auxv = 0;
	env.auxv_addr = 0;
	env.stack_pointer = 0;

	/* system initialization (argv, env , auxv) */
	ppc_stack_fill_env(loader, ppc_get_memory(platform, PPC_MAIN_MEMORY), &env);
	ppc_registers_fill_env(&env, state);
	ppc_loader_close(loader);
	return 0;
}

void gliss2_cleanup(void) {
	ppc_delete_sim(sim);
}


/*** GLISS1 simulator ***/
state_t *real_state;

void prepare_gliss1(int argc, char **argv, char **envp) {
    code_t buff_instr[20];
    long int instruction_number;
    void *system_list[3];
    void *loader_list[4];
    int page_size_system;
    FILE *verbose_system;
    char * libs_path[]=	{
    	"/home/specmgr/Compilateur/powerpc/target/powerpc-linux-gnu/lib",
    	 NULL
    };
    int nb_bits=0; /* 0 => par defaut */
    int nb_mem=0;  /* 0 => par defaut */
    void *mem_list[2+1];

    page_size_system=4096;
    verbose_system=NULL;

    /* Set System Parameters (Stack...) */
	system_list[0] = &page_size_system;
    system_list[1] = verbose_system;
    system_list[2] = NULL;

    /* Loader args */
    loader_list[0]=&(argv[1]);   	/* pointe sur les parametres commencant par
                                       le nom du prog */
    loader_list[1]=envp;            /* environnement */
    loader_list[2]="../../gel/src"; /* gel plugin path */
    loader_list[3]=libs_path;

    /* Memory args */
    mem_list[0]=&nb_mem;
    mem_list[1]=&nb_bits;
    mem_list[2]=NULL;

    /* Init Emulator */
    real_state=iss_init(mem_list,loader_list,system_list,NULL,NULL);
}


/*** main program ***/
int main(int argc, char **argv)
{
	int res;

	/* test argument count */
	assert(argc >= 2);

	/* preparation */
	res = gliss2_prepare(argc, argv);
	if(res != 0)
		return res;

	/* perform the simulation */
	while(!ppc_is_sim_ended(sim))
		ppc_step(sim);

	/* cleanup */
	gliss2_cleanup();
	return 0;
}
