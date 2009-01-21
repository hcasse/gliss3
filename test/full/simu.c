#include <stdio.h>
#include <ppc/api.h>

int main(int argc, char **argv) {
	ppc_platform_t *pf;
	
	/* test arguments */
	if(argc != 2) {
		fprintf(stderr, "ERROR: one argument required: the simulated program !\n");
		return 2;
	}
	
	/* create the platform */
	pf = ppc_new_platform();
	if(pf == NULL) {
		fprintf(stderr, "ERROR: cannot create the platform.");
		return 1;
	}
	
	/* load it */
	if(ppc_load_platform(pf, argv[1]) == -1) {
		fprintf(stderr, "ERROR: cannot load %s\n", argv[1]);
		return 3;
	}
	
	/* delete the platform */
	ppc_unlock_platform(pf);
	fprintf(stderr, "SUCCESS !\n");
	return 0;
}
