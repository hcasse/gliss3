#include <stdio.h>
#include <ppc/api.h>

int main(void) {
	ppc_platform_t *pf;
	
	/* create the platform */
	pf = ppc_new_platform();
	if(pf == NULL) {
		fprintf(stderr, "ERROR: cannot create the platform.");
		return 1;
	}
	
	/* delete the platform */
	ppc_unlock_platform(pf);
	fprintf(stderr, "SUCCESS !\n");
	return 0;
}
