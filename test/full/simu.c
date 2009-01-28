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
	
	ppc_fetch_t *f = ppc_new_fetch(pf);
	/* the exe i have tested have their code beginnin at this address */
	ppc_address_t a = 0x10000080;
	int i;
	for (i=0; i<100; i++)
	{
		int n = ppc_fetch(f, a);
		uint32_t code = ppc_mem_read32(ppc_get_memory(pf, 0), a);
		printf("@ %08X => %08X => %d\t\t%s.\n", a, code, n, ppc_string_ident[n]);
		a += 4;
	}

	ppc_delete_fetch(f);

	printf("testing decode");
	
	/* delete the platform */
	ppc_unlock_platform(pf);
	fprintf(stderr, "SUCCESS !\n");
	return 0;
}

/* decoding */
ppc_decoder_t *ppc_new_decoder(ppc_platform_t *state);
void ppc_delete_decoder(ppc_decoder_t *fetch);
ppc_inst_t *ppc_decode(ppc_decoder_t *decoder, ppc_address_t address);
void ppc_free_inst(ppc_inst_t *inst);
