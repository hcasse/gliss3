#include <stdio.h>
#include <ppc/api.h>
#include <../src/loader.h>

int main(int argc, char **argv) {
	ppc_platform_t *pf;
	ppc_sym_t s_it;
	Elf32_Sym *s;
	
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
	
	/*ppc_fetch_t *f = ppc_new_fetch(pf);*/
	/* the exe i have tested have their code beginnin at this address */
	/*ppc_address_t a = 0x10000080;
	int i;
	for (i=0; i<100; i++)
	{
		int n = ppc_fetch(f, a);
		uint32_t code = ppc_mem_read32(ppc_get_memory(pf, 0), a);
		//printf("@ %08X => %08X => %d\t\t%s.\n", a, code, n, ppc_get_string_ident(n));
		a += 4;
	}
	ppc_delete_fetch(f);
	
	printf("\ntesting decode\n");
	ppc_decoder_t *d = ppc_new_decoder(pf);
	a = 0x10000080;
	for (i=0; i<100; i++)
	{
		char buff[100];
		ppc_inst_t *inst = ppc_decode(d, a);
		ppc_disasm(buff, inst);
		uint32_t code = ppc_mem_read32(ppc_get_memory(pf, 0), a);
		printf("@ %08X => %08X => %s.\n", a, code, buff);
		a += 4;
	}
	ppc_delete_decoder(d);*/
	
	/* dump symbol table */

	printf("dump symbol table\n");
	s = gliss_loader_first_sym()
	
	
	/* delete the platform */
	ppc_unlock_platform(pf);
	fprintf(stderr, "SUCCESS !\n");
	return 0;
}

