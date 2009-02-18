#include <stdio.h>
#include <ppc/api.h>
#include <../src/loader.h>





int main(int argc, char **argv) {
	ppc_platform_t *pf;
	ppc_sym_t s_it;
	Elf32_Shdr *s;
	Elf32_Shdr *s_tab;
	int nb_sect_disasm = 0;
	ppc_loader_t *loader;
	int i;

	
	/* test arguments */
	if(argc != 2) {
		fprintf(stderr, "ERROR: one argument required: the simulated program !\n");
		return 1;
	}
	
	/* we need a loader alone for sections */
	loader = ppc_loader_open(argv[1]);
	if (loader == NULL)
	{
		fprintf(stderr, "ERROR: cannot load the given executable : %s.", argv[1]);
		return 2;
	}

	printf("found %d sections in the executable %s\n", ppc_loader_count_sects(loader), argv[1]);
	s_tab = malloc(ppc_loader_count_sects(loader) * sizeof(Elf32_Shdr));
	
	s = ppc_loader_first_sect(loader, &s_it);
	printf("\ttype:%08X\tflag:%08X\taddr:%08X\tsize:%08X\n", s->sh_type, s->sh_flags, s->sh_addr, s->sh_size);
	while (s_it >= 0)
	{
		s = ppc_loader_next_sect(loader, &s_it);
		if (s)
		{
			/* if exec section, keep it to disasemble it */
			if ((s->sh_type == SHT_PROGBITS) && (s->sh_flags == (SHF_ALLOC | SHF_EXECINSTR)))
			{
				s_tab[nb_sect_disasm++] = *s;
				printf("[X]");
			}
			printf("\ttype:%08X\tflag:%08X\taddr:%08X\tsize:%08X\n", s->sh_type, s->sh_flags, s->sh_addr, s->sh_size);
		}
	}
printf("found %d section to disasemble\n", nb_sect_disasm);

	ppc_loader_close(loader);

	/* create the platform */
	pf = ppc_new_platform();
	if(pf == NULL) {
		fprintf(stderr, "ERROR: cannot create the platform.");
		return 1;
	}
	
	/* load it */
	if(ppc_load_platform(pf, argv[1]) == -1) { /* SIGSEGV ici */
		fprintf(stderr, "ERROR: cannot load %s\n", argv[1]);
		return 3;
	}
	
	/* CAUTION: C99 valid declarations, BUT C89 invalid */
	int i_sect;
	ppc_decoder_t *d = ppc_new_decoder(pf);
	
	for (i_sect = 0; i_sect<nb_sect_disasm; i_sect++)
	{
		ppc_address_t adr = s_tab[i_sect].sh_addr;
		/* here we assume an instr is always 4 bytes (work only with 4 byte RISC ISAs) */
		uint32_t nb_instr = s_tab[i_sect].sh_size / 4;
		uint32_t size_instr = 4;
		
		printf("\ndisasm new section, addr=%08X, size=%08X\n", s_tab[i_sect].sh_addr, s_tab[i_sect].sh_size);

		for (i=0; i<nb_instr; i++)
		{
			char buff[100];
			ppc_inst_t *inst = ppc_decode(d, adr);
			ppc_disasm(buff, inst);
			uint32_t code = ppc_mem_read32(ppc_get_memory(pf, 0), adr);
			printf("\t@ %08X => %08X => %s.\n", adr, code, buff);
			adr += size_instr;
		}
	}
	
	ppc_delete_decoder(d);

	/* delete the platform */
	ppc_unlock_platform(pf);
	fprintf(stderr, "SUCCESS !\n");

	return 0;
}

