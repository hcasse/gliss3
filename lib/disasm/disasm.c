/*
 * $Id: disasm.c,v 1.1 2009/02/25 17:30:25 casse Exp $
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

#define gliss_inst_size(inst) ((inst)->instrinput[1].val.size)

typedef struct list_entry_t
{
	char *name;
	gliss_address_t addr;
	struct list_entry_t *next;
} list_entry_t;

void add_to_list(list_entry_t **m, char *n, gliss_address_t a)
{
	list_entry_t *e = (list_entry_t *)malloc(sizeof(list_entry_t));
	if (e == 0)
	{
		fprintf(stderr, "ERROR: malloc failed\n");
	}
	e->name = n;
	e->addr = a;
	e->next = 0;

	if (*m == 0)
	{
		*m = e;
		return;
	}

	list_entry_t *t1 = *m;
	list_entry_t *t2 = 0;

	while (t1)
	{
		if (a >= t1->addr)
		{
			if (t1->next)
			{
				if (t1->next->addr > a)
				{
					/* we found the right place */
					t2 = t1->next;
					t1->next = e;
					e->next = t2;
					break;
				}
				else
					/* let's see the next one */
					t1 = t1->next;
			}
			else
			{
				/* insertion in end of list */
				t1->next = e;
				break;
			}
		}
		else
		{
			/* this case should occur only when testing the first entry of a list */
			*m = e;
			e->next = t1;
			break;
		}
	}
}

void print_list(list_entry_t *l)
{
	printf("printing list\n");

	list_entry_t *e = l;
	while (e)
	{
		printf("\t\"%s\"\t%08X\n", e->name, e->addr);
		if (e->next)
			e = e->next;
		else
			break;
	}
	printf("end list.\n");
}

/**
 * Get the label name associated with an address
 * @param	m	the sorted list to search within
 * @para	addr	the address whose label (if any) is wanted
 * @param	name	will point to the name if a label exists, NULL otherwise
 * @return	0 if no label exists for the given address, non zero otherwise
*/
int get_label_from_list(list_entry_t *m, gliss_address_t addr, char **name)
{
	list_entry_t *e = m;
	while (e)
	{
		if (e->addr > addr)
		{
			*name = 0;
			return 0;
		}
		
		if (e->addr == addr)
		{
			*name = e->name;
			return 1;
		}
		
		if (e->next)
			e = e->next;
		else
			break;
	}
	return 0;
}

void destroy_list(list_entry_t *m)
{
	if (m == 0)
		return;
	list_entry_t *t1 = m;
	list_entry_t *t2 = 0;
	while (t1->next)
	{
		t2 = t1;
		t1 = t1->next;
		free(t2);
	}
	free(t1);
}

int main(int argc, char **argv) {
	gliss_platform_t *pf;
	gliss_sect_t s_it;
	Elf32_Shdr *s;
	Elf32_Shdr *s_tab;
	gliss_sym_t sym_it;
	Elf32_Sym *sym;
	int nb_sect_disasm = 0;
	gliss_loader_t *loader;

	
	/* test arguments */
	if(argc != 2) {
		fprintf(stderr, "ERROR: one argument required: the simulated program !\n");
		return 1;
	}
	
	/* we need a loader alone for sections */
	loader = gliss_loader_open(argv[1]);
	if (loader == NULL)
	{
		fprintf(stderr, "ERROR: cannot load the given executable : %s.\n", argv[1]);
		return 2;
	}

	printf("found %d sections in the executable %s\n", gliss_loader_count_sects(loader)-1, argv[1]);
	s_tab = (Elf32_Shdr*)malloc(gliss_loader_count_sects(loader) * sizeof(Elf32_Shdr));
	s = gliss_loader_first_sect(loader, &s_it);
	while (s_it >= 0)
	{
		s = gliss_loader_next_sect(loader, &s_it);
		if (s)
		{
			/* if exec section, keep it to disasemble it */
			if ((s->sh_type == SHT_PROGBITS) && (s->sh_flags == (SHF_ALLOC | SHF_EXECINSTR)))
			{
				s_tab[nb_sect_disasm++] = *s;
				printf("[X]");
			}
			printf("\t%20s\ttype:%08X\tflag:%08X\taddr:%08X\tsize:%08X\n", gliss_loader_name_of_sect(loader, s_it), s->sh_type, s->sh_flags, s->sh_addr, s->sh_size);
		}
	}
	printf("found %d sections to disasemble\n", nb_sect_disasm);

	printf("\nfound %d symbols in the executable %s\n", gliss_loader_count_syms(loader)-1, argv[1]);
	list_entry_t *list_labels = 0;
	sym = gliss_loader_first_sym(loader, &sym_it);
	while (sym_it >= 0)
	{
		sym = gliss_loader_next_sym(loader, &sym_it);
		if (sym)
		{
			uint8_t st_type = sym->st_info & 0x0f;
			if ((st_type == 2) || (st_type == 1)) /* STT_FUNC or STT_OBJECT */
			{
				printf("[L]");
				add_to_list(&list_labels, gliss_loader_name_of_sym(loader, sym_it), sym->st_value);
			}
			printf("\t%20s\tvalue:%08X\tsize:%08X\tinfo:%08X\tshndx:%08X\n", gliss_loader_name_of_sym(loader, sym_it), sym->st_value, sym->st_size, sym->st_info, sym->st_shndx);
			
		}
	}

	/* create the platform */
	pf = gliss_new_platform();
	if(pf == NULL) {
		fprintf(stderr, "ERROR: cannot create the platform.");
		destroy_list(list_labels);
		return 1;
	}
	
	/* load it */
	gliss_loader_load(loader, gliss_get_memory(pf, GLISS_MAIN_MEMORY));
	
	/* CAUTION: C99 valid declarations, BUT C89 invalid */
	int i_sect;
	gliss_decoder_t *d = gliss_new_decoder(pf);
	
	/* disassemble the sections */
	for (i_sect = 0; i_sect<nb_sect_disasm; i_sect++)
	{
		gliss_address_t adr_start = s_tab[i_sect].sh_addr;
		gliss_address_t adr_end = s_tab[i_sect].sh_addr + s_tab[i_sect].sh_size;
		
		printf("\ndisasm new section, addr=%08X, size=%08X\n", s_tab[i_sect].sh_addr, s_tab[i_sect].sh_size);

		while (adr_start < adr_end)
		{
			char buff[100];
			gliss_inst_t *inst = gliss_decode(d, adr_start);
			gliss_disasm(buff, inst);
			uint32_t code = gliss_mem_read32(gliss_get_memory(pf, 0), adr_start);
			char *n;
			if (get_label_from_list(list_labels, adr_start, &n))
				printf("\n%08X <%s>\n", adr_start, n);
			printf("%08X:\t%08X\t%s.\n", adr_start, code, buff);
			/* inst size is given in bit, we want it in byte */
			adr_start += gliss_inst_size(inst) / 8;
		}
	}
	
	gliss_delete_decoder(d);
	gliss_unlock_platform(pf);
	destroy_list(list_labels);

	return 0;
}
