/*
 * Copyright (c) 2010, IRIT - UPS <casse@irit.fr>
 *
 * This file is part of GLISS V2.
 *
 * GLISS V2 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GLISS V2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GLISS V2; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <inttypes.h>
#include <gliss/api.h>
#include <gliss/loader.h>
#include <gliss/config.h>

/**
 * Data structure for storing labels.
 */
typedef struct list_entry_t {
	const char *name;
	gliss_address_t addr;
	struct list_entry_t *next;
} list_entry_t;


/**
 * List of labels.
 */
static list_entry_t *labels = 0;


/**
 * Print list of labels.
 */
void print_list(list_entry_t *l) {
	fprintf(stderr, "printing list\n");
	list_entry_t *e = l;
	while(e) {
		fprintf(stderr, "\t\"%s\"\t%08X\n", e->name, e->addr);
		e = e->next;
	}
	fprintf(stderr, "end list.\n");
}


/**
 * Add a symbol to the label list.
 * @param m		List header.
 * @param n		Name of label.
 * @param a		Address of label.
 */
void add_to_list(list_entry_t **m, const char *n, gliss_address_t a) {

#	ifdef GLISS_PROCESS_CODE_LABEL
		{ GLISS_PROCESS_CODE_LABEL(a); }
#	endif

	/* build the node */
	list_entry_t *e = (list_entry_t *)malloc(sizeof(list_entry_t));
	if(e == 0) {
		fprintf(stderr, "ERROR: malloc failed\n");
		return;
	}
	e->name = n;
	e->addr = a;
	e->next = 0;

	/* find the position */
	list_entry_t *cur = *m;
	list_entry_t **prev = m;
	while(cur && cur->addr < a) {
		prev = &cur->next;
		cur = cur->next;
	}

	/* insert the node */
	*prev = e;
	if(cur)
		e->next = cur;
}


/**
 * Get the label name associated with an address
 * @param	m	the sorted list to search within
 * @para	addr	the address whose label (if any) is wanted
 * @param	name	will point to the name if a label exists, NULL otherwise
 * @return	0 if no label exists for the given address, non zero otherwise
*/
int get_label_from_list(list_entry_t *m, gliss_address_t addr, const char **name) {

	/* find the entry */
	list_entry_t *e = m;
	while(e && e->addr < addr)
		e = e->next;

	/* found ? */
	if(e && e->addr == addr) {
		*name = e->name;
		return 1;
	}

	/* not found */
	else {
		*name = 0;
		return 0;
	}
}


/**
 * Get the closer name associated with an address
 * @param	m	the sorted list to search within
 * @para	addr	the address whose label (if any) is wanted
 * @return	Closer entry associated with address or null.
*/
list_entry_t *get_closer_label_from_list(list_entry_t *m, gliss_address_t addr) {
	list_entry_t *e = m, *prev = 0;
	while(e && e->addr <= addr) {
		prev = e;
		e = e->next;
	}
	return prev;
}


/**
 * Destroy the label list.
 * @param m		Label list header.
 */
void destroy_list(list_entry_t *m) {
	list_entry_t *cur = m;
	while(cur) {
		list_entry_t *next = cur->next;
		free(cur);
		cur = next;
	}
}


/**
 * Called when the option parsing fails.
 * @param msg	Formatted string of the message.
 * @param ...	Free arguments.
 */
static void fail_with_help(const char *msg, ...) {
	va_list args;

	/* display syntax */
	fprintf(stderr, "SYNTAX: disasm ");
	if(gliss_modes[0].name)
		fprintf(stderr, "[-m MODE] ");
	fprintf(stderr, "EXECUTABLE\n");

	/* display modes */
	if(gliss_modes[0].name) {
		int i, fst = 1;
		fprintf(stderr, "MODE may be one of ");
		for(i = 0; gliss_modes[i].name; i++) {
			if(!fst)
				fprintf(stderr, ", ");
			else
				fst = 0;
			fputs(gliss_modes[i].name, stderr);
		}
		fprintf(stderr, "\n");
	}

	/* display error message */
	fprintf(stderr, "\nERROR: ");
	va_start(args, msg);
	vfprintf(stderr, msg, args);
	va_end(args);
	fprintf(stderr, "\n");
	exit(1);
}


/**
 * Convert an address to a label if available.
 * @param address	Address to get label for.
 * @return			Result string of conversion.
 */
char *gliss_solve_label_disasm(gliss_address_t address) {
	static char buf[256];
	list_entry_t *lab = get_closer_label_from_list(labels, address);
	if(!lab)
		sprintf(buf, "%08x", address);
	else if(lab->addr == address)
		snprintf(buf, sizeof(buf), "%08x <%s>", address, lab->name);
	else
		snprintf(buf, sizeof(buf), "%08x <%s+0x%x>", address, lab->name, address - lab->addr);
	return buf;
}


/**
 * Disassembly entry point.
 */
int main(int argc, char **argv) {
	gliss_platform_t *pf;
	int s_it;
	gliss_loader_sect_t *s_tab;
	int sym_it;
	int nb_sect_disasm = 0;
	gliss_loader_t *loader;
	int max_size = 0, i, j;
	char *exe_path = 0;
	gliss_inst_t *(*decode)(gliss_decoder_t *decoder, gliss_address_t address) = gliss_decode;
	int i_sect;
	gliss_decoder_t *d;

	/* test arguments */
	for(i = 1; i < argc; i++) {
		if(gliss_modes[0].name && strcmp(argv[i], "-m") == 0) {
			i++;
			if(i >= argc)
				fail_with_help("no argument for -m option");
			for(j = 0; gliss_modes[j].name; j++)
				if(strcmp(argv[i], gliss_modes[j].name) == 0) {
					decode = gliss_modes[j].decode;
					break;
				}
			if(!gliss_modes[j].name)
				fail_with_help("no mode named %s", argv[i]);
		}
		else if(argv[i][0] == '-')
			fail_with_help("unknown option %s", argv[i]);
		else if(exe_path)
			fail_with_help("several executable paths given");
		else
			exe_path = argv[i];
	}
	if(!exe_path)
		fail_with_help("no executable path given!");

	/* we need a loader alone for sections */
	loader = gliss_loader_open(exe_path);
	if (loader == NULL) {
		fprintf(stderr, "ERROR: cannot load the given executable : %s.\n", exe_path);
		return 2;
	}

	/* display sections */
	printf("found %d sections in the executable %s\n", gliss_loader_count_sects(loader)-1, exe_path);
	s_tab = (gliss_loader_sect_t *)malloc(gliss_loader_count_sects(loader) * sizeof(gliss_loader_sect_t));
	for(s_it = 0; s_it < gliss_loader_count_sects(loader); s_it++) {
		gliss_loader_sect_t data;
		gliss_loader_sect(loader, s_it, &data);
		if(data.type == GLISS_LOADER_SECT_TEXT) {
			s_tab[nb_sect_disasm++] = data;
			printf("[X]");
		}
		printf("\t%20s\ttype:%08x\taddr:%08x\tsize:%08x\n", data.name, data.type, data.addr, data.size);
	}
	printf("found %d sections to disasemble\n", nb_sect_disasm);

	/* display symbols */
	printf("\nfound %d symbols in the executable %s\n", gliss_loader_count_syms(loader)-1, exe_path);
	for(sym_it = 0; sym_it < gliss_loader_count_syms(loader); sym_it++) {
		gliss_loader_sym_t data;
		gliss_loader_sym(loader, sym_it, &data);
		if(data.type == GLISS_LOADER_SYM_CODE || data.type == GLISS_LOADER_SYM_DATA) {
			printf("[L]");
			add_to_list(&labels, data.name, data.value);
		}
		printf("\t%20s\tvalue:%08X\tsize:%08X\tinfo:%08X\tshndx:%08X\n", data.name, data.value, data.size, data.type, data.sect);
	}

	/* configure disassembly */
	gliss_solve_label = gliss_solve_label_disasm;

	/* create the platform */
	pf = gliss_new_platform();
	if(pf == NULL) {
		fprintf(stderr, "ERROR: cannot create the platform.");
		destroy_list(labels);
		return 1;
	}

	/* load it */
	gliss_loader_load(loader, pf);

	d = gliss_new_decoder(pf);
	/* multi iss part, TODO: improve */
	gliss_state_t *state = gliss_new_state(pf);
	/* not really useful as select condition for instr set will never change as we don't execute here,
	 * changing instr set should be done manually by manipulating state */
	gliss_set_cond_state(d, state);

	/* compute instruction max size */
	for(i = 1; i < GLISS_TOP; i++) {
		int size = gliss_get_inst_size_from_id(i) / 8;
		if(size > max_size)
			max_size = size;
	}

	/* disassemble the sections */
	for(i_sect = 0; i_sect<nb_sect_disasm; i_sect++) {

		/* display new section */
		gliss_address_t adr_start = s_tab[i_sect].addr;
		gliss_address_t adr_end = s_tab[i_sect].addr + s_tab[i_sect].size;
		gliss_address_t prev_addr = 0;
		printf("\ndisasm new section, addr=%08x, size=%08x\n", s_tab[i_sect].addr, s_tab[i_sect].size);

		/* traverse all instructions */
		while (adr_start < adr_end) {

			/* disassemble instruction */
			int size;
			char buff[100];
			gliss_inst_t *inst = decode(d, adr_start);
			gliss_disasm(buff, inst);
			const char *n;

			/* display label */
			if(get_label_from_list(labels, adr_start, &n)) {
				printf("\n%08x <%s>\n", adr_start, n);
				prev_addr = adr_start;
			}

			/* display the address */
			if((prev_addr & 0xffff0000) == (adr_start & 0xffff0000))
				printf("    %04x:\t", adr_start & 0x0000ffff);
			else
				printf("%08x:\t", adr_start);
			prev_addr = adr_start;

			/* display the instruction bytes */
			size = gliss_get_inst_size(inst) / 8;
			for(i = 0; i < max_size; i++) {
				if(i < size)
					printf("%02x", gliss_mem_read8(gliss_get_memory(pf, 0), adr_start + i));
				else
					fputs("  ", stdout);
			}

			/* displat the instruction */
			printf("\t%s\n", buff);
			/* inst size is given in bit, we want it in byte */
			adr_start += size;
		}
	}

	/* cleanup */
	gliss_delete_decoder(d);
	gliss_unlock_platform(pf);
	destroy_list(labels);

	return 0;
}
