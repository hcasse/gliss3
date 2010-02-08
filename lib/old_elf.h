/*
 *	$Id: old_elf.h,v 1.10 2009/07/21 13:17:58 barre Exp $
 *	old_elf module interface
 *
 *	This file is part of OTAWA
 *	Copyright (c) 2008, IRIT UPS.
 *
 *	GLISS is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	GLISS is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with OTAWA; if not, write to the Free Software
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#ifndef GLISS_OLD_ELF_H
#define GLISS_OLD_ELF_H

#include "grt.h"
#include "../include/gliss/mem.h"
#include "../include/gliss/api.h"

#if defined(__cplusplus)
    extern  "C" {
#endif

#define GLISS_LOADER_STATE
#define GLISS_LOADER_INIT(s)
#define GLISS_LOADER_DESTROY(s)

/* ELF definitions */
typedef uint32_t Elf32_Addr;
typedef uint16_t Elf32_Half;
typedef uint32_t Elf32_Off;
typedef uint16_t Elf32_Section;
typedef uint32_t Elf32_Word;

#define EI_MAG0		0		/* File identification byte 0 index */
#define ELFMAG0		0x7f		/* Magic number byte 0 */

#define EI_MAG1		1		/* File identification byte 1 index */
#define ELFMAG1		'E'		/* Magic number byte 1 */

#define EI_MAG2		2		/* File identification byte 2 index */
#define ELFMAG2		'L'		/* Magic number byte 2 */

#define EI_MAG3		3		/* File identification byte 3 index */
#define ELFMAG3		'F'		/* Magic number byte 3 */

/* Conglomeration of the identification bytes, for easy testing as a word.  */
#define	ELFMAG		"\177ELF"
#define	SELFMAG		4

#define EI_CLASS	4		/* File class byte index */
#define ELFCLASSNONE	0		/* Invalid class */
#define ELFCLASS32	1		/* 32-bit objects */
#define ELFCLASS64	2		/* 64-bit objects */

#define EI_DATA		5		/* Data encoding byte index */
#define ELFDATANONE	0		/* Invalid data encoding */
#define ELFDATA2LSB	1		/* 2's complement, little endian */
#define ELFDATA2MSB	2		/* 2's complement, big endian */

#define EI_VERSION	6		/* File version byte index */
					/* Value must be EV_CURRENT */

#define EI_PAD		7		/* Byte index of padding bytes */

/* Legal values for e_type (object file type).  */

#define ET_NONE		0		/* No file type */
#define ET_REL		1		/* Relocatable file */
#define ET_EXEC		2		/* Executable file */
#define ET_DYN		3		/* Shared object file */
#define ET_CORE		4		/* Core file */
#define	ET_NUM		5		/* Number of defined types.  */
#define ET_LOPROC	0xff00		/* Processor-specific */
#define ET_HIPROC	0xffff		/* Processor-specific */

#define SHT_NULL	0		/* Section header table entry unused */
#define SHT_PROGBITS	1		/* Program data */
#define SHT_SYMTAB	2		/* Symbol table */
#define SHT_STRTAB	3		/* String table */
#define SHT_RELA	4		/* Relocation entries with addends */
#define SHT_HASH	5		/* Symbol hash table */
#define SHT_DYNAMIC	6		/* Dynamic linking information */
#define SHT_NOTE	7		/* Notes */
#define SHT_NOBITS	8		/* Program space with no data (bss) */
#define SHT_REL		9		/* Relocation entries, no addends */
#define SHT_SHLIB	10		/* Reserved */
#define SHT_DYNSYM	11		/* Dynamic linker symbol table */
#define	SHT_NUM		12		/* Number of defined types.  */

#define SHF_WRITE	(1 << 0)	/* Writable */
#define SHF_ALLOC	(1 << 1)	/* Occupies memory during execution */
#define SHF_EXECINSTR	(1 << 2)	/* Executable */
#define SHF_MASKPROC	0xf0000000	/* Processor-specific */

typedef struct
{
  Elf32_Word	sh_name;		/* Section name (string tbl index) */
  Elf32_Word	sh_type;		/* Section type */
  Elf32_Word	sh_flags;		/* Section flags */
  Elf32_Addr	sh_addr;		/* Section virtual addr at execution */
  Elf32_Off		sh_offset;		/* Section file offset */
  Elf32_Word	sh_size;		/* Section size in bytes */
  Elf32_Word	sh_link;		/* Link to another section */
  Elf32_Word	sh_info;		/* Additional section information */
  Elf32_Word	sh_addralign;		/* Section alignment */
  Elf32_Word	sh_entsize;		/* Entry size if section holds table */
} Elf32_Shdr;

typedef struct
{
  Elf32_Word	st_name;		/* Symbol name (string tbl index) */
  Elf32_Addr	st_value;		/* Symbol value */
  Elf32_Word	st_size;		/* Symbol size */
  unsigned char	st_info;		/* Symbol type and binding */
  unsigned char	st_other;		/* No defined meaning, 0 */
  Elf32_Section	st_shndx;		/* Section index */
} Elf32_Sym;

typedef struct
{
  Elf32_Word	p_type;			/* Segment type */
  Elf32_Off		p_offset;		/* Segment file offset */
  Elf32_Addr	p_vaddr;		/* Segment virtual address */
  Elf32_Addr	p_paddr;		/* Segment physical address */
  Elf32_Word	p_filesz;		/* Segment size in file */
  Elf32_Word	p_memsz;		/* Segment size in memory */
  Elf32_Word	p_flags;		/* Segment flags */
  Elf32_Word	p_align;		/* Segment alignment */
} Elf32_Phdr;

#define EI_NIDENT (16)
typedef struct
{
  unsigned char	e_ident[EI_NIDENT];	/* Magic number and other info */
  Elf32_Half	e_type;			/* Object file type */
  Elf32_Half	e_machine;		/* Architecture */
  Elf32_Word	e_version;		/* Object file version */
  Elf32_Addr	e_entry;		/* Entry point virtual address */
  Elf32_Off	    e_phoff;		/* Program header table file offset */
  Elf32_Off	    e_shoff;		/* Section header table file offset */
  Elf32_Word	e_flags;		/* Processor-specific flags */
  Elf32_Half	e_ehsize;		/* ELF header size in bytes */
  Elf32_Half	e_phentsize;	/* Program header table entry size */
  Elf32_Half	e_phnum;		/* Program header table entry count */
  Elf32_Half	e_shentsize;	/* Section header table entry size */
  Elf32_Half	e_shnum;		/* Section header table entry count */
  Elf32_Half	e_shstrndx;		/* Section header string table index */
} Elf32_Ehdr;


/* gliss_loader_t type */
typedef struct gliss_loader_t gliss_loader_t;


/* loader management */
gliss_loader_t *gliss_loader_open(const char *path);
void gliss_loader_close(gliss_loader_t *loader);
void gliss_loader_load(gliss_loader_t *loader, gliss_memory_t *memory);
gliss_address_t gliss_loader_start(gliss_loader_t *loader);


/* stack and environment */

/* auxiliary vector */
/* struct's size = 2 words (64 bits) */
typedef struct auxv_t {
	int	a_type;
	union {
		long a_val;
		void *a_ptr;
		void (*a_fcn)();
	} a_un;
} auxv_t;

 /* auxiliary vector types */
 #define AT_NULL			0
 #define AT_IGNORE		1
 #define AT_EXECFD		2
 #define AT_PHDR			3
 #define AT_PHENT		4
 #define AT_PHNUM		5
 #define AT_PAGESZ		6
 #define AT_BASE			7
 #define AT_FLAGS		8
 #define AT_ENTRY		9
 #define AT_DCACHEBSIZE	10
 #define AT_ICACHEBSIZE	11
 #define AT_UCACHEBSIZE	12


typedef struct gliss_env_t
{
	int argc;

	/* NULL terminated */
	char **argv;
	gliss_address_t argv_addr;

	/* NULL terminated */
	char **envp;
	gliss_address_t envp_addr;

	auxv_t *auxv;
	gliss_address_t auxv_addr;

	gliss_address_t stack_pointer;
} gliss_env_t;

/* system initialization */
void gliss_stack_fill_env(gliss_loader_t *loader, gliss_memory_t *memory, gliss_env_t *env);
void gliss_registers_fill_env(gliss_env_t *env, gliss_state_t *state);

/* section access */
typedef struct gliss_loader_sect_t {
	const char *name;
	gliss_address_t addr;
	int size;
	enum {
		GLISS_LOADER_SECT_UNKNOWN = 0,
		GLISS_LOADER_SECT_TEXT,
		GLISS_LOADER_SECT_DATA,
		GLISS_LOADER_SECT_BSS
	} type;
} gliss_loader_sect_t;
int gliss_loader_count_sects(gliss_loader_t *loader);
void gliss_loader_sect(gliss_loader_t *loader, int sect, gliss_loader_sect_t *data);

/* symbol access */
typedef struct {
	const char *name;
	gliss_address_t value;
	int size;
	int sect;
	enum {
		GLISS_LOADER_SYM_NO_TYPE,
		GLISS_LOADER_SYM_DATA,
		GLISS_LOADER_SYM_CODE
	} type;
	enum {
		GLISS_LOADER_NO_BINDING,
		GLISS_LOADER_LOCAL,
		GLISS_LOADER_GLOBAL,
		GLISS_LOADER_WEAK
	} bind;
} gliss_loader_sym_t;
int gliss_loader_count_syms(gliss_loader_t *loader);
/* the name of an ELF "element" (symbol, section...) cannot be accessed directly from an Elf32_* */
/*const char *gliss_loader_name_of_sym(gliss_loader_t *loader, int sym);*/
void gliss_loader_sym(gliss_loader_t *loader, int sym, gliss_loader_sym_t *data);



#if defined(__cplusplus)
}
#endif

#endif	/* GLISS_OLD_ELF_H */
