/*
 *	$Id: old_elf.h,v 1.1 2008/12/23 09:36:36 casse Exp $
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

#include "gliss.h"
#include "memory.h"

#if defined(__cplusplus)
    extern  "C" {
#endif

typedef struct gliss_loader_t gliss_loader_t;
 
gliss_loader_t *gliss_loader_open(const char *path);
void gliss_loader_close(gliss_loader_t *loader);
void gliss_loader_load(gliss_loader_t *loader, gliss_memory_t *memory);
gliss_address_t gliss_loader_start(gliss_loader_t *loader);

#if defined(__cplusplus)
}
#endif

#endif	/* GLISS_OLD_ELF_H */
