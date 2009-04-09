/*
 *	$Id: exception.h,v 1.1 2009/04/09 08:17:27 casse Exp $
 *	syscall-linux module interface
 *
 *	This file is part of GLISS2
 *	Copyright (c) 2009, IRIT UPS.
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
 *	along with GLISS2; if not, write to the Free Software 
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#ifndef GLISS_PPC_EXCEPTION_H
#define GLISS_PPC_EXCEPTION_H

#include "../include/gliss/api.h"

#if defined(__cplusplus)
extern  "C" {
#endif

#define GLISS_EXCEPTION_STATE
#define GLISS_EXCEPTION_INIT(s)
#define GLISS_EXCEPTION_DESTROY(s)

void launch_exception(const char *, int);

#if defined(__cplusplus)
}
#endif

#endif /* GLISS_PPC_EXCEPTION_H */
