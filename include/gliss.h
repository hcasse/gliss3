/*
 *	$Id: gliss.h,v 1.1 2008/12/19 21:15:21 casse Exp $
 *	gliss declarations
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

#ifndef GLISS_H
#define GLISS_H

/* rotations */
#define gliss_mask32(n)	((n) == 32 ? (-1) : (1 << (n)) - 1)

#define gliss_rotate_left32(v, r, n) \
	((((v) << ((r) % (n))) | ((v) >> ((n) - (r) % (n)))) & gliss_mask32(n))
#define gliss_rotate_left8(v, r, n) gliss_rotate_left32(v, r, n)
#define gliss_rotate_left16(v, r, n) gliss_rotate_left32(v, r, n)

#define gliss_rotate_right32(v, r, n) \
	((((v) << ((n) - (r) % (n))) | ((v) >> ((r) % (n)))) & gliss_mask32(n))
#define gliss_rotate_right8(v, r, n) gliss_rotate_right32(v, r, n)
#define gliss_rotate_right16(v, r, n) gliss_rotate_right32(v, r, n)

#endif /* GLISS_H */
