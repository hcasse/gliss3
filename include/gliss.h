/*
 *	$Id: gliss.h,v 1.2 2008/12/22 12:21:53 casse Exp $
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

#include <stdint.h>

/* rotations */
#define gliss_mask32(n)	((n) == 32 ? (-1L) : (1L << (n)) - 1)
#define gliss_mask64(n)	((n) == 64 ? (-1LL) : (1LL << (n)) - 1)

#define gliss_rotate_left32(v, r, n) \
	((((uint32_t)(v) << ((r) % (n))) | ((v) >> ((n) - (r) % (n)))) & gliss_mask32(n))
#define gliss_rotate_left8(v, r, n) gliss_rotate_left32(v, r, n)
#define gliss_rotate_left16(v, r, n) gliss_rotate_left32(v, r, n)
#define gliss_rotate_left64(v, r, n) \
	((((uint64_t)(v) << ((r) % (n))) | ((v) >> ((n) - (r) % (n)))) & gliss_mask64(n))

#define gliss_rotate_right32(v, r, n) \
	((((uint32_t)(v) << ((n) - (r) % (n))) | ((v) >> ((r) % (n)))) & gliss_mask32(n))
#define gliss_rotate_right8(v, r, n) gliss_rotate_right32(v, r, n)
#define gliss_rotate_right16(v, r, n) gliss_rotate_right32(v, r, n)
#define gliss_rotate_right64(v, r, n) \
	((((uint64_t)(v) << ((n) - (r) % (n))) | ((v) >> ((r) % (n)))) & gliss_mask64(n))

/* concatenation */
#define gliss_concat32(v1, v2, n1, n2)  ((((uint32_t)(v1) << n2) | ((v2) & gliss_mask32(n2))))
#define gliss_concat8(v1, v2, n1, n2) gliss_concat32(v1, v2, n1, n2)
#define gliss_concat16(v1, v2, n1, n2) gliss_concat16(v1, v2, n1, n2)
#define gliss_concat64(v1, v2, n1, n2)  ((((uint64_t)(v1) << n2) | ((v2) & gliss_mask32(n2))))

/* exponent */
#define gliss_exp8(v1, v2) gliss_exp32(v1, v2)
#define gliss_exp8u(v1, v2) gliss_exp32u(v1, v2)
#define gliss_exp16(v1, v2) gliss_exp32(v1, v2)
#define gliss_exp16u(v1, v2) gliss_exp32u(v1, v2)
int32_t gliss_exp32(int32_t v1, int32_t v2);
uint32_t gliss_exp32u(uint32_t v1, uint32_t v2);
int64_t gliss_exp64(int64_t v1, int64_t v2);
uint64_t gliss_exp64u(uint64_t v1, uint64_t v2);

#endif /* GLISS_H */
