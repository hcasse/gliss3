/*
 *	$Id: grt.h,v 1.3 2009/07/08 12:09:19 barre Exp $
 *	gliss declarations
 *
 *	This file is part of OTAWA
 *	Copyright (c) 2008, IRIT UPS.
 *
 *	GLISS2 is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	GLISS2 is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with GLISS2; if not, write to the Free Software
 *	Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifndef GLISS_GRT_H
#define GLISS_GRT_H

#include <stdint.h>

/* module */
#define GLISS_GRT_STATE
#define GLISS_GRT_INIT(s)
#define GLISS_GRT_DESTROY(s)

/*	Notations :
 * - v  : value
 * - u  : uppermost (meaning the most significant bit)
 * - l  : lowermost (meaning the least significant bit)
 * - s  : set value (the field defined by [u l] in v is set to s)
 * - n  :
 * - r  : rotate    (number of times we rotate v)
 * - bo : bit order ()
 * */

/* rotations */
#define gliss_mask32(n)	((n) == 32 ? (-1L) : (1L << (n)) - 1)
#define gliss_mask64(n)	((n) == 64 ? (-1LL) : (1LL << (n)) - 1)

uint32_t gliss_rotate_left32(uint32_t v, int r, int n);
#define gliss_rotate_left8(v, r, n) gliss_rotate_left32(v, r, n)
#define gliss_rotate_left16(v, r, n) gliss_rotate_left32(v, r, n)
uint64_t gliss_rotate_left64(uint64_t v, int r, int n);

uint32_t gliss_rotate_right32(uint32_t v, int r, int n);
#define gliss_rotate_right8(v, r, n) gliss_rotate_right32(v, r, n)
#define gliss_rotate_right16(v, r, n) gliss_rotate_right32(v, r, n)
uint64_t gliss_rotate_right64(uint64_t v, int r, int n);

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


/* for the field modification or extraction functions, there are 3 family:
 *  - function name not suffixed, used when field's bounds are known (constant)
 * and in the same order as specified by the nml var bit_order (no bit inversion needed)
 *  - <name>_inverted, bounds are known and order is different from bit_order (inversion needed)
 *  - <name>_generic, bounds known or not, will call a function from 1 of the 2 previous family
 * depending on the order of the bounds and bit_order.
 * Convention for bit_order:
 *  - 0 => lowermost
 *  - !=0 => uppermost
 */

/* set field */
#define gliss_set_field8(v, s, u, l) ((int8_t)gliss_set_field32u(v, s, u, l))
#define gliss_set_field8u(v, s, u, l) gliss_set_field32u(v, s, l, u)
#define gliss_set_field16(v, s, u, l) ((int16_t)gliss_set_field32u(v, s, u, l))
#define gliss_set_field16u(v, s, u, l) gliss_set_field32u(v, s, l, u)
#define gliss_set_field32(v, s, u, l) ((int32_t)gliss_set_field32u(v, s, u, l))
uint32_t gliss_set_field32u(uint32_t v, uint32_t s, int32_t u, int32_t l);
#define gliss_set_field64(v, s, u, l) ((int64_t)gliss_set_field64u(v, s, u, l))
uint64_t gliss_set_field64u(uint64_t v, uint64_t s, int32_t u, int32_t l);
float gliss_set_fieldf(float v, uint32_t s, int32_t u, int32_t l);
double gliss_set_fieldd(double v, uint64_t s, int32_t u, int32_t l);

#define gliss_set_field8_inverted(v, s, u, l) ((int8_t)gliss_set_field32u_inverted(v, s, u, l))
#define gliss_set_field8u_inverted(v, s, u, l) gliss_set_field32u_inverted(v, s, l, u)
#define gliss_set_field16_inverted(v, s, u, l) ((int16_t)gliss_set_field32u_inverted(v, s, u, l))
#define gliss_set_field16u_inverted(v, s, u, l) gliss_set_field32u_inverted(v, s, l, u)
#define gliss_set_field32_inverted(v, s, u, l) ((int32_t)gliss_set_field32u_inverted(v, s, u, l))
uint32_t gliss_set_field32u_inverted(uint32_t v, uint32_t s, int32_t u, int32_t l);
#define gliss_set_field64_inverted(v, s, u, l) ((int64_t)gliss_set_field64u_inverted(v, s, u, l))
uint64_t gliss_set_field64u_inverted(uint64_t v, uint64_t s, int32_t u, int32_t l);
float gliss_set_fieldf_inverted(float v, uint32_t s, int32_t u, int32_t l);
double gliss_set_fieldd_inverted(double v, uint64_t s, int32_t u, int32_t l);

#define gliss_set_field8_generic(v, s, a, b, bo) ((int8_t)gliss_set_field32u_generic(v, s, a, b, bo))
#define gliss_set_field8u_generic(v, s, a, b, bo) gliss_set_field32u_generic(v, s, a, b, bo)
#define gliss_set_field16_generic(v, s, a, b, bo) ((int16_t)gliss_set_field32u_generic(v, s, a, b, bo))
#define gliss_set_field16u_generic(v, s, a, b, bo) gliss_set_field32u_generic(v, s, a, b, bo)
#define gliss_set_field32_generic(v, s, a, b, bo) ((int32_t)gliss_set_field32u_generic(v, s, a, b, bo))
uint32_t gliss_set_field32u_generic(uint32_t v, uint32_t s, int32_t a, int32_t b, int bit_order);
#define gliss_set_field64_generic(v, s, a, b, bo) ((int64_t)gliss_set_field64u_generic(v, s, a, b, bo))
uint64_t gliss_set_field64u_generic(uint64_t v, uint64_t s, int32_t a, int32_t b, int bit_order);
float gliss_set_fieldf_generic(float v, uint32_t s, int32_t a, int32_t b, int bit_order);
double gliss_set_fieldd_generic(double v, uint64_t s, int32_t a, int32_t b, int bit_order);

/* field :
 * gliss_fieldx_xxxxx(v, u, l) returns the value of v field defined by
 * the bit intervalle [u l].
 * */
#define gliss_field8(v, u, l) gliss_field32u(v, u, l)
#define gliss_field8u(v, u, l) gliss_field32u(v, u, l)
#define gliss_field16(v, u, l) gliss_field32u(v, u, l)
#define gliss_field16u(v, u, l) gliss_field32u(v, u, l)
#define gliss_field32(v, u, l) gliss_field32u(v, u, l)
uint32_t gliss_field32u(uint32_t v, uint32_t u, uint32_t l);
#define gliss_field64(v, u, l) gliss_field64u(v, u, l)
uint64_t gliss_field64u(uint64_t v, uint32_t u, uint32_t l);
uint32_t gliss_fieldf(float v, uint32_t u, uint32_t l);
uint64_t gliss_fieldd(double v, uint32_t u, uint32_t l);

#define gliss_field8_inverted(v, u, l) gliss_field32u_inverted(v, u, l)
#define gliss_field8u_inverted(v, u, l) gliss_field32u_inverted(v, u, l)
#define gliss_field16_inverted(v, u, l) gliss_field32u_inverted(v, u, l)
#define gliss_field16u_inverted(v, u, l) gliss_field32u_inverted(v, u, l)
#define gliss_field32_inverted(v, u, l) gliss_field32u_inverted(v, u, l)
uint32_t gliss_field32u_inverted(uint32_t v, uint32_t u, uint32_t l);
#define gliss_field64_inverted(v, u, l) gliss_field64u_inverted(v, u, l)
uint64_t gliss_field64u_inverted(uint64_t v, uint32_t u, uint32_t l);
uint32_t gliss_fieldf_inverted(float v, uint32_t u, uint32_t l);
uint64_t gliss_fieldd_inverted(double v, uint32_t u, uint32_t l);

#define gliss_field8_generic(v, a, b, bo) gliss_field32u_generic(v, a, b, bo)
#define gliss_field8u_generic(v, a, b, bo) gliss_field32u_generic(v, a, b, bo)
#define gliss_field16_generic(v, a, b, bo) gliss_field32u_generic(v, a, b, bo)
#define gliss_field16u_generic(v, a, b, bo) gliss_field32u_generic(v, a, b, bo)
#define gliss_field32_generic(v, a, b, bo) gliss_field32u_generic(v, a, b, bo)
uint32_t gliss_field32u_generic(uint32_t v, uint32_t a, uint32_t b, int bit_order);
#define gliss_field64_generic(v, a, b, bo) gliss_field64u_generic(v, a, b, bo)
uint64_t gliss_field64u_generic(uint64_t v, uint32_t a, uint32_t b, int bit_order);
uint32_t gliss_fieldf_generic(float v, uint32_t a, uint32_t b, int bit_order);
uint64_t gliss_fieldd_generic(double v, uint32_t a, uint32_t b, int bit_order);

/* enumeration */
int gliss_enumerate(int val, int max);

/* coercition */
int32_t gliss_coerce_ftoi(float f);
uint32_t gliss_coerce_ftou(float f);
int64_t gliss_coerce_dtoi(double d);
uint64_t gliss_coerce_dtou(double d);
float gliss_coerce_itof(int32_t i);
float gliss_coerce_utof(uint32_t u);
double gliss_coerce_itod(int64_t i);
double gliss_coerce_utod(uint64_t u);

/* cast */
#define gliss_cast_fto8(f) (uint8_t)gliss_cast_fto32(f)
#define gliss_cast_fto16(f) (uint16_t)gliss_cast_fto32(f)
uint32_t gliss_cast_fto32(float f);
#define gliss_cast_fto64(f) (uint64_t)gliss_cast_fto32(f)

#define gliss_cast_dto8(f) (uint8_t)gliss_cast_dto64(f)
#define gliss_cast_dto16(f) (uint16_t)gliss_cast_dto64(f)
#define gliss_cast_dto32(f) (uint32_t)gliss_cast_dto64(f)
uint64_t gliss_cast_dto64(double f);

#define gliss_cast_8tof(i)	gliss_cast_32tof(i)
#define gliss_cast_16tof(i)	gliss_cast_32tof(i)
float gliss_cast_32tof(uint32_t i);
#define gliss_cast_64tof(i)	gliss_cast_32tof(i)

#define gliss_cast_8tod(i) gliss_cast_64tod(i)
#define gliss_cast_16tod(i) gliss_cast_64tod(i)
#define gliss_cast_32tod(i) gliss_cast_64tod(i)
double gliss_cast_64tod(uint64_t d);

/* signed/unsigned support */
#define __GLISS_MASK32(n, e)	((n)==32 ? (e) : ((e) & ((1 << n) - 1)))
#define __GLISS_MASK64(n, e)	((n)==64 ? (e) : ((e) & ((1LL << n) - 1)))
#define __GLISS_EXTS(n, e)		(((e) << n) >> n)

#endif /* GLISS_GRT_H */
