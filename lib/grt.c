/*
 *	$Id: grt.c,v 1.2 2009/05/19 08:59:49 barre Exp $
 *	gliss definitions
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

#include "error.h"
#include "grt.h"

int32_t gliss_exp32(int32_t v1, int32_t v2) {
	if(v1 >= 0)
		return gliss_exp32u(v1, v2);
	else if(v2 & 1)
		return -gliss_exp32u(-v1, v2);
	else
		return gliss_exp32u(-v1, v2);
}


uint32_t gliss_exp32u(uint32_t v1, uint32_t v2) {
	/* !!TODO!! may be improved */
	uint32_t res = 0;

	if(!v2)
		res = 1;
	else
		while(v2) {
			if(v2 & 1)
				res += v1;
			v2 >>= 1;
			v1 <<= 1;
		}	
	return res;
}


int64_t gliss_exp64(int64_t v1, int64_t v2) {
	if(v1 >= 0)
		return gliss_exp64u(v1, v2);
	else if(v2 & 1)
		return -gliss_exp64u(-v1, v2);
	else
		return gliss_exp64u(-v1, v2);
}

uint64_t gliss_exp64u(uint64_t v1, uint64_t v2) {
	/* !!TODO!! may be improved */
	uint64_t res = 0;

	if(!v2)
		res = 1;
	else
		while(v2) {
			if(v2 & 1)
				res += v1;
			v2 >>= 1;
			v1 <<= 1;
		}	
	return res;
}


uint32_t gliss_set_field32u(uint32_t v, uint32_t s, int32_t u, int32_t l) {
printf("gliss_set_field_32u, args. v=%08X, s=%08X, u=%08X, l=%08X.", v, s, u, l);
	uint32_t mask = ((1 << (u - l + 1) ) - 1) << l;
printf(" (mask init=%08X)", mask);
printf(" => %08X\n", (v & ~mask) | ((s << l) & mask));
	return (v & ~mask) | ((s << l) & mask);
}


uint64_t gliss_set_field64u(uint64_t v, uint64_t s, int32_t u, int32_t l) {
	uint64_t mask = ((1 << (u - l + 1) ) - 1) << l;
	return (v & ~mask) | ((s << l) & mask);
}

double gliss_set_fieldd(double v, uint64_t s, int32_t u, int32_t l) {
	union {
		double d;
		uint64_t w;
	} t;
	t.d = v;
	t.w = gliss_set_field64u(t.w, s, u, l);
	return t.d;
}


uint32_t gliss_field32u(uint32_t v, uint32_t u, uint32_t l) {
	return (v & ((1 << (u - l)) - 1)) >> l;
}


uint64_t gliss_field64u(uint64_t v, uint32_t u, uint32_t l) {
	return (v & ((1 << (u - l)) - 1)) >> l;
}


uint64_t gliss_fieldd(double v, uint32_t u, uint32_t l) {
	return gliss_field64u(*(uint64_t *)&v, u, l);
}



/**
 * Test if the value in the range [0, max].
 * Call gliss_error() else.
 * @param val	Value to test.
 * @param max	Maximum enumration value.
 * @return		Value.
 */
int gliss_enumerate(int val, int max) {
	if(val >= max)
		gliss_panic("coercition to enumeration out of bounds");
	else
		return val;
}


/* bit-to-bit conversions */
#define COERCE(name, tr, ta) \
	tr gliss_coerce_##name(ta arg) { \
		union { \
			tr res; \
			ta arg; \
		} v; \
		v.arg = arg; \
		return v.res; \
	}

COERCE(ftoi, int32_t, float)
COERCE(ftou, uint32_t, float)
COERCE(dtoi, int64_t, double)
COERCE(dtou, uint64_t, double)
COERCE(itof, float, int32_t)
COERCE(utof, float, uint32_t)
COERCE(itod, double, int64_t)
COERCE(utod, double, uint64_t)
