/*
 *	$Id: gliss.c,v 1.3 2009/02/16 18:42:09 casse Exp $
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

#include "gliss.h"

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


uint32_t gliss_set_field32u(uint32_t v, uint32_t s, int32_t l, int32_t u) {
	uint32_t mask = ((1 << (u - l) ) - 1) << l;
	return (v & ~mask) | ((s << l) & mask);
}


uint64_t gliss_set_field64u(uint64_t v, uint64_t s, int32_t l, int32_t u) {
	uint64_t mask = ((1 << (u - l) ) - 1) << l;
	return (v & ~mask) | ((s << l) & mask);
}

uint32_t gliss_field32u(uint32_t v, uint32_t l, uint32_t u) {
	return (v & ((1 << (u - l)) - 1)) >> l;
}


uint64_t gliss_field64u(uint64_t v, uint32_t l, uint32_t u) {
	return (v & ((1 << (u - l)) - 1)) >> l;
}
