/*
 *	$Id: grt.c,v 1.6 2009/07/08 12:09:19 barre Exp $
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


uint32_t gliss_rotate_left32(uint32_t v, int r, int n)
{
	r = r % n;
	int len = n - r;
	uint32_t mask = gliss_mask32(len);
	return (((v & mask) << r) | ((v & ~mask) >> len));
}

uint64_t gliss_rotate_left64(uint64_t v, int r, int n)
{
	r = r % n;
	int len = n - r;
	uint64_t mask = gliss_mask64(len);
	return (((v & mask) << r) | ((v & ~mask) >> len));
}

uint32_t gliss_rotate_right32(uint32_t v, int r, int n)
{
	r = r % n;
	uint32_t mask = gliss_mask32(r);
	return (((v & mask) << (n - r)) | ((v & ~mask) >> r));
}

uint64_t gliss_rotate_right64(uint64_t v, int r, int n)
{
	r = r % n;
	uint64_t mask = gliss_mask64(r);
	return (((v & mask) << (n - r)) | ((v & ~mask) >> r));
}


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

/* inversion of the n lowest bit of a given value */
uint32_t gliss_invert32(uint32_t v, uint32_t n)
{
	uint32_t res = 0;
	int i = 0;
	
	/* !!DEBUG!! */
	//printf("gliss_invert32(0X%08X, 0X%08X) => ", v, n);

	for ( ; i < n ; i++)
	{
		res <<= 1;
		if (v & 0x01)
			res |= 0x01;
		v >>= 1;
	}
	/* !!DEBUG!! */
	//printf("0X%08X\n", res);
	
	return res;
}

uint64_t gliss_invert64(uint64_t v, uint64_t n)
{
	uint64_t res = 0;
	int i = 0;

	/* !!DEBUG!! */
	//printf("gliss_invert64(0X%016LX, 0X%016LX) => ", v, n);

	for ( ; i < n ; i++)
	{
		res <<= 1;
		if (v & 0x01)
			res |= 0x01;
		v >>= 1;
	}
	/* !!DEBUG!! */
	//printf("0X%016LX\n", res);
	
	return res;
}

float gliss_invertf(float v, uint32_t n)
{
	*((uint32_t *)&v) = gliss_invert32(*(uint32_t *)&v, n);
	return v;
}

double gliss_invertd(double v, uint32_t n)
{
	*((uint64_t *)&v) = gliss_invert64(*(uint64_t *)&v, n);
	return v;
}



/* for these functions no inversion is done and l <= u */
uint32_t gliss_set_field32u(uint32_t v, uint32_t s, int32_t u, int32_t l) {
	uint32_t mask = gliss_mask32(u - l + 1) << l;

	/* !!DEBUG!! */
	//printf("gliss_set_field32u(0X%08X, 0X%08X, 0X%08X, 0X%08X) => ", v, s, u, l);
	//printf("(mask=%08X) ", mask);
	//printf("0X%08X\n", (v & ~mask) | ((s << l) & mask));
	
	return (v & ~mask) | ((s << l) & mask);
}


uint64_t gliss_set_field64u(uint64_t v, uint64_t s, int32_t u, int32_t l) {
	uint64_t mask = gliss_mask64(u - l + 1) << l;

	/* !!DEBUG!! */
	//printf("gliss_set_field64u(0X%016llX, 0X%016llX, 0X%08X, 0X%08X) => ", v, s, u, l);
	//printf("0X%016llX\n", (v & ~mask) | ((s << l) & mask));
	
	return (v & ~mask) | ((s << l) & mask);
}

float gliss_set_fieldf(float v, uint32_t s, int32_t u, int32_t l) {
	*((uint32_t *)&v) = gliss_set_field32u(*((uint32_t *)&v), s, u, l);
	return v;
}

double gliss_set_fieldd(double v, uint64_t s, int32_t u, int32_t l) {
	*((uint64_t *)&v) = gliss_set_field64u(*((uint64_t *)&v), s, u, l);
	return v;
}


/* the read bits are inversed before being assigned to the result, l <= u */
uint32_t gliss_set_field32u_inverted(uint32_t v, uint32_t s, int32_t u, int32_t l) {
	uint32_t mask = gliss_mask32(u - l + 1);

	/* !!DEBUG!! */
	//printf("gliss_set_field32u_inverted(0X%08X, 0X%08X, 0X%08X, 0X%08X) => ", v, s, u, l);
	//printf("0X%08X\n", (v & ~mask) | (gliss_invert32(s & mask, u-l+1) << l));
	
	return (v & ~(mask << l)) | (gliss_invert32(s, u-l+1) << l);
}

uint64_t gliss_set_field64u_inverted(uint64_t v, uint64_t s, int32_t u, int32_t l) {
	uint64_t mask = gliss_mask64(u - l + 1);

	/* !!DEBUG!! */
	//printf("gliss_set_field64u_inverted(0X%016llX, 0X%016llX, 0X%08X, 0X%08X) => ", v, s, u, l);
	//printf("0X%016llX\n", (v & ~mask) | (gliss_invert64(s & mask, u-l+1) << l));
	
	return (v & ~(mask << l)) | (gliss_invert64(s, u-l+1) << l);
}

float gliss_set_fieldf_inverted(float v, uint32_t s, int32_t u, int32_t l) {
	*((uint32_t *)&v) = gliss_set_field32u_inverted(*((uint32_t *)&v), s, u, l);
	return v;
}

double gliss_set_fieldd_inverted(double v, uint64_t s, int32_t u, int32_t l) {
	*((uint64_t *)&v) = gliss_set_field64u_inverted(*((uint64_t *)&v), s, u, l);
	return v;
}


/* will perform v<a..b> = s, s is reversed if needed depending on the value of bit_order (0 : lowermost (default), !=0 : uppermost) */
uint32_t gliss_set_field32u_generic(uint32_t v, uint32_t s, int32_t a, int32_t b, int bit_order)
{
	/* !!DEBUG!! */
	//printf("gliss_set_field32u_generic(0X%08X, 0X%08X, 0X%08X, 0X%08X, %d) ==> \\\\\\\n++++", v, s, a, b, bit_order);
	
	/* only 1 bit to copy, do not care about bit_order, just do it the most convenient way */
	if (a == b)
		return gliss_set_field32u(v, s, a, b);

	/* lower bits first and uppermost OR upper bits first and lowermost => inversion */
	if (a < b)
	{
		if (bit_order)
			return gliss_set_field32u_inverted(v, s, b, a);
		else
			return gliss_set_field32u(v, s, b, a);
	}
	else
	{
		if (bit_order)
			return gliss_set_field32u(v, s, a, b);
		else
			return gliss_set_field32_inverted(v, s, a, b);
	}
}

uint64_t gliss_set_field64u_generic(uint64_t v, uint64_t s, int32_t a, int32_t b, int bit_order)
{
	/* !!DEBUG!! */
	//printf("gliss_set_field64u_generic(0X%016llX, 0X%016llX, 0X%08X, 0X%08X, %d) ==> \\\\\\\n++++", v, s, a, b, bit_order);
	
	/* only 1 bit to copy, do not care about bit_order, just do it the most convenient way */
	if (a == b)
		return gliss_set_field64u(v, s, a, b);

	/* lower bits first and uppermost OR upper bits first and lowermost => inversion */
	if (a < b)
	{
		if (bit_order)
			return gliss_set_field64u_inverted(v, s, b, a);
		else
			return gliss_set_field64u(v, s, b, a);
	}
	else
	{
		if (bit_order)
			return gliss_set_field64u(v, s, a, b);
		else
			return gliss_set_field64_inverted(v, s, a, b);
	}
}

float gliss_set_fieldf_generic(float v, uint32_t s, int32_t a, int32_t b, int bit_order)
{
	*((uint32_t *)&v) = gliss_set_field32u_generic(*((uint32_t *)&v), s, a, b, bit_order);
	return v;
}

double gliss_set_fieldd_generic(double v, uint64_t s, int32_t a, int32_t b, int bit_order)
{
	*((uint64_t *)&v) = gliss_set_field64u_generic(*((uint64_t *)&v), s, a, b, bit_order);
	return v;
}


/* for these functions no inversion is done and l <= u */
uint32_t gliss_field32u(uint32_t v, uint32_t u, uint32_t l)
{
	uint32_t mask = gliss_mask32(u - l + 1) << l;

	/* !!DEBUG!! */
	//printf("gliss_field32u(0X%08X, 0X%08X, 0X%08X) => ", v, u, l);
	//printf("0X%08X\n", (v & mask) >> l);
	
	return (v & mask) >> l;
}

uint64_t gliss_field64u(uint64_t v, uint32_t u, uint32_t l)
{
	uint64_t mask = gliss_mask64(u - l + 1) << l;

	/* !!DEBUG!! */
	//printf("gliss_field64u(0X%016LX, 0X%08X, 0X%08X) => ", v, u, l);
	//printf("0X%016LX\n", (v & mask) >> l);
	
	return (v & mask) >> l;
}

uint32_t gliss_fieldf(float v, uint32_t u, uint32_t l) {
	return gliss_field32u(*(uint32_t *)&v, u, l);
}

uint64_t gliss_fieldd(double v, uint32_t u, uint32_t l) {
	return gliss_field64u(*(uint64_t *)&v, u, l);
}


/* for these functions inversion is done and l <= u */
uint32_t gliss_field32u_inverted(uint32_t v, uint32_t u, uint32_t l)
{
	uint32_t mask = gliss_mask32(u - l + 1) << l;

	/* !!DEBUG!! */
	//printf("gliss_field32u_inverted(0X%08X, 0X%08X, 0X%08X) => ", v, u, l);
	//printf("0X%08X\n", gliss_invert32((v & mask) >> l, u-l+1));
	
	return gliss_invert32((v & mask) >> l, u-l+1);
}

uint64_t gliss_field64u_inverted(uint64_t v, uint32_t u, uint32_t l)
{
	uint64_t mask = gliss_mask64(u - l + 1) << l;

	/* !!DEBUG!! */
	//printf("gliss_field64u_inverted(0X%016LX, 0X%08X, 0X%08X) => ", v, u, l);
	//printf("0X%016LX\n", gliss_invert64((v & mask) >> l, u-l+1));
	
	return gliss_invert64((v & mask) >> l, u-l+1);
}


uint32_t gliss_fieldf_inverted(float v, uint32_t u, uint32_t l) {
	return gliss_field32u_inverted(*(uint32_t *)&v, u, l);
}

uint64_t gliss_fieldd_inverted(double v, uint32_t u, uint32_t l) {
	return gliss_field64u_inverted(*(uint64_t *)&v, u, l);
}


/* generic functions, will interpret bitfield expression v<a..b> depending on the value of bit_order (0 : lowermost (default), !=0 : uppermost) */
uint32_t gliss_field32u_generic(uint32_t v, uint32_t a, uint32_t b, int bit_order)
{
	/* !!DEBUG!! */
	//printf("gliss_field32u_generic(0X%08X, 0X%08X, 0X%08X, %d) ==> \\\\\\\n++++", v, a, b, bit_order);
	
	/* only 1 bit to extract, do not care about bit_order, just do it the most convenient way */
	if (a == b)
		return gliss_field32u(v, a, b);

	/* lower bits first and uppermost OR upper bits first and lowermost => inversion */
	if (a < b)
	{
		if (bit_order)
			return gliss_field32u_inverted(v, b, a);
		else
			return gliss_field32u(v, b, a);
	}
	else
	{
		if (bit_order)
			return gliss_field32u(v, a, b);
		else
			return gliss_field32_inverted(v, a, b);
	}
}

uint64_t gliss_field64u_generic(uint64_t v, uint32_t a, uint32_t b, int bit_order)
{
	/* !!DEBUG!! */
	//printf("gliss_field64u_generic(0X%016LX, 0X%08X, 0X%08X, %d) ==> \\\\\\\n++++", v, a, b, bit_order);
	
	/* only 1 bit to extract, do not care about bit_order, just do it the most convenient way */
	if (a == b)
		return gliss_field64u(v, a, b);

	/* lower bits first and uppermost OR upper bits first and lowermost => inversion */
	if (a < b)
	{
		if (bit_order)
			return gliss_field64u_inverted(v, b, a);
		else
			return gliss_field64u(v, b, a);
	}
	else
	{
		if (bit_order)
			return gliss_field64u(v, a, b);
		else
			return gliss_field64_inverted(v, a, b);
	}
}

uint32_t gliss_fieldf_generic(float v, uint32_t a, uint32_t b, int bit_order)
{
	return gliss_field32u_generic(*(uint32_t *)&v, a, b, bit_order);
}

uint64_t gliss_fieldd_generic(double v, uint32_t a, uint32_t b, int bit_order)
{
	return gliss_field64u_generic(*(uint64_t *)&v, a, b, bit_order);
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
