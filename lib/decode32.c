/* Generated by gep ($(date)) copyright (c) 2008 IRIT - UPS */

#include <stdlib.h>
#include <stdio.h>
/* #include <math.h>  needed for affiche_valeur_binaire (which is not well coded) */

#include "fetch.h"
#include "decode.h" /* api.h will be in it, for fetch functions, decode_table.h also */
#include "config.h" /* for memory endiannesses */

#define gliss_error(e) fprintf(stderr, (e))

/* endianness */
typedef enum gliss_endianness_t {
  little = 0,
  big = 1
} gliss_endianness_t;

/* decode structure */
struct gliss_decoder_t
{
	/* the fetch unit used to retrieve instruction ID */
	gliss_fetch_t *fetch;
};

/* Extern Modules */
/* Constants */


/* Variables & Fonctions */

/* decoding */
gliss_inst_t *gliss_decode(gliss_decoder_t *decoder, gliss_address_t address);


/* free a dynamically allocated instruction, we try not to free an already freed or NULL pointer */
void gliss_free_inst(gliss_inst_t *inst)
{
	if (inst)
		free(inst);
	inst = 0;
}


/* initialization and destruction of gliss_decode_t object */

static int number_of_decoder_objects = 0;

static void init_decoder(gliss_decoder_t *d, gliss_platform_t *state)
{
	d->fetch = gliss_new_fetch(state);
}

static void halt_decoder(gliss_decoder_t *d)
{
	gliss_delete_fetch(d->fetch);
}

gliss_decoder_t *gliss_new_decoder(gliss_platform_t *state)
{
	gliss_decoder_t *res = malloc(sizeof(gliss_decoder_t));
	if (res == NULL)
		gliss_error("not enough memory to create a gliss_decoder_t object"); /* I assume error handling will remain the same, we use gliss_error istead of iss_error ? */
	/*assert(number_of_decode_objects >= 0);*/
	init_decoder(res, state);
	number_of_decoder_objects++;
	return res;
}

void gliss_delete_decoder(gliss_decoder_t *decode)
{
	if (decode == NULL)
		/* we shouldn't try to free a void decoder_t object, should this output an error ? */
		gliss_error("cannot delete an NULL gliss_decoder_t object");
	number_of_decoder_objects--;
	/*assert(number_of_decode_objects >= 0);*/
	halt_decoder(decode);
	free(decode);
}


/* Fonctions Principales */

gliss_inst_t *gliss_decode(gliss_decoder_t *decoder, gliss_address_t address)
{
	/* first, fetch the instruction at the given address */

	gliss_inst_t *res = 0;
	gliss_ident_t id = gliss_fetch(decoder->fetch, address);
	uint32_t code = gliss_mem_read32(decoder->fetch->mem, address);
	/* revert bytes if endianness of host and target are not equals */
	if (HOST_ENDIANNESS != TARGET_ENDIANNESS)
		code = ((code&0x0FF)<<24)|((code&0x0FF00)<<8)|((code&0x0FF0000)>>8)|((code&0xFF000000)>>24);
	
	/* then decode it */
	
	return gliss_decode_table[id](code);
}


/* End of file gliss_decode.c */
