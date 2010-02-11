/* Generated by gep ($(date)) copyright (c) 2008 IRIT - UPS */

#ifndef GLISS_$(PROC)_INCLUDE_$(PROC)_DECODE_TABLE_H
#define GLISS_$(PROC)_INCLUDE_$(PROC)_DECODE_TABLE_H


#if defined(__cplusplus)
extern  "C"
{
#endif

#include "../include/$(proc)/api.h"
#include "../include/$(proc)/macros.h"

/* TODO: add some error messages when malloc fails */
#define gliss_error(e) fprintf(stderr, (e))



/*
	donne la valeur d'une zone m�moire (une instruction) en ne prenant
	en compte que les bits indiqu�s par le mask

	on fait un ET logique entre l'instruction et le masque,
	on conserve seulement les bits indiqu�s par le masque
	et on les concat�ne pour avoir un nombre sur 32 bits

	on suppose que le masque n'a pas plus de 32 bits � 1,
	sinon d�bordement

	instr : instruction (de 32 bits)
	mask  : masque (32 bits aussi)
*/
static uint32_t valeur_sur_mask_bloc(uint32_t instr, uint32_t mask)
{
	int i;
	uint32_t tmp_mask;
	uint32_t res = 0;

	/* on fait un parcours du bit de fort poids de instr[0]
	� celui de poids faible de instr[nb_bloc-1], "de gauche � droite" */

	tmp_mask = mask;
	for (i = 31; i >= 0; i--)
	{
		/* le bit i du mask est 1 ? */
		if (tmp_mask & 0x80000000UL)
		{
			/* si oui, recopie du bit i de l'instruction
			� droite du resultat avec decalage prealable */
			res <<= 1;
			res |= ((instr >> i) & 0x01);
		}
		tmp_mask <<= 1;
	}
	return res;
}

static $(proc)_inst_t *$(proc)_instr_UNKNOWN_decode($(proc)_address_t address, uint32_t code_inst)
{
	$(proc)_inst_t *inst = malloc(sizeof($(proc)_inst_t));
	inst->ident = $(PROC)_UNKNOWN;
	inst->instrinput = malloc(sizeof($(proc)_ii_t) * 2);

	/* set size and address */
	inst->instrinput[0].type = $(PROC)_ADDR;
	inst->instrinput[0].val.addr = address;
	inst->instrinput[1].type = $(PROC)_SIZE;
	inst->instrinput[1].val.size = 32;

	return inst;
}

$(foreach instructions)
static $(proc)_inst_t *$(proc)_instr_$(IDENT)_decode($(proc)_address_t address, uint32_t code_inst)
{
	$(if has_param)uint32_t mask;
	$(proc)_inst_t *inst = malloc(sizeof($(proc)_inst_t));
	inst->ident = $(PROC)_$(IDENT);
	inst->instrinput = malloc(sizeof($(proc)_ii_t) * ($(num_params) + 2));

	/* set size and address */
	inst->instrinput[0].type = $(PROC)_ADDR;
	inst->instrinput[0].val.addr = address;
	inst->instrinput[1].type = $(PROC)_SIZE;
	inst->instrinput[1].val.size = 32;

	/* put other parameters */
	$(foreach params)/* param number $(INDEX) */
	mask = $(mask_32)UL;
	$(PROC)_$(IDENT)_$(PARAM) = valeur_sur_mask_bloc(code_inst, mask); /* res->instrinput[$(INDEX)].val.$(param_type) */
	inst->instrinput[$(INDEX) + 2].type = $(PROC)_PARAM_$(PARAM_TYPE)_T;
	$(end)
	return inst;
}

$(else)$(proc)_inst_t *res = malloc(sizeof($(proc)_inst_t));
	res->ident = $(PROC)_$(IDENT);
	res->instrinput = malloc(sizeof($(proc)_ii_t) * 2);

	/* set size and address */
	res->instrinput[0].type = $(PROC)_ADDR;
	res->instrinput[0].val.addr = address;
	res->instrinput[1].type = $(PROC)_SIZE;
	res->instrinput[1].val.size = 32;

	return res;
}

$(end)
$(end)


typedef $(proc)_inst_t *$(proc)_decode_function_t($(proc)_address_t address, uint32_t code_inst);

static $(proc)_decode_function_t *$(proc)_decode_table[] =
{
	$(proc)_instr_UNKNOWN_decode$(foreach instructions),
	$(proc)_instr_$(IDENT)_decode$(end)
};



#if defined(__cplusplus)
}
#endif

#endif /* GLISS_$(PROC)_INCLUDE_$(PROC)_DECODE_TABLE_H */
