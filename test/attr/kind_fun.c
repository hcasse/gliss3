/* Generated by gliss-attr ($(date)) copyright (c) 2009 IRIT - UPS */

#include "../include/$(proc)/api.h"
#include "../include/$(proc)/id.h"
#include "../include/$(proc)/macros.h"

typedef unsigned long kind_t;
typedef kind_t (*fun_t)($(proc)_inst_t *inst);

/*** function definition ***/

static kind_t kind_UNKNOWN($(proc)_inst_t *inst) {
	return 0;
}

$(foreach instructions)
static kind_t kind_$(IDENT)($(proc)_inst_t *inst) {
$(kind)
};

$(end)


/*** function table ***/
static fun_t kind_funs[] = {
	kind_UNKNOWN$(foreach instructions),
	kind_$(IDENT)$(end)
};

/**
 * Get the OTAWA kind of the instruction.
 * @return OTAWA kind.
 */
kind_t $(proc)_kind($(proc)_inst_t *inst) {
	return kind_funs[inst->ident](inst);
}
