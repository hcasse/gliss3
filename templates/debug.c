/* Generated by gep ($(date)) copyright (c) 2011 IRIT - UPS */

#include <assert.h>
#include <$(proc)/debug.h>
#include <$(proc)/macros.h>

static register_bank_t $(proc)_registers[] = {
$(foreach registers)$(if !aliased)
	{
		$(id),
		$(format),
		$(size),
		$(if is_pc)RTYPE_ADDR$(else)$(if is_float)RTYPE_FLOAT$(else)RTYPE_INT$(end)$(end),
		$(type_size)
	},
$(end)$(end)	{
		-1
	}
};


/**
 * Get the description of registers of the architecture.
 * @return		Array of registers (last one has a negative identifier).
 */
register_bank_t *gliss_get_registers(void) {
	return $(proc)_registers;
}

/**
 * Get the value of a register.
 * @param state		Current state.
 * @param id		Register identifier (from register_bank_t).
 * @param idx		Index in the bank (ignored for non-mutiple register bank).
 * @return			Value of the register.
 */
register_value_t gliss_get_register($(proc)_state_t *state, int id, int idx) {
	register_value_t r;
	switch(id) {
$(foreach registers)$(if !aliased)
	case $(id):
		r.$(if !is_float)$(if !is_64)iv$(else)lv$(end)$(else)$(if !is_64)fv$(else)dv$(end)$(end) = $(PROC)_$(NAME)$(if array)[idx]$(end);
		break;
$(end)$(end)
	default:
		assert(0);
	}
	return r;
}


/**
 * Set the value of a register in the given state.
 * @param state		Current state.
 * @param id		Register identifier (from register_bank_t).
 * @param idx		Index in the bank (ignored for non-mutiple register bank).
 * @param value		Value to put in register.
 */
void gliss_set_register($(proc)_state_t *state, int id, int idx, register_value_t value) {
	switch(id) {
$(foreach registers)$(if !aliased)
	case $(id):
		$(PROC)_$(NAME)$(if array)[idx]$(end) = value.$(if !is_float)$(if !is_64)iv$(else)lv$(end)$(else)$(if !is_64)fv$(else)dv$(end)$(end);
		break;
$(end)$(end)
	default:
		assert(0);
	}
}