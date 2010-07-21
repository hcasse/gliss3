#ifndef LEON_REGISTER_H
#define LEON_REGISTER_H

#include <stdint.h>
#include GLISS_API_H

/* the python script, as given, cannot "render" a "moving" register
 * like the windowed ones of the leon */

uint32_t get_win_reg(PROC(_state_t) * st, unsigned int idx);

void set_win_reg(PROC(_state_t) * st, unsigned int idx, uint32_t val);

void dump_all_windows(PROC(_state_t) * st);
void dump_float_registers(PROC(_state_t) * st);

/* return the ID of the gliss reg IDed by the text description and the index */
void get_gliss_reg_addr(char *desc, PROC(_state_t) * st, int *bank, int *idx);

/* get the value of the register #idx in reg_infos[] */
uint64_t get_gliss_reg(PROC(_state_t) * st, int idx);

#endif /* LEON_REGISTER_H */
