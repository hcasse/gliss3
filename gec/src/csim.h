/*
 * Component simulator main header
 * Copyright (c) 2019, IRIT - UPS <casse@irit.fr>
 *
 * This file is part of GLISS2.
 *
 * GLISS2 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GLISS2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GLISS2; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */
#ifndef GLISS2_CSIM_H
#define GLISS2_CSIM_H

#include <stdint.h>

#define CSIM_READ	0x0001
#define CSIM_WRITE	0x0002

typedef uint32_t csim_addr_t;
typedef uint32_t csim_word_t;
typedef uint64_t csim_date_t;
typedef uint64_t csim_time_t;
typedef uint64_t csim_clock_t;

typedef enum csim_rtype_t {
	CSIM_NORTYPE = 0,
	CSIM_BITS,
	CSIM_INT,
	CSIM_ADDR,
	CSIM_FLOAT32,
	CSIM_FLOAT64
} csim_rtype_t;

typedef enum csim_mtype_t {
	CSIM_NOMTYPE = 0,
	CSIM_DIGITAL,
	CSIM_ANALOG,
	CSIM_CLOCK,
	CSIM_SERIAL
} csim_mtype_t;

typedef enum csim_ctype_t {
	CSIM_NOCTYPE = 0,
	CSIM_SIMPLE,
	CSIM_CORE
} csim_ctype_t;

typedef enum csim_log_t {
	CSIM_NOLOG = 0,
	CSIM_INFO,
	CSIM_WARN,
	CSIM_ERROR,
	CSIM_FATAL
} csim_log_t;

typedef struct csim_component_t csim_component_t;
typedef struct csim_reg_t csim_reg_t;
typedef struct csim_pin_t csim_pin_t;
typedef struct csim_msg_t csim_msg_t;
typedef struct csim_evt_t csim_evt_t;
typedef struct csim_board_t csim_board_t;
typedef struct csim_core_t csim_core_t;

struct csim_reg_t {
	const char *name;
	uint32_t offset;
	uint32_t size;
	uint32_t stride;
	uint32_t flags;
	csim_rtype_t type;
	csim_component_t *comp;
	void (*make_name)(int num, char buf, int size);
	void (*display)(csim_reg_t *comp, int num, char buf, int size);
	csim_word_t (*read)(csim_reg_t *comp, int num);
	void (*write)(csim_reg_t *comp, int num, csim_word_t val);
	csim_word_t (*get)(csim_reg_t *comp, int num);
	void (*set)(csim_reg_t *comp, int num, csim_word_t val);
};

struct csim_pin_t {
	const char *name;
	struct csim_pin_t *link;
	csim_component_t *comp;
	void (*receive)(csim_pin_t *pin, const csim_msg_t *msg);
};

struct csim_msg_t {
	csim_mtype_t type;
	csim_date_t date;
	union {
		int digital;
		double analog;
		uint32_t clock;
		char serial;
	} val;
};

struct csim_evt_t {
	struct csim_evt_t *next, *prev;
	csim_date_t date;
	uint32_t period;
	csim_component_t *comp;
	void (*trigger)(csim_evt_t *evt);
};

struct csim_component_t {
	struct csim_component_t *next;
	const char *name;
	const char *cls;
	csim_ctype_t type;
	uint32_t version;
	csim_addr_t base;
	csim_reg_t *regs;
	csim_pin_t *pins;
	csim_board_t *board;
	void (*init)(csim_component_t *comp);
	void (*destroy)(csim_component_t *comp);
	csim_core_t *core;
};

struct csim_core_t {
	csim_core_t *next;
	csim_component_t *comp;
	csim_clock_t clock;
	void (*step)(csim_core_t *comp);
};

struct csim_board_t {
	csim_component_t *comps;
	csim_core_t *cores;
	csim_clock_t clock;
	csim_date_t date;
	csim_evt_t *evts;
	void (*log)(csim_board_t *board, csim_log_t level, const char *msg);
};

void csim_init_board(csim_board_t *board);
void csim_destroy_board(csim_board_t *board);
void csim_add_component(csim_board_t *board, csim_component_t *comp);
void csim_link(csim_pin_t *pin1, csim_pin_t *pin2);
void csim_log(csim_board_t *board, csim_log_t level, const char *msg);

void csim_send(csim_pin_t *pin, csim_msg_t *msg);
void csim_record_event(csim_board_t *board, csim_evt_t *evt);
void csim_cancel_event(csim_board_t *board, csim_evt_t *evt);

void csim_run(csim_board_t *board, csim_time_t time);


#endif	/* GLISS2_CSIM_H */
