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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "csim.h"

/**
 * Default log function: log to stderr.
 * @param board	Current board.
 * @param level	Logging level.
 * @param msg	Message to display.
 */
void csim_log(csim_board_t *board, csim_log_t level, const char *msg) {
	static const char *pref[] = {
		"",
		"INFO",
		"WARNING",
		"ERROR",
		"FATAL"
	};
	fprintf(stderr, "%s: %ld: %s\n", pref[level], board->date, msg); 
}


/**
 * Initialize the board.
 * @param board	Board to initialize.
 */
void csim_init_board(csim_board_t *board) {
	board->comps = NULL;
	board->cores = NULL;
	board->clock = 0;
	board->date = 0;
	board->evts = 0;
	board->log = csim_log;
}

/**
 * Destroy the given board.
 * @param board		Board to destroy.
 */
void csim_destroy_board(csim_board_t *board) {
}


/**
 * Add a component to the board.
 * @param board	Board to add to.
 * @param comp	Added component.
 */
void csim_add_component(csim_board_t *board, csim_component_t *comp) {
	
	comp->board = board;
	comp->next = board->comps;
	board->comps = comp;
	
	if(comp->core != NULL) {
		comp->core->next = board->cores;
		board->cores = comp->core;
		if(board->clock == 0)
			board->clock = comp->core->clock;
		else {
			if(board->clock != comp->core->clock) {
				fprintf(stderr, "ERROR: current version only supports multiple core with same clock.");
				abort();
			}
		}
	}
	
	comp->init(comp);
}


/**
 * Link two pins.
 * @param pin1	Pin 1.
 * @param pin2	Pin 2.
 */
void csim_link(csim_pin_t *pin1, csim_pin_t *pin2) {
	pin1->link = pin2;
	pin2->link = pin1;
}


/**
 * Send a message to a pin.
 * @param pin	Pin to send to.
 * @param msg	Message to send.
 */
void csim_send(csim_pin_t *pin, csim_msg_t *msg) {
	msg->date = pin->comp->board->date;
	pin->receive(pin, msg);
}


/**
 * Record a new event in the event queue.
 * @param board		Board to record event in.
 * @param evt		Event to record.
 */
void csim_record_event(csim_board_t *board, csim_evt_t *evt) {
	
	if(evt->date <= board->date) {
		evt->trigger(evt);
		if(evt->period == 0)
			return;
		else
			evt->date += evt->period;
	}
	
	if(board->evts == NULL || evt->date < board->evts->date) {
		evt->next = board->evts;
		board->evts = evt;
	}
	else {
		csim_evt_t *cur = board->evts;
		while(cur->next != NULL && cur->date < evt->date)
			cur = cur->next;
		if(cur->next != NULL)
			cur->next->prev = evt;
		evt->next = cur->next;
		cur->next = evt;
		evt->prev = cur;
	}
}


/**
 * Remove an event from the schedule.
 * @param board		Board to work with.
 * @param evt		Event to cancle.
 */
void csim_cancel_event(csim_board_t *board, csim_evt_t *evt) {
	if(board->evts == evt) {
		evt->next->prev = NULL;
		board->evts = evt->next;
		evt->next = NULL;
	}
	else {
		evt->next->prev = evt->prev;
		evt->prev->next = evt->next;
		evt->next = NULL;
		evt->prev = NULL;
	}
}


/**
 * Simulate for the given amount of time.
 * @param board		Board to simulate in.
 * @param time		Time in cycle (cycle duration depends on the board clock).
 */
void csim_run(csim_board_t *board, csim_time_t time) {
	csim_date_t end = board->date + time;
	while(board->date < end) {
		
		while(board->evts != NULL && board->evts->date <= board->date) {
			csim_evt_t *evt = board->evts;
			evt->trigger(evt);
			board->evts = evt->next;
			board->evts->prev = NULL;
			evt->next = NULL;
		}
		
		csim_core_t *core = board->cores;
		while(core != NULL)
			core->step(core);
		
		board->date++;
	}
}
