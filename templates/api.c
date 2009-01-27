/* Generated by gep ($(date)) copyright (c) 2008 IRIT - UPS */

#include <stdlib.h>
#include <errno.h>
#include "../include/$(proc)/api.h"
#include "platform.h"
#include "loader.h"


/* j'ai mis �a l� en attendant mieux, je m'en sers pour le d�buggage */
char *$(proc)_string_ident[] = {
	"$(PROC)_UNKNOWN"$(foreach instructions),
	"$(PROC)_$(IDENT)"$(end)};


/**
 * @typedef $(proc)_platform
 * This opaque type allows to represent an hardware and software platform.
 * It includes information about:
 * @li the hardware memories,
 * @li the module data (including system calls and interruption support).
 */

/**
 * Build a new platform for the platform $(proc).
 * @return	Created platform or null if there is no more memory (see errno).
 * @note To release the platform, use $(proc)_unlock_platform().
 */
$(proc)_platform_t *$(proc)_new_platform(void) {
	$(proc)_platform_t *pf;
	
	/* allocation */
	pf = ($(proc)_platform_t *)calloc(1, sizeof($(proc)_platform_t));
	if(pf == NULL) {
		errno = ENOMEM;
		return NULL;
	}
	pf->usage = 1;
	
	/* memory initialization */
$(foreach memories)
	pf->mems.named.$(name) = $(proc)_mem_new();
	if(pf->mems.named.$(name) == NULL) {
		$(proc)_unlock_platform(pf);
		return NULL;
	}
$(end)
	
	/* module initialization */
$(foreach modules)
	$(PROC)_$(NAME)_INIT(pf);
$(end)
	
	/* return platform */
	return pf;
}


/**
 * Get a memory in the platform.
 * @param platform	Platform to get memory from.
 * @param index		Index of the memory to get.
 * @return			Requested memory.
 */
$(proc)_memory_t *$(proc)_get_memory($(proc)_platform_t *platform, int index) {
	return platform->mems.array[index];
}


/**
 * Ensure that the platform is not released before a matching
 * $(proc)_platform_unlock() is performed.
 * @param platform	Platform to lock.
 */
void $(proc)_lock_platform($(proc)_platform_t *platform) {
	platform->usage++;
}


/**
 * Unlock the platform. If it the last lock on the platform,
 * the platform is fried.
 * @param platform	Platform to unlock.
 */
void $(proc)_unlock_platform($(proc)_platform_t *platform) {

	/* unlock */
	if(--platform->usage != 0)
		return;
	
	/* desotry the modules */
$(foreach modules)
	$(PROC)_$(NAME)_DESTROY(platform);
$(end)
	
	/* free the memories */
$(foreach memories)
	$(proc)_mem_delete(platform->mems.named.$(name));
$(end)

	/* free the platform */
	free(platform);
}


/**
 * Load the given program in the platform.
 * @param platform	Platform to load in.
 * @param path		Path of the file to load.
 * @return			0 for success, -1 for error (in errno).
 */
int $(proc)_load_platform($(proc)_platform_t *platform, const char *path) {
	$(proc)_loader_t *loader;

	/* open the file */
	loader = $(proc)_loader_open(path);
	if(loader == NULL)
		return -1;
	
	/* load in memory */
	$(proc)_loader_load(loader, $(proc)_get_memory(platform, $(PROC)_MAIN_MEMORY));
	
	/* initialize system information */
	platform->entry = $(proc)_loader_start(loader);
	
	/* close the file */
	$(proc)_loader_close(loader);

	/* return success */
	return 0;
}

