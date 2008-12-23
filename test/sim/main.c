#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include "./memory.h"
#include "loader.h"

int main(void) {
	gliss_memory_t *memory;
	gliss_loader_t *loader;
	
	/* allocate memory */
	memory = gliss_mem_new();
	assert(memory != NULL);
	
	/* open an ELF file */
	loader = gliss_loader_open("primes");
	if(loader == NULL) {
		printf("error %d:%s\n", errno, strerror(errno));
		return 1;
	}
	
	/* load it */
	gliss_loader_load(loader, memory);
	
	/* free it */
	gliss_loader_close(loader);
	
	/* free memory */
	gliss_mem_delete(memory);
	
	return 0;
}
