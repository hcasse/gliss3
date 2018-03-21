#include <stdio.h>

int main(void) {

$(with init)$(action)$(end)

	printf("%d %f %s\n", $(i), $(f), "$(s)");
	
	$(ifdef bool_x)
		printf("x defined!");
	$(else)
		printf("x not defined!");
	$(end)

	$(ifdef bool_y)
		printf("y defined!");
	$(else)
		printf("y not defined!");
	$(end)

	$(ifdef bool_z)
		printf("z defined!");
	$(else)
		printf("z not defined!");
	$(end)

	$(if bool_x)
		printf("x is true!");
	$(else)
		printf("x is false!");
	$(end)

	$(if bool_y)
		printf("y is true!");
	$(else)
		printf("y is false!");
	$(end)

	return 0;
}
