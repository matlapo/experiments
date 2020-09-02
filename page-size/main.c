#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define PGSIZE 4096 // bytes per page
#define PGSHIFT 12  // bits of offset within a page

#define PGROUNDUP(sz)  (((sz)+PGSIZE-1) & ~(PGSIZE-1))
#define PGROUNDDOWN(a) (((a)) & ~(PGSIZE-1))

int main() {
	/*int fd = dup(1);*/
    /*write(1, "hello ", 6);*/
    /*write(fd, "world\en", 6);*/

	unsigned int x = 7;
	unsigned int y = PGROUNDUP(x);
	unsigned int z = PGROUNDDOWN(x);

	printf("%d\n", y);
	printf("%d\n", z);

	return 0;
}
