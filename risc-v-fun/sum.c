#include <stdio.h>

int n = 100;
int main (int argc, char* argv[ ]) {
	int i;
	int m = n;
	int sum = 0;
	for (i = 1; i <= m; i++) {
		sum += i;
	}
	printf ("Sum 1 to %d is %d\n", n, sum);
}
