all: main

main: lib_simplemath.a main.c
	gcc -O2 -g3 -o main main.c -L. -l_simplemath

lib_simplemath.a: simplemath.c
	clang -O2 -g3 -c simplemath.c -o simplemath.o
	ar rcs lib_simplemath.a simplemath.o

clean:
	rm -f main simplemath.o lib_simplemath.a

.PHONY: all clean
