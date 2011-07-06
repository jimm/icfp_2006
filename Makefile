COMPILE_OPTS = -m32

vm:	vm.c
	gcc $(COMPILE_OPTS) -o vm vm.c

clean:
	rm -f vm
