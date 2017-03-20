qtest: testq.o queue.o
	gcc -g -Wall -Wextra --std=gnu99 -o $@ $^
%.o: %.c
	gcc -c -g -Wall -Wextra --std=gnu99 -o $@ $<
clean:
	rm -f *.o qtest
