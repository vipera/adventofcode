.PHONY: all clean run-example run

all:
	@+$(MAKE) -s -C 2023 all

clean:
	@+$(MAKE) -s -C 2023 clean

run-example:
	@+$(MAKE) -s -C 2023 run-example

run:
	@+$(MAKE) -s -C 2023 run
