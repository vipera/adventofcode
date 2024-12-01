.PHONY: all clean run-example run

all:

clean:

run-example:
	@erlc ../common.erl
	@./distances.escript example.txt
	@./similarity.escript example.txt

run:
	@erlc ../common.erl
	@./distances.escript actual_input.txt
	@./similarity.escript actual_input.txt
