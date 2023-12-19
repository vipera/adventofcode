.PHONY: all clean run-example run

all:

build:
	@cargo build

clean:
	@cargo clean

run-example:
	@cargo run -- -f example.txt
	@cargo run -- -f example.txt -j

run:
	@cargo run -- -f actual_input.txt
	@cargo run -- -f actual_input.txt -j

test:
	cargo test
