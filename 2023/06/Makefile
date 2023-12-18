.PHONY: all clean run-example run

all:

build:
	@bundle install

clean:

run-example:
	@./race.rb -f example.txt
	@./race.rb -f example.txt -i

run:
	@./race.rb -f actual_input.txt
	@./race.rb -f actual_input.txt -i

test:
	bundle exec rspec
