.PHONY: all clean run-example run

CC = gcc
IDIR = .
ODIR = obj
CFLAGS = -I$(IDIR) -Werror -Wall -Wextra -Wpedantic

_DEPS = schematic.h utils.h
_OBJ = schematic.o utils.o main.o

DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

$(ODIR)/%.o: %.c $(DEPS)
	@mkdir -p $(ODIR)
	@$(CC) -c -o $@ $< $(CFLAGS)

all: $(OBJ)
	@$(CC) $^ $(CFLAGS)

clean:
	@rm -rf $(ODIR)
	@rm -f a.out core

run-example: all
	@./a.out example.txt

run: all
	@./a.out actual_input.txt
