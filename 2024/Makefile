SUBMAKEFILES = $(shell find . -mindepth 2 -maxdepth 2 -type f -name Makefile -not -path '*/.*' | sort)
SUBDIRS = $(filter-out ./,$(dir $(SUBMAKEFILES)))

.PHONY: all clean run-example run

all:
	@for dir in $(SUBDIRS); do \
		echo ""; \
		echo "Entering $$dir"; \
		echo "--------------------------------------------------------"; \
		$(MAKE) -C $$dir all; \
	done

clean:
	@for dir in $(SUBDIRS); do \
		echo ""; \
		echo "Entering $$dir"; \
		echo "--------------------------------------------------------"; \
		$(MAKE) -C $$dir clean; \
	done

run-example:
	@for dir in $(SUBDIRS); do \
		echo ""; \
		echo "Entering $$dir"; \
		echo "--------------------------------------------------------"; \
		$(MAKE) -C $$dir run-example; \
	done

run:
	@for dir in $(SUBDIRS); do \
		echo ""; \
		echo "Entering $$dir"; \
		echo "--------------------------------------------------------"; \
		$(MAKE) -C $$dir run; \
	done
