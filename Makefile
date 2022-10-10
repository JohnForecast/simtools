# all of these can be over-ridden on the "make" command line if they don't suit your environment.

CFLAGS=-O2 -Wall -Wshadow -Wextra -pedantic -Woverflow -Wstrict-overflow
BIN=/usr/local/bin
INSTALL=install
CC=gcc

SUBDIRS=config11 converters crossassemblers extracters

.PHONY: all clean install uninstall

# Omitted: putr: has no sources.
all:
	for subdir in $(SUBDIRS); do \
		$(MAKE) -C $$subdir CFLAGS="$(CFLAGS)" BIN="$(BIN)" INSTALL="$(INSTALL)" CC="$(CC)"; \
	done

clean install uninstall:
	for subdir in $(SUBDIRS); do \
		$(MAKE) -C $$subdir CFLAGS="$(CFLAGS)" BIN="$(BIN)" INSTALL="$(INSTALL)" CC="$(CC)" $@; \
	done
