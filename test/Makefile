# Should be equivalent to your list of C files, if you don't build selectively
SRC=$(wildcard *.erl)
BEAM = $(SRC:.erl=.beam)

.PHONY: all
all: $(BEAM)

%.beam: %.erl
	erlc +debug_info -S $< && erlc $< 
