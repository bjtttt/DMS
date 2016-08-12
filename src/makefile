EBIN := ../ebin

%.beam: %.erl
	erlc +debug_info -W -o $(EBIN) $<
        
MODS = $(shell ls *.erl) $(shell ls cc/*.erl) $(shell ls helper/*.erl) $(shell ls vdr/*.erl) $(shell ls eredis/*.erl)

compile: ${MODS:%.erl=%.beam}
	@echo "make clean - clean up"
        
all: compile

clean:
	rm -rf  $(EBIN)/*.beam