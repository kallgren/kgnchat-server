.SILENT:

all: clean server test doc

ERLC := erlc
ERLC_FLAGS := -W -I include

ERL_FILES := $(wildcard src/*.erl)
BEAM_FILES := $(patsubst src/%.erl,ebin/%.beam,${ERL_FILES})

comma:= ,
empty:=
space:= $(empty) $(empty)

EDOC_SRC := $(filter-out %_test.erl, $(ERL_FILES))
EDOC_SRC_LIST := [$(subst $(space),$(comma),$(patsubst src/%.erl,'src/%.erl', $(EDOC_SRC)))]

server: $(BEAM_FILES)

ebin/%.beam: src/%.erl
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

start: server
	(cd ebin && erl -noshell -eval 'server:start(8040), init:stop()')

test: server
	(cd ebin && erl -noinput -eval 'eunit:test({dir, "."}, [verbose]), init:stop()')

doc: $(BEAM_FILES)
	erl -noshell -eval "edoc:files($(EDOC_SRC_LIST), [{dir, 'doc'}])" -s init stop

clean:
	rm -fr .#* *.dump
	rm -fr ebin/*.dump
	rm -fr ebin/*.beam
	(cd doc && find . -name "*" -a ! -name overview.edoc ! -name . -exec rm -rf {} \;)