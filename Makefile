# Makefile
LIBDIR		= `erl -eval \
	'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
VERSION		= 0.0.1
CC				= erlc
ERL				= erl
EBIN			= ebin
CFLAGS		= -I include -I /usr/local/Cellar/yaws//1.87/lib/yaws/include/ -pa $(EBIN) -Ddebug
COMPILE		= $(CC) $(CFLAGS) -o $(EBIN)
EBIN_DIRS = $(wildcard deps/*/ebin)

#all: mochi ebin compile
all: ebin compile
all_boot: all make_boot
run: all run_app
tests: all generate_tests

#mochi:
#	 @(cd deps/mochiweb;$(MAKE))

compile:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	-eval 'case make:all() of up_to_date -> halt(0); \
				error -> halt(1) end.'

edoc:
	@echo Generating $(APP) documentation from srcs
	@erl -noinput -eval 'edoc:application($(APP), "./", \
				[{doc, "doc/"}, {files, "src/"}])' -s erlang halt

make_boot:
	(cd ebin; erl -pa ebin -noshell \
		-run make_boot write_scripts rest_app)

start_all:
	(cd ebin; erl -pa ebin -noshell -sname _name_ -boot _name_)

run_app:
	erl -pa ebin -config cdb -eval "application:start(cdb)." -boot start_sasl

ebin:
	@mkdir ebin

clean:
	rm -rf ebin/*.beam ebin/erl_crash.dump erl_crash.dump
	rm -rf ebin/*.boot ebin/*.rel ebin/*.script
	rm -rf doc/*.html doc/*.css doc/erlang.png doc/edoc-info
	rm -rf test/*.beam

generate_tests:
	run_test -dir $(PWD)/test/ -include $(PWD)/include -pa $(PWD)/ebin/ -logdir log/test/