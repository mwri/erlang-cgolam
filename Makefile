all: compile

compile:
	rebar -v compile

docs:
	rebar skip_deps=true doc

clean:
	rebar clean

otp.plt: Makefile
	dialyzer --build_plt --output_plt otp.plt --apps \
		erts kernel stdlib hipe compiler crypto syntax_tools wx

cgolam.plt: otp.plt
	dialyzer --add_to_plt --plt otp.plt --output_plt cgolam.plt ebin

dialyzer: compile cgolam.plt
	dialyzer -o dialyzer.log --add_to_plt --plt otp.plt --output_plt cgolam.plt ebin
	dialyzer --plt cgolam.plt -o dialyzer.log ebin

test: compile dialyzer
	mkdir -p deps
	rm -f deps/cgolam
	ln -s .. deps/cgolam
	rebar skip_deps=true ct
	rm deps/cgolam
