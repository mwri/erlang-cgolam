all: compile

compile:
	rebar3 compile

docs:
	rebar3 doc

clean:
	rebar3 clean
	rm -rf doc/*.html doc/edoc-info doc/erlang.png doc/stylesheet.css _build \
		cgolam.plt otp.plt

otp.plt: Makefile
	dialyzer --build_plt --output_plt otp.plt --apps \
		erts kernel stdlib hipe compiler crypto syntax_tools wx

cgolam.plt: otp.plt
	dialyzer --add_to_plt --plt otp.plt --output_plt cgolam.plt \
		_build/default/lib/cgolam/ebin

dialyzer: compile cgolam.plt
	dialyzer -o dialyzer.log --add_to_plt --plt otp.plt --output_plt cgolam.plt \
		_build/default/lib/cgolam/ebin
	dialyzer --plt cgolam.plt -o dialyzer.log \
		_build/default/lib/cgolam/ebin

test: compile dialyzer
	rebar3 do ct,cover
