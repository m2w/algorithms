ERLC=erlc
ERL=erl

all:
	@ mkdir -p ./ebin ;
	@ $(ERLC) -o ./ebin src/*.erl ;
clean:
	@ rm -rf ./ebin ;
console:
	@ $(ERL) -pa ./ebin ;
test: all
	@ $(ERL) -noshell -pa ebin -eval 'eunit:test("ebin", [verbose])' -s init stop
