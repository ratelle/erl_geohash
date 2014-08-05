REBAR=./rebar

all: deps compile

deps:
	@$(REBAR) update-deps
	@$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:
	@rm -rf deps
	@$(REBAR) clean

.PHONY: all deps compile clean
