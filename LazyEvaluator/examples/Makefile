run=runghc --ghc-arg=-fdefer-type-errors -XBlockArguments

all: Ex1 Church

# .PHONY: Ex1 Church

% : %.hs Makefile
	@$(run) $<

# EOF
