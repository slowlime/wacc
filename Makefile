test: coolc-parse
	cargo insta test --review

include tests/parser/Makefile

.PHONY: test clean

clean: coolc-parse-clean

./Makefile: tests/parser/Makefile
