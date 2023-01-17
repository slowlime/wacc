test: coolc-parse coolc-typeck
	cargo insta test --review

include tests/parser/Makefile
include tests/typeck/Makefile

.PHONY: test clean

clean: coolc-parse-clean coolc-typeck-clean

./Makefile: tests/parser/Makefile tests/typeck/Makefile
