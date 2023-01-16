.PHONY: coolc-parse coolc-parse-clean

COOL_PARSER_SOURCES := $(shell find tests/parser/ -type f -name '*.cl')
COOL_PARSER_OUT := $(COOL_PARSER_SOURCES:%.cl=%.coolc-parse)

coolc-parse: $(COOL_PARSER_OUT)

%.coolc-parse: %.cl
ifndef COOLC_LEXER
	echo "COOLC_LEXER is not defined" > /dev/stderr; false
else
ifndef COOLC_PARSER
	echo "COOLC_PARSER is not defined" > /dev/stderr; false
else
	$(COOLC_LEXER) $< | $(COOLC_PARSER) > $@; if [ $$? -ne 0 ]; then echo "ERR parsing failed" > $@; fi;
endif
endif


coolc-parse-clean:
	-rm $(COOL_PARSER_OUT)
