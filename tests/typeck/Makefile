.PHONY: coolc-typeck coolc-typeck-clean

COOL_TYPECK_SOURCES := $(shell find tests/typeck/ -type f -name '*.cl')
COOL_TYPECK_OUT := $(COOL_TYPECK_SOURCES:%.cl=%.coolc-typeck)

coolc-typeck: $(COOL_TYPECK_OUT)

%.coolc-typeck: %.cl
ifndef COOLC_LEXER
	echo "COOLC_LEXER is not defined" > /dev/stderr; false
else
ifndef COOLC_PARSER
	echo "COOLC_PARSER is not defined" > /dev/stderr; false
else
ifndef COOLC_SEM
	echo "COOLC_SEM is not defined" > /dev/stderr; false
else
	$(COOLC_LEXER) $< | $(COOLC_PARSER) | $(COOLC_SEM) > $@; if [ $$? -ne 0 ]; then echo "ERR typeck failed" > $@; fi;
endif
endif
endif

coolc-typeck-clean:
	-rm $(COOL_TYPECK_OUT)
