YKC := cargo run --quiet --release --
YKC_FLAGS := --emit-comments --opt

EXAMPLES_DIR := examples
SRCS := $(wildcard $(EXAMPLES_DIR)/*.yk)
EXAMPLES := $(SRCS:$(EXAMPLES_DIR)/%.yk=%)

ykc: src/*.rs
	cargo build --release
	cp ./target/release/ykc ykc

examples: $(EXAMPLES)

$(EXAMPLES): %: $(EXAMPLES_DIR)/%.yk
	$(YKC) $(YKC_FLAGS) $<

all: ykc examples

.PHONY: all examples
