YKC := cargo run --quiet --release --
YKC_FLAGS := --emit-comments --opt

EXAMPLES_DIR := examples
SRCS := $(wildcard $(EXAMPLES_DIR)/*.yk)
BINS := $(SRCS:$(EXAMPLES_DIR)/%.yk=%)

ykc: src/*.rs
	cargo build --release
	cp ./target/release/ykc ykc

$(BINS): %: $(EXAMPLES_DIR)/%.yk
	$(YKC) $(YKC_FLAGS) $<

all: $(BINS)

clean:
	rm -f $(BINS)

.PHONY: all clean
