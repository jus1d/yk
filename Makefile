main: main.o
	ld -o main main.o -lSystem -syslibroot `xcrun --show-sdk-path` -e _main -arch arm64

main.o: main.s
	as -arch arm64 -o main.o main.s

main.s: main.yk ykc
	./ykc --emit-comments --opt -o main.s ./main.yk

ykc: src/*.rs
	cargo build --release
	cp ./target/release/ykc ykc

all: examples/hello main

clean:
	rm main main.o main.s examples/hello examples/hello.o examples/hello.s

.PHONY: all clean
