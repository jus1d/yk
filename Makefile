main: main.o
	ld -o main main.o -lSystem -syslibroot `xcrun --show-sdk-path` -e _main -arch arm64

main.o: main.s
	as -arch arm64 -o main.o main.s

main.s: main.yk src/*.rs
	cargo run --release -- --emit-comments --opt -o main.s ./main.yk

examples/hello: examples/hello.o
	ld -o examples/hello examples/hello.o -lSystem -syslibroot `xcrun --show-sdk-path` -e _main -arch arm64

examples/hello.o: examples/hello.s
	as -arch arm64 -o examples/hello.o examples/hello.s

examples/hello.s: examples/hello.yk
	cargo run --release -- --emit-comments -o examples/hello.s examples/hello.yk

all: examples/hello main

clean:
	rm main main.o main.s examples/hello examples/hello.o examples/hello.s

.PHONY: all clean
