main: main.o
	ld -o main main.o -lSystem -syslibroot `xcrun --show-sdk-path` -e _main -arch arm64

main.o: main.s
	as -arch arm64 -o main.o main.s

main.s: main.yk target/release/ykc
	./target/release/ykc ./main.yk > main.s

target/release/ykc: src/*.rs
	cargo build --release

clean:
	rm main main.o

.PHONY: clean
