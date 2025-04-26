main: main.o
	ld -o main main.o -lSystem -syslibroot `xcrun --show-sdk-path` -e _main -arch arm64

main.o: main.s
	as -arch arm64 -o main.o main.s

main.s: main.yk src/*.rs
	cargo run --release -- --emit-comments -o main.s ./main.yk

clean:
	rm main main.o

.PHONY: clean
