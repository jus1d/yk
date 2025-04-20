main: main.o
	ld -o main main.o -lSystem -syslibroot `xcrun --show-sdk-path` -e _main -arch arm64

main.o: main.s
	as -arch arm64 -o main.o main.s

# main.s: main.w target/release/compiler
# 	./target/release/compiler ./main.w > main.s

# target/release/compiler: src/main.rs src/lexer.rs src/parser.rs src/compiler.rs
# 	cargo build --release

clean:
	rm main main.o

.PHONY: clean
