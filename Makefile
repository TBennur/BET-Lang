UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
ARCH := elf64
target:=
endif
ifeq ($(UNAME), Darwin)
ARCH := macho64
target:= --target=x86_64-apple-darwin
endif

tests/%.s: tests/%.bet src/main.rs
	cargo run -- $< tests/lib$*.s

tests/%.run: tests/%.s runtime/start.rs
	nasm -f $(ARCH) tests/lib$*.s -o tests/lib$*.o
	ar rcs tests/lib$*.a tests/lib$*.o
	rustc -L tests/ -lour_code:$* runtime/start.rs -o tests/$*.run $(target)

.PHONY: test
test:
	cargo build
	cargo test

clean:
	rm -f tests/**/*.a tests/**/*.s tests/**/*.run tests/**/*.o
	rm -f tests/*.a tests/*.s tests/*.run tests/*.o
