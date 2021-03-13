all: build

build:
	@dune build @all
	@cp -f _build/default/bin/main.exe /usr/local/bin/test_chungus
	@echo "\n"
	@test_chungus

install:
	@dune install

test: build
	@dune runtest

doc: build
	@opam install odoc
	@dune build @doc

clean:
	@dune clean

# Create a release on Github, then run git pull
publish:
	@git tag 1.0
	@git push origin 1.0
	@git pull
	@opam pin .
	@opam publish https://github.com/chrisnevers/chungus/archive/1.0.tar.gz
