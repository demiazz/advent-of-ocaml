YEAR=2024
DAY=2
PART=2

.PHONY: init build-native build-web build dev-web run test

init:
	opam switch create . 5.2.0 -y --deps-only
	pnpm install
	opam install -y . --deps-only --with-dev --with-test
	opam exec opam-check-npm-deps

init-ci:
	pnpm install
	opam install . --deps-only --locked --with-test --yes
	opam exec opam-check-npm-deps

build-native:
	opam exec -- dune build

build-web: build-native
	pnpm vite build

build: build-native build-web

dev-web: build-native
	pnpm vite dev

dev-native:
	opam exec -- dune build --watch

run: build-native
	_build/default/bin/main.exe -y $(YEAR) -d $(DAY) -p $(PART) -i input.txt

test:
	opam exec -- dune test
