run: 
	nix-shell --run "make prepare-dev && make dev-server"

prepare-dev:
	rm -rf  ./dist
	cp -r ./src/assets ./dist

dev-server:
	elm-live ./src/Main.elm --pushstate --dir=./dist/ -- --output=./dist/Main.min.js --debug

build:
	nix build

tests:
	nix-shell --run "elm-test"

format:
	nix-shell --run "elm-format src/"

configure:
	nix-shell --run "elm2nix convert > elm-srcs.nix && elm2nix snapshot > registry.dat"