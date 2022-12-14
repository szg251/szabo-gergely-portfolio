ENV=staging

run: 
	nix-shell --run "make prepare-dev && make dev-server"

prepare-dev:
	rm -rf  ./dist
	cp -r ./src/assets ./dist
	haskell-mustache ./dist/index.html.mustache ./env.${ENV}.json > ./dist/index.html
	rm ./dist/index.html.mustache

dev-server:
	elm-live ./src/Main.elm --pushstate --dir=./dist/ -- --output=./dist/Main.min.js --debug

build:
	nix-build --argstr env ${ENV}

# deploy: build 
deploy:  
	nix-shell --run "ENV=${ENV} ./scripts/deploy.sh"

tests:
	nix-shell --run "elm-test"

format:
	nix-shell --run "elm-format src/"

configure:
	nix-shell --run "elm2nix convert > elm-srcs.nix && elm2nix snapshot > registry.dat"
