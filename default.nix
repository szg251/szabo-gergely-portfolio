{ env ? "staging" }:

let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };

  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , src
    , name
    , srcdir ? "./src"
    , targets ? [ ]
    , registryDat ? ./registry.dat
    , outputJavaScript ? false
    }:
    nixpkgs.stdenv.mkDerivation {
      inherit name src;

      buildInputs = [ nixpkgs.elmPackages.elm nixpkgs.haskellPackages.mustache ]
        ++ nixpkgs.lib.optional outputJavaScript nixpkgs.nodePackages.uglify-js;

      buildPhase = nixpkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        elmVersion = "0.19.1";
        inherit registryDat;
      };

      installPhase =
        let
          elmfile = module:
            "${srcdir}/${builtins.replaceStrings [ "." ] [ "/" ] module}.elm";

          extension = if outputJavaScript then "js" else "html";
        in
        ''
          ${nixpkgs.lib.concatStrings (map (module: ''
            echo "compiling ${elmfile module}"
            elm make ${
              elmfile module
            } --output $out/${module}.${extension} --optimize --docs $out/share/doc/${module}.json
            ${nixpkgs.lib.optionalString outputJavaScript ''
              echo "minifying ${elmfile module}"
              uglifyjs $out/${module}.${extension} --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
                  | uglifyjs --mangle --output $out/${module}.min.${extension}
            ''}
          '') targets)}
          cp -r $PWD/${srcdir}/assets/* $out
          haskell-mustache $out/index.html.mustache $PWD/env.${env}.json > $out/index.html
          rm $out/index.html.mustache
          cp vercel.json $out/
        '';
    };
in
mkDerivation {
  name = "szabo-gergely-portfolio";
  srcs = ./elm-srcs.nix;
  src = ./.;
  targets = [ "Main" ];
  srcdir = "./src";
  outputJavaScript = true;
}
