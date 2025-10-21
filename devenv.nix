{
  pkgs,
  lib,
  ...
}:

{
  packages = [
    pkgs.elmPackages.elm-live
    pkgs.elmPackages.elm-review
    pkgs.haskellPackages.mustache
    pkgs.nodePackages.uglify-js
    pkgs.nodePackages.vercel
  ];

  languages.elm.enable = true;

  git-hooks.hooks = {
    typos.enable = true;
    elm-format.enable = true;
  };

  tasks =
    let
      envs = [
        "local"
        "staging"
        "prod"
      ];
    in
    {
      "packages:elm2nix" = {
        exec = "elm2nix convert > elm-srcs.nix && elm2nix snapshot";
        execIfModified = [ "elm.json" ];
        before = [ "devenv:enterShell" ];
      };

      "deploy:prod" = {
        exec = ''
          test -d dist/prod
          cp -r .vercel dist/prod/
          vercel deploy dist/prod --local-config vercel.json --prod -y
        '';
      };

      "deploy:staging" = {
        exec = ''
          test -d dist/staging
          cp -r .vercel dist/staging/
          vercel deploy dist/staging --local-config vercel.json -y
        '';
      };

    }
    // pkgs.lib.mergeAttrsList (
      builtins.map (env: {

        # Prepare index.html
        "${env}:prepare-index" = {
          exec = ''
            mkdir -p dist/${env}
            haskell-mustache src/index.html.mustache $PWD/env.${env}.json > dist/${env}/index.html
          '';
        }
        // lib.optionalAttrs (env == "local") {
          before = [ "devenv:processes:dev" ];
        };

        # Transpile Elm code into JS
        "${env}:make-elm" = {
          exec = ''
            mkdir -p dist/${env}
            elm make src/Main.elm --output=dist/${env}/Main.min.js
          '';
        };

        # Minify JS file
        "${env}:minify" = {
          exec = ''
              uglifyjs dist/${env}/Main.min.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
            | uglifyjs --mangle --output dist/${env}/Main.min.js
          '';
        }
        // lib.optionalAttrs (env == "staging" || env == "prod") {
          after = [ "${env}:make-elm" ];
        };

        # Copy assets to dist directory
        "${env}:prepare-assets" = {
          exec = ''
            mkdir -p dist/${env}
            cp -r src/assets/* dist/${env}/
          '';
        }
        // lib.optionalAttrs (env == "local") {
          before = [ "devenv:processes:dev" ];
        };

      }) envs
    );

  processes.dev.exec = ''
    elm-live src/Main.elm \
    --dir dist/local \
    --open \
    --pushstate \
    -- --output=dist/local/Main.min.js
  '';
}
