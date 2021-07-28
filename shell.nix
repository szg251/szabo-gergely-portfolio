let
  sources = import ./nix/sources.nix;
in
  with import sources.nixpkgs {};
  mkShell {
    buildInputs = with elmPackages; [
      elm
      elm-live
      elm-format
      elm-analyse
      elm-test
      nodePackages.uglify-js
      elm2nix
      haskellPackages.mustache
    ];
  }
