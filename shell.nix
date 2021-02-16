let sources = import ./nix/sources.nix;
in with import sources.nixpkgs { };
stdenv.mkDerivation {
  name = "szabo-gergely-portfolio";
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
