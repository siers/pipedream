with (import <nixpkgs> {});

let
  ghc = haskell.packages.ghc865.ghcWithPackages (packages: with packages; [
    matrix
  ]);
in
  mkShell {
    buildInputs = [
      ghc
    ];
  }
