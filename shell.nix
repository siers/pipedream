with (import <nixpkgs> {});

let
  ghc = haskell.packages.ghc884.ghcWithPackages (packages: with packages; [
    aeson
    base
    bytestring
    containers
    extra
    fgl
    graphviz
    hip
    lens
    mtl
    network
    parallel
    parallel-io
    primitive
    text
    transformers
    unordered-containers
    vector
    websockets
  ]);
in
  mkShell {
    buildInputs = [
      ghc
      haskell.packages.ghc884.haskell-language-server
      ws
      cabal-install
      graphviz
    ];
  }
