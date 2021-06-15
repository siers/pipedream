with (import <nixpkgs> {});

let
  haskellPackages' =
    (haskell.packages.ghc884.override {
      all-cabal-hashes = pkgs.fetchurl {
        url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/12206435934e53eeda06b73e93bdee29a0ff713b.tar.gz";
        sha256 = "0d3xis5mli5yskvyxiqr3z7gzvnnm22zrm6dlcpbj4mdqrp2rh10";
      };
    }).extend (self: super: {
      pomaps = self.callHackage "pomaps" "0.2.0.1" {};
    });

  ghc = haskellPackages'.ghcWithPackages (packages: with packages; [
  # ghc = haskell.packages.ghc884.ghcWithPackages (packages: with packages; [
    aeson
    base
    bytestring
    clock
    containers
    derive-storable
    directory
    extra
    fgl
    hip
    lattices
    lens
    mtl
    network
    parallel-io
    pomaps
    primitive
    text
    time
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
