{ mkDerivation, aeson, base, bytestring, clock, containers
, derive-storable, directory, extra, fgl, hip, lattices, lens, lib
, mtl, network, parallel-io, pomaps, primitive, template-haskell
, text, time, transformers, unordered-containers, vector
, websockets
}:
mkDerivation {
  pname = "solve";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring clock containers derive-storable directory
    extra fgl hip lattices lens mtl network parallel-io pomaps
    primitive template-haskell text time transformers
    unordered-containers vector websockets
  ];
  description = "solves maze";
  license = "unknown";
}
