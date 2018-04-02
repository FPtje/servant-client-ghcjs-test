{pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv}:
let
  nixpkgs = import (pkgs.fetchFromGitHub {
    rev = "2070830ba845b69c7c597537786d9b9498fa4b28";
    sha256 = "0b8dyz53k5hkg0ngnp1kxjysz0hdw7vr47g5xcrprrfh32piqcil";
    owner = "NixOS";
    repo = "nixpkgs-channels";
  }) {};

  hlib = pkgs.haskell.lib;

  ghc = nixpkgs.haskellPackages;
  ghcjs = nixpkgs.haskell.packages.ghcjsHEAD;

  # Obvious TODO is not linking to a relative path. Servant doesn't have nix
  # files by default though (you have to run
  # servant/scripts/generate-nix-files.sh), so it's not trivial
  servant-ghc = hlib.doJailbreak (ghc.callPackage ../servant/servant {});
  servant-server = hlib.dontCheck (hlib.doJailbreak (ghc.callPackage ../servant/servant-server { servant = servant-ghc; }));

  servant-ghcjs = hlib.doJailbreak (ghcjs.callPackage ../servant/servant {});
  servant-client-core = hlib.doJailbreak (ghcjs.callPackage ../servant/servant-client-core { servant = servant-ghcjs; });
  servant-client-ghcjs = hlib.doJailbreak (ghcjs.callPackage ../servant/servant-client-ghcjs { inherit servant-client-core; });
in
{
  test-server = ghc.callPackage ./test-server { servant = servant-ghc; inherit servant-server;};
  test-client = ghcjs.callPackage ./test-client { servant = servant-ghcjs; inherit servant-client-core servant-client-ghcjs; };
}
