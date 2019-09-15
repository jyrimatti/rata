{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:

let
  inherit (nixpkgs) pkgs;

  hp = if compiler == "default"
       then pkgs.haskellPackages
       else pkgs.haskell.packages.${compiler};

  haskellPackages = hp.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
      QuickCheck = pkgs.haskell.lib.dontCheck super.QuickCheck;
      comonad = pkgs.haskell.lib.dontCheck super.comonad;
      lens = pkgs.haskell.lib.dontCheck super.lens;
      extra = pkgs.haskell.lib.dontCheck super.extra;
      http-types = pkgs.haskell.lib.dontCheck super.http-types;
      semigroupoids = pkgs.haskell.lib.dontCheck super.semigroupoids;
      yaml = pkgs.haskell.lib.dontCheck super.yaml;
      geojson = pkgs.haskell.lib.dontCheck super.geojson;
    });
  });

  ghcjs-base-stub-forked = { fetchgit, stdenv, mkDerivation,
        aeson, attoparsec, containers, deepseq, ghc-prim, primitive, scientific, text, transformers, unordered-containers, vector
     }:
     mkDerivation {
       pname = "ghcjs-base-stub";
       version = "0.2.0.0";
       src = ../ghcjs-base-stub/.;
       libraryHaskellDepends = [
        aeson attoparsec containers deepseq ghc-prim primitive scientific text transformers unordered-containers vector
       ];
       buildDepends = [];
       license = stdenv.lib.licenses.bsd3;
     };

  ghcjsbase = if compiler == "ghcjs"
              then haskellPackages.ghcjs-base
              else haskellPackages.ghcjs-base-stub;
              #else haskellPackages.callPackage ghcjs-base-stub-forked {};

  myproject = { mkDerivation, base, deepseq, ghcjs-base, react-hs, react-native-hs, stdenv, nodejs, z3,
        text, time, transformers, containers, network-uri, ghcjs-fetch, geojson, bytestring, generic-data
      }:
      mkDerivation {
        pname = "myproject";
        version = "0.1.0.0";
        src = if pkgs.lib.inNixShell then null else ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base deepseq ghcjs-base react-hs react-native-hs text time transformers containers network-uri ghcjs-fetch geojson bytestring generic-data
        ];
        buildDepends = [pkgs.haskellPackages.cabal-install];
        license = stdenv.lib.licenses.mit;
      };

  ghcjs-fetch-forked = { fetchgit, stdenv, mkDerivation,
        aeson, bytestring, case-insensitive, http-types, ghcjs-base, QuickCheck, hspec, hspec-core
     }:
     mkDerivation {
       pname = "ghcjs-fetch";
       version = "0.1.0.1";
       src = ../ghcjs-fetch/.;
       libraryHaskellDepends = [
        aeson bytestring case-insensitive http-types ghcjs-base
       ];
       buildDepends = [ QuickCheck hspec hspec-core ];
       license = stdenv.lib.licenses.bsd3;
     };

  # my fork, until it gets to Hackage
  react-hs-forked = { fetchgit, stdenv, mkDerivation,
        aeson, base, bytestring, ghcjs-base, mtl, string-conversions,
        template-haskell, text, time, unordered-containers
     }:
     mkDerivation {
       pname = "react-hs";
       version = "0.1.1";
       src = fetchgit {
         url = "https://github.com/jyrimatti/react-hs.git";
         sha256 = "0s7c15pmfhlccr6qgl6jn6izbvnqchh739i0h0sx1yiaarqwmzwy";
         rev = "9390f850861102e84e38514a577ce7f3b1aac23f";
       };
       postUnpack = "sourceRoot=$sourceRoot/react-hs";
       libraryHaskellDepends = [
         aeson base bytestring ghcjs-base mtl string-conversions
         template-haskell text time unordered-containers
       ];
       homepage = "https://github.com/jyrimatti/react-hs";
       description = "A binding to React based on the Flux application architecture for GHCJS";
       license = stdenv.lib.licenses.bsd3;
     };

  react-native-hs-git = { fetchgit, mkDerivation, base, deepseq, ghcjs-base, react-hs, stdenv, nodejs,
        text, time, transformers, containers, network-uri
     }:
     mkDerivation {
       pname = "react-native-hs";
       version = "0.1.1";
       src = fetchgit {
         url = "https://github.com/jyrimatti/react-native-hs.git";
         sha256 = "062ypzgk1dyv74svgkz6sqvs6a8a8c3ky6r1nr97qbg0jgipb8y3";
         rev = "73734c37ec787d3fd785ee4612675bb8956f3b46";
       };
       libraryHaskellDepends = [
        react-hs text time transformers containers network-uri
       ];
       homepage = "https://github.com/jyrimatti/react-native-hs";
       description = "React-native support for react-hs";
       license = stdenv.lib.licenses.mit;
     };

  react-native-hs-local = { mkDerivation, base, deepseq, ghcjs-base, react-hs, stdenv, nodejs,
        text, time, transformers, containers, network-uri
     }:
     mkDerivation {
       pname = "react-native-hs";
       version = "0.1.1";
       src = ../react-native-hs/.;
       libraryHaskellDepends = [
        react-hs text time transformers containers network-uri
       ];
       homepage = "https://github.com/jyrimatti/react-native-hs";
       description = "React-native support for react-hs";
       license = stdenv.lib.licenses.mit;
     };

  react-hs-local = { mkDerivation, aeson, base, bytestring, ghcjs-base, mtl, stdenv, string-conversions, template-haskell, text, time, unordered-containers, base-compat-batteries
     }:
     mkDerivation {
       pname = "react-hs";
       version = "0.1.1";
       src = ../react-hs/react-hs/.;
       libraryHaskellDepends = [
        aeson base bytestring ghcjs-base mtl string-conversions template-haskell text time unordered-containers base-compat-batteries
       ];
       license = stdenv.lib.licenses.bsd3;
     };

  #react-hs = haskellPackages.callPackage react-hs-local { ghcjs-base = ghcjsbase; };
  react-hs = haskellPackages.callPackage react-hs-forked { ghcjs-base = ghcjsbase; };

  #react-native-hs = haskellPackages.callPackage react-native-hs-local { react-hs = react-hs; ghcjs-base = ghcjsbase; };
  react-native-hs = haskellPackages.callPackage react-native-hs-git { react-hs = react-hs; ghcjs-base = ghcjsbase; };

  drv = haskellPackages.callPackage myproject {
    react-hs = react-hs;
    react-native-hs = react-native-hs;
    ghcjs-base = ghcjsbase;
    ghcjs-fetch = pkgs.haskell.lib.dontCheck(haskellPackages.callPackage ghcjs-fetch-forked { ghcjs-base = ghcjsbase; });
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
