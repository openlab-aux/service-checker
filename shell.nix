{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, basic-prelude, blaze-html
      , optparse-applicative, process, stdenv, text, turtle
      }:
      mkDerivation {
        pname = "service-checker";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          async base basic-prelude blaze-html optparse-applicative process
          text turtle
        ];
        description = "Check the status of services with a simple filesystem interface";
        license = stdenv.lib.licenses.gpl3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
