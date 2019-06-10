let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          base = haskellPackagesNew.callPackage ./nix/base-4.8.2.0.nix { };
          ghc-boot-th = haskellPackagesNew.callPackage ./nix/ghc-boot-th-8.4.4.nix { };
          template-haskell = haskellPackagesNew.callPackage ./nix/template-haskell-2.13.0.0.nix { };
          gtk = haskellPackagesNew.callPackage ./nix/gtk-0.14.7.nix { };
          ghc-events = haskellPackagesNew.callPackage ./nix/ghc-events-0.8.0.2.nix { };
          threadscope = haskellPackagesNew.callPackage ./nix/threadscope-0.2.11.1.nix { };

          project = haskellPackagesNew.callPackage ./project.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project = pkgs.haskellPackages.project;
  }
