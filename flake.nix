# SPDX-FileCopyrightText: 2025 Mercury Technologies, Inc
#
# SPDX-License-Identifier: MIT

{
  description = "Slack library for Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    let
      ghcVer = "ghc96";
      makeHaskellOverlay = overlay: final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcVer} = prev.haskell.packages."${ghcVer}".override (oldArgs: {
              overrides =
                prev.lib.composeExtensions (oldArgs.overrides or (_: _: { }))
                  (overlay prev);
            });
          };
        };
      };

      out = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
            config.allowBroken = true;
          };

        in
        {
          packages = rec {
            default = tls-sslkeylogfile;
            tls-sslkeylogfile = pkgs.haskell.packages.${ghcVer}.tls-sslkeylogfile;
          };

          checks = rec {
            # due to https://github.com/NixOS/nix/issues/4265 breaking
            # import-from-derivation inside nix flake checks, nix flake check
            # does not work and you need to use `nix build .#checks.yourSystem.all`
            all = pkgs.writeTextFile {
              name = "all-checks";
              text = ''
                ${pre-commit-check}
                ${tls-sslkeylogfile}
              '';
            };

            inherit (self.packages.${system}) tls-sslkeylogfile;

            pre-commit-check = pre-commit-hooks.lib.${system}.run {
              src = ./.;
              tools.fourmolu = pkgs.haskell.packages.${ghcVer}.fourmolu;
              hooks = {
                fourmolu.enable = true;
                # XXX: For bizarre reasons, it seems that the fourmolu pre-commit
                # hook has Fourmolu not read these from the cabal files as it
                # normally would. Seems like a bug...
                # ormolu.defaultExtensions = [
                #   "ImportQualifiedPost"
                # ];
              };
            };
          };

          # for debugging
          inherit pkgs;

          devShells.default =
            let haskellPackages = pkgs.haskell.packages.${ghcVer};
            in
            haskellPackages.shellFor {
              packages = p: [ self.packages.${system}.tls-sslkeylogfile ];
              withHoogle = true;
              buildInputs = [
                haskellPackages.haskell-language-server
                haskellPackages.fourmolu
                haskellPackages.cabal-install
                haskellPackages.fast-tags
                haskellPackages.hpack
              ] ++ [
                pkgs.sqlite
              ];
              shellHook = self.checks.${system}.pre-commit-check.shellHook;
            };
        };
    in
    flake-utils.lib.eachDefaultSystem out // {
      # this stuff is *not* per-system
      overlays = {
        default = makeHaskellOverlay (prev: hfinal: hprev:
          {
            tls-sslkeylogfile = hprev.callCabal2nix "tls-sslkeylogfile" ./. { };
          });
      };
    };
}
