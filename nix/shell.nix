{ repoRoot, inputs, pkgs, system, lib }:

cabalProject:
let
  cardano-cli = inputs.cardano-node.legacyPackages.cardano-cli;
  cardano-node = inputs.cardano-node.legacyPackages.cardano-node;
  cardano-submit-api = inputs.cardano-node.legacyPackages.cardano-submit-api;
in
{
  name = "en-registration-plutus";

  packages = [
    pkgs.scriv
    pkgs.cargo
    cardano-cli
    cardano-node
    cardano-submit-api
    inputs.mithril.packages.mithril-client
    pkgs.ghcid
    pkgs.haskellPackages.hoogle
  ];

  env = {
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
    CARDANO_SUBMIT_API = "${cardano-submit-api}/bin/cardano-submit-api";
  };

  preCommit = {
    cabal-fmt.enable = true;
    cabal-fmt.extraOptions = "--no-tabular";
    nixpkgs-fmt.enable = true;
    shellcheck.enable = true;
    fourmolu.enable = true;
    fourmolu.extraOptions = "-o -XCPP";
    hlint.enable = true;
  };
}
