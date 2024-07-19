{ repoRoot, inputs, pkgs, system, lib }:
let
  project = repoRoot.nix.project;
in
[
  # Default packages, apps, devShells, checks, hydraJobs coming from the Haskell project
  (
    project.flake
  )
  {
    packages.validators = pkgs.runCommand "validators" { } ''
      mkdir -p $out
      cd $out
      ${inputs.self.packages.bin/en-registration-validators}/bin/en-registration-validators
      rm -rf out/
    '';
    packages.traced-en-registration-validators = project.variants.traced.packages.en-registration-validators;
  }
]