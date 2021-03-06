let
  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/faad370edcb37162401be50d45526f52bb16a713.tar.gz";
    sha256 = "1d82d4vh0layf6n925j0h2nym16jbvcvps3l5m8ln9hxn0m6gadn";
  };

  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old : {
      overrides =
        pkgsNew.lib.fold pkgsNew.lib.composeExtensions
          (old.overrides or (_: _: { }))
          [ (pkgsNew.haskell.lib.packageSourceOverrides {
              HasCal = ./.;
            })
            (haskellPackagesNew: haskellPackagesOld: {
              doctest-parallel =
                pkgsNew.haskell.lib.dontCheck haskellPackagesOld.doctest-parallel;

              HasCal =
                pkgsNew.haskell.lib.overrideCabal
                  haskellPackagesOld.HasCal
                  (_: {
                    testTarget = "tasty";
                  });
            })
          ];
    });
  };

  config = { allowBroken = true; };

  pkgs = import nixpkgs { inherit config; overlays = [ overlay ]; };

in
  pkgs.haskellPackages.HasCal
