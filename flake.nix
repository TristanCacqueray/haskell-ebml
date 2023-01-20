{
  nixConfig.bash-prompt = "[nix(ebml)] ";
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/ba5d181089900f376f765e4a6889bd30c4f96993";
    # "path:///srv/github.com/podenv/hspkgs";
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;
      haskellExtend = hpFinal: hpPrev: {
        ebml = hpPrev.callCabal2nix "ebml" self { };
      };
      hsPkgs = pkgs.hspkgs.extend haskellExtend;
      pkg-exe = pkgs.haskell.lib.justStaticExecutables hsPkgs.ebml;

    in {
      packages."x86_64-linux".default = pkg-exe;
      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.ebml ];
        buildInputs = with pkgs;
          [
            hpack
            hlint
            cabal-install
            ghcid
            haskell-language-server
            fourmolu
            hsPkgs.doctest
            mkvtoolnix-cli
          ];
      };
    };
}
