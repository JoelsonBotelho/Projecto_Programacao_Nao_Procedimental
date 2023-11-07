{ pkgs }: {
    deps = [
        pkgs.shopify-themekit
        pkgs.wallabag
        (pkgs.haskellPackages.ghcWithPackages (pkgs: [
            # Put your dependencies here!
        ]))
        pkgs.haskell-language-server
    ];
}