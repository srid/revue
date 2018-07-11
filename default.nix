{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "ca.srid.revue.dev";
  android.displayName = "Revue (srid)";
  ios.bundleIdentifier = "ca.srid.revue.dev";
  ios.bundleName = "Revue (srid)";

  packages = {
    clay = pkgs.fetchFromGitHub {
      owner = "sebastiaanvisser";
      repo = "clay";
      rev = "54dc9eaf0abd180ef9e35d97313062d99a02ee75";
      sha256 = "0y38hyd2gvr7lrbxkrjwg4h0077a54m7gxlvm9s4kk0995z1ncax";
    };
    # mmark packages
    mmark = pkgs.fetchFromGitHub {
      owner = "mmark-md";
      repo = "mmark";
      rev = "61c29e3b363e582cc317971760a6e4737264589a";
      sha256 = "17j8ylwimw5l2314l80il7044j8zmd0z7vaqyh4vk2rf4ya0xwvr";
    };
    html-entity-map = pkgs.fetchFromGitHub {
      owner = "mrkkrp";
      repo = "html-entity-map";
      rev = "cab10a4c0df64d3fd39b69f86244c272b62a8e1d";  # 0.1.0.0
      sha256 = "0m6rppnnvf5fdzjp1mmxq6jfqdk7xnlw56viki8h72xl71bj4s5w";
    };
    megaparsec = pkgs.fetchFromGitHub {
      owner = "mrkkrp";
      repo = "megaparsec";
      rev = "7b271a5edc1af59fa435a705349310cfdeaaa7e9";  # 6.5.0
      sha256 = "0415z18gl8dgms57rxzp870dpz7rcqvy008wrw5r22xw8qq0s13c";
    };
    parser-combinators = pkgs.fetchFromGitHub {
      owner = "mrkkrp";
      repo = "parser-combinators";
      rev = "dd6599224fe7eb224477ef8e9269602fb6b79fe0";  # 0.4.0
      sha256 = "11cpfzlb6vl0r5i7vbhp147cfxds248fm5xq8pwxk92d1f5g9pxm";
    };
    hspec-megaparsec = pkgs.fetchFromGitHub {
      owner = "mrkkrp";
      repo = "hspec-megaparsec";
      rev = "6da16a699ca7908f8c98aee2ce813d6bebdc65ae"; # 1.0.0
      sha256 = "1dx1vppcwjvlnh665dpv6wifn8fiwxqgsn279k1byzxgz4h8n31i";
    };
  };

  overrides = self: super: with pkgs.haskell.lib; {
    clay = dontCheck super.clay;
    mmark = dontCheck super.mmark;
    email-validate = dontCheck super.email-validate;
  };
})
