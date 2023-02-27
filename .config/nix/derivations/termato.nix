{ stdenv
, fetchFromGitHub
, ncurses
}:

stdenv.mkDerivation rec {
    pname = "termato";
    version = "0.2";

    src = fetchFromGitHub {
        owner = "felix-u";
        repo = pname;
        rev = "v${version}";
        sha256 = "sha256-9Mq6My+WL7VbI4cFuYj6ZF5cDeQ6njNhW6Q/P+OTxJs=";
    };

    buildInputs = [ ncurses ];

    buildPhase = ''
        make release NAME=termato
    '';

    installPhase = ''
        mkdir -p $out/bin
        mv ./termato $out/bin/
    '';
}
