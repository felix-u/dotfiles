{ stdenv
, fetchFromGitHub
, ncurses
}:

stdenv.mkDerivation rec {
    pname = "termato";
    version = "0.3";

    src = fetchFromGitHub {
        owner = "felix-u";
        repo = pname;
        rev = "v${version}";
        sha256 = "sha256-F9P+6XefhA34WJy/gT6YYexm5nHKDCYCN+nx17ZIZDY=";
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
