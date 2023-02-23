{ stdenv
, fetchFromGitHub
, ncurses
}:

stdenv.mkDerivation rec {
    pname = "termato";
    version = "0.1";

    src = fetchFromGitHub {
        owner = "felix-u";
        repo = pname;
        rev = "v${version}";
        sha256 = "sha256-LKJw5U4eRmFgCbL8KpCBpUz+S8ugKqqh7EcSp9pg93k=";
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
