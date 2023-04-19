{ pkgs, config, ... }:


let
    homedir = config.home-manager.users.felix.home.homeDirectory;
    pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

    vim-cutlass = pkgs.vimUtils.buildVimPlugin {
      name = "vim-cutlass";
      src = pkgs.fetchFromGitHub {
        owner = "svermeulen";
        repo = "vim-cutlass";
        rev = "7afd649415541634c8ce317fafbc31cd19d57589";
        sha256 = "sha256-j5W9q905ApDf3fvCIS4UwyHYnEZu5Ictn+6JkV/xjig=";
      };
    };
in {

    imports = [ <home-manager/nixos> ];

    # user account
    users.users.felix = {
        isNormalUser = true;
        extraGroups = [ "wheel" "networkmanager" "input" "uinput" "sway" ];
    };

    # shell
    programs.zsh = {
        enable = true;
        autosuggestions= {
            enable = true;
            strategy = [ "history" "completion" ];
        };
        histFile = "$XDG_CACHE_HOME/zsh_history";
        histSize = 10000;
        promptInit = ''
            any-nix-shell zsh --info-right | source /dev/stdin
        '';
        setOptions = [ "SHARE_HISTORY" "AUTO_CD" "AUTOMENU" "EXTENDEDGLOB" "GLOBDOTS" ];
        syntaxHighlighting = {
            enable = false;
            highlighters = [ "main" ]; # "brackets"
            styles = {
                "alias" = "fg=fg,bold";
                "builtin" = "fg=fg,bold";
                "function" = "fg=fg,bold";
                "command" = "fg=fg,bold";
                # "bracket-level-1" = "fg=magenta";
                # "bracket-level-2" = "fg=magenta";
                # "bracket-level-3" = "fg=magenta";
                # "bracket-level-4" = "fg=magenta";
                # "commandseparator" = "fg=blue";
                # "single-quoted-argument" = "fg=cyan";
                # "double-quoted-argument" = "fg=cyan";
                # "reserved-word" = "fg=fg,bold";
            };
        };
    };
    users.defaultUserShell = pkgs.zsh;

    # home-manager
    home-manager = {
      useUserPackages = true;
      useGlobalPkgs = true;
      verbose = true;
      users.felix = {pkgs, ...}: {
        home.username = "felix";
        home.homeDirectory = "/home/felix";
        home.stateVersion = "21.11";

        gtk = {
            enable = true;
            cursorTheme.name = "Adwaita";
            cursorTheme.size = 24;
            font.name = "FreeSans";
            font.size = 11;
            iconTheme.name = "Adwaita";
            theme.name = "adw-gtk3";
            gtk3.extraConfig = {
                gtk-decoration-layout = "menu:";
                # button-layout = "";
            };
        };

        xdg.userDirs.enable = true;
        xdg.userDirs.createDirectories = true;

        services.udiskie.enable = true;

	programs.vim = {
	    enable = true;	
	    plugins = with pkgs.vimPlugins; [
          auto-pairs
          targets-vim
          vim-commentary
          vim-cutlass
          vim-easymotion
          vim-endwise
          vim-lastplace
          vim-repeat
          vim-speeddating
          vim-surround
          vim-unimpaired
          vim-vinegar
          vim-wayland-clipboard
        ];
	    settings = {
          expandtab = true;		
          ignorecase = true;
          mouse = "a";
          shiftwidth = 4;
          tabstop = 4;
          undofile = true;
	    };
	    extraConfig = ''
		  set autoread
		  set backspace=indent,eol,start " backspace in insert mode
          set clipboard="unnamedplus"
		  set complete-=i " don't complete keywords in included files
          set cursorline
		  set display+=lastline
		  set display+=truncate
		  set history=1000
		  set incsearch
		  set laststatus=2
		  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
          set nocompatible
		  set nolangremap
		  set nrformats-=octal
		  set ruler
		  set sessionoptions-=options
		  set smarttab
		  set scrolloff=1
		  set sidescrolloff=2
		  set tabpagemax=50
		  setglobal tags-=./tags tags-=./tags; tags^=./tags;	
		  set ttimeout
		  set ttimeoutlen=100
		  set wildmenu
		  set viewoptions-=options
		  set viminfo^=!
		  filetype plugin indent on

          highlight clear
          hi Comment    cterm=italic ctermfg=02
          hi String     cterm=NONE   ctermfg=06
          hi Constant   cterm=NONE   ctermfg=06
          hi Identifier cterm=NONE   ctermfg=White
          hi Function   cterm=NONE   ctermfg=White
          hi Statement  cterm=NONE   ctermfg=White
          hi PreProc    cterm=NONE   ctermfg=01
          hi Type       cterm=NONE   ctermfg=White
          hi Special    cterm=NONE   ctermfg=05
          hi Delimiter  cterm=NONE   ctermfg=White 
          hi CursorLine cterm=NONE   ctermbg=00

          " return to last edit position when opening files
          " autocmd BufReadPost *
          "      \ if line("'\"") > 0 && line("'\"") <= line("$") |
          "      \   exe "normal! g`\"" |
          "      \ endif
          "`'")"'")"

          let mapleader = " "

          " print syntax grouping under cursor
          function! SynStack()
            if !exists("*synstack")
                return
              endif
            echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
          endfunc
          noremap <leader>hg :call SynStack()<CR>

          " vim-cutlass
          noremap <leader>c  c
          noremap <leader>C  C
          noremap <leader>cc cc
          noremap <leader>d  d
          noremap <leader>D  d
          noremap <leader>dd dd

          " copy and paste from system clipboard
          map <leader>y "+y
          map <leader>Y "+y$
          map <leader>p "+p
          map <leader>P "+P

          " vim-easymotion
          noremap s <Plug>(easymotion-overwin-f2)

	    '';
	};

        programs.neovim = {
            enable = true;
            package = pkgs-unstable.neovim-unwrapped;
            plugins = with pkgs.vimPlugins; [
                nvim-treesitter.withAllGrammars
            ];
        };

        programs.nnn = {
          enable = true;
          bookmarks = {
            d = "${homedir}/dotfiles";
            h = "${homedir}";
            m = "/mnt";
            r = "${homedir}/Desktop/recordings";
            s = "${homedir}/Pictures/screenshots";
            u = "${homedir}/uni/2022/spring";
          };
          plugins.mappings = {
            d = "dragdrop";
            p = "preview-tui";
            r = "imgresize";
            w = "waypaper";
            v = "imgview";
          };
        };

        programs.chromium = {
          enable = true;
          package = pkgs.chromium;
          commandLineArgs = [
            "-animation-duration-scale=0.5"
            "-enable-features=UseOzonePlatform"
            "-ozone-platform=wayland"
            "-homepage='https://search.brave.com'"
          ];
          extensions = [
            # ublock origin
            { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; }
            # surfingkeys
            { id = "gfbliohnnapiefjpjlpjnehglfpaknnc"; }
            # dark reader
            { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; }
            # decentraleyes
            { id = "ldpochfccmkkmhdbclfhpagapcfdljkj"; }
            # stylus
            { id = "clngdbkpkpeebahjckkjfobafhncgmne"; }
          ];
        };

        # w3m config
        home.file.".w3m/config".text = ''
                confirm_qq false
            '';

      };

    };

    # doas instead of sudo (why not)
    security.sudo.enable = false;
    security.doas = {
      enable = true;
      extraRules = [{
        users = [ "felix" ];
        keepEnv = true;
        persist = true;
      }];
    };

}
