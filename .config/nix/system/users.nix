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

        programs.neovim = {
            enable = true;
            package = pkgs-unstable.neovim-unwrapped;
            plugins = with pkgs.vimPlugins; [
                # nvim-treesitter.withAllGrammars
                auto-pairs
                leap-nvim
                targets-vim
                vim-commentary
                vim-cutlass
                vim-endwise
                vim-lastplace
                vim-repeat
                vim-speeddating
                vim-surround
                vim-unimpaired
                which-key-nvim
            ];
            extraConfig = ''
              set autoindent
              set autoread
              set backspace=indent,eol,start " backspace in insert mode
              set clipboard="unnamedplus"
              set complete-=i " don't complete keywords in included files
              set cursorline
              set display+=lastline
              set display+=truncate
              set expandtab
              setglobal tags-=./tags tags-=./tags; tags^=./tags;	
              set history=1000
              set hlsearch
              set ignorecase
              set incsearch
              set laststatus=2
              set lazyredraw
              set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
              set mouse="a"
              set nocompatible
              set nolangremap
              set noshowcmd
              set noswapfile
              set nowrap
              set nowritebackup
              set nrformats-=octal
              set ruler
              set scrolloff=1
              set sessionoptions-=options
              set shiftwidth=4
              set showmatch
              set sidescrolloff=2
              set smartindent
              set smarttab
              set splitbelow
              set splitright
              set tabpagemax=50
              set tabstop=4
              set timeout
              set timeoutlen=200
              set undofile
              set viewoptions-=options
              set viminfo^=!
              set wildmenu
              filetype plugin indent on

              highlight clear
              hi Comment      cterm=italic ctermfg=02
              hi Constant     cterm=NONE   ctermfg=06
              hi CursorLine   cterm=NONE   ctermbg=00
              hi CursorLineNr cterm=NONE   ctermfg=15 ctermbg=00
              hi Delimiter    cterm=NONE   ctermfg=White 
              hi Function     cterm=NONE   ctermfg=White
              hi Identifier   cterm=NONE   ctermfg=White
              hi LineNr       ctermfg=07   ctermbg=00
              hi MatchParen   cterm=bold,underline ctermfg=Black ctermbg=04
              hi Pmenu        ctermfg=White ctermbg=Black
              hi PmenuSbar    ctermbg=08
              hi PmenuSel     ctermfg=Black ctermbg=White
              hi PmenuThumb   ctermfg=08 ctermbg=08
              hi PreProc      cterm=NONE   ctermfg=01
              hi Search       cterm=bold   ctermfg=Black ctermbg=03
              hi Special      cterm=NONE   ctermfg=05
              hi Statement    cterm=NONE   ctermfg=White
              hi String       cterm=NONE   ctermfg=06
              hi Type         cterm=NONE   ctermfg=White
              hi Visual       ctermbg=08
              hi WildMenu     ctermfg=Black ctermbg=White

              let mapleader = " "

              set commentstring=#\ %s
              autocmd FileType c   set commentstring=//\ %s
              autocmd FileType cpp set commentstring=//\ %s

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

              " Copy and paste from system clipboard
              noremap y "+y
              noremap Y "+y$
              noremap yy "+yy
              noremap p "+p
              noremap P "+P

              lua require('leap').add_default_mappings()
              lua require("which-key").setup{}

              " Directional keys should navigate visual lines, not actual lines
              nnoremap j gj
              nnoremap k gk
              nnoremap <Down> gj
              nnoremap <Up> gk

              " Keep search results centred
              nnoremap n nzzzv
              nnoremap N Nzzzv

              " Text formatting
              nnoremap <leader>stf :set linebreak!<CR> :set fo+=t<CR> :set tw=120<CR> :set wrap!<CR>

              " Yank whole file
              nnoremap <C-y> :%y+<CR>

              " Swap more easily between last two buffers
              nnoremap <leader>j <C-^>

              " More breakpoints for undo
              inoremap , ,<C-g>u
              inoremap . .<C-g>u
              inoremap ! !<C-g>u
              inoremap ? ?<C-g>u

              " Stay in visual mode when indenting
              vnoremap < <gv
              vnoremap > >gv

              " Shift+Up/Down moves text
              vnoremap J :m '>+1<CR>gv=gv
              vnoremap K :m '>-2<CR>gv=gv
              vnoremap <S-Down> :m '>+1<CR>gv=gv
              vnoremap <S-Up> :m '>-2<CR>gv=gv

              " toggling various things
              nnoremap <leader>ts :setlocal spell! spelllang=en_gb<CR>
              nnoremap <leader>tls :set number<CR> :set relativenumber<CR>
              nnoremap <leader>tlh :set nonumber<CR> :set norelativenumber<CR>
            '';
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
