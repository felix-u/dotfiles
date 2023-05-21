{ pkgs, config, git, ... }:


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

    resizeAmount = "4";
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
                fzf-vim
                goyo-vim
                harpoon
                leap-nvim
                # pkgs-unstable.vimPlugins.lsp-zero-nvim
                plenary-nvim
                targets-vim
                vim-commentary
                vim-cutlass
                vim-endwise
                vim-lastplace
                vim-manpager
                vim-pager
                vim-plugin-AnsiEsc
                vim-repeat
                vim-speeddating
                vim-surround
                vimtex
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

              function! s:tweak_default_colours()
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
                  hi StatusLine   cterm=NONE   ctermfg=White ctermbg=00
                  hi String       cterm=NONE   ctermfg=06
                  hi Type         cterm=NONE   ctermfg=White
                  hi Visual       ctermbg=08
                  hi WildMenu     ctermfg=Black ctermbg=White
                  autocmd BufReadPost *.c,*.h hi cError cterm=NONE
              endfunction
              autocmd! ColorScheme default call s:tweak_default_colours()
              colorscheme default

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
              noremap <leader>d  "+d
              noremap <leader>d  "+d
              noremap <leader>dd "+dd
              noremap <leader>D  "+d
              noremap <leader>c  "+c
              noremap <leader>c  "+c
              noremap <leader>cc "+cc
              noremap <leader>c  "+c

              " Copy and paste from system clipboard
              noremap y "+y
              noremap Y "+y$
              noremap yy "+yy
              noremap p "+p
              noremap P "+P

              " Plugins
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
              let g:goyo_width = "80%"
              nnoremap <silent> <leader>tf :Goyo<CR>
              nnoremap <leader>ts :setlocal spell! spelllang=en_gb<CR>
              nnoremap <leader>tls :set number<CR> :set relativenumber<CR>
              nnoremap <leader>tlh :set nonumber<CR> :set norelativenumber<CR>
              nnoremap <leader>tn :Lex<CR>
              function! ToggleTerminal()
                " Check if any terminal window exists
                let terminal_winid = -1
                let terminal_bufnr = -1
                for winid in range(1, winnr('$'))
                  let buf = winbufnr(winid)
                  if getbufvar(buf, '&buftype') ==# 'terminal'
                    let terminal_winid = winid
                    let terminal_bufnr = buf
                    break
                  endif
                endfor
                " If a terminal window exists, close it
                if terminal_winid != -1
                  execute terminal_winid . 'wincmd c'
                " Otherwise, create a new terminal with the desired height
                else
                  execute "split" | resize 16 | terminal
                endif
              endfunction
              nnoremap <silent> <leader>tt :call ToggleTerminal()<CR>

              " harpoon
              nnoremap <leader>a :lua require("harpoon.mark").add_file()<CR>
              nnoremap <leader>v :lua require("harpoon.ui").toggle_quick_menu()<CR>
              nnoremap <C-Left>  :lua require("harpoon.ui").nav_file(3)<CR>
              nnoremap <C-Down>  :lua require("harpoon.ui").nav_file(1)<CR>
              nnoremap <C-Up>    :lua require("harpoon.ui").nav_file(2)<CR>
              nnoremap <C-Right> :lua require("harpoon.ui").nav_file(4)<CR>

              " convenience
              nnoremap <leader>q :q<CR>

              " netrw is far too large by default (50%)
              let g:netrw_winsize = 16

              " escape to enter normal mode in terminal mode
              tnoremap <Esc> <C-\><C-n>

              " vimtex
              let maplocalleader="\\"
              let g:vimtex_view_method = 'zathura'

              " fzf
              nnoremap <leader>fi :Files<CR>
              nnoremap <leader>fb :Buffers<CR>
              nnoremap <leader>fg :Rg<CR>
              nnoremap <leader>fl :Lines<CR>
              nnoremap <leader>fc :Commands<CR>
              nnoremap <leader>fm :Maps<CR>
              nnoremap <leader>fh :Helptags<CR>
              nnoremap <leader>ft :Filetypes<CR>

              " lua << EOF
              "   local lsp = require("lsp-zero")
              "   lsp.preset(recommended)
              "   lsp.ensure_installed({ })
              "   lsp.nvim_workspace()
              "   local cmp = require('cmp')
              "   local cmp_select = {behavior = cmp.SelectBehavior.Select}
              "   local cmp_mappings = lsp.defaults.cmp_mappings({
              "     ['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
              "     ['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
              "     ['<C-y>'] = cmp.mapping.confirm({ select = true }),
              "     ["<C-k>"] = cmp.mapping.complete(),
              "   })
              "   cmp_mappings['<Tab>'] = nil
              "   cmp_mappings['<S-Tab>'] = nil
              "   lsp.setup_nvim_cmp({
              "     mapping = cmp_mappings
              "   })
              "   lsp.set_preferences({
              "       suggest_lsp_servers = true,
              "       sign_icons = {
              "           error = 'E',
              "           warn = 'W',
              "           hint = 'H',
              "           info = 'I'
              "       }
              "   })
              "   lsp.on_attach(function(client, bufnr)
              "     local opts = {buffer = bufnr, remap = false}
              "     vim.keymap.set("n", "<leader>ld", function() vim.lsp.buf.definition() end, opts)
              "     vim.keymap.set("n", "<leader>lk", function() vim.lsp.buf.hover() end, opts)
              "     vim.keymap.set("n", "<leader>vws", function() vim.lsp.buf.workspace_symbol() end, opts)
              "     vim.keymap.set("n", "<leader>vd", function() vim.diagnostic.open_float() end, opts)
              "     vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts)
              "     vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts)
              "     vim.keymap.set("n", "<leader>la", function() vim.lsp.buf.code_action() end, opts)
              "     vim.keymap.set("n", "<leader>lr", function() vim.lsp.buf.references() end, opts)
              "     vim.keymap.set("n", "<leader>ln", function() vim.lsp.buf.rename() end, opts)
              "     vim.keymap.set("i", "<C-h>", function() vim.lsp.buf.signature_help() end, opts)
              "   end)
              "   lsp.setup()
              "   vim.diagnostic.config({
              "       virtual_text = true
              "   })
              "   local notify = vim.notify
              "   vim.notify = function(msg, ...)
              "       if msg:match("warning: multiple different client offset_encodings") then
              "           return
              "       end

              "       notify(msg, ...)
              "   end
              " EOF

            '';
        };

        programs.tmux = {
            aggressiveResize = true;
            baseIndex = 1;
            clock24 = true;
            enable = true;
            escapeTime = 0; 
            extraConfig = ''
                set -g repeat-time 400
                set -g automatic-rename on
                set -g renumber-windows on
                set -g set-titles on
                set -g display-time 2500 # increase tmux message display duration 
                set -g status-interval 5 # redraw more often
                set -g status-justify left
                set-option -g status-position top
                set-option -g status-style bg=black
                set -g status-right-style default
                set -g status-right "[#S]"
                set -g status-left-style default
                set -g status-left "#[default]"
                # inactive window style
                set -g window-status-style fg=white,bg=default
                set -g window-status-format '#I #W '
                # active window style
                set -g window-status-current-style fg=default,bold,bg=default
                set -g window-status-current-format '#I #W '
                set -g focus-events on
                set-option -sa terminal-overrides ",xterm*:Tc"
                bind C-p previous-window
                bind C-n next-window
                bind f last-window # should be same key as prefix
                unbind '"'
                unbind %
                bind v split-window -h -c "#{pane_current_path}"
                bind s split-window -v -c "#{pane_current_path}"
                bind Enter if-shell "[ $(($(tmux display -p '8*#{pane_width}-20*#{pane_height}'))) -lt 0 ]" "splitw -v -c '#{pane_current_path}'" "splitw -h -c '#{pane_current_path}' "
                bind o if-shell "[ $(($(tmux display -p '8*#{pane_width}-20*#{pane_height}'))) -lt 0 ]" "splitw -v -c '#{pane_current_path}'" "splitw -h -c '#{pane_current_path}' "
                bind-key -T copy-mode-vi v send-keys -X begin-selection
                bind-key -T copy-mode-vi C-v send-keys -X rectangle-toggle
                bind-key -T copy-mode-vi y send-keys -X copy-selection-and-cancel
                # better navigation
                bind -r j last-window
                bind -n c-left select-pane -L
                bind -n c-right select-pane -R
                bind -n c-up select-pane -U
                bind -n c-down select-pane -D
                bind -n c-m-left resize-pane -L ${resizeAmount}
                bind -n c-m-right resize-pane -R ${resizeAmount}
                bind -n c-m-up resize-pane -U ${resizeAmount}
                bind -n c-m-down resize-pane -D ${resizeAmount}
                bind -n c-s-left swap-pane -U
                bind -n c-s-right swap-pane -D
                bind -n c-s-up swap-pane -U
                bind -n c-s-down swap-pane -D
                unbind x
                bind w kill-pane
                # pane separator style
                set -g pane-border-style "fg=black bg=terminal"
                set -g pane-active-border-style "fg=black bg=black"
                set -g mouse on
                # PLUGINS
                set -g @continuum-restore 'on'
            '';
            historyLimit = 5000;
            keyMode = "vi"; # hmm
            plugins = with pkgs.tmuxPlugins; [
                continuum 
                resurrect   
                yank
            ];
            shortcut = "f";
            terminal = "screen-256color";
        };

        # programs.nnn = {
        #   enable = true;
        #   bookmarks = {
        #     d = "${homedir}/dotfiles";
        #     h = "${homedir}";
        #     m = "/mnt";
        #     r = "${homedir}/Desktop/recordings";
        #     s = "${homedir}/Pictures/screenshots";
        #     u = "${homedir}/uni/2022/spring";
        #   };
        #   plugins.mappings = {
        #     d = "dragdrop";
        #     p = "preview-tui";
        #     r = "imgresize";
        #     w = "waypaper";
        #     v = "imgview";
        #   };
        # };

        programs.chromium = {
          enable = true;
          package = pkgs.chromium;
          commandLineArgs = [
            "-animation-duration-scale=0.5"
            "-enable-features=UseOzonePlatform"
            "-ozone-platform=wayland"
            "file"
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
