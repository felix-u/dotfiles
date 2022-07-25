" Base16 xresources (https://github.com/chriskempson/base16)
" Scheme: xresources

" This enables the coresponding base16-shell script to run so that
" :colorscheme works in terminals supported by base16-shell scripts
" User must set this variable in .vimrc
"   let g:base16_shell_path=base16-builder/output/shell/
if !has('gui_running')
  if exists("g:base16_shell_path")
    execute "silent !/bin/sh ".g:base16_shell_path."/base16-xresources.".&background.".sh"
  endif
endif

" GUI color definitions
let s:background = "#002b36"
let s:black00 = "#073642"
let s:black08 = "#224750"
let s:black08 = "#224750"
let s:white15 = "#839496"
let s:white15 = "#839496"
let s:grey07 = "#657b83"
let s:foreground = "#93a1a1"
let s:red01 = "#dc322f"
let s:orange = "#d64f15" " this is meant to actually be orange
let s:yellow03 = "#b58900"
let s:green02 = "#859900"
let s:cyan06 = "#2aa198"
let s:blue04 = "#268bd2"
let s:magenta05 = "#6c71c4"
let s:pink = "#cf4072" " red OR pink

" Terminal color definitions
let s:cterm00 = "00"
let s:cterm03 = "08"
let s:cterm05 = "07"
let s:cterm07 = "15"
let s:cterm08 = "01"
let s:cterm0A = "03"
let s:cterm0B = "02"
let s:cterm0C = "06"
let s:cterm0D = "04"
let s:cterm0E = "05"
if exists('base16colorspace') && base16colorspace == "256"
  let s:cterm01 = "18"
  let s:cterm02 = "19"
  let s:cterm04 = "20"
  let s:cterm06 = "21"
  let s:cterm09 = "16"
  let s:cterm0F = "17"
else
  let s:cterm01 = "10"
  let s:cterm02 = "11"
  let s:cterm04 = "12"
  let s:cterm06 = "13"
  let s:cterm09 = "09"
  let s:cterm0F = "14"
endif

" Theme setup
hi clear
syntax reset
let g:colors_name = "base16-xresources"

" Highlighting function
fun <sid>hi(group, guifg, guibg, ctermfg, ctermbg, attr, guisp)
  if a:guifg != ""
    exec "hi " . a:group . " guifg=#" . s:gui(a:guifg)
  endif
  if a:guibg != ""
    exec "hi " . a:group . " guibg=#" . s:gui(a:guibg)
  endif
  if a:ctermfg != ""
    exec "hi " . a:group . " ctermfg=" . s:cterm(a:ctermfg)
  endif
  if a:ctermbg != ""
    exec "hi " . a:group . " ctermbg=" . s:cterm(a:ctermbg)
  endif
  if a:attr != ""
    exec "hi " . a:group . " gui=" . a:attr . " cterm=" . a:attr
  endif
  if a:guisp != ""
    exec "hi " . a:group . " guisp=#" . a:guisp
  endif
endfun

" Return GUI color for light/dark variants
fun s:gui(color)
  if &background == "dark"
    return a:color
  endif

  if a:color == s:background
    return s:foreground
  elseif a:color == s:black00
    return s:grey07
  elseif a:color == s:black08
    return s:white15
  elseif a:color == s:black08
    return s:white15
  elseif a:color == s:white15
    return s:black08
  elseif a:color == s:white15
    return s:black08
  elseif a:color == s:grey07
    return s:black00
  elseif a:color == s:foreground
    return s:background
  endif

  return a:color
endfun

" Return terminal color for light/dark variants
fun s:cterm(color)
  if &background == "dark"
    return a:color
  endif

  if a:color == s:cterm00
    return s:cterm07
  elseif a:color == s:cterm01
    return s:cterm06
  elseif a:color == s:cterm02
    return s:cterm05
  elseif a:color == s:cterm03
    return s:cterm04
  elseif a:color == s:cterm04
    return s:cterm03
  elseif a:color == s:cterm05
    return s:cterm02
  elseif a:color == s:cterm06
    return s:cterm01
  elseif a:color == s:cterm07
    return s:cterm00
  endif

  return a:color
endfun

" Vim editor colors
call <sid>hi("Bold",          "", "", "", "", "bold", "")
call <sid>hi("Debug",         s:red01, "", s:cterm08, "", "", "")
call <sid>hi("Directory",     s:blue04, "", s:cterm0D, "", "", "")
call <sid>hi("Error",         s:background, s:red01, s:cterm00, s:cterm08, "", "")
call <sid>hi("ErrorMsg",      s:red01, s:background, s:cterm08, s:cterm00, "", "")
call <sid>hi("Exception",     s:red01, "", s:cterm08, "", "", "")
call <sid>hi("FoldColumn",    s:cyan06, s:black00, s:cterm0C, s:cterm01, "", "")
call <sid>hi("Folded",        s:black08, s:black00, s:cterm03, s:cterm01, "", "")
call <sid>hi("IncSearch",     s:black00, s:orange, s:cterm01, s:cterm09, "none", "")
call <sid>hi("Italic",        "", "", "", "", "none", "")
call <sid>hi("Macro",         s:red01, "", s:cterm08, "", "", "")
call <sid>hi("MatchParen",    s:background, s:black08, s:cterm00, s:cterm03,  "", "")
call <sid>hi("ModeMsg",       s:green02, "", s:cterm0B, "", "", "")
call <sid>hi("MoreMsg",       s:green02, "", s:cterm0B, "", "", "")
call <sid>hi("Question",      s:blue04, "", s:cterm0D, "", "", "")
call <sid>hi("Search",        s:black08, s:yellow03, s:cterm03, s:cterm0A,  "", "")
call <sid>hi("SpecialKey",    s:black08, "", s:cterm03, "", "", "")
call <sid>hi("TooLong",       s:red01, "", s:cterm08, "", "", "")
call <sid>hi("Underlined",    s:red01, "", s:cterm08, "", "", "")
call <sid>hi("Visual",        s:background, s:grey07, s:cterm00, s:cterm06, "", "")
call <sid>hi("VisualNOS",     s:red01, "", s:cterm08, "", "", "")
call <sid>hi("WarningMsg",    s:yellow03, "", s:cterm0A, "", "", "")
call <sid>hi("WildMenu",      s:red01, s:yellow03, s:cterm08, "", "", "")
call <sid>hi("Title",         s:blue04, "", s:cterm0D, "", "none", "")
call <sid>hi("Conceal",       s:blue04, s:background, s:cterm0D, s:cterm00, "", "")
call <sid>hi("Cursor",        s:background, s:white15, s:cterm00, s:cterm05, "", "")
call <sid>hi("NonText",       s:black08, "", s:cterm03, "", "", "")
call <sid>hi("Normal",        s:white15, s:background, s:cterm05, s:cterm00, "", "")
call <sid>hi("LineNr",        s:black08, "", s:cterm03, "",  "", "")
call <sid>hi("SignColumn",    s:black08, s:background, s:cterm03, s:cterm00, "", "")
call <sid>hi("StatusLine",    s:grey07, s:black08, s:cterm04, s:cterm02, "none", "")
call <sid>hi("StatusLineNC",  s:black08, s:black00, s:cterm03, s:cterm01, "none", "")
call <sid>hi("VertSplit",     s:black08, s:black08, s:cterm02, s:cterm02, "none", "")
call <sid>hi("ColorColumn",   "", s:black00, "", s:cterm01, "none", "")
call <sid>hi("CursorColumn",  "", s:black00, "", s:cterm01, "none", "")
call <sid>hi("CursorLine",    "", s:black00, "", s:cterm01, "none", "")
call <sid>hi("CursorLineNr",  s:grey07, s:background, s:cterm03, s:cterm00, "", "")
call <sid>hi("PMenu",         s:white15, s:black00, s:cterm04, s:cterm01, "none", "")
call <sid>hi("PMenuSel",      s:black00, s:white15, s:cterm01, s:cterm04, "", "")
call <sid>hi("PmenuThumb",    s:black08, s:black08, s:cterm02, s:black08, "none", "")
call <sid>hi("TabLine",       s:black08, s:black00, s:cterm03, s:cterm01, "none", "")
call <sid>hi("TabLineFill",   s:black08, s:black00, s:cterm03, s:cterm01, "none", "")
call <sid>hi("TabLineSel",    s:green02, s:black00, s:cterm0B, s:cterm01, "none", "")

" Standard syntax highlighting
call <sid>hi("Boolean",      s:orange, "", s:cterm09, "", "", "")
call <sid>hi("Character",    s:red01, "", s:cterm08, "", "", "")
call <sid>hi("Comment",      s:grey07, "", s:cterm06, "", "italic", "")
call <sid>hi("Conditional",  s:magenta05, "", s:cterm0E, "", "", "")
call <sid>hi("Constant",     s:orange, "", s:cterm09, "", "", "")
call <sid>hi("Define",       s:magenta05, "", s:cterm0E, "", "none", "")
call <sid>hi("Delimiter",    s:pink, "", s:cterm0F, "", "", "")
call <sid>hi("Float",        s:orange, "", s:cterm09, "", "", "")
call <sid>hi("Function",     s:blue04, "", s:cterm0D, "", "", "")
call <sid>hi("Identifier",   s:red01, "", s:cterm08, "", "none", "")
call <sid>hi("Include",      s:blue04, "", s:cterm0D, "", "", "")
call <sid>hi("Keyword",      s:magenta05, "", s:cterm0E, "", "", "")
call <sid>hi("Label",        s:yellow03, "", s:cterm0A, "", "", "")
call <sid>hi("Number",       s:orange, "", s:cterm09, "", "", "")
call <sid>hi("Operator",     s:white15, "", s:cterm05, "", "none", "")
call <sid>hi("PreProc",      s:yellow03, "", s:cterm0A, "", "", "")
call <sid>hi("Repeat",       s:yellow03, "", s:cterm0A, "", "", "")
call <sid>hi("Special",      s:cyan06, "", s:cterm0C, "", "", "")
call <sid>hi("SpecialChar",  s:pink, "", s:cterm0F, "", "", "")
call <sid>hi("Statement",    s:red01, "", s:cterm08, "", "", "")
call <sid>hi("StorageClass", s:yellow03, "", s:cterm0A, "", "", "")
call <sid>hi("String",       s:green02, "", s:cterm0B, "", "", "")
call <sid>hi("Structure",    s:magenta05, "", s:cterm0E, "", "", "")
call <sid>hi("Tag",          s:yellow03, "", s:cterm0A, "", "", "")
call <sid>hi("Todo",         s:yellow03, s:black00, s:cterm0A, s:cterm01, "", "")
call <sid>hi("Type",         s:yellow03, "", s:cterm0A, "", "none", "")
call <sid>hi("Typedef",      s:yellow03, "", s:cterm0A, "", "", "")

" C highlighting
call <sid>hi("cOperator",   s:cyan06, "", s:cterm0C, "", "", "")
call <sid>hi("cPreCondit",  s:magenta05, "", s:cterm0E, "", "", "")

" C# highlighting
call <sid>hi("csClass",                 s:yellow03, "", s:cterm0A, "", "", "")
call <sid>hi("csAttribute",             s:yellow03, "", s:cterm0A, "", "", "")
call <sid>hi("csModifier",              s:magenta05, "", s:cterm0E, "", "", "")
call <sid>hi("csType",                  s:red01, "", s:cterm08, "", "", "")
call <sid>hi("csUnspecifiedStatement",  s:blue04, "", s:cterm0D, "", "", "")
call <sid>hi("csContextualStatement",   s:magenta05, "", s:cterm0E, "", "", "")
call <sid>hi("csNewDecleration",        s:red01, "", s:cterm08, "", "", "")

" CSS highlighting
call <sid>hi("cssBraces",      s:white15, "", s:cterm05, "", "", "")
call <sid>hi("cssClassName",   s:magenta05, "", s:cterm0E, "", "", "")
call <sid>hi("cssColor",       s:cyan06, "", s:cterm0C, "", "", "")

" Diff highlighting
call <sid>hi("DiffAdd",      s:green02, s:black00,  s:cterm0B, s:cterm01, "", "")
call <sid>hi("DiffChange",   s:black08, s:black00,  s:cterm03, s:cterm01, "", "")
call <sid>hi("DiffDelete",   s:red01, s:black00,  s:cterm08, s:cterm01, "", "")
call <sid>hi("DiffText",     s:blue04, s:black00,  s:cterm0D, s:cterm01, "", "")
call <sid>hi("DiffAdded",    s:green02, s:background,  s:cterm0B, s:cterm00, "", "")
call <sid>hi("DiffFile",     s:red01, s:background,  s:cterm08, s:cterm00, "", "")
call <sid>hi("DiffNewFile",  s:green02, s:background,  s:cterm0B, s:cterm00, "", "")
call <sid>hi("DiffLine",     s:blue04, s:background,  s:cterm0D, s:cterm00, "", "")
call <sid>hi("DiffRemoved",  s:red01, s:background,  s:cterm08, s:cterm00, "", "")

" Git highlighting
call <sid>hi("gitCommitOverflow",  s:red01, "", s:cterm08, "", "", "")
call <sid>hi("gitCommitHeader",    s:magenta05, "", s:cterm0E, "", "", "")
call <sid>hi("gitCommitBranch",    s:orange, "", s:cterm09, "", "", "")
call <sid>hi("gitCommitSummary",   s:green02, "", s:cterm0B, "", "", "")
call <sid>hi("gitCommitSelectedType",   s:blue04, "", s:cterm0D, "", "", "")
call <sid>hi("gitCommitSelectedFile",   s:green02, "", s:cterm0B, "", "", "")

" GitGutter highlighting
call <sid>hi("GitGutterAdd",     s:green02, s:black00, s:cterm0B, s:cterm01, "", "")
call <sid>hi("GitGutterChange",  s:blue04, s:black00, s:cterm0D, s:cterm01, "", "")
call <sid>hi("GitGutterDelete",  s:red01, s:black00, s:cterm08, s:cterm01, "", "")
call <sid>hi("GitGutterChangeDelete",  s:magenta05, s:black00, s:cterm0E, s:cterm01, "", "")

" HTML highlighting
call <sid>hi("htmlBold",    s:yellow03, "", s:cterm0A, "", "", "")
call <sid>hi("htmlItalic",  s:magenta05, "", s:cterm0E, "", "", "")
call <sid>hi("htmlEndTag",  s:white15, "", s:cterm05, "", "", "")
call <sid>hi("htmlTag",     s:white15, "", s:cterm05, "", "", "")

" JavaScript highlighting
call <sid>hi("javaScript",        s:white15, "", s:cterm05, "", "", "")
call <sid>hi("javaScriptBraces",  s:white15, "", s:cterm05, "", "", "")
call <sid>hi("javaScriptNumber",  s:orange, "", s:cterm09, "", "", "")

" Mail highlighting
call <sid>hi("mailQuoted1",  s:yellow03, "", s:cterm0A, "", "", "")
call <sid>hi("mailQuoted2",  s:green02, "", s:cterm0B, "", "", "")
call <sid>hi("mailQuoted3",  s:magenta05, "", s:cterm0E, "", "", "")
call <sid>hi("mailQuoted4",  s:cyan06, "", s:cterm0C, "", "", "")
call <sid>hi("mailQuoted5",  s:blue04, "", s:cterm0D, "", "", "")
call <sid>hi("mailQuoted6",  s:yellow03, "", s:cterm0A, "", "", "")
call <sid>hi("mailURL",      s:blue04, "", s:cterm0D, "", "", "")
call <sid>hi("mailEmail",    s:blue04, "", s:cterm0D, "", "", "")

" Markdown highlighting
call <sid>hi("markdownCode",              s:green02, "", s:cterm0B, "", "", "")
call <sid>hi("markdownError",             s:white15, s:background, s:cterm05, s:cterm00, "", "")
call <sid>hi("markdownCodeBlock",         s:green02, "", s:cterm0B, "", "", "")
call <sid>hi("markdownHeadingDelimiter",  s:blue04, "", s:cterm0D, "", "", "")
call <sid>hi("markdownItalic",            s:magenta05, "", s:cterm0E, "", "italic", "")
call <sid>hi("markdownBold",              s:yellow03, "", s:cterm0A, "", "bold", "")

" NERDTree highlighting
call <sid>hi("NERDTreeDirSlash",  s:blue04, "", s:cterm0D, "", "", "")
call <sid>hi("NERDTreeExecFile",  s:white15, "", s:cterm05, "", "", "")

" PHP highlighting
call <sid>hi("phpMemberSelector",  s:white15, "", s:cterm05, "", "", "")
call <sid>hi("phpComparison",      s:white15, "", s:cterm05, "", "", "")
call <sid>hi("phpParent",          s:foreground, "", s:cterm05, "", "", "")

" Python highlighting
call <sid>hi("pythonOperator",  s:magenta05, "", s:cterm0E, "", "", "")
call <sid>hi("pythonRepeat",    s:magenta05, "", s:cterm0E, "", "", "")

" Ruby highlighting
call <sid>hi("rubyAttribute",               s:blue04, "", s:cterm0D, "", "", "")
call <sid>hi("rubyConstant",                s:yellow03, "", s:cterm0A, "", "", "")
call <sid>hi("rubyInterpolation",           s:green02, "", s:cterm0B, "", "", "")
call <sid>hi("rubyInterpolationDelimiter",  s:pink, "", s:cterm0F, "", "", "")
call <sid>hi("rubyRegexp",                  s:cyan06, "", s:cterm0C, "", "", "")
call <sid>hi("rubySymbol",                  s:green02, "", s:cterm0B, "", "", "")
call <sid>hi("rubyStringDelimiter",         s:green02, "", s:cterm0B, "", "", "")

" SASS highlighting
call <sid>hi("sassidChar",     s:red01, "", s:cterm08, "", "", "")
call <sid>hi("sassClassChar",  s:orange, "", s:cterm09, "", "", "")
call <sid>hi("sassInclude",    s:magenta05, "", s:cterm0E, "", "", "")
call <sid>hi("sassMixing",     s:magenta05, "", s:cterm0E, "", "", "")
call <sid>hi("sassMixinName",  s:blue04, "", s:cterm0D, "", "", "")

" Signify highlighting
call <sid>hi("SignifySignAdd",     s:green02, s:black00, s:cterm0B, s:cterm01, "", "")
call <sid>hi("SignifySignChange",  s:blue04, s:black00, s:cterm0D, s:cterm01, "", "")
call <sid>hi("SignifySignDelete",  s:red01, s:black00, s:cterm08, s:cterm01, "", "")

" Spelling highlighting
call <sid>hi("SpellBad",     "", s:background, "", s:cterm00, "undercurl", s:red01)
call <sid>hi("SpellLocal",   "", s:background, "", s:cterm00, "undercurl", s:cyan06)
call <sid>hi("SpellCap",     "", s:background, "", s:cterm00, "undercurl", s:blue04)
call <sid>hi("SpellRare",    "", s:background, "", s:cterm00, "undercurl", s:magenta05)

" Treesitter
call <sid>hi("TSFuncBuiltin", s:cyan06, "", s:cterm0C, "", "italic", "")
call <sid>hi("TSConstBuiltin", s:cyan06, "", s:cterm0C, "", "bold", "")
call <sid>hi("TSKeywordOperator", s:white15, "", s:cterm04, "", "italic", "")
call <sid>hi("TSInclude", s:blue04, "", s:cterm0D, "", "bold", "")
call <sid>hi("TSType", s:yellow03, "", s:cterm0A, "", "italic", "")

" Lspsaga
call <sid>hi("LspSagaFinderSelection", s:foreground, s:black00, s:cterm07, s:cterm01, "bold", "")
call <sid>hi("LspSagaLspFinderBorder", s:black08, "", s:cterm02, "", "", "")
call <sid>hi("LspFloatWinNormal", s:foreground, "", s:cterm07, "", "", "")
call <sid>hi("LspSagaRenameBorder", s:black08, "", s:cterm02, "", "", "")
call <sid>hi("LspSagaHoverBorder", s:black08, "", s:cterm02, "", "", "")
call <sid>hi("LspSagaSignatureHelpBorder", s:black08, "", s:cterm02, "", "", "")
call <sid>hi("LspSagaCodeActionBorder", s:black08, "", s:cterm02, "", "", "")
call <sid>hi("LspSagaAutoPreview", s:black08, "", s:cterm02, "", "", "")
call <sid>hi("LspSagaDefPreviewBorder", s:black08, "", s:cterm02, "", "", "")
call <sid>hi("LspLinesDiagBorder", s:black08, "", s:cterm02, "", "", "")
call <sid>hi("LspSagaDiagnosticBorder", s:black08, "", s:cterm02, "", "", "")
call <sid>hi("SagaShadow", s:black08, "", s:cterm02, "", "", "")
call <sid>hi("TargetWord", s:green02, "", s:cterm0B, "", "", "")
call <sid>hi("LspSagaRenamePromptPrefix", s:cyan06, "", s:cterm0C, "", "", "")

" Remove functions
delf <sid>hi
delf <sid>gui
delf <sid>cterm

" Remove color variables
unlet s:background s:black00 s:black08 s:black08  s:white15  s:white15  s:grey07  s:foreground  s:red01  s:orange s:yellow03  s:green02  s:cyan06  s:blue04  s:magenta05  s:pink
unlet s:cterm00 s:cterm01 s:cterm02 s:cterm03 s:cterm04 s:cterm05 s:cterm06 s:cterm07 s:cterm08 s:cterm09 s:cterm0A s:cterm0B s:cterm0C s:cterm0D s:cterm0E s:cterm0F
