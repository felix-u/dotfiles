local lsp = require 'lspconfig'

-- ansible
lsp.ansiblels.setup{}

-- bash
lsp.bashls.setup{}

-- clang
lsp.clangd.setup{}

-- cmake
lsp.cmake.setup{}

-- css, scss, less
lsp.cssls.setup{}

-- emmet (for HTML)
lsp.emmet_ls.setup{}

-- gdscript
lsp.gdscript.setup{}

-- html
lsp.html.setup{}

-- json, yaml
lsp.spectral.setup{}

-- javascript
lsp.quick_lint_js.setup{}

-- latex
lsp.texlab.setup{}

-- lua
lsp.sumneko_lua.setup{}

-- nix
lsp.rnix.setup{}

-- python
lsp.pyright.setup{}

-- rust
lsp.rust_analyzer.setup{}

-- vimscript
lsp.vimls.setup{}

-- third party integration
require("coq_3p") {
  { src = "nvimlua", short_name = "nLUA" },
  { src = "vimtex", short_name = "vTEX" },
  { src = "bc", short_name = "MATH", precision = 6 },
  { src = "figlet",  short_name = "BIG"},
  { src = "orgmode", short_name = "ORG" },
  -- { src = "copilot", short_name = "COP", tmp_accept_key = "<c-r>" },
  {
      src = "repl",
      sh = "zsh",
      shell = { p = "perl", n = "node"},
      max_lines = 999,
      deadline = 500,
      short_name = "SH",
      unsafe = { "rm", "poweroff", "mv", "shutdown", "reboot"}
    },
}
