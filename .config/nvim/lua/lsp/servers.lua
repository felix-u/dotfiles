require("mason").setup()
require("mason-lspconfig").setup({
    automatic_installation = true,
})

local lsp = require "lspconfig"

-- https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md
local servers = {
    "ansiblels",
    "bashls",
    "clangd",
    "cmake",
    "cssls",
    "cssmodules_ls",
    "diagnosticls",
    "emmet_ls",
    "gdscript",
    "gopls",
    "html",
    "jsonls",
    "pyright",
    "quick_lint_js",
    "rnix",
    "rust_analyzer",
    "spectral",
    "sumneko_lua",
    "texlab",
    "tsserver",
    "vimls",
    "wgsl_analyzer",
    "zls",
}

for _, server in pairs(servers) do
    lsp[server].setup{}
end
