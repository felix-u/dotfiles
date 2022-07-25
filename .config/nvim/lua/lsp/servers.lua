require("mason").setup()
require("mason-lspconfig").setup({
    automatic_installation = true,
})

local lsp = require "lspconfig"

local servers = {
    "ansiblels",
    "bashls",
    "clangd",
    "cmake",
    "cssls",
    "emmet_ls",
    "gdscript",
    "html",
    "spectral",
    "quick_lint_js",
    "texlab",
    "sumneko_lua",
    "rnix",
    "pyright",
    "rust_analyzer",
    "vimls",
}

for _, server in pairs(servers) do
    lsp[server].setup{}
end
