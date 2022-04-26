local cmp = require "cmp"
local luasnip = require "luasnip"
local lspkind = require "lspkind"

local cmp_autopairs = require('nvim-autopairs.completion.cmp')
cmp.event:on( 'confirm_done', cmp_autopairs.on_confirm_done({ map_char = { tex = '' } }))

cmp.setup {

    snippet = {
        expand = function(args)
            require('luasnip').lsp_expand(args.body)
        end,
    },

    window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
    },

    formatting = {
      format = lspkind.cmp_format({
        mode = "symbol_text",
        menu = ({
          buffer = "[Buffer]",
          nvim_lsp = "[LSP]",
          luasnip = "[LuaSnip]",
          nvim_lua = "[Lua]",
          latex_symbols = "[Latex]",
        })
      }),
    },

    mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
        }),
        ['<Tab>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            else
                fallback()
            end
        end, { 'i', 's' }),
        ['<S-Tab>'] = cmp.mapping(function(fallback)
              if cmp.visible() then
                cmp.select_prev_item()
              elseif luasnip.jumpable(-1) then
                luasnip.jump(-1)
              else
                fallback()
              end
        end, { 'i', 's' }),
    }),
    sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        { name = 'nvim_lua' },
        { name = 'path' },
        { name = 'luasnip' },
    }, {
        { name = 'buffer' },
    })
}

-- set up servers
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
local lspconfig = require 'lspconfig'
local servers = {
    'ansiblels', -- ansible
    'bashls', -- bash
    'clangd', 'cmake', -- C and cmake
    'cssls', 'emmet_ls', 'html', -- HTML and CSS
    'gdscript', -- gdscript (godot
    'spectral', -- json, yaml
    'quick_lint_js', -- javascript
    'texlab', -- latex
    'sumneko_lua', -- lua
    'rnix', -- nix
    'pyright', -- python
    'rust_analyzer', -- rust
    'vimls', -- vimscript
}
for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup { capabilities = capabilities }
end
