local M = {}

function M.setup()
    local npairs = require "nvim-autopairs"
    local Rule = require('nvim-autopairs.rule')
    npairs.setup {
        check_ts = true,
    }
    npairs.add_rules(require "nvim-autopairs.rules.endwise-lua")
    npairs.add_rule(Rule("$$", "$$", "tex"))
end

return M
