local beautiful = require("beautiful")

client.connect_signal("focus", function(c)
    c.border_color = beautiful.border_color_active
end)

client.connect_signal("unfocus", function(c)
    c.border_color = beautiful.border_color_normal
end)
