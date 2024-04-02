-- import lualine plugin safely
local lualine_setup, lualine = pcall(require, "lualine")
if not lualine_setup then
	return
end

lualine.setup({
	theme = "gruvbox-material",
})
