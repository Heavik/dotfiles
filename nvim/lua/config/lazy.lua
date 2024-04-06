
local isUnix = vim.fn.has("macunix")

local runCmd = "make"
if not isUnix then
	runCmd = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build"
end

-- lazy.nvim setup
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

return require("lazy").setup({
	{"nvim-lua/plenary.nvim"}, -- lua functions that many plugins use

	-- Gruvbox Material Theme
	{"sainnhe/gruvbox-material"},

	-- dev icons
	{"nvim-tree/nvim-web-devicons"},

	-- LuaLine mode line
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons", lazy = true },
	},

	{"szw/vim-maximizer"}, -- maximizes and restores current window

	{"tpope/vim-surround"},

	-- commenting with gc
	{"numToStr/Comment.nvim"},

	-- fuzzy finding w/ telescope

	{ "nvim-telescope/telescope-fzf-native.nvim", build = runCmd }, -- dependency for better sorting performance
	{ "nvim-telescope/telescope.nvim", branch = "0.1.x" },

	-- autocompletion
	{"hrsh7th/nvim-cmp"}, -- completion plugin
	{"hrsh7th/cmp-buffer"}, -- source for text in buffer
	{"hrsh7th/cmp-path"}, -- source for text in paths

	-- snippets
	{"L3MON4D3/LuaSnip"}, -- snippet engine
	{"saadparwaiz1/cmp_luasnip"}, -- for autocompletion

	-- managing & installing lsp servers, linters & formatters
	{"williamboman/mason.nvim"}, -- in charge of managing lsp servers, linters & formatters
	{"williamboman/mason-lspconfig.nvim"}, -- bridges gap b/w mason & lspconfig

	-- configuring lsp servers
	{"neovim/nvim-lspconfig"}, -- easily configure language servers
	{"hrsh7th/cmp-nvim-lsp"}, -- for autocompletion

	{"jose-elias-alvarez/null-ls.nvim"}, -- configure formatters & linters
	{"jayp0521/mason-null-ls.nvim"}, -- bridges gap b/w mason & null-ls

	-- treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		build = function()
			local ts_update = require("nvim-treesitter.install").update({ with_sync = true })
			ts_update()
		end,
	},

	-- auto closing
	{"windwp/nvim-autopairs"}, -- autoclose parens, brackets, quotes, etc...
	{ "windwp/nvim-ts-autotag", dependencies = "nvim-treesitter" }, -- autoclose tags
})
