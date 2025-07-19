vim.cmd(":let $LANG='en'")

vim.opt.number = true

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

vim.opt.smartindent = true

vim.opt.wrap = false

vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.termguicolors = true

vim.opt.splitright = true
vim.opt.splitbelow = true

vim.opt.clipboard = "unnamedplus,unnamed"

-------- Key Bindings -----
vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.keymap.set("n", "Y", "y$", { desc = "Yank to end of line" })

vim.keymap.set("i", "<C-u>", "<C-g>u<C-u>")
vim.keymap.set("i", "<C-w>", "<C-g>u<C-w>")

vim.keymap.set("n", "<leader>e", vim.cmd.Lexplore, { desc = "Toggle [E]xplorer" })

-- window management
vim.keymap.set("n", "<leader>sv", "<C-w>v", { desc = "[S]plit Window [V]ertically" })
vim.keymap.set("n", "<leader>sh", "<C-w>s", { desc = "[S]plit Window [H]orizontally" })
vim.keymap.set("n", "<leader>se", "<C-w>=", { desc = "Make split window [E]qual width & height" })
vim.keymap.set("n", "<leader>sc", ":close<CR>", { desc = "[C]lose current split window" })
vim.keymap.set(
	"n",
	"<leader>sH",
	"<C-w>t<C-w>K",
	{ desc = "Change two vertically split windows to [H]orizontally split" }
)
vim.keymap.set(
	"n",
	"<leader>sV",
	"<C-w>t<C-w>H",
	{ desc = "Change two horizontally split windows to [V]ertically split" }
)

-- Window movement
vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "Switch to left window" })
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "Switch to bottom window" })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "Switch to top window" })
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "Switch to right window" })

-- tabs management
vim.keymap.set("n", "<leader>to", ":tabnew<CR>", { desc = "[Open] new [T]ab" })
vim.keymap.set("n", "<leader>tc", ":tabclose<CR>", { desc = "[C]lose current [T]ab" })
vim.keymap.set("n", "<leader>tn", ":tabn<CR>", { desc = "Go to [N]ext [T]ab" })
vim.keymap.set("n", "<leader>tp", ":tabp<CR>", { desc = "Go to [P]revious [T]ab" })

