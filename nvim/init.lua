vim.cmd(":let $LANG='en'")

vim.opt.number = true
vim.opt.scrolloff = 10
vim.opt.sidescrolloff = 8

vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.autoindent = true

vim.opt.wrap = false

vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.termguicolors = true
vim.opt.showmatch = true

vim.opt.splitright = true
vim.opt.splitbelow = true

vim.opt.clipboard = "unnamedplus,unnamed"

vim.opt.undofile = true
vim.opt.undodir = vim.fn.expand("~/.vim/undodir")
vim.opt.autoread = true
vim.opt.autowrite = false

vim.opt.autochdir = false
vim.opt.path:append("**")
vim.opt.modifiable = true
vim.opt.encoding = "UTF-8"

vim.g.mapleader = " "
vim.g.maplocalleader = " "

-------- Plugins -------- 

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.uv.fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
            { out, "WarningMsg" },
            { "\nPress any key to exit..." }
        }, true, {})
        vim.fn.getchar()
        os.exit(1)
    end
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    {
        "sainnhe/gruvbox-material",
        lazy = false,
        priority = 1000,
        config = function()
            vim.g.gruvbox_material_enable_italic = true
            vim.g.gruvbox_material_background = "soft"
            vim.cmd.colorscheme('gruvbox-material')
        end
    },
    
    -- Autocompletion
    { "hrsh7th/nvim-cmp" },
    { "hrsh7th/cmp-buffer" },
    { "hrsh7th/cmp-path" }
})

require("cmp").setup({
    sources = {
        { name = "buffer" },
        { name = "path" }
    }
})

-------- Key Bindings -----
vim.keymap.set("n", "Y", "y$", { desc = "Yank to end of line" })

vim.keymap.set("i", "<C-u>", "<C-g>u<C-u>")
vim.keymap.set("i", "<C-w>", "<C-g>u<C-w>")

vim.keymap.set("n", "<leader>e", vim.cmd.Lexplore, { desc = "Toggle [E]xplorer" })

vim.keymap.set("n", "<leader>bn", ":bnext<CR>", { desc = "Next buffer" })
vim.keymap.set("n", "<leader>bp", ":bprevious<CR>", { desc = "Previous buffer" })
vim.keymap.set("n", "<leader>bd", ":bdelete<CR>", { desc = "Delete buffer" })

vim.keymap.set("n", "<M-j>", ":m .+1<CR>==", { desc = "Move line down" })
vim.keymap.set("n", "<M-k>", ":m .-2<CR>==", { desc = "Move line up" })
vim.keymap.set("v", "<M-j>", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
vim.keymap.set("v", "<M-k>", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

vim.keymap.set("v", "<", "<gv", { desc = "Indent left and reselect" })
vim.keymap.set("v", ">", ">gv", { desc = "Indent right and reselect" })

vim.keymap.set("n", "J", "mzJ`z", { desc = "Join lines and keep cursor position" })

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

