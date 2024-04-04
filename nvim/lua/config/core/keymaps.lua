vim.g.mapleader = " "
vim.g.maplocalleader = " "

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
vim.keymap.set("n", "<leader>h", "<C-w>h", { desc = "Switch to left window" })
vim.keymap.set("n", "<leader>j", "<C-w>j", { desc = "Switch to bottom window" })
vim.keymap.set("n", "<leader>k", "<C-w>k", { desc = "Switch to top window" })
vim.keymap.set("n", "<leader>l", "<C-w>l", { desc = "Switch to right window" })

-- tabs management
vim.keymap.set("n", "<leader>to", ":tabnew<CR>", { desc = "[Open] new [T]ab" })
vim.keymap.set("n", "<leader>tc", ":tabclose<CR>", { desc = "[C]lose current [T]ab" })
vim.keymap.set("n", "<leader>tn", ":tabn<CR>", { desc = "Go to [N]ext [T]ab" })
vim.keymap.set("n", "<leader>tp", ":tabp<CR>", { desc = "Go to [P]revious [T]ab" })

-- Plugins Keymaps

-- vim-maximizer
vim.keymap.set("n", "<leader>sm", ":MaximizerToggle<CR>", { desc = "Toggle [S]plit window [M]aximization" })

-- telescope
vim.keymap.set("n", "<leader>ff", "<cmd>Telescope find_files<CR>", { desc = "[F]ind [F]iles" })
vim.keymap.set("n", "<leader>fs", "<cmd>Telescope live_grep<CR>", { desc = "[F]ind [S]trings" })
vim.keymap.set("n", "<leader>fc", "<cmd>Telescope grep_string<CR>", { desc = "[F]ind strings under [C]ursor" })
vim.keymap.set("n", "<leader>fb", "<cmd>Telescope buffers<CR>", { desc = "[F]ind [B]uffers" })
