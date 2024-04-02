-- Packer Setup
require("config/packer")

-- Core Settings
require("config/core/options")
require("config/core/colorscheme")
require("config/core/keymaps")

-- Plugins
require("config/plugins/treesitter")
require("config/plugins/autocompletion")
require("config/plugins/autopairs")
require("config/plugins/comment")
require("config/plugins/telescope")
require("config/plugins/status-line")

-- LSP
require("config/plugins/lsp/mason")
require("config/plugins/lsp/lspconfig")
require("config/plugins/lsp/null-ls")
