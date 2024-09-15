-- import lspconfig plugin safely
local lspconfig_status, lspconfig = pcall(require, "lspconfig")
if not lspconfig_status then
	print("lspconfig is not installed")
	return
end

-- import cmp-nvim-lsp plugin safely
local cmp_nvim_lsp_status, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
if not cmp_nvim_lsp_status then
	print("cmp_nvim_lsp is not installed")
	return
end

-- enable keybinds only for when lsp server available
local on_attach = function(client, bufnr)
	-- keybind options
	local opts = { noremap = true, silent = true, buffer = bufnr }

	vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)
	vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
	vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
	vim.keymap.set("n", "gr", vim.lsp.buf.references, opts)
	vim.keymap.set("n", "<leader>D", vim.lsp.buf.type_definition, opts)
	vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)
end

-- used to enable autocompletion (assign to every lsp server config)
local capabilities = cmp_nvim_lsp.default_capabilities()

-- configure python lsp server
lspconfig["pyright"].setup({
	capabilities = capabilities,
	on_attach = on_attach,
	cmd = { "pyright-langserver", "--stdio" },
	filetypes = { "python" },
	settings = {
		pylsp = {
			plugins = {
				pycodestyle = { enabled = false },
				pylint = { enabled = false },
				flake8 = { enabled = false },
				jedi_completion = { enabled = true },
			},
		},
	},
})

-- configure C/C++ lsp server
lspconfig.clangd.setup({
	capabilities = capabilities,
	on_attach = on_attach,
})

-- configure go lsp server
lspconfig.gopls.setup({
	capabilities = capabilities,
	on_attach = on_attach,
})
-- configure lua server (with special settings)
lspconfig["lua_ls"].setup({
	capabilities = capabilities,
	on_attach = on_attach,
	settings = { -- custom settings for lua
		Lua = {
			-- make the language server recognize "vim" global
			diagnostics = {
				globals = { "vim" },
			},
			workspace = {
				-- make language server aware of runtime files
				library = {
					[vim.fn.expand("$VIMRUNTIME/lua")] = true,
					[vim.fn.stdpath("config") .. "/lua"] = true,
				},
			},
		},
	},
})

-- configure typescript lsp server
lspconfig.ts_ls.setup({
	init_options = { hostinfo = "neovim" },
	cmd = { "typescript-language-server", "--stdio" },
	filetypes = {
		"javascript",
		"javascriptreact",
		"javascript.jsx",
		"typescript",
		"typescriptreact",
		"typescript.tsx",
	},
	root_dir = lspconfig.util.root_pattern("tsconfig.json", "package.json", "jsconfig.json", ".git"),
	single_file_support = true,
	capabilities = capabilities,
	on_attach = on_attach,
})
