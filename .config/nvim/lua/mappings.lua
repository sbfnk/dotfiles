vim.g.mapleader = " "
vim.g.maplocalleader = " "

local opts = { silent = true, noremap = true }
vim.api.nvim_set_keymap("", "j", "h", opts)
vim.api.nvim_set_keymap("", "k", "j", opts)
vim.api.nvim_set_keymap("", "l", "k", opts)
vim.api.nvim_set_keymap("", "ö", "l", opts)
vim.api.nvim_set_keymap("", "ß", "$", opts)
vim.api.nvim_set_keymap("", "Y", "y$", opts)
vim.api.nvim_set_keymap("", "ä", "za", opts)
vim.api.nvim_set_keymap("", "Ä", "zR", opts)

vim.api.nvim_set_keymap("n", "s", ":w<CR>", opts)
vim.api.nvim_set_keymap("n", "Q", ":q<CR>", opts)
vim.api.nvim_set_keymap("n", "<TAB>", ":BufferLineCycleNext<CR>", opts)
vim.api.nvim_set_keymap("n", "<S-TAB>", ":BufferLineCyclePrev<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>x", ':bd<CR>', opts)
vim.api.nvim_set_keymap("n", "<leader>c", "<cmd> :lua require('Comment.api').toggle.linewise.current()<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>fw", ':lua require("telescope")<CR> <cmd>Telescope live_grep<cr>', opts)
vim.api.nvim_set_keymap("n", "<leader>n", ':lua require("nnn")<CR> :NnnPicker<CR>', opts)
vim.api.nvim_set_keymap("n", "<leader>N", ':lua require("nnn")<CR> :NnnExplorer<CR>', opts)
vim.api.nvim_set_keymap("n", "f", ':lua require("leap")<CR> <Plug>(leap-forward)', {})
vim.api.nvim_set_keymap("n", "F", ':lua require("leap")<CR> <Plug>(leap-backward)', {})


vim.api.nvim_set_keymap("v", "<leader>c", "<esc><cmd> :lua require('Comment.api').toggle.linewise(vim.fn.visualmode())<CR>", opts)
vim.api.nvim_set_keymap("v", "<leader>a", ":EasyAlign<CR>", opts)

vim.api.nvim_set_keymap("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
vim.api.nvim_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
vim.api.nvim_set_keymap("n", "<leader>q", ':lua require"dap".close()<CR>', opts)
vim.api.nvim_set_keymap("n", "<leader>d", ':lua require"dapui".toggle({ reset = true })<CR>', opts)
vim.api.nvim_set_keymap("n", "<leader>b", ':lua require"dap".toggle_breakpoint()<CR>', opts)
vim.api.nvim_set_keymap("n", "<leader>e", ':lua require("dapui").eval()<CR>', opts)
vim.api.nvim_set_keymap("n", "<C-c>", ':lua require"dap".continue()<CR>', opts)
vim.api.nvim_set_keymap("n", "<C-s>", ':lua require"dap".step_into()<CR>', opts)
vim.api.nvim_set_keymap("n", "<C-o>", ':lua require"dap".step_over()<CR>', opts)
vim.api.nvim_set_keymap("n", "<C-Left>", "<C-w>h", opts)
vim.api.nvim_set_keymap("n", "<C-Down>", "<C-w>j", opts)
vim.api.nvim_set_keymap("n", "<C-Up>", "<C-w>k", opts)
vim.api.nvim_set_keymap("n", "<C-Right>", "<C-w>l", opts)
