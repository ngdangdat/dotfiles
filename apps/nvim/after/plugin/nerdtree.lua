local cwd = vim.fn.getcwd()
local argv = vim.fn.argv()

vim.api.nvim_create_autocmd("VimEnter", {
  pattern = "*",
  callback = function()
    if vim.fn.argc() == 1 then
      local parent_dir = vim.fn.fnamemodify(argv[1], ":h")
      if vim.fn.isdirectory(argv[1]) == 0 and vim.fn.isdirectory(parent_dir) == 1 then
        vim.cmd("NERDTree" .. parent_dir)
      end
    end
  end,
})

vim.api.nvim_create_autocmd("VimEnter", {
  pattern = "*",
  callback = function()
    vim.cmd("wincmd l")
  end,
})

vim.api.nvim_create_autocmd("BufEnter", {
  desc = "Close nvim if NERDTree is the only buffer remains",
  pattern = "*",
  callback = function()
    if (vim.fn.winnr("$") == 1 and vim.b.NERDTree ~= nil and vim.b.NERDTree._type == "tab") then
      vim.cmd.q()
    end
  end,
})

vim.keymap.set("n", "<C-b>", function()
  if vim.fn.exists(":NERDTreeToggle") then
    vim.cmd("NERDTreeToggle")
  end
end)

