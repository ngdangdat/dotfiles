local builtin = require('telescope.builtin')
local argv = vim.fn.argv()
local cwd = vim.fn.getcwd()


local function get_search_dirs()
  search_dirs = {}
  if (vim.fn.argc() == 1) then
    local parent_dir = vim.fn.fnamemodify(argv[1], ":h")
    if (vim.fn.isdirectory(argv[1]) == 1) then
      table.insert(search_dirs, argv[1])
    elseif (vim.fn.isdirectory(parent_dir) == 1) then
      table.insert(search_dirs, parent_dir)
    end
  end
  if #search_dirs == 0 then
    table.insert(search_dirs, cwd)
  end
  return search_dirs
end

vim.keymap.set('n', '<C-p>', function()
  builtin.find_files({ search_dirs = get_search_dirs() })
end, {})

vim.keymap.set('n', '<leader>pg', function()
  builtin.git_files({ search_dirs = get_search_dirs() })
end, {})

vim.keymap.set('n', '<leader>ps', function()
  builtin.grep_string({ search = vim.fn.input("Grep > "), search_dirs = get_search_dirs()});
end)

vim.keymap.set('n', '<leader>fd', function() 
  builtin.find_files({ cwd = vim.fn.input("Find files in > ") })
end)

vim.keymap.set('n', '<leader>fg', function() 
  builtin.git_files({ cwd = vim.fn.input("Find (git) files in > ") })
end)

