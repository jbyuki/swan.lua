-- Generated using ntangle.nvim
local M = {}
function M.syms(str)
	local tokens = {}
	for token in str:gmatch("%S") do
		table.insert(tokens, token)
	end
end

return M

