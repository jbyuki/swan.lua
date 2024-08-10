-- Generated using ntangle.nvim
local M = {}
function M.symbols(str)
	local tokens = {}
	for token in str:gmatch("%S") do
		local name, s1, s2 = token:match("(.+)(%d+):(%d+)$")
		if name then
			tokens.insert({
				s1 = s1,
				s2 = s2,
				name = name,
			})
		else
			table.insert({
				name = name,
			})
		end
	end

end

return M

