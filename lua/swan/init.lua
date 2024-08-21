-- Generated using ntangle.nvim
local M = {}
local exp_methods = {}

local sym_methods = {}

local sym_array_mt = {}

local is_integer

local sym_mt = {}

local EXP_TYPE = {
	ADD = 1,
	SCALAR = 3,
	ARRAY = 4,

	MUL = 2,

}

local add_exp_mt = {}

local mul_exp_mt = {}

function exp_methods:expand()
	if self.type == EXP_TYPE.SCALAR then
		return self:clone()
	end

	local expanded_children = {}
	for _, child in ipairs(self.children) do
		table.insert(expanded_children, child:expand())
	end

	if self.type == EXP_TYPE.MUL then
		local new_children = {}
		local idx = {}
		for i=1,#expanded_children do
			idx[i] = 1
		end

		while true do
			local exp = {}
			exp.type = EXP_TYPE.MUL
			exp.children = {}
			exp = setmetatable(exp, mul_exp_mt)

			for i=1,#expanded_children do
				if expanded_children[i].type == EXP_TYPE.ADD then
					table.insert(exp.children, expanded_children[i].children[idx[i]])
				else
					table.insert(exp.children, expanded_children[i])
				end
			end

			table.insert(new_children, exp)

			local good = false
			for i=1,#idx do
				if expanded_children[i].type == EXP_TYPE.ADD then
					if idx[i] + 1 <= #expanded_children[i].children then
						idx[i] = idx[i] + 1
						good = true
						break
					else
						idx[i] = 1
					end
				end
			end

			if not good then
				break
			end
		end


		local exp = {}
		exp.type = EXP_TYPE.ADD
		exp.children = new_children
		exp = setmetatable(exp, add_exp_mt)

		return exp

	elseif self.type == EXP_TYPE.ADD then
		local exp = {}
		exp.type = EXP_TYPE.ADD
		exp.children = expanded_children
		exp = setmetatable(exp, add_exp_mt)

		return exp
	else
		assert(false)
	end

end

function sym_methods:clone()
	local sym = {}
	sym.name = self.name
	sym.type = EXP_TYPE.SCALAR

	sym = setmetatable(sym, sym_mt)

	return sym
end

function M.symbols(str)
	local elems = vim.split(str, "%s+", { trimempty = true })

	local syms = {}
	for _, elem in ipairs(elems) do
		local sym = {}
		if elem:match(".+%d+:%d+$") then
			local name, idx1, idx2 = elem:match("(.+)(%d+):(%d+)$")
			sym.name = name
			sym.range = {tonumber(idx1), tonumber(idx2)}
			sym.type = EXP_TYPE.ARRAY

			sym = setmetatable(sym, sym_array_mt)
			sym.syms = {}
			local sym_array = sym
			for idx=sym.range[1],sym.range[2] do
				local sym = {}
				sym.name = ("%s%d"):format(name, idx)
				sym.type = EXP_TYPE.SCALAR

				sym = setmetatable(sym, sym_mt)

				table.insert(sym_array.syms, sym)
			end


		else
			local name = vim.trim(elem)
			sym.name = name
			sym.type = EXP_TYPE.SCALAR

			sym = setmetatable(sym, sym_mt)

		end

		table.insert(syms, sym)
	end
	return unpack(syms)
end

function sym_array_mt:__index(index)
	return self.syms[index]
end

function M.matrix(arr)
	assert(type(arr) == "table")
	local N = #arr
	assert(N > 0)

	local M = #arr[1]
	for i=2,N do
		assert(#arr[i] == M)
	end

	local mat = {}
	mat.elems = arr
	return mat
end

function sym_mt:__pow(sup)
	assert(type(sup) == "number", "exponent must be a constant number")
	assert(is_integer(sup), "exponent must be a constant integer number")

	local exp = {}
	exp.type = EXP_TYPE.MUL
	exp.children = {}
	exp = setmetatable(exp, mul_exp_mt)

	for i=1,sup do
		table.insert(exp.children, self)
	end

	return exp
end

function is_integer(x)
	return math.floor(x) == x
end

function sym_mt:__tostring()
	return self.name
end

function add_exp_mt:__tostring()
	local children_str = {}
	for i=1,#self.children do
		table.insert(children_str, tostring(self.children[i]))
	end

	return table.concat(children_str, " + ")
end

function mul_exp_mt:__tostring()
	local children_str = {}
	for i=1,#self.children do
		local child_str = tostring(self.children[i])
		if self.children[i].type == EXP_TYPE.ADD then
			table.insert(children_str, ("(%s)"):format(child_str))
		else
			table.insert(children_str, child_str)
		end
	end
	return table.concat(children_str, "*")
end

function sym_mt:__add(other)
	local exp = {}
	exp.type = EXP_TYPE.ADD
	exp.children = {}

	exp = setmetatable(exp, add_exp_mt)

	if self.type and self.type == EXP_TYPE.ADD then
		for _, child in ipairs(self.children) do
			table.insert(exp.children, child)
		end
	else
		table.insert(exp.children, self)
	end

	if other.type and other.type == EXP_TYPE.ADD then
		for _, child in ipairs(other.children) do
			table.insert(exp.children, child)
		end
	else
		table.insert(exp.children, other)
	end

	return exp
end

function sym_mt:__mul(other)
	local exp = {}
	exp.type = EXP_TYPE.MUL
	exp.children = {}

	exp = setmetatable(exp, mul_exp_mt)
	if self.type and self.type == EXP_TYPE.MUL then
		for _, child in ipairs(self.children) do
			table.insert(exp.children, child)
		end
	else
		table.insert(exp.children, self)
	end

	if other.type and other.type == EXP_TYPE.MUL then
		for _, child in ipairs(other.children) do
			table.insert(exp.children, child)
		end
	else
		table.insert(exp.children, other)
	end

	return exp
end

mul_exp_mt.__index = exp_methods
add_exp_mt.__index = exp_methods

sym_mt.__index = sym_methods
sym_methods.expand = exp_methods.expand

mul_exp_mt.__pow = sym_mt.__pow
add_exp_mt.__pow = sym_mt.__pow
add_exp_mt.__add = sym_mt.__add 

mul_exp_mt.__mul = sym_mt.__mul
add_exp_mt.__mul = sym_mt.__mul
mul_exp_mt.__add = sym_mt.__add

return M

