-- Generated using ntangle.nvim
local M = {}
local exp_methods = {}

local sym_methods = {}

local constant_methods = {}

local sym_array_mt = {}

local create_add_exp

local create_mul_exp

local is_integer

local constant_mt = {}

local superscript = {
	["0"] = "⁰", ["1"] = "¹", ["2"] = "²", ["3"] = "³", ["4"] = "⁴", ["5"] = "⁵", ["6"] = "⁶", ["7"] = "⁷", ["8"] = "⁸", ["9"] = "⁹"
}

local sym_mt = {}

local EXP_TYPE = {
	ADD = 1,
	SCALAR = 3,
	ARRAY = 4,

	CONSTANT = 5,

	MUL = 2,

}

local add_exp_mt = {}

local mul_exp_mt = {}

local create_constant

function constant_mt:__lt(other)
	assert(other.type == self.type)
	return self.value < other.value
end

function sym_mt:__lt(other)
	assert(other.type == self.type)
	return self.name < other.name
end

function add_exp_mt:__lt(other)
	assert(other.type == self.type)
	if #self.children ~= #other.children then
		return #self.children < #other.children
	end
	for i=1,#self.children do
		if self.children[i] ~= other.children[i] then
			return self.children[i] < other.children[i]
		end

	end
	return false
end


function constant_mt:__eq(other)
	return other.type == EXP_TYPE.CONSTANT and self.value == other.value
end

function sym_mt:__eq(other)
	return other.type == EXP_TYPE.SCALAR and self.name == other.name
end

function add_exp_mt:__eq(other)
	if other.type ~= self.type then
		return false
	end

	if #self.children ~= #other.children then
		return false
	end

	for i=1,#self.children do
		if self.children[i] ~= other.children[i] then
			return false
		end
	end
	return true
end

function exp_methods:expand()
	if self.type == EXP_TYPE.SCALAR then
		return self:clone()
	end

	if self.type == EXP_TYPE.CONSTANT then
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
			local exp = create_mul_exp()

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


		local exp
		if #new_children > 1 then
			exp = create_add_exp()
			exp.children = new_children
		else
			exp = new_children[1]
		end
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
	-- local sym = {}
	-- sym.name = self.name
	-- ; set type sym
	-- ; register scalar sym metamethods
	-- return sym
	return self
end

function constant_methods:clone()
	local constant = {}
	constant.type = EXP_TYPE.CONSTANT
	constant.value = self.value
	constant = setmetatable(constant, constant_mt)

	return constant
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

function constant_methods:normal_form()
	return self:clone()
end

function sym_methods:normal_form()
	return self:clone()
end

function exp_methods:normal_form()
	local children_normal = {}
	for i=1,#self.children do
		table.insert(children_normal, self.children[i]:normal_form())
	end

	local sorted_idx = {}
	for i=1,#children_normal do
		table.insert(sorted_idx, i)
	end

	local order = {}
	local order_lookup = {
		[EXP_TYPE.CONSTANT] = 1,
		[EXP_TYPE.SCALAR] = 20,
		[EXP_TYPE.MUL] = 30,
		[EXP_TYPE.ADD] = 40,
	}

	for i=1,#children_normal do
		table.insert(order, order_lookup[children_normal[i].type])
	end

	table.sort(sorted_idx, function(i,j)
		if order[i] == order[j] then
			local left = children_normal[i]
			local right = children_normal[j]
			return left < right
		end
		return order[i] < order[j]
	end)

	local sorted_children_normal = {}
	for i=1,#children_normal do
		table.insert(sorted_children_normal, children_normal[sorted_idx[i]])
	end

	if self.type == EXP_TYPE.ADD then
		local exp = create_add_exp()
		exp.children = sorted_children_normal
		return exp

	elseif self.type == EXP_TYPE.MUL then
		local exp = create_mul_exp()
		exp.children = sorted_children_normal
		return exp
	else
		assert(false)
	end
end

function create_add_exp()
	local exp = {}
	exp.type = EXP_TYPE.ADD
	exp.children = {}
	exp = setmetatable(exp, add_exp_mt)

	return exp
end

function create_mul_exp()
	local exp = {}
	exp.type = EXP_TYPE.MUL
	exp.children = {}
	exp = setmetatable(exp, mul_exp_mt)
	return exp
end

function M.poly(exp, ...)
	local vars = { ... }
	for i=1,#vars do
		assert(vars[i].type == EXP_TYPE.SCALAR)
	end

	assert(#vars > 0)

	local norm_form = exp:expand():simplify()
	if norm_form.type ~= EXP_TYPE.ADD then
		local add_exp = create_add_exp()
		add_exp.children = { norm_form }
		norm_form = add_exp
	end

	for i=1,#norm_form.children do
		assert(norm_form.children[i]:is_monomial(), tostring(norm_form.children[i]))
	end


	local vars_lookup = {}
	for i=1,#vars do
		vars_lookup[vars[i]] = i
	end

	for i=1,#norm_form.children do
		local term = norm_form.children[i]
		local gen = {}
		for j=1,#vars do
			table.insert(gen, 0)
		end

		local coeffs = {}
		if term.type == EXP_TYPE.CONSTANT then
			table.insert(coeffs, term)
		elseif term.type == EXP_TYPE.SCALAR then
			local idx = vars_lookup[term]
			if idx then
				table.insert(coeffs, 1)
				gen[idx] = 1
			else
				table.insert(coeffs, term)
			end

		elseif term.type == EXP_TYPE.MUL then
			for j=1,#term.children do
				local found_gen = false
				if term.children[j].type == EXP_TYPE.SCALAR then
					local idx = vars_lookup[term.children[j]]
					if idx then
						gen[idx] = gen[idx] + 1
						found_gen = true
					end
				end

				if not found_gen then
					table.insert(coeffs, term.children[j])
				end
			end

		else
			assert(false)
		end

		local coeff_exp = create_mul_exp()
		coeff_exp.children = coeffs
		io.write(tostring(coeff_exp) .. "\n")
		io.write(vim.inspect(gen) .. "\n")
	end

end

function constant_methods:is_monomial()
	return true
end

function sym_methods:is_monomial()
	return true
end

function exp_methods:is_monomial()
	if self.type == EXP_TYPE.ADD then
		return false
	elseif self.type == EXP_TYPE.MUL then
		return true
	end
	return false
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

	local power = {}
	local factors = {}
	local last_child

	for i=1,#children_str do
		local child = children_str[i]
		if last_child == child then
			power[#power] = power[#power] + 1
		else
			table.insert(power, 1)
			table.insert(factors, child)
		end
		last_child = factors[#factors]
	end

	local elems = {}
	for i=1,#factors do
		if power[i] == 1 then
			table.insert(elems, factors[i])
		else
			local sup = tostring(power[i])
			local new_sup = ""
			for j=1,#sup do
				new_sup = new_sup .. superscript[sup:sub(j,j)]
			end
			sup = new_sup
			table.insert(elems, factors[i] .. sup)
		end
	end

	return table.concat(elems,"")

end

function constant_mt:__tostring()
	return tostring(self.value)
end

function sym_mt:__add(other)
	local exp = {}
	exp.type = EXP_TYPE.ADD
	exp.children = {}

	exp = setmetatable(exp, add_exp_mt)

	if type(self) == "number" then
		local constant = create_constant()
		constant.value = self
		table.insert(exp.children, constant)
	elseif self.type and self.type == EXP_TYPE.ADD then
		for _, child in ipairs(self.children) do
			table.insert(exp.children, child)
		end
	else
		table.insert(exp.children, self)
	end

	if type(other) == "number" then
		local constant = create_constant()
		constant.value = other
		table.insert(exp.children, constant)
	elseif other.type and other.type == EXP_TYPE.ADD then
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
	if type(self) == "number" then
		local constant = {}
		constant.type = EXP_TYPE.CONSTANT
		constant.value = self
		constant = setmetatable(constant, constant_mt)

		table.insert(exp.children, constant)
	elseif self.type and self.type == EXP_TYPE.MUL then
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

function exp_methods:simplify()
	if self.type == EXP_TYPE.CONSTANT then
		return self:clone()

	elseif self.type == EXP_TYPE.SCALAR then
		return self:clone()

	elseif self.type == EXP_TYPE.ADD or self.type == EXP_TYPE.MUL then
		local children_simplified = {}

		for i=1,#self.children do
			table.insert(children_simplified, self.children[i]:simplify())
		end

		if self.type == EXP_TYPE.ADD then
			local all_terms = 0
			local new_children_simplified = {}
			for i=1,#children_simplified do
				if children_simplified[i].type == EXP_TYPE.CONSTANT then
					all_terms = all_terms + children_simplified[i].value
				else
					table.insert(new_children_simplified, children_simplified[i])
				end
			end

			if all_terms ~= 0 then
				local constant = create_constant()
				constant.value = all_terms
				table.insert(new_children_simplified, 1, constant)
			end

			children_simplified = new_children_simplified
			local exp = create_add_exp()
			local terms = {}
			local coeffs = {}

			for i=1,#children_simplified do
				local candidate = children_simplified[i]
				candidate = candidate:normal_form()


				local found = false
				for j=1,#terms do
					local equal = false
					local term = terms[j]
					equal = term == candidate

					if equal then
						coeffs[j] = coeffs[j] + 1
						found = true
						break
					end

				end

				if not found then
					table.insert(terms, candidate)
					table.insert(coeffs, 1)
				end

			end

			local add_exp = exp
			for i=1,#terms do
				if coeffs[i] > 1 then
					local exp
					if terms[i].type == EXP_TYPE.MUL then
						exp = terms[i]
						table.insert(exp.children, 1, create_constant(coeffs[i]))

					else
						exp = create_mul_exp()
						exp.children = { create_constant(coeffs[i]), terms[i] }
					end
					table.insert(add_exp.children, exp:simplify())
				else
					table.insert(add_exp.children, terms[i])
				end

			end

			return exp
		end

		if self.type == EXP_TYPE.MUL then
			local all_factor = 1
			local new_children_simplified = {}
			for i=1,#children_simplified do
				if children_simplified[i].type == EXP_TYPE.CONSTANT then
					all_factor = all_factor * children_simplified[i].value
				else
					table.insert(new_children_simplified, children_simplified[i])
				end
			end

			if all_factor ~= 1 then
				local constant = create_constant()
				constant.value = all_factor
				table.insert(new_children_simplified, 1, constant)
			end

			children_simplified = new_children_simplified

			local exp = create_mul_exp()
			exp.children = children_simplified
			return exp
		end

	end

end

function create_constant(value)
	local constant = {}
	constant.type = EXP_TYPE.CONSTANT
	constant.value = value
	constant = setmetatable(constant, constant_mt)

	return constant
end

mul_exp_mt.__lt = add_exp_mt.__lt
mul_exp_mt.__eq = add_exp_mt.__eq
mul_exp_mt.__index = exp_methods
add_exp_mt.__index = exp_methods

sym_mt.__index = sym_methods
sym_methods.expand = exp_methods.expand

constant_mt.__index = constant_methods
constant_methods.expand = exp_methods.expand

mul_exp_mt.__pow = sym_mt.__pow
add_exp_mt.__pow = sym_mt.__pow
add_exp_mt.__add = sym_mt.__add 

mul_exp_mt.__mul = sym_mt.__mul
add_exp_mt.__mul = sym_mt.__mul
mul_exp_mt.__add = sym_mt.__add

sym_methods.simplify = exp_methods.simplify
constant_methods.simplify = exp_methods.simplify

return M

