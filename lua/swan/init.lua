-- Generated using ntangle.nvim
local M = {}
local poly_methods = {}

local exp_methods = {}

local sym_methods = {}

local constant_methods = {}

local sym_array_mt = {}

local create_add_exp

local create_mul_exp

local create_poly

local poly_ring_methods = {}
local poly_ring_mt = {}

local poly_mt = {}

local is_integer

local constant_mt = {}

local superscript = {
	["0"] = "⁰", ["1"] = "¹", ["2"] = "²", ["3"] = "³", ["4"] = "⁴", ["5"] = "⁵", ["6"] = "⁶", ["7"] = "⁷", ["8"] = "⁸", ["9"] = "⁹"
}

local imag_mt = {}

local imag_methods = {}

local create_rational
local rational_mt = {}
local rational_methods = {}

local isint

local rational_simplify_neg

local sym_mt = {}

local EXP_TYPE = {
	ADD = 1,
	SCALAR = 3,
	ARRAY = 4,

	IMAGINARY_i = 7,
	IMAGINARY_j = 8,
	IMAGINARY_k = 9,

	RATIONAL = 6,

	CONSTANT = 5,

	MUL = 2,

}

local add_exp_mt = {}

local mul_exp_mt = {}

local create_constant

local gcd

local imag_i = {}
imag_i.type = EXP_TYPE.IMAGINARY_i
imag_i = setmetatable(imag_i, imag_mt)


local imag_j = {}
imag_j.type = EXP_TYPE.IMAGINARY_j
imag_j = setmetatable(imag_j, imag_mt)


local imag_k = {}
imag_k.type = EXP_TYPE.IMAGINARY_k
imag_k = setmetatable(imag_k, imag_mt)


local imag_mul = {}
imag_mul[1] = {}
imag_mul[imag_i] = {}
imag_mul[imag_j] = {}
imag_mul[imag_k] = {}

imag_mul[1][imag_i] = {1, imag_i}
imag_mul[1][imag_j] = {1, imag_j}
imag_mul[1][imag_k] = {1, imag_k}

imag_mul[imag_i][1] = {1, imag_i}
imag_mul[imag_j][1] = {1, imag_j}
imag_mul[imag_k][1] = {1, imag_k}

imag_mul[imag_i][imag_i] = {-1, 1}
imag_mul[imag_j][imag_i] = {-1, imag_k}
imag_mul[imag_k][imag_i] = {1, imag_j}

imag_mul[imag_i][imag_j] = {1, imag_k}
imag_mul[imag_j][imag_j] = {-1, 1}
imag_mul[imag_k][imag_j] = {-1, imag_i}

imag_mul[imag_i][imag_k] = {-1, imag_j}
imag_mul[imag_j][imag_k] = {1, imag_i}
imag_mul[imag_k][imag_k] = {-1, 1}

local is_imag = {}
is_imag[EXP_TYPE.IMAGINARY_i] = true
is_imag[EXP_TYPE.IMAGINARY_j] = true
is_imag[EXP_TYPE.IMAGINARY_k] = true

function M.buchberger(...)
  local G = { ... }
  local first_i = 1
  while true do
    local Gp = {}
    for i=1,#G do
      table.insert(Gp,G[i])
    end

    for i=1,#Gp do
      for j=math.max(i+1,first_i),#Gp do
        local s_poly = Gp[i]:s_poly(Gp[j])
        local _,r = s_poly:div(unpack(Gp))
        if r and r:num_mono() > 0 then
          table.insert(G, r)
        end
      end
    end
    first_i = #Gp+1

    if #Gp == #G then
      break
    end

  end
  return G
end

function poly_methods:div(...)
  local divisors = { ... }

  local dividend = self
  local q = {}
  local r
  while dividend:num_mono() > 0 do
    local has_divided = false
    for i=1,#divisors do
      local divisible
      local gen1 = dividend:multideg()
      local gen2 = divisors[i]:multideg()

      divisible = true
      for i=1,#gen1 do
        if gen1[i] < gen2[i] then
          divisible = false
          break
        end
      end

      if divisible then
        has_divided = true
        local qi = ((dividend:lc() / divisors[i]:lc())):simplify()
        local mono_q = nil
        for i=1,#gen1 do
          local diff_gen = gen1[i] - gen2[i]
          if diff_gen > 0 then
            if not mono_q then
              mono_q = self.ring.vars[i]^diff_gen
            else
              mono_q = mono_q * self.ring.vars[i]^diff_gen
            end
          end
        end

        if qi.value and qi.value == 1 then
          qi = mono_q or create_constant(1)
        else
          if mono_q then
            qi = qi * mono_q
          end
        end

        qi = M.poly(qi, self.ring)
        dividend = dividend - divisors[i] * qi
        if q[i] then
          q[i] = q[i] + qi
        else
          q[i] = qi
        end

        break
      end
    end

    if not has_divided then
      local ri = M.poly(dividend:lt(), self.ring)
      dividend:pop_lt()
      if not r then
        r = ri
      else
        r = r + ri
      end
    end
  end
  return q, r
end

function poly_methods:num_mono()
  return #self.gens
end

function poly_methods:pop_lt()
  table.remove(self.coeffs)
  table.remove(self.gens)
end

function poly_methods:lcm(other)
  assert(self.ring == other.ring)

  local multideg_self = self:multideg()
  local multideg_other = other:multideg()

  local mono = nil
  for i=1,#multideg_self do
    local gamma = math.max(multideg_self[i], multideg_other[i])
    if gamma > 0 then
      if not mono then
        mono = self.ring.vars[i]^gamma
      else
        mono = mono * self.ring.vars[i]^gamma
      end
    end
  end
  return mono or create_constant(1)
end

function poly_methods:lcm_multideg(other)
  assert(self.ring == other.ring)

  local multideg_self = self:multideg()
  local multideg_other = other:multideg()

  local lcm_gen = {}
  for i=1,#multideg_self do
    table.insert(lcm_gen,math.max(multideg_self[i], multideg_other[i]))
  end
  return lcm_gen
end
function poly_methods:multideg()
  return self.gens[#self.gens]
end

function poly_methods:lc()
  return self.coeffs[#self.coeffs]
end

function poly_methods:lm()
  local mono = nil
  local last_gen = self.gens[#self.gens]
  for i=1,#last_gen do
    if last_gen[i] > 0 then
      if not mono then
        mono = self.ring.vars[i]^last_gen[i]
      else
        mono = mono * self.ring.vars[i]^last_gen[i]
      end
    end
  end
  return mono or create_constant(1)
end

function poly_methods:lt()
  local lc = self:lc()
  local lm = self:lm()
  if lc.type == EXP_TYPE.CONSTANT and lc.value == 1 then
    return lm
  else
    return lc * lm
  end
end
function poly_methods:s_poly(other)
  assert(self.ring == other.ring)

  local multideg_self = self:multideg()
  local multideg_other = other:multideg()

  local multideg_lcm = self:lcm_multideg(other)

  local mul_self = nil
  for i=1,#multideg_self do
    local diff = multideg_lcm[i] - multideg_self[i]
    if diff > 0 then
      if not mul_self then
        mul_self = self.ring.vars[i] ^ diff
      else
        mul_self = mul_self * self.ring.vars[i] ^ diff
      end
    end
  end

  if not mul_self then
    mul_self = create_constant(1) / self:lc() 
  else
    mul_self = (create_constant(1) / self:lc()) * mul_self
  end

  mul_self = mul_self or create_constant(1)

  local mul_other = nil
  for i=1,#multideg_other do
    local diff = multideg_lcm[i] - multideg_other[i]
    if diff > 0 then
      if not mul_other then
        mul_other = other.ring.vars[i] ^ diff
      else
        mul_other = mul_other * other.ring.vars[i] ^ diff
      end
    end
  end

  if not mul_other then
    mul_other = create_constant(1) / other:lc() 
  else
    mul_other = (create_constant(1) / other:lc()) * mul_other
  end
  mul_other = mul_other or create_constant(1)

  mul_self = M.poly(mul_self, self.ring)
  mul_other = M.poly(mul_other, other.ring)

  local lhs = mul_self * self
  local rhs = mul_other * other
  return lhs - rhs
end

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

	if self.type == EXP_TYPE.RATIONAL then
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
	local constant = create_constant()
	constant.value = self.value
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
		[EXP_TYPE.IMAGINARY_i] = 15,
		[EXP_TYPE.IMAGINARY_j] = 16,
		[EXP_TYPE.IMAGINARY_k] = 17,

		[EXP_TYPE.RATIONAL] = 10,

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

function M.poly(exp, poly_ring)
	local vars = poly_ring.vars
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

	local all_gens = {}

	for i=1,#norm_form.children do
		local term = norm_form.children[i]
		local gen = {}
		for j=1,#vars do
			table.insert(gen, 0)
		end

		local coeffs = {}
		if term.type == EXP_TYPE.CONSTANT or term.type == EXP_TYPE.RATIONAL then
			table.insert(coeffs, term)
		elseif term.type == EXP_TYPE.SCALAR then
			local idx = vars_lookup[term]
			if idx then
				table.insert(coeffs, create_constant(1))
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
		if #coeffs == 0 then
			local constant = create_constant()
			constant.value = 1
			table.insert(coeffs, constant)
		end
		coeff_exp.children = coeffs

		local curmap = all_gens
		local acc = nil
		for i=1,#gen do
			if i == #gen then
				acc = curmap[gen[i]]
				if not acc then
					acc = create_add_exp()
					curmap[gen[i]] = acc
				end
			else
				if not curmap[gen[i]] then
					curmap[gen[i]] = {}
				end
				curmap = curmap[gen[i]]
			end
		end

		table.insert(acc.children, coeff_exp)


	end

	local current = {}
	current[{}] = all_gens

	for i=1,#vars do
		local next = {}
		for gen, curmap in pairs(current) do
			for geni, nmap in pairs(curmap) do
				local ngen = {}
				for _, g in ipairs(gen) do
					table.insert(ngen, g)
				end
				table.insert(ngen, geni)
				next[ngen] = nmap
			end
		end
		current = next
	end

	for gen, coeffs in pairs(current) do
		current[gen] = coeffs:simplify()
	end


	local sorted_gens = {}
	for gen, coeffs in pairs(current) do
		table.insert(sorted_gens, gen)
	end

	table.sort(sorted_gens, poly_ring.mono_order)
	local poly = create_poly(poly_ring)

	local ring_gens = {}
	for i=1,#sorted_gens do
	  table.insert(ring_gens, poly_ring:unique_gen(sorted_gens[i]))
	end
	poly.gens = ring_gens

	for _, gen in ipairs(sorted_gens) do
		table.insert(poly.coeffs, current[gen])
	end

	poly:update_lookup()

	return poly
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

function create_poly(ring)
	local poly = {}
	poly = setmetatable(poly, poly_mt)

	poly.gens = {}
	poly.coeffs = {}
	poly.ring = ring
	return poly
end

function create_poly_ring()
	local poly_ring = {}
  poly_ring = setmetatable(poly_ring, poly_ring_mt)
  poly_ring.vars = {}
  poly_ring.gen_list = {}
  poly_ring.mono_order = nil
  return poly_ring
end

function M.poly_ring(order, ...)
  local poly_ring = create_poly_ring()
  poly_ring.vars = { ... }
  local mono_order = nil
  local mono_order = nil

  if order == 'lex' then
  	mono_order = function(a,b) 
  		for i=1,#a do
  			if a[i] < b[i] then
  				return true
  			elseif a[i] > b[i] then
  				return false
  			end
  		end

  	end
  elseif order == 'grlex' then
  	mono_order = function(a,b)
  		local total_a = 0
  		local total_b = 0
  		for i=1,#a do
  			total_a = total_a + a[i]
  			total_b = total_b + b[i]
  		end

  		if total_a < total_b then
  			return true
  		elseif total_a > total_b then
  			return false
  		else
  			for i=1,#a do
  				if a[i] < b[i] then
  					return true
  				elseif a[i] > b[i] then
  					return false
  				end
  			end

  		end
  	end
  elseif order == 'grevlex' then
  	mono_order = function(a,b)
  		local total_a = 0
  		local total_b = 0
  		for i=1,#a do
  			total_a = total_a + a[i]
  			total_b = total_b + b[i]
  		end

  		if total_a < total_b then
  			return true
  		elseif total_a > total_b then
  			return false
  		else
  			for i=#a,1 do
  				if a[i] - b[i] < 0 then
  					return false
  				elseif a[i] - b[i] > 0 then
  					return true
  				end
  			end
  			return true

  		end
  	end
  end

  poly_ring.mono_order = mono_order
  return poly_ring
end

function poly_ring_methods:unique_gen(gen)
  assert(#gen == #self.vars)

  for i=1,#self.gen_list do
    local same = true
    for j=1,#gen do
      if self.gen_list[i][j] ~= gen[j] then
        same = false
        break
      end
    end

    if same then
      return self.gen_list[i]
    end
  end

  table.insert(self.gen_list, gen)
  return gen
end

function poly_methods:update_lookup()
  self.gen_lookup = {}
  for i=1,#self.gens do
    self.gen_lookup[self.gens[i]] = i
  end
end

function poly_mt:__add(other)
  assert(self.ring == other.ring)

  local coeffs = {}
  local overlap = {}
  for i, gen in ipairs(self.gens) do
    local j = other.gen_lookup[gen]
    if j then
      local result = (self.coeffs[i] + other.coeffs[j]):simplify()
      coeffs[gen] = result
      if result.children and #result.children == 0 then
        coeffs[gen] = nil
      elseif result.value and result.value == 0 then
        coeffs[gen] = nil
      end
      overlap[j] = true
    else
      coeffs[gen] = self.coeffs[i]
    end
  end

  for j, gen in ipairs(other.gens) do
    if not overlap[j] then
      coeffs[gen] = other.coeffs[j]
    end
  end

  local gens = {}
  for gen, _ in pairs(coeffs) do
    table.insert(gens, gen)
  end

  table.sort(gens, self.ring.mono_order)

  local poly = create_poly(self.ring)
  poly.gens = gens
  local sorted_coeffs = {}
  for _, gen in ipairs(gens) do
    table.insert(sorted_coeffs, coeffs[gen])
  end
  poly.coeffs = sorted_coeffs

  poly:update_lookup()

  return poly
end

function poly_mt:__sub(other)
  assert(self.ring == other.ring)

  local coeffs = {}
  local overlap = {}
  for i, gen in ipairs(self.gens) do
    local j = other.gen_lookup[gen]
    if j then
      local result = (self.coeffs[i] - other.coeffs[j]):simplify()
      coeffs[gen] = result
      if result.children and #result.children == 0 then
        coeffs[gen] = nil
      elseif result.value and result.value == 0 then
        coeffs[gen] = nil
      end
      overlap[j] = true
    else
      coeffs[gen] = self.coeffs[i]
    end
  end

  for j, gen in ipairs(other.gens) do
    if not overlap[j] then
      coeffs[gen] = -other.coeffs[j]
    end
  end

  local gens = {}
  for gen, _ in pairs(coeffs) do
    table.insert(gens, gen)
  end

  table.sort(gens, self.ring.mono_order)

  local poly = create_poly(self.ring)
  poly.gens = gens
  local sorted_coeffs = {}
  for _, gen in ipairs(gens) do
    table.insert(sorted_coeffs, coeffs[gen])
  end
  poly.coeffs = sorted_coeffs

  poly:update_lookup()

  return poly
end

function poly_mt:__mul(other)
  assert(self.ring == other.ring)

  local coeffs = {}
  for i, gen_i in ipairs(self.gens) do
    for j, gen_j in ipairs(other.gens) do
      local gen_add = {}
      for i=1,#gen_i do
        table.insert(gen_add, gen_i[i] + gen_j[i])
      end
      gen_add = self.ring:unique_gen(gen_add)

      local coeffs_mul = (self.coeffs[i] * other.coeffs[j]):simplify()
      if not coeffs[gen_add] then
        coeffs[gen_add] = coeffs_mul
      else
        local result = (coeffs[gen_add] + coeffs_mul):simplify()
        if result.children and #result.children == 0 then
          coeffs[gen_add] = nil
        else
          coeffs[gen_add] = result
        end
      end
    end
  end

  local gens = {}
  for gen, _ in pairs(coeffs) do
    table.insert(gens, gen)
  end

  table.sort(gens, self.ring.mono_order)

  local poly = create_poly(self.ring)
  poly.gens = gens
  local sorted_coeffs = {}
  for _, gen in ipairs(gens) do
    table.insert(sorted_coeffs, coeffs[gen])
  end
  poly.coeffs = sorted_coeffs

  poly:update_lookup()

  return poly
end

function poly_mt:__tostring()
  local result = ""
  for i=#self.gens,1,-1 do
    local gen = self.gens[i]
    local coeff = self.coeffs[i]
    local term = ""
    term = tostring(coeff)  
    if term == "1" and not is_zero_gen(gen) then
      term = ""
    elseif term == "-1" and not is_zero_gen(gen) then
      term = "-"
    end

    if result ~= "" then
      term = " + " .. term 
    end

    for i=1,#gen do
      if gen[i] > 0 then
        local sup = tostring(gen[i])
        if gen[i] ~= 1 then
          local new_sup = ""
          for j=1,#sup do
            new_sup = new_sup .. superscript[sup:sub(j,j)]
          end
          sup = new_sup
        else
          sup = ""
        end

        term = term .. tostring(self.ring.vars[i]) .. sup
      end
    end
    result = result .. term
  end
  local vars = {}
  for i=1,#self.ring.vars do
    table.insert(vars, tostring(self.ring.vars[i]))
  end
  result = result .. " ∈ k[" .. table.concat(vars, ",") .. "]"
  return result
end

function is_zero_gen(gen)
  for i=1,#gen do
    if gen[i] ~= 0 then
      return false
    end
  end
  return true
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

function M.i()
  return imag_i
end

function M.j()
  return imag_j
end

function M.k()
  return imag_k
end

function imag_mt:__tostring()
  if self == imag_i then
    return "i"
  elseif self == imag_j then
    return "j"
  elseif self == imag_k then
    return "k"
  end
end

function imag_methods:normal_form()
	return self
end

function create_rational(num, den)
  assert(den ~= 0)

	local rational = {}
	rational.type = EXP_TYPE.RATIONAL
	rational.num = num
	rational.den = den
  rational = setmetatable(rational, rational_mt)
  return rational
end

function isint(n)
  return math.floor(n) == n
end

function rational_methods:gcd()
  local a = gcd(self.num, self.den)
  local num = self.num / a
  local den = self.den / a

  if den == 1 then
    return create_constant(num)
  else
    return create_rational(num, den)
  end
end

function rational_simplify_neg(num,den) 
  if num < 0 and den < 0 then
    num = -num
    den = -den
  elseif den < 0 then
    num = -num
    den = -den
  end
  return num, den
end

function rational_methods:normal_form()
  local result = self:gcd()
  result.num, result.den = rational_simplify_neg(result.num, result.den)
  return result
end

function rational_mt:__div(other)
	if other.type == EXP_TYPE.CONSTANT then
		return create_rational(self.num, self.den * other.value):gcd()
	elseif other.type == EXP_TYPE.RATIONAL then
		return create_rational(self.num * self.den, self.den * other.num):gcd()
	else
		assert(false)
	end
end

function rational_mt:__tostring()
  return "(" .. tostring(self.num) .. "/" .. tostring(self.den) .. ")"
end

function rational_methods:clone()
  return self
end

function rational_methods:is_monomial()
	return true
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

function sym_mt:__sub(other)
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
		table.insert(exp.children, -constant)
	elseif other.type and other.type == EXP_TYPE.ADD then
		for _, child in ipairs(other.children) do
			table.insert(exp.children, -child)
		end
	else
		table.insert(exp.children, -other)
	end

	return exp
end

function sym_mt:__unm()
	if self.type == EXP_TYPE.CONSTANT then
		local result = create_constant()
		result.value = -self.value
		return result
	elseif self.type == EXP_TYPE.RATIONAL then
	  return create_rational(-self.num, self.den)

	elseif self.type == EXP_TYPE.ADD then
		local exp = create_add_exp()
		for i=1,#result.children do
			table.insert(exp.children, -self.children[i])
		end
		return exp
	else
		local exp = create_mul_exp()
		local constant = create_constant()
		constant.value = -1
		table.insert(exp.children, constant)
		if self.type == EXP_TYPE.MUL then
			for _,child in ipairs(self.children) do
				table.insert(exp.children, child)
			end
		else
			table.insert(exp.children, self)
		end
		return exp
	end
end

function constant_mt:__div(other)
	if other.type == EXP_TYPE.CONSTANT then
		return create_rational(self.value, other.value):gcd()
	elseif other.type == EXP_TYPE.RATIONAL then
		return create_rational(self.value * other.den, other.num):gcd()
	else
		assert(false)
	end
end

function M.constant(value)
	return create_constant(value)
end

function mul_exp_mt:__div(other)
	return self * (M.constant(1) / other)
end

function exp_methods:simplify()
	if self.type == EXP_TYPE.CONSTANT then
		return self:clone()

	elseif self.type == EXP_TYPE.RATIONAL then
	  return self:gcd()

	elseif self.type == EXP_TYPE.SCALAR then
		return self:clone()

	elseif is_imag[self.type] then
	    return self
	elseif self.type == EXP_TYPE.ADD or self.type == EXP_TYPE.MUL then
		local children_simplified = {}

		for i=1,#self.children do
			table.insert(children_simplified, self.children[i]:simplify())
		end

		if self.type == EXP_TYPE.ADD then
			local all_terms_num = 0
			local all_terms_den = 1

			local new_children_simplified = {}
			for i=1,#children_simplified do
				if children_simplified[i].type == EXP_TYPE.CONSTANT then
					all_terms_num = all_terms_num + children_simplified[i].value * all_terms_den

					local div = gcd(all_terms_num , all_terms_den)
					all_terms_num = all_terms_num / div
					all_terms_den = all_terms_den / div

				elseif children_simplified[i].type == EXP_TYPE.RATIONAL then
					all_terms_num = all_terms_num * children_simplified[i].den + children_simplified[i].num * all_terms_den
					all_terms_den = all_terms_den  * children_simplified[i].den

					local div = gcd(all_terms_num , all_terms_den)
					all_terms_num = all_terms_num / div
					all_terms_den = all_terms_den / div
				else
					table.insert(new_children_simplified, children_simplified[i])
				end
			end

			all_terms_num, all_terms_den = rational_simplify_neg(all_terms_num, all_terms_den)

			if all_terms_num ~= 0 and all_terms_den == 1 then
				table.insert(new_children_simplified, 1, create_constant(all_terms_num ))
			elseif all_terms_num ~= 0 and all_terms_den ~= 1 then
				table.insert(new_children_simplified, 1, create_rational(all_terms_num , all_terms_den))
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

			if #add_exp.children == 1 then
				return add_exp.children[1]
			end

			if #add_exp.children == 0 then
				return create_constant(0)
			end


			return exp
		end

		if self.type == EXP_TYPE.MUL then
			local all_factor_num = 1
			local all_factor_den = 1

			local new_children_simplified = {}
			for i=1,#children_simplified do
				if children_simplified[i].type == EXP_TYPE.CONSTANT then
					all_factor_num = all_factor_num  * children_simplified[i].value
				elseif children_simplified[i].type == EXP_TYPE.RATIONAL then
					all_factor_num = all_factor_num  * children_simplified[i].num
					all_factor_den = all_factor_den  * children_simplified[i].den
				else
					table.insert(new_children_simplified, children_simplified[i])
				end
			end

			local div = gcd(all_factor_num, all_factor_den)
			all_factor_num = all_factor_num / div 
			all_factor_den = all_factor_den / div 

			all_factor_num, all_factor_den = rational_simplify_neg(all_factor_num, all_factor_den)

			if all_factor_num ~= 1 and all_factor_den == 1 then
				table.insert(new_children_simplified, 1, create_constant(all_factor_num ))
			elseif all_factor_den ~= 1 then
				table.insert(new_children_simplified, 1, create_rational(all_factor_num , all_factor_den))
			end

			children_simplified = new_children_simplified

			local current_imag = 1
			local current_imag_sign = 1

			new_children_simplified = {}
			for i=1,#children_simplified do
			  if is_imag[children_simplified[i].type] then
			    local sign, res = unpack(imag_mul[current_imag][children_simplified[i]])
			    current_imag_sign = current_imag_sign * sign
			    current_imag = res
			  else
			    table.insert(new_children_simplified, children_simplified[i])
			  end
			end

			if current_imag ~= 1 or current_imag_sign ~= 1 then
			  if current_imag_sign == -1 then
			    table.insert(new_children_simplified, create_constant(-1))
			  end
			  if current_imag ~= 1 then
			    table.insert(new_children_simplified, current_imag)
			  end
			end

			children_simplified = new_children_simplified

			if #children_simplified == 0 then
				return create_constant(1)
			end

			if #children_simplified == 1 then
				return children_simplified[1]
			end

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

function gcd(a,b)
  assert(isint(a))
  assert(isint(b))

  a = math.abs(a)
  b = math.abs(b)
  local t
  while b ~= 0 do
    t = b
    b = math.fmod(a,b)
    a = t
  end
	return a
end

poly_mt.__index = poly_methods

mul_exp_mt.__lt = add_exp_mt.__lt
mul_exp_mt.__eq = add_exp_mt.__eq
mul_exp_mt.__index = exp_methods
add_exp_mt.__index = exp_methods

sym_mt.__index = sym_methods
sym_methods.expand = exp_methods.expand

constant_mt.__index = constant_methods
constant_methods.expand = exp_methods.expand

poly_ring_mt.__index = poly_ring_methods

mul_exp_mt.__pow = sym_mt.__pow
add_exp_mt.__pow = sym_mt.__pow
imag_mt.__pow = sym_mt.__pow
imag_mt.__add = sym_mt.__add
imag_mt.__sub = sym_mt.__sub
imag_mt.__mul = sym_mt.__mul
imag_mt.__div = sym_mt.__div

imag_mt.__index = imag_methods

imag_methods.simplify = exp_methods.simplify

rational_mt.__index = rational_methods

rational_methods.expand = exp_methods.expand
rational_methods.simplify = exp_methods.simplify

rational_mt.__unm = sym_mt.__unm
rational_mt.__add = sym_mt.__add
rational_mt.__sub = sym_mt.__sub
rational_mt.__mul = sym_mt.__mul

add_exp_mt.__add = sym_mt.__add 

mul_exp_mt.__mul = sym_mt.__mul
add_exp_mt.__mul = sym_mt.__mul
mul_exp_mt.__add = sym_mt.__add

add_exp_mt.__unm = sym_mt.__unm
mul_exp_mt.__unm = sym_mt.__unm
add_exp_mt.__sub = sym_mt.__sub
mul_exp_mt.__sub = sym_mt.__sub
constant_mt.__unm = sym_mt.__unm
constant_mt.__sub = sym_mt.__sub
constant_mt.__add = sym_mt.__add
constant_mt.__mul = sym_mt.__mul

add_exp_mt.__mul = mul_exp_mt.__mul
sym_methods.simplify = exp_methods.simplify
constant_methods.simplify = exp_methods.simplify

return M

