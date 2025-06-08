-- Generated using ntangle.nvim
local M = {}
local poly_methods = {}

local exp_methods = {}

local sym_methods = {}

local constant_methods = {}

local greek_etc = {
  ["Alpha"] = "Α", ["Beta"] = "Β", ["Gamma"] = "Γ", ["Delta"] = "Δ", ["Epsilon"] = "Ε", ["Zeta"] = "Ζ", ["Eta"] = "Η", ["Theta"] = "Θ", ["Iota"] = "Ι", ["Kappa"] = "Κ", ["Lambda"] = "Λ", ["Mu"] = "Μ", ["Nu"] = "Ν", ["Xi"] = "Ξ", ["Omicron"] = "Ο", ["Pi"] = "Π", ["Rho"] = "Ρ", ["Sigma"] = "Σ", ["Tau"] = "Τ", ["Upsilon"] = "Υ", ["Phi"] = "Φ", ["Chi"] = "Χ", ["Psi"] = "Ψ", ["Omega"] = "Ω",

  ["alpha"] = "α", ["beta"] = "β", ["gamma"] = "γ", ["delta"] = "δ", ["epsilon"] = "ε", ["zeta"] = "ζ", ["eta"] = "η", ["theta"] = "θ", ["iota"] = "ι", ["kappa"] = "κ", ["lambda"] = "λ", ["mu"] = "μ", ["nu"] = "ν", ["xi"] = "ξ", ["omicron"] = "ο", ["pi"] = "π", ["rho"] = "ρ", ["final"] = "ς", ["sigma"] = "σ", ["tau"] = "τ", ["upsilon"] = "υ", ["phi"] = "φ", ["chi"] = "χ", ["psi"] = "ψ", ["omega"] = "ω",

  ["nabla"] = "∇",
}

local grid_to_str
local grid_mt = {}
local grid_methods = {}
local new_grid
grid_mt.__index = grid_methods

local sym_array_mt = {}

local MAT_TYPE = {
	INVERTIBLE = 1,
	ORTHOGONAL = 2,
	SPECIAL_ORTHOGONAL = 3,
	SYMMETRIC = 4,
	POSITIVE_DEFINITE = 5,
	POSITIVE_SEMIDEFINITE = 6,
	NEGATIVE_SEMIDEFINITE = 7,
	NEGATIVE_DEFINITE = 8,

}

local mat_mt = {}
local mat_methods = {}
mat_mt.__index = mat_methods

local to_sup

local real_bb = "ℝ"
local latex_symbols = {
	["in"] = "∈"
}

local create_mat_add
local create_mat_mul
local mat_add_mt = {}
local mat_mul_mt = {}

local mat_add_methods = {}
local mat_mul_methods = {}
mat_add_methods.__index = mat_add_mt
mat_mul_methods.__index = mat_mul_mt

local create_add_exp

local create_mul_exp

local create_poly

local poly_ring_methods = {}
local poly_ring_mt = {}

local poly_mt = {}

local is_integer

local create_pow_exp
local pow_exp_mt = {}
local pow_exp_methods = {}
pow_exp_mt.__index = pow_exp_methods

local constant_mt = {}

local superscript = { 
	["+"] = "⁺", ["-"] = "⁻", ["="] = "⁼", ["("] = "⁽", [")"] = "⁾",
	["n"] = "ⁿ",
	["0"] = "⁰", ["1"] = "¹", ["2"] = "²", ["3"] = "³", ["4"] = "⁴", ["5"] = "⁵", ["6"] = "⁶", ["7"] = "⁷", ["8"] = "⁸", ["9"] = "⁹",
	["x"] = "ˣ",
	["i"] = "ⁱ", ["j"] = "ʲ", ["w"] = "ʷ",
  ["T"] = "ᵀ", ["A"] = "ᴬ", ["B"] = "ᴮ", ["D"] = "ᴰ", ["E"] = "ᴱ", ["G"] = "ᴳ", ["H"] = "ᴴ", ["I"] = "ᴵ", ["J"] = "ᴶ", ["K"] = "ᴷ", ["L"] = "ᴸ", ["M"] = "ᴹ", ["N"] = "ᴺ", ["O"] = "ᴼ", ["P"] = "ᴾ", ["R"] = "ᴿ", ["U"] = "ᵁ", ["V"] = "ⱽ", ["W"] = "ᵂ",
}

local imag_mt = {}

local imag_methods = {}

local FUNCTION_TYPE = {
  UNDEFINED = 1,
  LOG = 5,

  EXP = 2,

  SIN = 3,
  COS = 4,

}

local create_fun_exp
local fun_methods = {}
local fun_mt = {}
fun_mt.__index = fun_methods

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

	MAT = 15,

	MAT_ELEM = 16,

	ADD_MAT = 17,
	MUL_MAT = 18,

	POW = 14,

	IMAGINARY_i = 7,
	IMAGINARY_j = 8,
	IMAGINARY_k = 9,

	FUNCTION = 10,

	RATIONAL = 6,

	CONSTANT = 5,

	MUL = 2,

	DIST = 11,

	ADD_DIST = 12,
	MUL_DIST = 13,

}

local add_exp_mt = {}

local mul_exp_mt = {}

local create_constant

local gcd

local add_disp_methods = {}
local mul_disp_methods = {}

local DIST_TYPE = {
  UNDEFINED = 0,
  NORMAL = 1,

}

local create_dist_exp
local dist_methods = {}
local dist_mt = {}
dist_mt.__index = dist_methods

local create_add_disp_exp
local create_mul_disp_exp
local add_disp_mt = {}
local mul_disp_mt = {}

local sub_letters = { 
	["+"] = "₊", ["-"] = "₋", ["="] = "₌", ["("] = "₍", [")"] = "₎",
	["a"] = "ₐ", ["e"] = "ₑ", ["o"] = "ₒ", ["x"] = "ₓ", ["ə"] = "ₔ", ["h"] = "ₕ", ["k"] = "ₖ", ["l"] = "ₗ", ["m"] = "ₘ", ["n"] = "ₙ", ["p"] = "ₚ", ["s"] = "ₛ", ["t"] = "ₜ", ["i"] = "ᵢ", ["j"] = "ⱼ", ["r"] = "ᵣ", ["u"] = "ᵤ", ["v"] = "ᵥ",
	["0"] = "₀", ["1"] = "₁", ["2"] = "₂", ["3"] = "₃", ["4"] = "₄", ["5"] = "₅", ["6"] = "₆", ["7"] = "₇", ["8"] = "₈", ["9"] = "₉",
}

local imag_i = {}
imag_i.type = EXP_TYPE.IMAGINARY_i
setmetatable(imag_i, imag_mt)


local imag_j = {}
imag_j.type = EXP_TYPE.IMAGINARY_j
setmetatable(imag_j, imag_mt)


local imag_k = {}
imag_k.type = EXP_TYPE.IMAGINARY_k
setmetatable(imag_k, imag_mt)


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

local addable_with_sym = {
	[EXP_TYPE.ADD] = true,
	[EXP_TYPE.SCALAR] = true,
	[EXP_TYPE.ARRAY] = true,
	[EXP_TYPE.IMAGINARY_i] = true,
	[EXP_TYPE.IMAGINARY_j] = true,
	[EXP_TYPE.IMAGINARY_k] = true,
	[EXP_TYPE.FUNCTION] = true,
	[EXP_TYPE.RATIONAL] = true,
	[EXP_TYPE.CONSTANT] = true,
	[EXP_TYPE.MUL] = true,
	[EXP_TYPE.POW] = true,

}

local mulable_with_sym = {
	[EXP_TYPE.ADD] = true,
	[EXP_TYPE.SCALAR] = true,
	[EXP_TYPE.ARRAY] = true,
	[EXP_TYPE.IMAGINARY_i] = true,
	[EXP_TYPE.IMAGINARY_j] = true,
	[EXP_TYPE.IMAGINARY_k] = true,
	[EXP_TYPE.FUNCTION] = true,
	[EXP_TYPE.RATIONAL] = true,
	[EXP_TYPE.CONSTANT] = true,
	[EXP_TYPE.MUL] = true,
	[EXP_TYPE.POW] = true,

}

local addable_with_dist = {
  [EXP_TYPE.DIST] = true,
  [EXP_TYPE.SCALAR] = true,
  [EXP_TYPE.CONSTANT] = true,
  [EXP_TYPE.ADD_DIST] = true,
  [EXP_TYPE.MUL_DIST] = true,
}

local mulable_with_dist = {
  [EXP_TYPE.CONSTANT] = true,
  [EXP_TYPE.SCALAR] = true,
  [EXP_TYPE.DIST] = true,
  [EXP_TYPE.ADD_DIST] = true,
  [EXP_TYPE.MUL_DIST] = true,
}

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


function imag_mt:__lt(other)
	assert(is_imag[self.type] and is_imag[other.type])
	return order_lookup[self.type] < order_lookup[self.type]
end

function imag_mt:__eq(other)
	return is_imag[self.type] and is_imag[other.type] and self.type == other.type
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

	if is_imag[self.type] then
	    return self
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
				local child = nil
				if expanded_children[i].type == EXP_TYPE.ADD then
					child = expanded_children[i].children[idx[i]]
				else
					child = expanded_children[i]
				end

				if child.type == EXP_TYPE.MUL then
					for _,grandchild in ipairs(child.children) do
						table.insert(exp.children, grandchild)
					end

				else
					table.insert(exp.children, child)
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
function grid_new(s)
  local lines 
  if type(s) == "string" then
    lines = vim.split(s, "\n")
  elseif type(s) == "table" then
    lines = s
  else
    lines = {}
  end
  local max_length = 0
  for i=1,#lines do
    max_length = math.max(max_length, vim.api.nvim_strwidth(lines[i]))
  end

  for i=1,#lines do
    while vim.api.nvim_strwidth(lines[i]) < max_length do
      lines[i] = lines[i] .. " "
    end
  end

  local g = {}
  g.lines = lines
  if #lines > 0 then
    g.w = vim.api.nvim_strwidth(lines[1])
  else
    g.w = 0
  end
  g.h = #lines
  return setmetatable(g, grid_mt)
end

function grid_mt:__tostring()
  return table.concat(self.lines, "\n")
end

function grid_methods:join_hori(other)
  if self.h < other.h then
    local down = math.floor((other.h - self.h)/2)
    local up = (other.h - self.h) - down
    self:grow_down(down)
    self:grow_up(up)
  elseif other.h < self.h then
    local down = math.floor((self.h - other.h)/2)
    local up = (self.h - other.h) - down
    other:grow_up(up)
    other:grow_down(down)
  end

  for i=1,self.h do
    self.lines[i] = self.lines[i] .. other.lines[i]
  end
  self.w = self.w + other.w

end

function grid_methods:join_vert(other)
  if self.w < other.w then
    self:grow_right(other.w - self.w)
  elseif other.w < self.w then
    other:grow_right(self.w - other.w)
  end

  for i=1,other.h do
    table.insert(self.lines, other.lines[i])
  end
  self.h = self.h + other.h

end

function grid_methods:grow_down(num)
  for i=1,num do
    local empty_line = ""
    for j=1,self.w do
      empty_line = empty_line .. " "
    end

    self.h = self.h + 1
    table.insert(self.lines, empty_line)
  end
end

function grid_methods:grow_up(num)
  for i=1,num do
    local empty_line = ""
    for j=1,self.w do
      empty_line = empty_line .. " "
    end

    self.h = self.h + 1
    table.insert(self.lines, 1, empty_line)
  end
end

function grid_methods:grow_right(num)
  for i=1,self.h do
    for j=1,num do
      self.lines[i] = self.lines[i] .. " "
    end

  end
  self.w = self.w + num
end

function grid_methods:grow_left(num)
  for i=1,self.h do
    for j=1,num do
      self.lines[i] = " " .. self.lines[i]
    end

  end
  self.w = self.w + num
end

function grid_methods:has_minus_prefix()
  if self.w == 0 then
    return false
  end

  for i=1,self.h do
    if self.lines[i]:sub(i,i) == "-" then
      return true
    end
  end
  return false
end

function grid_hori_concat(tbl, sep)
  local res = grid_new()
  for i=1,#tbl do
    if i > 1 and sep then
      res:join_hori(sep)
    end
    res:join_hori(tbl[i])
  end
  return res
end

function grid_vert_concat(tbl, sep)
  local res = grid_new()
  for i=1,#tbl do
    if i > 1 and sep then
      res:join_vert(sep)
    end
    res:join_vert(tbl[i])
  end
  return res
end

function grid_methods:enclose_paren()
  local paren_left = grid_new()
  local paren_right = grid_new()
  if self.h == 1 then
    paren_left = grid_new("(")
    paren_right = grid_new(")")
  elseif self.h > 1 then
    for i=1,self.h do
      if i == 1 then
        paren_left:join_vert(grid_new("⎛"))
        paren_right:join_vert(grid_new("⎞"))
      elseif i == self.h then
        paren_left:join_vert(grid_new("⎝"))
        paren_right:join_vert(grid_new("⎠"))
      else
        paren_left:join_vert(grid_new("⎜"))
        paren_right:join_vert(grid_new("⎟"))
      end
    end
  end

  self:join_hori_left(paren_left)
  self:join_hori(paren_right)
end

function grid_methods:join_hori_left(other)
  if self.h < other.h then
    local down = math.floor((other.h - self.h)/2)
    local up = (other.h - self.h) - down
    self:grow_down(down)
    self:grow_up(up)
  elseif other.h < self.h then
    local down = math.floor((self.h - other.h)/2)
    local up = (self.h - other.h) - down
    other:grow_up(up)
    other:grow_down(down)
  end

  for i=1,self.h do
    self.lines[i] =  other.lines[i] .. self.lines[i]
  end
  self.w = self.w + other.w
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
			if name:sub(1,1) == "\\" then
			  local pat = name:sub(2):match("^%a+")
			  if pat and greek_etc[pat] then
			    name = greek_etc[pat] .. name:sub(2 + #pat)
			  end
			end
			local pat_start, pat_end = name:find("_{.+}")
			if pat_start and pat_end then
				local pat = name:sub(pat_start+2,pat_end-1)
				local good = true
				for i=1,#pat do
					if not sub_letters[pat:sub(i,i)] then
						good = false
						break
					end
				end

				if good then
					local pat_sub = ""
					for i=1,#pat do
						pat_sub = pat_sub .. sub_letters[pat:sub(i,i)]
					end
					name = name:sub(1,pat_start-1) .. pat_sub ..  name:sub(pat_end+1)
				end
			end

			pat_start, pat_end = name:find("_.+")
			if pat_start and pat_end then
				local pat = name:sub(pat_start+1,pat_end)
				local good = true
				for i=1,#pat do
					if not sub_letters[pat:sub(i,i)] then
						good = false
						break
					end
				end

				if good then
					local pat_sub = ""
					for i=1,#pat do
						pat_sub = pat_sub .. sub_letters[pat:sub(i,i)]
					end
					name = name:sub(1,pat_start-1) .. pat_sub ..  name:sub(pat_end+1)
				end
			end

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

function M.log(x)
  local fn_exp = create_fun_exp()
  fn_exp.fun_type = FUNCTION_TYPE.LOG
  table.insert(fn_exp.args, x)
  return fn_exp
end

function M.mat_sym(name, dims,flags)
	local m,n
	if type(dims) == "number" then
		m = dims
		n = dims
	elseif #dims == 2 then
		m = dims[1]
		n = dims[2]
	elseif #dims == 1 then
		m = dims[1]
		n = dims[1]
	else
		assert(false)
	end

	local exp = {}
	exp.type = EXP_TYPE.MAT
	exp.name = name
	exp.mat_types = {}
	if flags then
		if type(flags) ~= "table" then
			flags = { flags }
		end
		for _, flag in ipairs(flags) do
			if flag == MAT_TYPE.ORTHOGONAL or flag == MAT_TYPE.SPECIAL_ORTHOGONAL or flags == MAT_TYPE.POSITIVE_DEFINITE or flags == MAT_TYPE.NEGATIVE_DEFINITE then
				exp.mat_types[MAT_TYPE.INVERTIBLE] = true
			end

			if flag == MAT_TYPE.SPECIAL_ORTHOGONAL then
				exp.mat_types[MAT_TYPE.ORTHOGONAL] = true
			end

			exp.mat_types[flag] = true
		end
	end

	exp.m = m
	exp.n = n
	exp.elems = {}
	for i=1,m do
		exp.elems[i] = {}
		for j=1,n do
			local mat_elem = {}
			mat_elem.type = EXP_TYPE.MAT_ELEM
			mat_elem.i = i
			mat_elem.j = j
			mat_elem.mat = exp


			exp.elems[i][j] = mat_elem
		end
	end

	setmetatable(exp, mat_mt)

	return exp
end

function mat_mt:__tostring()
	local result = grid_new()
	local get_dim = function()
		if self.m == self.n then
			return "(" .. tostring(self.m) .. ")"
		else
			return "(" .. tostring(self.m) .. "," .. tostring(self.n) .. ")"
		end
	end

	local matrix_set = ""
	if vim.tbl_count(self.mat_types) == 0 then
		matrix_set = ""
	end

	if vim.tbl_count(self.mat_types) == 1 and self.mat_types[MAT_TYPE.INVERTIBLE] then
		matrix_set = "GL" .. get_dim()
	end

	if self.mat_types[MAT_TYPE.ORTHOGONAL] then
		if self.mat_types[MAT_TYPE.SPECIAL_ORTHOGONAL] then
			matrix_set = "SO" .. get_dim()
		else
			matrix_set = "O" .. get_dim()
		end
	end

	if self.elems[1] and self.elems[1][1] and self.elems[1][1].value then
		local left_grid = {}
		local right_grid = {}
		local block = grid_new()
		for j=1,self.n do
			local col = grid_new()
			for i=1,self.m do
				local cell = grid_new(tostring(self.elems[i][j].value))
				if not cell:has_minus_prefix() then
					cell:grow_left(1)
				end
				col:join_vert(cell)
			end
			if block.h > 0 then
				block:join_hori(grid_new(" "))
			end
			block:join_hori(col)
		end

		for i=1,self.m do
			local border_right, border_left
			if self.m == 1 then
				border_left = "⟮"
				border_right = "⟯"
			elseif i == 1 then
				border_left = "⎡"
				border_right = "⎤"
			elseif i == self.m then
				border_left = "⎣"
				border_right = "⎦"
			else
				border_left = "⎢"
				border_right = "⎥"
			end

			table.insert(left_grid, border_left)
			table.insert(right_grid, border_right)
		end

		left_grid = grid_new(left_grid)
		right_grid = grid_new(right_grid)
		left_grid:join_hori(block)
		left_grid:join_hori(right_grid)
		block = left_grid

		result:join_hori(block)


	else
		result:join_hori(grid_new(self.name))
	end

	if #matrix_set ~= 0 then
		result:join_hori(grid_new(" " .. latex_symbols["in"] .. " " .. matrix_set))
	end

	return tostring(result)
end

function to_sup(s)
	local r = ""
	for i=1,#s do
		r = r .. superscript[s:sub(i,i)]
	end
	return r
end

function M.mat_sym_o(...)
	local args = { ... }
	table.insert(args, MAT_TYPE.ORTHOGONAL)
	return M.mat_sym(unpack(args))
end

function M.mat_sym_so(...)
	local args = { ... }
	table.insert(args, MAT_TYPE.SPECIAL_ORTHOGONAL)
	return M.mat_sym(unpack(args))
end

function M.mat_sym_gl(...)
	local args = { ... }
	table.insert(args, MAT_TYPE.INVERTIBLE)
	return M.mat_sym(unpack(args))
end

function M.mat(arr, flags)
	local exp = {}
	exp.type = EXP_TYPE.MAT
	exp.mat_types = {}
	if flags then
		if type(flags) ~= "table" then
			flags = { flags }
		end
		for _, flag in ipairs(flags) do
			if flag == MAT_TYPE.ORTHOGONAL or flag == MAT_TYPE.SPECIAL_ORTHOGONAL or flags == MAT_TYPE.POSITIVE_DEFINITE or flags == MAT_TYPE.NEGATIVE_DEFINITE then
				exp.mat_types[MAT_TYPE.INVERTIBLE] = true
			end

			if flag == MAT_TYPE.SPECIAL_ORTHOGONAL then
				exp.mat_types[MAT_TYPE.ORTHOGONAL] = true
			end

			exp.mat_types[flag] = true
		end
	end

	local m = #arr
	local n = #arr[1]
	exp.m = m
	exp.n = n
	exp.elems = {}
	for i=1,m do
		exp.elems[i] = {}
		for j=1,n do
			local mat_elem = {}
			mat_elem.type = EXP_TYPE.MAT_ELEM
			mat_elem.i = i
			mat_elem.j = j
			mat_elem.mat = exp


			mat_elem.value = arr[i][j]

			exp.elems[i][j] = mat_elem
		end
	end

	setmetatable(exp, mat_mt)

	return exp
end

function M.mat_o(...)
	local args = { ... }
	table.insert(args, MAT_TYPE.ORTHOGONAL)
	return M.mat(unpack(args))
end

function M.mat_so(...)
	local args = { ... }
	table.insert(args, MAT_TYPE.SPECIAL_ORTHOGONAL)
	return M.mat(unpack(args))
end

function M.mat_gl(...)
	local args = { ... }
	table.insert(args, MAT_TYPE.INVERTIBLE)
	return M.mat(unpack(args))
end

function create_mat_add()
  local exp = {}
  exp.type = EXP_TYPE.ADD_MAT
  exp.children = {}
  return setmetatable(exp, mat_add_mt)
end

function create_mat_mul()
  local exp = {}
  exp.type = EXP_TYPE.MUL_MAT
  exp.children = {}
  return setmetatable(exp, mat_mul_mt)
end

function mat_mt:__add(other)
  assert(self.m == other.m)
  assert(self.n == other.n)
  local add_children = {}
  for _, elem in ipairs({self, other}) do
    if elem.type == EXP_TYPE.ADD_MAT then
      for _, child in ipairs(elem.children) do
        table.insert(add_children, child)
      end
    else
      table.insert(add_children, elem)
    end
  end

  local exp = create_mat_add()
  exp.children = add_children
  exp.m = self.m
  exp.n = self.n
  return exp
end

function mat_add_mt:__tostring()
  local tbl = {}
  for i=1,#self.children do
    table.insert(tbl, grid_new(tostring(self.children[i])))
  end
  local result = grid_hori_concat(tbl, grid_new(" + "))
  return tostring(result)
end

function mat_mul_mt:__tostring()
  local tbl = {}
  for i=1,#self.children do
    local g = grid_new(tostring(self.children[i]))
    if self.children[i].type == EXP_TYPE.ADD_MAT then
      g:enclose_paren()
    end
    table.insert(tbl, g)
  end
  local result = grid_hori_concat(tbl)
  return tostring(result)
end

function mat_mt:__mul(other)
  assert(self.n == other.m)
  local mul_children = {}
  for _, elem in ipairs({self, other}) do
    if elem.type == EXP_TYPE.MUL_MAT then
      for _, child in ipairs(elem.children) do
        table.insert(mul_children, child)
      end
    else
      table.insert(mul_children, elem)
    end
  end

  local exp = create_mat_mul()
  exp.children = mul_children
  exp.m = self.m
  exp.n = other.n
  return exp
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
	if type(sup) == "number" and is_integer(sup) then
		if sup >= 1 then
			local exp = {}
			exp.type = EXP_TYPE.MUL
			exp.children = {}
			exp = setmetatable(exp, mul_exp_mt)


			for i=1,sup do
				table.insert(exp.children, self)
			end
			return exp

		else
			local exp = create_pow_exp()
			exp.base = self
			exp.sup = M.constant(sup)
			return exp

		end
	else
			local exp = create_pow_exp()
			exp.base = self
			exp.sup = sup
			return exp

	end
	assert(false)
end

function is_integer(x)
	return math.floor(x) == x
end

function constant_mt:__pow(sup)
	return create_constant(self.value ^ sup)
end

function create_pow_exp()
	local exp = {}
	exp.type = EXP_TYPE.POW
	setmetatable(exp, pow_exp_mt)
	return exp
end

function pow_exp_mt:__tostring()
	local base_str = tostring(self.base)
	if self.base.type == EXP_TYPE.ADD or self.base.type == EXP_TYPE.MUL then
		base_str = "(" .. base_str .. ")"
	end

	local sup_str = tostring(self.sup)
	if #sup_str >= 2 and sup_str:sub(1,1) == "(" and sup_str:sub(#sup_str,#sup_str) == ")" then
		sup_str = sup_str:sub(2,#sup_str-1)
	end

	local new_sup_str = ""
	local ok = true
	for i=1,#sup_str do
		if not superscript[sup_str:sub(i,i)] then
			ok = false
			break
		end
		new_sup_str = new_sup_str .. superscript[sup_str:sub(i,i)]
	end

	if ok then
		sup_str = new_sup_str
	else
		sup_str = "^(" .. sup_str .. ")"
	end

	return base_str .. sup_str
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
  if self.type == imag_i.type then
    return "i"
  elseif self.type == imag_j.type then
    return "j"
  elseif self.type == imag_k.type then
    return "k"
  end
end

function imag_methods:normal_form()
	return self
end

function create_fun_exp(fun_type)
  local exp = {}
  exp.type = EXP_TYPE.FUNCTION
  exp.args = {}
  exp.fun_type = fun_type or FUNCTION_TYPE.UNDEFINED
  setmetatable(exp, fun_mt)
  return exp
end

function fun_mt:__tostring()
  local fun_name = "unknown"
  local args_str = {}
  for i=1,#self.args do
    table.insert(args_str, tostring(self.args[i]))
  end
  args_str = table.concat(args_str, ",")

  if self.fun_type == FUNCTION_TYPE.UNDEFINED then
    fun_name = "undefined"
  elseif self.fun_type == FUNCTION_TYPE.LOG then
    fun_name = "log"
  elseif self.fun_type == FUNCTION_TYPE.EXP then
    fun_name = "exp"

  elseif self.fun_type == FUNCTION_TYPE.SIN then
    fun_name = "sin"
  elseif self.fun_type == FUNCTION_TYPE.COS then
    fun_name = "cos"

  end

  return fun_name .. "(" .. args_str .. ")"
end

function M.exp(x)
  local fn_exp = create_fun_exp()
  fn_exp.fun_type = FUNCTION_TYPE.EXP
  table.insert(fn_exp.args, x)
  return fn_exp
end

function exp_methods:as_real_mat()
  local quat_coeffs = self:collect_quat()

  local a,b,c,d = unpack(quat_coeffs)
  -- One of 48 possibilities to represent a quaternion as a 4x4 matrix
  -- https://en.wikipedia.org/wiki/Quaternion#Representation_as_real_4_%C3%97_4_matrices
  local mat = M.mat {
    { a, -b, -c, -d },
    { b, a, -d, c },
    { c, d, a, -b },
    { d, -c, b, a },
  }
  return mat
end

function exp_methods:collect_quat()
  local r = self:expand():simplify()

  local quat_coeffs = {}
  local add_exp
  if r.type ~= EXP_TYPE.ADD then
    add_exp = create_add_exp()
    add_exp.children = { r }
  else
    add_exp = r
  end

  for _, term in ipairs(add_exp.children) do
    local mul_term
    if term.type == EXP_TYPE.MUL then
      mul_term = term
    else
      mul_term = create_mul_exp()
      mul_term.children = { term }
    end

    local idx = 1
    local rest = {}
    for _, coeff in ipairs(mul_term.children) do
      assert(coeff.type ~= EXP_TYPE.ADD and coeff.type ~= EXP_TYPE.MUL)
      if is_imag[coeff.type] then
        assert(idx == 1)
        if coeff.type == EXP_TYPE.IMAGINARY_i then
          idx = 2
        elseif coeff.type == EXP_TYPE.IMAGINARY_j then
          idx = 3
        elseif coeff.type == EXP_TYPE.IMAGINARY_k then
          idx = 4
        end
      else
        table.insert(rest, coeff)
      end
    end

    assert(not quat_coeffs[idx])
    if #rest == 0 then
      quat_coeffs[idx] = M.constant(1)
    elseif #rest == 1 then
      quat_coeffs[idx] = rest[1]
    else
      local mul_exp = create_mul_exp()
      mul_exp.children = rest
      quat_coeffs[idx] = mul_exp
    end

  end
  for idx=1,4 do
    if not quat_coeffs[idx] then
      quat_coeffs[idx] = M.constant(0)
    end
  end

  return quat_coeffs
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
		assert(addable_with_sym[self.type])
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
		assert(addable_with_sym[other.type])
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
		assert(mulable_with_sym[self.type])
		table.insert(exp.children, self)
	end

	if other.type and other.type == EXP_TYPE.MUL then
		for _, child in ipairs(other.children) do
			table.insert(exp.children, child)
		end
	else
		assert(mulable_with_sym[other.type])
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
		assert(addable_with_sym[self.type])
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
	if other.type == EXP_TYPE.CONSTANT then
		return self * (M.constant(1) / other)
	else
		return self * (other^(-1))
	end
end

function sym_mt:__div(other)
	if other.type == EXP_TYPE.CONSTANT then
		return self * (M.constant(1) / other)
	else
		return self * (other^(-1))
	end
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

	elseif self.type == EXP_TYPE.POW then
		local base = self.base:simplify()
		local sup = self.sup:simplify()


		if sup.type == EXP_TYPE.CONSTANT and sup.value == 0 then
			return M.constant(1)
		elseif sup.type == EXP_TYPE.CONSTANT and sup.value == 1 then
			return base
		else
			if base.type == EXP_TYPE.MUL then
				base = base:normal_form()
				local children = base.children
				local power = {}
				local factors = {}
				local last_child

				for i=1,#children do
					local child = children[i] 
					local child_sup = M.constant(1)
					if child.type == EXP_TYPE.POW then
						child = child.base
						child_sup = child.sup
					end

					if last_child == child then
						power[#power] = power[#power] + child_sup
					else
						table.insert(power, M.constant(1))
						table.insert(factors, child)
					end
					last_child = factors[#factors]
				end

				local exp = create_mul_exp()
				for i=1,#factors do
					local new_sup = (power[i] * sup):simplify()
					if new_sup.type == EXP_TYPE.CONSTANT and new_sup.value == 1 then
						table.insert(exp.children, factors[i])
					else
						local factor_exp = create_pow_exp()
						factor_exp.base = factors[i]
						factor_exp.sup = new_sup
						table.insert(exp.children, factor_exp)
					end
				end
				return exp


			elseif base.type == EXP_TYPE.POW then
				local new_sup = (base.sub * sup):simplify()
				if new_sup.type == EXP_TYPE.CONSTANT and new_sup.value == 1 then
					return base.base
				else
					local exp = create_pow_exp()
					exp.base = base.base
					exp.sup = new_sup
					return exp
				end
			end

			local exp = create_pow_exp()
			exp.base = base
			exp.sup = sup
			return exp
		end

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

				local candidate_coeff, candidate_term = split_term(candidate)

				local found = false
				if candidate_term then
					for j=1,#terms do
						local equal = false
						local term = terms[j]
						equal = term == candidate_term

						if equal then
							coeffs[j] = coeffs[j] + (candidate_coeff or create_constant(1))
							found = true
							break
						end

					end
				end

				if not found then
					table.insert(terms, candidate_term or create_constant(1))
					table.insert(coeffs, candidate_coeff or create_constant(1))
				end

			end

			local add_exp = exp
			for i=1,#terms do
				coeffs[i] = coeffs[i]:simplify()
				if coeffs[i] == create_constant(0) then
				elseif coeffs[i] ~= create_constant(1) then
					local exp
					if terms[i].type == EXP_TYPE.MUL then
						exp = terms[i]
						table.insert(exp.children, 1, coeffs[i])

					else
						exp = create_mul_exp()
						exp.children = { coeffs[i], terms[i] }
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

M.create_constant = create_constant

function split_term(exp)
	if exp.type == EXP_TYPE.MUL then
		local left = {}
		local right = {}
		local i = 1
		while i <= #exp.children do
			local child = exp.children[i]
			if child.type == EXP_TYPE.CONSTANT or child.type == EXP_TYPE.RATIONAL then
				table.insert(left, child)
				i = i + 1
			else
				break
			end
		end

		while i <= #exp.children do
			table.insert(right, exp.children[i])
			i = i + 1
		end

		if #left == 0 then
			return nil, exp
		elseif #right == 0 then
			return exp, nil
		else
			return create_mul_or_single(left), create_mul_or_single(right)
		end
	else
		return nil, exp
	end
end

function create_mul_or_single(children)
	if #children == 1 then
		return children[1]
	else
		local exp = create_mul_exp()
		exp.children = children
		return exp
	end
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

function M.sqrt(exp)
  return exp ^ (M.constant(1)/M.constant(2))
end
function dist_mt:__eq(other)
  if self.type ~= other.type then
    return false
  end

  if self.type == EXP_TYPE.DIST then
    if self.dist_type == DIST_TYPE.NORMAL then
      return self.mu == other.mu and self.var == other.var
    end
  elseif self.type == EXP_TYPE.ADD_DIST or self.type == EXP_TYPE.MUL_DIST then
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
  return false
end

function dist_methods:simplify()
  if self.type == EXP_TYPE.ADD_DIST then
    local new_children = {}
    for _, child in pairs(self.children) do
      table.insert(new_children, child:simplify())
    end

    local normal_add = nil
    local constants_add = nil

    local new_children_simplified = {}
    for _, child in pairs(new_children) do
      if child.type == EXP_TYPE.CONSTANT or child.type == EXP_TYPE.SCALAR then
        if not constants_add then
          constants_add = child
        else
          constants_add = (constants_add + child):simplify()
        end
      elseif child.type == EXP_TYPE.DIST and child.dist_type == DIST_TYPE.NORMAL then
        if not normal_add then
          normal_add = child
        else
          normal_add.mu = (normal_add.mu + child.mu):simplify()
          normal_add.var = (normal_add.var + child.var):simplify()
        end
      else
        table.insert(new_children_simplified, child)
      end
    end

    if normal_add and constants_add then
      normal_add.mu = (normal_add.mu + constants_add):simplify()
      table.insert(new_children_simplified, normal_add)
    elseif normal_add then
      table.insert(new_children_simplified, normal_add)
    elseif constants_add then
      table.insert(new_children_simplified, constants_add)
    end

    new_children = new_children_simplified

    assert(#new_children > 0)
    if #new_children == 1 then
      return new_children[1]
    else
      local exp = create_add_disp_exp()
      exp.children = new_children
      return exp
    end

    return result
  elseif self.type == EXP_TYPE.MUL_DIST then
    local new_children = {}
    for _, child in pairs(self.children) do
      table.insert(new_children, child:simplify())
    end

    local normal_mul = nil
    local constants_mul = nil

    local new_children_simplified = {}
    for _, child in pairs(new_children) do
      if child.type == EXP_TYPE.CONSTANT or child.type == EXP_TYPE.SCALAR then
        if not constants_mul then
          constants_mul = child
        else
          constants_mul = (constants_mul * child):simplify()
        end
      elseif child.type == EXP_TYPE.DIST and child.dist_type == DIST_TYPE.NORMAL then
        if not normal_mul then
          normal_mul = child
        else
          table.insert(new_children_simplified, child)
        end
      else
        table.insert(new_children_simplified, child)
      end
    end


    if normal_mul and constants_mul then
      normal_mul.mu = (constants_mul * normal_mul.mu):simplify()
      normal_mul.var = (constants_mul^2 * normal_mul.var):simplify()
      table.insert(new_children_simplified, normal_mul)
    elseif normal_mul then
      table.insert(new_children_simplified, normal_mul)
    elseif constants_mul then
      table.insert(new_children_simplified, constants_mul)
    end

    new_children = new_children_simplified

    assert(#new_children > 0)
    if #new_children == 1 then
      return new_children[1]
    else
      local exp = create_mul_disp_exp()
      exp.children = new_children
      return exp
    end
    return result
  else
    return self
  end
end

function dist_methods:E()
  if self.type == EXP_TYPE.DIST and self.dist_type == DIST_TYPE.NORMAL then
    return self.mu
  elseif self.type == EXP_TYPE.ADD_DIST then
    local sum_e = M.constant(0)
    for _, child in ipairs(self.children) do
      sum_e = sum_e +  child:E()
    end

    return sum_e:simplify()

  elseif self.type == EXP_TYPE.MUL_DIST then
    if #self.children == 2 and self.children[1] == self.children[2] then
      local child = self.children[1]
      if child.type == EXP_TYPE.DIST and child.dist_type == DIST_TYPE.NORMAL then
        return child.mu ^2 + child.var
      end
    end
    local new_children = self.children
    local normal_mul = nil
    local constants_mul = nil

    local new_children_simplified = {}
    for _, child in pairs(new_children) do
      if child.type == EXP_TYPE.CONSTANT or child.type == EXP_TYPE.SCALAR then
        if not constants_mul then
          constants_mul = child
        else
          constants_mul = (constants_mul * child):simplify()
        end
      elseif child.type == EXP_TYPE.DIST and child.dist_type == DIST_TYPE.NORMAL then
        if not normal_mul then
          normal_mul = child
        else
          table.insert(new_children_simplified, child)
        end
      else
        table.insert(new_children_simplified, child)
      end
    end

    if #new_children_simplified == 0 then
      if constants_mul and normal_mul then
        return constants_mul * normal_mul.mu
      elseif constants_mul then
        return constants_mul
      else
        return normal_mul.mu
      end
    end

  end
  assert(false)
end

function dist_mt:__pow(sup)
	assert(type(sup) == "number", "exponent must be a constant number")
	assert(is_integer(sup), "exponent must be a constant integer number")

  local exp = create_mul_disp_exp()
  for i=1,sup do
    table.insert(exp.children, self)
  end
  return exp
end

function constant_methods:E()
  return self.value
end
function dist_methods:kl_div(other)
  if self.dist_type == DIST_TYPE.NORMAL and other.type == EXP_TYPE.DIST and other.dist_type == DIST_TYPE.NORMAL then
    local e1 = M.log((M.sqrt(other.var)/M.sqrt(self.var)):simplify())
    local e2 = (self.var + (self.mu - other.mu)^2)/(2*other.var)
    local e3 = M.constant(1)/M.constant(2)
    return e1 + e2 - e3
  end
end

function create_dist_exp(dist_type)
  local exp = {}
  exp.type = EXP_TYPE.DIST
  exp.args = {}
  exp.dist_type = dist_type or DIST_TYPE.UNDEFINED
  setmetatable(exp, dist_mt)
  return exp
end

function dist_mt:__tostring()
  if self.dist_type == DIST_TYPE.UNDEFINED then
    return "undefined"
  elseif self.dist_type == DIST_TYPE.NORMAL then
    return "N(" .. tostring(self.mu) .. "," .. tostring(self.var) .. ")"

  end
  return "undefined"
end

function M.normal(mu, sigma)
  local exp = create_dist_exp(DIST_TYPE.NORMAL)
  exp.mu = mu
  exp.var = sigma^2
  return exp
end

function create_add_disp_exp()
  local exp = {}
  exp.type = EXP_TYPE.ADD_DIST
  exp.children = {}
  setmetatable(exp, add_disp_mt)
  return exp
end

function create_mul_disp_exp()
  local exp = {}
  exp.type = EXP_TYPE.MUL_DIST
  exp.children = {}
  setmetatable(exp, mul_disp_mt)
  return exp
end

function dist_mt:__add(other)
  local exp = create_add_disp_exp()
  exp.children = {}

  for _, term in ipairs({self, other}) do
    if type(term) == "number" then
      term = create_constant(term)
    end
    assert(addable_with_dist[term.type])
    if term.type == EXP_TYPE.ADD_DIST then
      for _, child in ipairs(term.children) do
        table.insert(exp.children, child)
      end
    else
      table.insert(exp.children, term)
    end
  end

  return exp
end

function dist_mt:__mul(other)
  local exp = create_mul_disp_exp()
  exp.children = {}

  for _, fac in ipairs({self, other}) do
    if type(fac) == "number" then
      fac = create_constant(fac)
    end
    assert(mulable_with_dist[fac.type])
    if fac.type == EXP_TYPE.MUL_DIST then
      for _, child in ipairs(fac.children) do
        table.insert(exp.children, child)
      end
    else
      table.insert(exp.children, fac)
    end
  end

  return exp
end

function mul_disp_mt:__tostring()
	local children_str = {}
	for i=1,#self.children do
		local child_str = tostring(self.children[i])
		if self.children[i].type == EXP_TYPE.ADD_DIST then
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

function dist_methods:Var()
  return ((self^2):E() - (self:E())^2):simplify()
end
function M.sin(x)
  local fn_exp = create_fun_exp(FUNCTION_TYPE.SIN)
  table.insert(fn_exp.args, x)
  return fn_exp
end

function M.cos(x)
  local fn_exp = create_fun_exp(FUNCTION_TYPE.COS)
  table.insert(fn_exp.args, x)
  return fn_exp
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

mat_add_mt.__add = mat_mt.__add
mat_mul_mt.__add = mat_mt.__add

mat_add_mt.__mul = mat_mt.__mul
mat_mul_mt.__mul = mat_mt.__mul
poly_ring_mt.__index = poly_ring_methods

mul_exp_mt.__pow = sym_mt.__pow
add_exp_mt.__pow = sym_mt.__pow
pow_exp_mt.__pow = sym_mt.__pow

pow_exp_mt.__unm = sym_mt.__unm
pow_exp_mt.__add = sym_mt.__add
pow_exp_mt.__sub = sym_mt.__sub
pow_exp_mt.__mul = sym_mt.__mul
pow_exp_mt.__div = sym_mt.__div

pow_exp_methods.simplify = exp_methods.simplify

imag_mt.__pow = sym_mt.__pow
imag_mt.__add = sym_mt.__add
imag_mt.__sub = sym_mt.__sub
imag_mt.__mul = sym_mt.__mul
imag_mt.__div = sym_mt.__div

imag_mt.__index = imag_methods

imag_methods.simplify = exp_methods.simplify
imag_methods.expand = exp_methods.expand

imag_methods.as_real_mat = exp_methods.as_real_mat
sym_methods.as_real_mat = exp_methods.as_real_mat

imag_methods.collect_quat = exp_methods.collect_quat
sym_methods.collect_quat = exp_methods.collect_quat

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

add_exp_mt.__div = mul_exp_mt.__div

sym_methods.simplify = exp_methods.simplify
constant_methods.simplify = exp_methods.simplify

add_disp_mt.__eq = dist_mt.__eq
mul_disp_mt.__eq = dist_mt.__eq
add_disp_methods.simplify = dist_methods.simplify
mul_disp_methods.simplify = dist_methods.simplify
add_disp_mt.__index = add_disp_methods
mul_disp_mt.__index = mul_disp_methods

add_disp_methods.E = dist_methods.E
mul_disp_methods.E = dist_methods.E

add_disp_mt.__add = dist_mt.__add
mul_disp_mt.__add = dist_mt.__add

add_disp_mt.__mul = dist_mt.__mul
mul_disp_mt.__mul = dist_mt.__mul

add_disp_mt.__tostring = add_exp_mt.__tostring

return M

