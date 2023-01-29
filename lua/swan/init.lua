-- Generated using ntangle.nvim
local M = {}

local convert_constant

local gcd

local sym_table = {}

local Exp = {}

local kind_order = {
  ["constant"] = 1,
  ["inf"] = 1.5,
  ["named_constant"] = 2,
  ["i"] = 3,
  ["sym"] = 4,
  ["add"] = 5,
  ["sub"] = 5,
  ["mul"] = 6,
  ["div"] = 7,
  ["pow"] = 8,
  ["cos"] = 9,
  ["sin"] = 10,
  ["matrix"] = 11,

}

local mt = { __index = Exp,
  __tostring = function(self)
    if self.kind == "sym" then
      return self.o.name

    elseif self.kind == "constant_div" then
      local lhs = tostring(self.o.lhs)
      local rhs = tostring(self.o.rhs)
      return lhs .. "/" .. rhs

    elseif self.kind == "ln" then
      return ("ln(%s)"):format(tostring(self.o.arg))

    elseif self.kind == "add" then
      local lhs = tostring(self.o.lhs)
      local rhs = tostring(self.o.rhs)

      return lhs .. " + " .. rhs

    elseif self.kind == "sub" then
      return ("%s - %s"):format(tostring(self.o.lhs), tostring(self.o.rhs))
    elseif self.kind == "mul" then
      local lhs = nil
      local rhs = nil
      if self.o.lhs.kind == "add" or self.o.lhs.kind == "div" then
        lhs = "(" .. tostring(self.o.lhs) .. ")"
      else
        lhs = tostring(self.o.lhs)
      end

      if self.o.rhs.kind == "add"  or self.o.rhs.kind == "div" then
        rhs = "(" .. tostring(self.o.rhs) .. ")"
      else
        rhs = tostring(self.o.rhs)
      end

      return lhs .. rhs


    elseif self.kind == "pow" then
      local lhs_pow = tostring(self.o.lhs)
      local rhs_pow = tostring(self.o.rhs)

      if not self.o.lhs:is_atomic() then
        lhs_pow = ("(%s)"):format(lhs_pow)
      end

      if not self.o.rhs:is_atomic() then
        rhs_pow = ("(%s)"):format(rhs_pow)
      end
      return ("%s^%s"):format(lhs_pow, rhs_pow)
    elseif self.kind == "constant" then
      if self.o.constant < 0 then
        return "(" .. tostring(self.o.constant) .. ")"
      else
        return tostring(self.o.constant)
      end

    elseif self.kind == "cos" then
      return ("cos(%s)"):format(tostring(self.o.arg))
    elseif self.kind == "sin" then
      return ("sin(%s)"):format(tostring(self.o.arg))

    elseif self.kind == "inf" then
      return "inf"

    elseif self.kind == "named_constant" then
      return self.o.name

    elseif self.kind == "i" then
      return "i"

    elseif self.kind == "matrix" then
      local rows = {}
      for i=1,#self.o.rows do
        local row = {}
        for j=1,#self.o.rows[i] do
          table.insert(row, tostring(self.o.rows[i][j]))
        end
        table.insert(rows, "  " .. table.concat(row, ", "))
      end
      return "[\n" .. table.concat(rows, "\n") .. "\n]"

    elseif self.kind == "div" then
      local lhs = tostring(self.o.lhs)
      local rhs = tostring(self.o.rhs)

      if not self.o.lhs:is_atomic() then
        lhs = "(" .. lhs .. ")"
      end

      if not self.o.rhs:is_atomic() then
        rhs = "(" .. rhs .. ")"
      end

      return lhs .. "/" .. rhs

    else
      return "[UNKNOWN]"
    end
  end,

  __add = function(lhs, rhs)
    lhs = convert_constant(lhs)
    rhs = convert_constant(rhs)

    return Exp.new("add", { lhs = lhs, rhs = rhs })
  end,

  __sub = function(lhs, rhs)
    lhs = convert_constant(lhs)
    rhs = convert_constant(rhs)

    rhs = Exp.new("mul", { lhs = M.constant(-1), rhs = rhs })
    return Exp.new("add", { lhs = lhs, rhs = rhs })
  end,

  __pow = function(lhs, rhs)
    lhs = convert_constant(lhs)
    rhs = convert_constant(rhs)

    return Exp.new("pow", { lhs = lhs, rhs = rhs })
  end,

  __mul = function(lhs, rhs)
    lhs = convert_constant(lhs)
    rhs = convert_constant(rhs)

    return Exp.new("mul", { lhs = lhs, rhs = rhs })
  end,


  __unm = function(lhs)
    return Exp.new("mul", { 
      lhs = M.constant(-1),
      rhs = lhs
    })
  end,

  __div = function(lhs, rhs)
    lhs = convert_constant(lhs)
    rhs = convert_constant(rhs)

  	if lhs:is_constant() and rhs:is_constant() then
  	  return Exp.new("constant_div", { lhs = lhs, rhs = rhs })
  	end


    return Exp.new("div", { lhs = lhs, rhs = rhs })
  end,

  __lt = function(lhs, rhs)
    if lhs.kind ~= rhs.kind then
      return kind_order[lhs.kind] < kind_order[rhs.kind]
    else
      if lhs.kind == "constant" then
        return lhs.o.constant < rhs.o.constant

      elseif lhs.kind == "sym" then
        return lhs.o.name < rhs.o.name
      elseif lhs.kind == "named_constant" then
        return lhs.o.name < rhs.o.name
      elseif lhs.kind == "pow" then
        return lhs.o.lhs < rhs.o.lhs

      end
    end
    return tostring(lhs) < tostring(rhs)
  end,

  __eq = function(lhs, rhs) 
    if lhs.kind == rhs.kind then
      if lhs.kind == "constant" then
        return lhs.o.constant == rhs.o.constant

      elseif lhs.kind == "named_constant" then
        return lhs.o.name == rhs.o.name
      elseif lhs.kind == "sym" then
        return lhs.o.name == rhs.o.name
      elseif lhs.kind == "i" then
        return true
      elseif lhs.kind == "pow" then
        return lhs.o.lhs == rhs.o.lhs and lhs.o.rhs == rhs.o.rhs
      elseif lhs.kind == "mul" then
        return lhs.o.lhs == rhs.o.lhs and lhs.o.rhs == rhs.o.rhs
      elseif lhs.kind == "add" then
        return lhs.o.lhs == rhs.o.lhs and lhs.o.rhs == rhs.o.rhs

      end
    else
      return false
    end
  end,

}

function Exp:is_constant_div()
  return self.kind == "constant_div"
end

function M.add_constant_div(lhs, rhs)
	local lhs_den = lhs.o.rhs.o.constant
	local rhs_den = rhs.o.rhs.o.constant

	local common_den = M.constant(lhs_den * rhs_den)

	local lhs_num = lhs.o.lhs.o.constant * rhs_den
	local rhs_num = rhs.o.lhs.o.constant * lhs_den

	local common_num = M.constant(lhs_num + rhs_num)

	if common_den:is_integer() and common_num:is_integer() then
		local gcd = M.find_gcd(math.abs(common_den.o.constant), math.abs(common_num.o.constant))
		common_den.o.constant = common_den.o.constant / gcd
		common_num.o.constant = common_num.o.constant / gcd
	end

	if common_den.o.constant == 1 then
		return common_num
	end

	return common_num / common_den

end

function M.find_gcd(A, B)
	if A > B then
		return M.find_gcd(A - B, B)
	elseif B > A then
		return M.find_gcd(A, B - A)
	else
		return A
	end
end

function M.mul_constant_div(lhs, rhs)
	local lhs_den = lhs.o.rhs.o.constant
	local rhs_den = rhs.o.rhs.o.constant

	local common_den = M.constant(lhs_den * rhs_den)

	local lhs_num = lhs.o.lhs.o.constant
	local rhs_num = rhs.o.lhs.o.constant

	local common_num = M.constant(lhs_num * rhs_num)

	if common_den:is_integer() and common_num:is_integer() then
		local gcd = M.find_gcd(math.abs(common_den.o.constant), math.abs(common_num.o.constant))
		common_den.o.constant = common_den.o.constant / gcd
		common_num.o.constant = common_num.o.constant / gcd
	end

	if common_den.o.constant == 1 then
		return common_num
	end

	return common_num / common_den

end

function Exp:derivate(dx)
  if self.kind == "constant" then
    return Exp.new("constant", { constant = 0 })

  elseif self.kind == "constant_div" then
    return Exp.new("constant", { constant = 0 })
  elseif self.kind == "sym" then
    if self == dx then
      return Exp.new("constant", { constant = 1 })
    else
      return Exp.new("constant", { constant = 0 })
    end
  elseif self.kind == "add" then
    local rhs = self.o.rhs:derivate(dx)
    local lhs = self.o.lhs:derivate(dx)
    if rhs:is_zero() then
      return lhs
    elseif lhs:is_zero() then
      return rhs
    end
    return Exp.new("add", { lhs = lhs, rhs = rhs })

  elseif self.kind == "sub" then
    local rhs = self.o.rhs:derivate(dx)
    local lhs = self.o.lhs:derivate(dx)
    return Exp.new("sub", { lhs = lhs, rhs = rhs })

  elseif self.kind == "pow" then
    if self.o.lhs.kind == "named_constant" and self.o.lhs.o.name == "e" then
      return self:clone() * self.o.rhs:derivate(dx)
    end


    if self.o.rhs.kind == "constant" then
      local der_lhs = self.o.lhs:derivate(dx)
      local coeff1 = Exp.new("constant", { constant = self.o.rhs.o.constant })
      local coeff2 = Exp.new("constant", { constant = self.o.rhs.o.constant-1 })

      if not coeff2:is_one() then
        res = Exp.new("pow", { lhs = self.o.lhs, rhs = coeff2 })
      else
        res = self.o.lhs
      end

      res = Exp.new("mul", { lhs = res, rhs = coeff1 })
      if not der_lhs:is_one() then
        res = Exp.new("mul", { lhs = res, rhs = der_lhs })
      end
      return res
    elseif self.o.rhs:derivate(dx):simplify():is_zero() then
      local der_lhs = self.o.lhs:derivate(dx)
      local coeff1 = self.o.rhs:clone()
      local coeff2 = self.o.rhs:clone()-M.constant(1)

      if not coeff2:is_one() then
        res = self.o.lhs ^ coeff2
      else
        res = self.o.lhs
      end

      res = coeff1 * res
      if not der_lhs:is_one() then
        res = res * der_lhs
      end
      return res

    end

    print("ERROR: Unsupported algebraic pow derivation")

  elseif self.kind == "mul" then
    local rhs_der = self.o.rhs:derivate(dx)
    local lhs_der = self.o.lhs:derivate(dx)

    local lhs_add = nil
    if not lhs_der:is_zero() then
      lhs_add = Exp.new("mul", { lhs = lhs_der, rhs = self.o.rhs })
    end

    local rhs_add = nil
    if not rhs_der:is_zero() then
      rhs_add = Exp.new("mul", { lhs = self.o.lhs, rhs = rhs_der })
    end

    if not lhs_add and not rhs_add then
      return Exp.new("constant", { constant = 0 })
    elseif not lhs_add then
      return rhs_add
    elseif not rhs_add then
      return lhs_add
    end

    return Exp.new("add", {lhs = lhs_add, rhs = rhs_add})

  elseif self.kind == "sin" then
    return M.cos(self.o.arg:clone()) * self.o.arg:derivate(dx)

  elseif self.kind == "cos" then
    return M.constant(-1) * M.sin(self.o.arg:clone()) * self.o.arg:derivate(dx)

  elseif self.kind == "ln" then
    return self.o.arg:derivate(dx) / self.o.arg:clone()

  elseif self.kind == "div" then
    local u = self.o.lhs
    local v = self.o.rhs

    return (u:derivate(dx) * v - u * v:derivate(dx))/(v*v)
  else
    print("ERROR! Cannot derivate of " .. self.kind)
  end
  return exp
end

function Exp:is_one()
  return self.kind == "constant" and self.o.constant == 1
end

function Exp:is_zero()
  return self.kind == "constant" and self.o.constant == 0
end

function Exp:is_constant()
  return self.kind == "constant"
end

function M.exp(x)
  return M.e ^ x
end

function M.ln(x)
  return Exp.new("ln", { arg = x })
end

function Exp.new(kind, opts)
  local o = { kind = kind, o = opts }
  return setmetatable(o, mt)
end

function Exp:expand()
  if self:is_atomic() then
    return self:clone()

  elseif self.kind == "pow" then
    local lhs = self.o.lhs:expand()
    local rhs = self.o.rhs:expand()

    -- Just compute if both are constants
    if rhs.kind == "constant" and lhs.kind == "constant" then
      return Exp.new("constant", { constant = lhs.o.constant ^ rhs.o.constant })
    end


    if rhs.kind == "constant" then
      local sup = rhs.o.constant

      -- Check if it is an integer

      if sup == math.floor(sup) and sup >= 0 then

        -- Transform to a series of multiplication
        local res = nil
        if sup == 0 then
          res = Exp.new("constant", { constant = 1 })
        else
          for i=1,sup do
            if not res then
              res = lhs:clone()
            else
              res = Exp.new("mul", { lhs = res, rhs = lhs:clone() })
            end
          end
        end

        res = res:expand()

        return res
      end
    end

    return Exp.new("pow", { lhs = lhs, rhs = rhs })

  elseif self.kind == "mul" then
    local lhs = self.o.lhs:expand()
    local rhs = self.o.rhs:expand()

    local rhs_term = rhs:collect_terms()

    local lhs_term = lhs:collect_terms()


    if #lhs_term > 1 or #rhs_term > 1 then
      local res = nil
      for i=1,#lhs_term do
        for j=1,#rhs_term do
          local term = Exp.new("mul", { lhs = lhs_term[i], rhs = rhs_term[j] })
          term = term:expand()
          if not res then
            res = term
          else
            res = Exp.new("add", { lhs = term, rhs = res })
          end
        end
      end

      return res
    end

    return self:clone()

  elseif self.kind == "div" then
    local lhs = self.o.lhs:expand()
    local rhs = self.o.rhs:expand()

    return Exp.new("div", { lhs = lhs, rhs = rhs })

  elseif self.kind == "add" then
    local lhs = self.o.lhs:expand()
    local rhs = self.o.rhs:expand()

    return Exp.new("add", { lhs = lhs, rhs = rhs })


  else
    return self:clone()
  end
end

function Exp:clone()
  if self.kind == "constant" then
    return Exp.new(self.kind, { constant = self.o.constant })

  elseif self.kind == "constant_div" then
    return Exp.new(self.kind, { lhs = self.o.lhs:clone(), rhs = self.o.rhs:clone() })

  elseif self.kind == "ln" then
    return Exp.new(self.kind, { arg = self.o.arg:clone() })
  elseif self.kind == "add" then
    return Exp.new(self.kind, { lhs = self.o.lhs:clone(), rhs = self.o.rhs:clone() })
  elseif self.kind == "sub" then
    return Exp.new(self.kind, { lhs = self.o.lhs:clone(), rhs = self.o.rhs:clone() })
  elseif self.kind == "mul" then
    return Exp.new(self.kind, { lhs = self.o.lhs:clone(), rhs = self.o.rhs:clone() })
  elseif self.kind == "pow" then
    return Exp.new(self.kind, { lhs = self.o.lhs:clone(), rhs = self.o.rhs:clone() })
  elseif self.kind == "sym" then
    -- Not sure if that's a good idea. Reasoning: when eval, need only to set the
    -- value at one location.
    return self

  elseif self.kind == "cos" then
    return Exp.new(self.kind, { arg = self.o.arg:clone() })
  elseif self.kind == "sin" then
    return Exp.new(self.kind, { arg = self.o.arg:clone() })
  elseif self.kind == "inf" then
    return Exp.new(self.kind, {})

  elseif self.kind == "named_constant" then
    return self

  elseif self.kind == "i" then
    return self
  elseif self.kind == "matrix" then
    local rows = {}
    for i=1,#self.o.rows do
      local row = {}
      for j=1,#self.o.rows[i] do
        table.insert(row, self.o.rows[i][j]:clone())
      end
      table.insert(rows, row)
    end

    return Exp.new("matrix", { rows = rows })

  elseif self.kind == "div" then
    return Exp.new(self.kind, { lhs = self.o.lhs:clone(), rhs = self.o.rhs:clone() })


  else
    print(("Error! Cannot clone kind = %s."):format(self.kind))
  end
end

function Exp:collect_terms()
  local terms = {}
  if self.kind == "add" then
    local lhs_term = self.o.lhs:collect_terms()
    local rhs_term = self.o.rhs:collect_terms()

    for i=1,#lhs_term do table.insert(terms, lhs_term[i]) end
    for i=1,#rhs_term do table.insert(terms, rhs_term[i]) end

    return terms
  end

  return { self:clone() }
end

function Exp:collect_factors()
  local factors = {}
  if self.kind == "mul" then
    local lhs_factor = self.o.lhs:collect_factors()
    local rhs_factor = self.o.rhs:collect_factors()

    for i=1,#lhs_factor do table.insert(factors, lhs_factor[i]) end
    for i=1,#rhs_factor do table.insert(factors, rhs_factor[i]) end

    return factors
  end
  return { self:clone() }
end

function Exp:is_atomic()
  return self.kind == "sym" or 
    self.kind == "constant" or 
    self.kind == "inf" or 
    self.kind == "named_constant" or
    self.kind == "i"
end

function M.cos(x)
  return Exp.new("cos", { arg = x })
end

function M.sin(x)
  return Exp.new("sin", { arg = x })
end

function M.inf()
  return Exp.new("inf", {})
end

function Exp:integrate(dx)
  if false then
  elseif self.kind == "add" then
    local rhs = self.o.rhs:integrate(dx)
    local lhs = self.o.lhs:integrate(dx)

    return Exp.new("add", { rhs = rhs, lhs = lhs })

  elseif self.kind == "sym" then
    if self == dx then
      pow_exp = Exp.new("pow", { lhs = dx, rhs = M.constant(2) })
      coeff = Exp.new("div", { lhs = M.constant(1), rhs = M.constant(2) })
      return Exp.new("mul", { lhs = coeff, rhs = pow_exp })
    else
      return Exp.new("mul", { lhs = self, rhs = dx })
    end
  else
    print("Unsupported!")
  end
end

function Exp:simplify()
  if self.kind == "mul" then
    local lhs = self.o.lhs
    local rhs = self.o.rhs

    if lhs:is_matrix() and rhs:is_matrix() then
      lhs = self.o.lhs:simplify()
      rhs = self.o.rhs:simplify()

      assert(lhs:cols() == rhs:rows(), "Matrix multiplication dimensions mismatch")

      local rows = {}
      -- lhs:rows is only called once btw
      for i=1,lhs:rows() do
        local row = {}
        for j=1,rhs:cols() do
          local cell
          for k=1,rhs:rows() do
            local mul_exp = Exp.new("mul", { lhs = lhs.o.rows[i][k]:clone(), rhs = rhs.o.rows[k][j]:clone() })

            if not cell then
              cell = mul_exp
            else
              cell = Exp.new("add", { lhs = cell, rhs = mul_exp })
            end

          end
          table.insert(row, cell:simplify())
        end
        table.insert(rows, row)
      end

      return Exp.new("matrix", { rows = rows })
    end

    if rhs:is_matrix() then
      local rows = {}
      for i=1,rhs:rows() do
        local row = {}
        for j=1,rhs:cols() do
          local mul_exp = Exp.new("mul", { lhs = lhs:clone(), rhs = rhs.o.rows[i][j]:clone() })
          table.insert(row, mul_exp:simplify())
        end
        table.insert(rows, row)
      end

      return Exp.new("matrix", { rows = rows })
    end

    if lhs:is_matrix() then
      local rows = {}
      for i=1,lhs:rows() do
        local row = {}
        for j=1,lhs:cols() do
          local mul_exp = Exp.new("mul", { rhs = rhs:clone(), lhs = lhs.o.rows[i][j]:clone() })
          table.insert(row, mul_exp:simplify())
        end
        table.insert(rows, row)
      end

      return Exp.new("matrix", { rows = rows })
    end


  	if lhs:is_constant() and rhs:is_constant() then
  		return M.constant(lhs.o.constant * rhs.o.constant)
  	elseif lhs:is_constant() and rhs:is_constant_div() then
  		return M.mul_constant_div(lhs / 1, rhs)

  	elseif lhs:is_constant_div() and rhs:is_constant() then
  		return M.mul_constant_div(lhs, rhs / 1)

  	elseif lhs:is_constant_div() and rhs:is_constant_div() then
  		return M.mul_constant_div(lhs, rhs)

  	end

    -- @handle_if_one_is_pow
    -- @handle_mul_simplify

    local factors = self:collect_factors()

    for i=1,#factors do
      factors[i] = factors[i]:simplify()
    end

    local has_div = false
    for _, factor in ipairs(factors) do
      if factor.kind == "div" then
        has_div = true
        break
      end
    end

    if has_div then
      local num = {}
      local den = {}

      for _, factor in ipairs(factors) do
        if factor.kind == "div" then
          table.insert(num, factor.o.lhs)
          table.insert(den, factor.o.rhs)
        else
          table.insert(num, factor)
        end
      end

      num = M.reduce_all("mul", num)
      den = M.reduce_all("mul", den)


      return num:simplify() / den:simplify()
    end


    local elem_i, factors = M.split_kind("i", factors)
    local i_const, i_fac = M.pow_i(#elem_i)
    local elem_consts, factors = M.split_kind({"constant", "constant_div"}, factors)
    table.insert(elem_consts, M.constant(i_const))
    local coeff = M.reduce_const(elem_consts)

    if coeff:is_zero() then
      return M.constant(0)
    end

    local pow_base = {}
    local list_fac = {}

    for _, fac in ipairs(factors) do
      local lhs, rhs
      if fac.kind == "pow" then
        lhs = fac.o.lhs
        rhs = fac.o.rhs
      else
        lhs = fac
        rhs = nil
      end

      local found = false
      for _, elem in ipairs(pow_base) do
        if elem[1] == lhs then
          elem[2] = elem[2] or M.constant(1)
          elem[2] = (elem[2] or M.constant(1)) + (rhs or M.constant(1))
          found = true
          break
        end
      end

      if not found then
        table.insert(pow_base, { lhs, rhs })
      end

    end

    local result = nil
    for _, elem in ipairs(pow_base) do
      local new_elem
      if not elem[2] then
        new_elem = elem[1]
      else
        new_elem = elem[1] ^ (elem[2]:simplify())
      end
      result = (result and (result * new_elem)) or new_elem
    end

    if i_fac then
      result = result and i_fac * result or i_fac
    end
    if not coeff:is_one() then
      result = result and coeff * result or coeff
    end
    result = result or M.constant(1)
    return result

  elseif self.kind == "add" then
    local lhs = self.o.lhs
    local rhs = self.o.rhs

    if lhs:is_matrix() and rhs:is_matrix() then
      lhs = lhs:simplify()
      rhs = rhs:simplify()
      assert(lhs:cols() == rhs:cols(), "Matrix addition dimensions mismatch")
      assert(lhs:rows() == rhs:rows(), "Matrix addition dimensions mismatch")

      local rows = {}
      for i = 1,rhs:rows() do
        local row = {}
        for j = 1,lhs:cols() do
          local cell = Exp.new("add", {
            lhs = lhs.o.rows[i][j]:clone(), 
            rhs = rhs.o.rows[i][j]:clone() })
          table.insert(row, cell:simplify())
        end
        table.insert(rows, row)
      end

      return Exp.new("matrix", { rows = rows })
  	elseif lhs:is_constant() and rhs:is_constant() then
  		return M.constant(lhs.o.constant + rhs.o.constant)
  	elseif lhs:is_constant() and rhs:is_constant_div() then
  		return M.add_constant_div(lhs / 1, rhs)

  	elseif lhs:is_constant_div() and rhs:is_constant() then
  		return M.add_constant_div(lhs, rhs / 1)

  	elseif lhs:is_constant_div() and rhs:is_constant_div() then
  		return M.add_constant_div(lhs, rhs)

    else
      local terms = self:collect_terms()
      for i=1,#terms do
        terms[i] = terms[i]:simplify()
      end

      local atomics = {}
      for _, term in ipairs(terms) do
        local facs = term:collect_factors()
        local const, facs = M.split_kind({"constant", "constant_div"}, facs)
        local coeff = M.reduce_const(const)
        table.sort(facs) 
        local added = false
        for i, atomic in ipairs(atomics) do
          if M.is_same_all(atomic[2], facs) then
            atomic[1] = (atomic[1] + (coeff or 1)):simplify()
            added = true
            break
          end
        end

        if not added then
          table.insert(atomics, { (coeff or M.constant(1)), facs })
        end

      end

      atomics = vim.tbl_filter(function(atomic)
        return not atomic[1]:is_zero()
      end, atomics)

      atomics = vim.tbl_map(function(atomic)
        local rhs = M.reduce_all("mul", atomic[2])
        
        if not rhs then
          return atomic[1]
        end

        if atomic[1]:is_one() then
          return rhs
        else
          return atomic[1] *  rhs
        end
      end, atomics)

      local result = M.reduce_all("add", atomics)
      result = result or M.constant(0)

      return result

    end

    return Exp.new("add", { lhs = lhs, rhs = rhs })

  elseif self.kind == "matrix" then
    local rows = {}
    for i=1,#self.o.rows do
      local row = {}
      for j=1,#self.o.rows[i] do
        table.insert(row, self.o.rows[i][j]:simplify())
      end
      table.insert(rows, row)
    end
    return Exp.new("matrix", { rows = rows })

  elseif self.kind == "div" then
    local lhs = self.o.lhs:simplify()
    local rhs = self.o.rhs:simplify()

    if lhs:is_zero() then
      return M.constant(0)
    end

    if lhs:is_integer() and rhs:is_integer() then
      local num = lhs.o.constant
      local den = rhs.o.constant

      local div = gcd(num, den)

      -- not really sure if all of this is necessary,
      -- floating numbers is always a grey zone for me
      num = math.floor(num / div + 0.5)
      den = math.floor(den / div + 0.5)

      if den == 1 then
        return M.constant(num)
      end

      return Exp.new("div", { 
        lhs = M.constant(num), 
        rhs = M.constant(den)
      })
    end

    return Exp.new("div", { lhs = lhs, rhs = rhs })

  elseif self.kind == "pow" then
    local lhs = self.o.lhs:simplify()
    local rhs = self.o.rhs:simplify()

    if lhs.kind == "constant" and rhs.kind == "constant" then
      return M.constant(lhs.o.constant ^ rhs.o.constant)
    elseif lhs.kind == "i" and rhs.kind == "constant" and rhs:is_integer() then
      return M.pow_i(rhs.o.constant) or M.constant(1)
    elseif rhs.kind == "constant" and rhs:is_integer() then
      if rhs.o.constant == 0 then
        return M.constant(1)
      elseif rhs.o.constant == 1 then
        return lhs
      end
    end

    return lhs ^ rhs
  end
  return self:clone()
end

function Exp:is_matrix()
  return self.kind == "matrix"
end

function Exp:rows()
  assert(self.kind == "matrix", "rows must be called on a matrix")
  return #self.o.rows
end

function Exp:cols()
  assert(self.kind == "matrix", "cols must be called on a matrix")
  return #self.o.rows[1]
end

function Exp:T()
  assert(self:is_matrix(), "T() argument must be a matrix.")

  local rows = {}
  for i = 1,#self.o.rows[1] do
    local row = {}
    for j = 1,#self.o.rows do
      table.insert(row, self.o.rows[j][i]:clone())
    end
    table.insert(rows, row)
  end
  return Exp.new("matrix", { rows = rows })
end

function Exp:mul_elem(other)
  assert(self.kind == "matrix", "lhs for mul_elem must be a matrix")
  assert(other.kind == "matrix", "rhs for mul_elem must be a matrix")
  assert(self:rows() == other:rows(), "lhs and rhs must have the same number of rows for mul_elem()")
  assert(self:cols() == other:cols(), "lhs and rhs must have the same number of columns for mul_elem()")

  local rows = {}
  for i=1,#self.o.rows do
    local row = {}
    for j=1,#self.o.rows[i] do
      table.insert(row, self.o.rows[i][j] * other.o.rows[i][j])
    end
    table.insert(rows, row)
  end

  local exp = Exp.new("matrix", { rows = rows })
  return exp
end

function Exp:normalize()
  if self:is_atomic() then
    return self:clone()

  elseif self.kind == "mul" then
    local facs = self:collect_factors()
    for i=1,#facs do
      facs[i] = facs[i]:normalize()
    end
    table.sort(facs)
    return M.reduce_all("mul", facs)

  elseif self.kind == "add" then
    local terms = self:collect_terms()
    for i=1,#terms do
      terms[i] = terms[i]:normalize()
    end
    table.sort(terms)
    return M.reduce_all("add", terms)
    
  elseif self.kind == "pow" then
    return self.o.lhs:normalize() ^ self.o.rhs:normalize()

  elseif self.kind == "div" then
    return self.o.lhs:normalize() / self.o.rhs:normalize()

  elseif self.kind == "matrix" then
    local rows = {}
    for i=1,#self.o.rows do
      local row = {}
      for j=1,#self.o.rows[i] do
        table.insert(row, self.o.rows[i][j]:normalize())
      end
      table.insert(rows, row)
    end
    return Exp.new("matrix", { rows = rows })
  end
  return self:clone()
end

function Exp:is_integer()
  return self.kind == "constant" and math.floor(self.o.constant) == self.o.constant
end

function M.is_atomic_all(tbl)
  for _, el in ipairs(tbl) do
    if not el:is_atomic() then
      return false
    end
  end
  return true
end

function M.is_same_all(tbl_a, tbl_b)
  if #tbl_a ~= #tbl_b then
    return false
  end

  for i=1,#tbl_a do
    if tbl_a[i] ~= tbl_b[i] then
      return false
    end
  end
  return true
end

function M.reduce_all(kind, tbl, prev)
  for _, elem in ipairs(tbl) do
    if not prev then
      prev = elem
    else
      prev = Exp.new(kind, { lhs = prev, rhs = elem})
    end
  end
  return prev
end
function M.split_kind(kind, facs)
	if type(kind) ~= "table" then
		kind = { kind }
	end

  local first = {}
  local second = {}

  for _, elem in ipairs(facs) do
		local is_first = false
		for _, ki in ipairs(kind) do
			if elem.kind == ki then
				is_first = true
				break
			end
		end

		if is_first then
			table.insert(first, elem)
		else
			table.insert(second, elem)
		end
  end
  return first, second
end

function M.pow_i(num)
  if num % 4 == 0 then
    return 1, nil
  elseif num % 4 == 1 then
    return 1, M.i
  elseif num % 4 == 2 then
    return -1, nil
  elseif num % 4 == 3 then
    return -1, M.i
  end
end

function M.reduce_const(facs) 
  local coeff = M.constant(1)
  for _, fac in ipairs(facs) do
		coeff = (coeff * fac):simplify()
  end
  return coeff
end

function M.sqrt(x)
  return x ^ (M.constant(1)/M.constant(2))
end
function Exp:cross(other)
	assert(self.kind == "matrix")
	assert(other.kind == "matrix")

	assert(#self.o.rows == #other.o.rows)

	assert(#self.o.rows[1] == 1)
	assert(#other.o.rows[1] == 1)


	assert(#self.o.rows == 3)

	local rows = {}
	table.insert(rows, { self.o.rows[2][1]:clone() * other.o.rows[3][1]:clone() - self.o.rows[3][1]:clone() * other.o.rows[2][1]:clone() })
	table.insert(rows, { self.o.rows[3][1]:clone() * other.o.rows[1][1]:clone() - self.o.rows[1][1]:clone() * other.o.rows[3][1]:clone() })
	table.insert(rows, { self.o.rows[1][1]:clone() * other.o.rows[2][1]:clone() - self.o.rows[2][1]:clone() * other.o.rows[1][1]:clone() })

	return Exp.new("matrix", { rows = rows })
end

function Exp:divergence()
  assert(self.kind == "matrix", "divergence must be called on a matrix")
  assert(self:cols() == 1, "divergence must be called on a matrix with one column")
  assert(self:rows() == 3, "divergence must be called on a matrix with three rows")

  local sym_coord = { 
    sym_table["x"], sym_table["y"], sym_table["z"] 
  }

  local terms = {}
  for i=1,3 do
    if sym_coord[i] then
      local term
      term = self.o.rows[i][1]:derivate(sym_coord[i])
      table.insert(terms, term)
    end
  end

  local exp = M.reduce_all("add", terms)
  return exp
end

function Exp:dot(other)
	assert(self.kind == "matrix")
	assert(other.kind == "matrix")

	assert(#self.o.rows == #other.o.rows)

	assert(#self.o.rows[1] == 1)
	assert(#other.o.rows[1] == 1)


	local terms = {}
	for i=1,#self.o.rows do
		table.insert(terms, self.o.rows[i][1] * other.o.rows[i][1])
	end

	local exp = M.reduce_all("add", terms)
	return exp
end

function M.sym(name)
  local exp = Exp.new("sym", { name = name })
  assert(not sym_table[name], ("%s is already defined!"):format(name))
  sym_table[name] = exp

  return exp
end

function convert_constant(x)
  if type(x) == "number" then
    return Exp.new("constant", {constant = x})
  end
  return x
end

function M.constant(num)
  return Exp.new("constant", { constant = num })
end

function M.named_constant(name)
  return Exp.new("named_constant", { name = name })
end

M.e = M.named_constant("e")
M.pi = M.named_constant("pi")

M.i = Exp.new("i", {})


function M.clear_syms()
  sym_table = {}
end
function Exp:gradient()
  local unknowns = {}
  self:collect_unknowns(unknowns)

  unknowns = vim.tbl_keys(unknowns)
  table.sort(unknowns, function(a, b)
    return a.o.name < b.o.name
  end)

  local exp
  if self.kind == "matrix" then
    local rows = {}
    for i=1,#self.o.rows do
      local row = {}
      for j=1,#unknowns do
        table.insert(row, self.o.rows[i][1]:derivate(unknowns[j]))
      end
      table.insert(rows, row)
    end

    exp = Exp.new("matrix", { rows = rows })

  else
    local rows = {}
    for j=1,#unknowns do
      table.insert(rows, { self:derivate(unknowns[j])} )
    end

    exp = Exp.new("matrix", { rows = rows })
  end
  return exp
end

function Exp:collect_unknowns(unknowns)
  if self.kind == "sym" then
    unknowns[self] = true
  elseif self.kind == "ln" then
    self.o.arg:collect_unknowns(unknowns)
  elseif self.kind == "add" then
    self.o.lhs:collect_unknowns(unknowns)
    self.o.rhs:collect_unknowns(unknowns)
  elseif self.kind == "sub" then
    self.o.lhs:collect_unknowns(unknowns)
    self.o.rhs:collect_unknowns(unknowns)
  elseif self.kind == "pow" then
    self.o.lhs:collect_unknowns(unknowns)
    self.o.rhs:collect_unknowns(unknowns)
  elseif self.kind == "cos" then
    self.o.arg:collect_unknowns(unknowns)
  elseif self.kind == "sin" then
    self.o.arg:collect_unknowns(unknowns)
  elseif self.kind == "inf" then
  elseif self.kind == "named_constant" then
  elseif self.kind == "i" then
  elseif self.kind == "matrix" then
    for i=1,#self.o.rows do
      for j=1,#self.o.rows[i] do
        self.o.rows[i][j]:collect_unknowns(unknowns)
      end
    end
  elseif self.kind == "div" then
    self.o.lhs:collect_unknowns(unknowns)
    self.o.rhs:collect_unknowns(unknowns)
  elseif self.kind == "mul" then
    self.o.lhs:collect_unknowns(unknowns)
    self.o.rhs:collect_unknowns(unknowns)

  end
end

function M.version()
  return "0.0.8"
end

function M.description()
  return "swan.lua is a symbolic math toolbox written in lua"
end
function Exp:laplacian()
  local sym_coord = { 
    sym_table["x"], sym_table["y"], sym_table["z"] 
  }

  local terms = {}
  for i=1,3 do
    if sym_coord[i] then
      local term
      term = self:derivate(sym_coord[i]):derivate(sym_coord[i])
      table.insert(terms, term)
    end
  end

  local exp = M.reduce_all("add", terms)
  return exp
end

function M.mat(array)
  assert(type(array) == "table", "Argument array must be table")

  for i=1,#array do
    assert(type(array[i]) == "table", "Argument array elements must be table")
    for j=1,#array[i] do
      local el = array[i][j]
      if type(el) == "number" then
        array[i][j] = Exp.new("constant", { constant = el })
      end
    end
  end

  local exp = Exp.new("matrix", { rows = array })

  return exp
end

function M.frac(num, den)
  num = convert_constant(num)
  den = convert_constant(den)
  return Exp.new("div", { lhs = num, rhs = den })
end

-- The oldest algorithm
function gcd(a, b)
  if a == b then
    return a
  end
  if a < b then
    a,b = b,a
  end
  return gcd(a-b, b)
end

function Exp:rotational()
  assert(self.kind == "matrix", "rotational must be called on a matrix")
  assert(self:cols() == 1, "rotational must be called on a matrix with one column")
  assert(self:rows() == 3, "rotational must be called on a matrix with three rows")

  local Fx = self.o.rows[1][1]
  local Fy = self.o.rows[2][1]
  local Fz = self.o.rows[3][1]

  local sym_x = sym_table["x"]
  local sym_y = sym_table["y"]
  local sym_z = sym_table["z"]

  local dyFz = sym_y and Fz:derivate(sym_y) or M.constant(0)
  local dzFy = sym_z and Fy:derivate(sym_z) or M.constant(0)

  local dzFx = sym_z and Fx:derivate(sym_z) or M.constant(0)
  local dxFz = sym_x and Fz:derivate(sym_x) or M.constant(0)

  local dxFy = sym_x and Fy:derivate(sym_x) or M.constant(0)
  local dyFx = sym_y and Fx:derivate(sym_y) or M.constant(0)

  local exp = M.mat {
    { dyFz - dzFy },
    { dzFx - dxFz },
    { dxFy - dyFx },
  }
  return exp
end


return M

