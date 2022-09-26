-- Generated using ntangle.nvim
local M = {}

local convert_constant

local gcd

local Exp = {}

local kind_priority = {
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

function Exp:derivate(dx)
  if self.kind == "constant" then
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

function Exp.new(kind, opts)
  local o = { kind = kind, o = opts }
  return setmetatable(o, { __index = Exp,
    __tostring = function(self)
      if self.kind == "sym" then
        return self.o.name
      elseif self.kind == "add" then
        local terms = self:collectTerm()
        local terms_str = {}
        local terms_str_list = {}

        for i=1,#terms do
          local str = tostring(terms[i])
          terms_str[str] = terms_str[str] or 0
          terms_str[str] = terms_str[str] + 1
        end

        for key, _ in pairs(terms_str) do
          table.insert(terms_str_list, tostring(key))
        end
        table.sort(terms_str_list)

        local str_list = {}
        for i, term in ipairs(terms_str_list) do
          coeff = terms_str[term]
          if coeff == 1 then
            table.insert(str_list, term)
          -- elseif coeff == 2 then
            -- str = str .. term .. "²"
          -- elseif coeff == 3 then
            -- str = str .. term .. "³"
          else
            table.insert(str_list, coeff .. term)
          end
        end
        return table.concat(str_list, " + ")

      elseif self.kind == "sub" then
        return ("%s - %s"):format(tostring(self.o.lhs), tostring(self.o.rhs))
      elseif self.kind == "mul" then
        local factors = self:collectFactors()
        local factors_str = {}
        local factors_str_list = {}
        local factors_str_ref = {}

        for i=1,#factors do
          local str = tostring(factors[i])
          factors_str[str] = factors_str[str] or 0
          factors_str[str] = factors_str[str] + 1
          factors_str_ref[str] = factors[i]
        end

        factors_str_list = vim.tbl_keys(factors_str)
        table.sort(factors_str_list)

        local num_factor = 1

        i = 1
        while i <= #factors_str_list do
          fac = factors_str_list[i]
          if factors_str_ref[fac].kind == "constant" then
            num_factor = num_factor * factors_str_ref[fac].o.constant
            table.remove(factors_str_list, i)
          else
            i = i + 1
          end
        end


        local str = ""
        for i, fac in ipairs(factors_str_list) do
          sup = factors_str[fac]
          if not factors_str_ref[fac]:is_atomic() then
            fac = "(" .. fac .. ")"
          end

          if sup == 1 then
            str = str .. fac
          -- elseif sup == 2 then
            -- str = str .. fac .. "²"
          -- elseif sup == 3 then
            -- str = str .. fac .. "³"
          else
            str = str .. fac .. "^" .. sup
          end
        end

        if num_factor ~= 1 then
          return num_factor .. str
        else
          return str
        end

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

      return Exp.new("div", { lhs = lhs, rhs = rhs })
    end,
  })
end

function Exp:expand()
  if self.kind == "pow" then
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

    local rhs_term = rhs:collectTerm()

    local lhs_term = lhs:collectTerm()


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

function Exp:collectTerm()
  local terms = {}
  if self.kind == "add" then
    local lhs_term = self.o.lhs:collectTerm()
    local rhs_term = self.o.rhs:collectTerm()

    for i=1,#lhs_term do table.insert(terms, lhs_term[i]) end
    for i=1,#rhs_term do table.insert(terms, rhs_term[i]) end

    return terms
  end

  return { self:clone() }
end

function Exp:collectFactors()
  local factors = {}
  if self.kind == "mul" then
    local lhs_factor = self.o.lhs:collectFactors()
    local rhs_factor = self.o.rhs:collectFactors()

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
  if false then
  elseif self.kind == "mul" then
    local lhs = self.o.lhs:simplify()
    local rhs = self.o.rhs:simplify()

    if lhs:is_constant() and rhs:is_constant() then
      return M.constant(lhs.o.constant * rhs.o.constant)
    end

    if lhs:is_zero() or rhs:is_zero()  then
      return M.constant(0)
    end

    if lhs:is_matrix() and rhs:is_matrix() then
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

    if lhs.kind == "pow" or rhs.kind == "pow" then
      local factors = self:collectFactors()
      local pow_base = {}
      local list_fac = {}

      for _, fac in ipairs(factors) do
        fac = fac:expand()
        if fac.kind == "pow" and fac.o.lhs:is_atomic() then
          local lhs = fac.o.lhs
          if not pow_base[lhs.kind] then
            pow_base[lhs.kind] = {}
          end
          local tpow_base = pow_base[lhs.kind]
          if lhs.kind == "constant" then
            c = lhs.o.constant
            if tpow_base[c] then
              tpow_base[c].o.rhs = Exp.new("add", {lhs=tpow_base[c].o.rhs, rhs=fac.o.rhs}):simplify()

            else
              tpow_base[c] = fac
              table.insert(list_fac, fac)
            end

          elseif lhs.kind == "named_constant" then
            n = lhs.o.name
            if tpow_base[n] then
              tpow_base[n].o.rhs = Exp.new("add", {lhs=tpow_base[n].o.rhs, rhs=fac.o.rhs}):simplify()

            else
              tpow_base[n] = fac
              table.insert(list_fac, fac)
            end

          else
            table.insert(list_fac, fac)
          end
        else
          table.insert(list_fac, fac)
        end
      end

      return M.reduce_mul(list_fac)
    end

    if lhs.kind == "div" and rhs.kind == "div" then
      local num_lhs = lhs.o.lhs
      local den_lhs = lhs.o.rhs

      local num_rhs = rhs.o.lhs
      local den_rhs = rhs.o.rhs

      local num = Exp.new("mul", { lhs = num_lhs, rhs = num_rhs })
      local den = Exp.new("mul", { lhs = den_lhs, rhs = den_rhs })

      num = num:simplify()
      den = den:simplify()

      return Exp.new("div", { lhs = num, rhs = den })
    end
    if lhs.kind == "i" and rhs.kind == "i" then
      return M.constant(-1)
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
    else
      local terms = self:collectTerm()

      local atomics = {}
      local rest = {}
      for _, term in ipairs(terms) do
        local fac_term = term:collectFactors()
        if M.is_atomic_all(fac_term) then
          local fac_term = M.reorderFactors(fac_term)
          local coeff, facs = M.splitCoeff(fac_term)
          local added = false
          for i, atomic in ipairs(atomics) do
            if M.is_same_all(atomic[2], facs) then
              atomic[1] = atomic[1] + coeff
              added = true
              break
            end
          end

          if not added then
            table.insert(atomics, { coeff, facs })
          end

        else
          table.insert(rest, term)
        end
      end

      atomics = vim.tbl_filter(function(atomic)
        return atomic[1] ~= 0
      end, atomics)

      atomics = vim.tbl_map(function(atomic)
        local rhs = M.reduce_all("mul", atomic[2])
        if atomic[1] == 1 then
          return rhs
        else
          return Exp.new("mul", { lhs = M.constant(atomic[1]), rhs = rhs })
        end
      end, atomics)

      local result = M.reduce_all("add", atomics)
      result = M.reduce_all("add", rest, result)
      result = result or M.constant(0)

      return result

    end

    return Exp.new("add", { lhs = lhs, rhs = rhs })

  elseif self.kind == "div" then
    local lhs = self.o.lhs:simplify()
    local rhs = self.o.rhs:simplify()

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

-- Mainly designed for atomic factors
function M.reorderFactors(tbl) 
  table.sort(tbl, function(a, b)
    if a.kind ~= b.kind then
      return kind_priority[a.kind] < kind_priority[b.kind]
    else
      if a.kind == "constant" then
        return a.o.constant < b.o.constant

      elseif a.kind == "sym" then
        return a.o.name < b.o.name
      elseif a.kind == "named_constant" then
        return a.o.name < b.o.name

      end
      return true
    end
  end)
  return tbl
end

function M.splitCoeff(tbl)
  local coeff = 1
  local facs = {}
  local idx = 1
  for _, term in ipairs(tbl) do
    if term.kind == "constant" then
      coeff = coeff * term.o.constant
      idx = idx + 1
    else
      break
    end
  end

  for i=idx,#tbl do
    table.insert(facs, tbl[i])
  end
  return coeff, facs
end

function Exp:is_same(other) 
  if self.kind == other.kind then
    if self.kind == "constant" then
      return self.o.constant == other.o.constant

    elseif self.kind == "named_constant" then
      return self.o.name == other.o.name
    elseif self.kind == "sym" then
      return self.o.name == other.o.name
    elseif self.kind == "i" then
      return true

    end
    return false
  end
  return false
end

function M.is_same_all(tbl_a, tbl_b)
  if #tbl_a ~= #tbl_b then
    return false
  end

  for i=1,#tbl_a do
    if not tbl_a[i]:is_same(tbl_b[i]) then
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
function M.reduce_mul(exp_list)
  local result = nil
  for _, exp in ipairs(exp_list) do
    if not result then
      result = exp
    else
      result = Exp.new("mul", {lhs = result, rhs = exp})
    end
  end
  return result
end

function M.sym(name)
  return Exp.new("sym", { name = name })
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


function M.version()
  return "0.0.5"
end

function M.description()
  return "swan.lua is a symbolic math toolbox written in lua"
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

-- The oldest algorithm, as far as I know
function gcd(a, b)
  if a == b then
    return a
  end
  if a < b then
    a,b = b,a
  end
  return gcd(a-b, b)
end


return M

