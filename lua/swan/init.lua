-- Generated using ntangle.nvim
local M = {}

local convert_constant

local Exp = {}

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
        return ("(%s - %s)"):format(tostring(self.o.lhs), tostring(self.o.rhs))
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
          if #factors_str_list > 1 then
            if not factors_str_ref[fac]:is_atomic() and factors_str_ref[fac].kind ~= "pow" then
              fac = "(" .. fac .. ")"
            end
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
        if self.o.lhs:is_atomic() then
          return ("%s^%s"):format(tostring(self.o.lhs), tostring(self.o.rhs))
        else
          return ("(%s)^%s"):format(tostring(self.o.lhs), tostring(self.o.rhs))
        end
      elseif self.kind == "constant" then
        return tostring(self.o.constant)

      elseif self.kind == "cos" then
        return ("cos(%s)"):format(tostring(self.o.arg))
      elseif self.kind == "sin" then
        return ("sin(%s)"):format(tostring(self.o.arg))

      elseif self.kind == "inf" then
        return "inf"

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

      return Exp.new("sub", { lhs = lhs, rhs = rhs })
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
          if not res then
            res = term
          else
            res = Exp.new("add", { lhs = term, rhs = res })
          end
        end
      end

      return res
    end

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
  return self.kind == "sym" or self.kind == "constant" or self.kind == "inf"
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

function Exp:integrate()
  print("ERROR: Unsupported!")
end
function Exp:simplify()
  if false then
  elseif self.kind == "mul" then
    local lhs = self.o.lhs:simplify()
    local rhs = self.o.rhs:simplify()

    if lhs:is_constant() and rhs:is_constant() then
      return M.constant(lhs.o.constant * rhs.o.constant)
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
    local lhs = self.o.lhs:simplify()
    local rhs = self.o.rhs:simplify()

    if lhs:is_constant() and rhs:is_constant() then
      return M.constant(lhs.o.constant + rhs.o.constant)
    end

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
function M.version()
  return "0.0.1"
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


return M

