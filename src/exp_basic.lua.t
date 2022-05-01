##swan
@define+=
function M.sym(name)
  return Exp.new("sym", { name = name })
end

@variables+=
local Exp = {}

@methods+=
function Exp.new(kind, opts)
  local o = { kind = kind, o = opts }
  return setmetatable(o, { __index = Exp,
    @metamethods
  })
end

@metamethods+=
__tostring = function(self)
  @print_exp
  else
    return "[UNKNOWN]"
  end
end,

__add = function(lhs, rhs)
  @convert_lhs_or_rhs_to_constant
  return Exp.new("add", { lhs = lhs, rhs = rhs })
end,

__sub = function(lhs, rhs)
  @convert_lhs_or_rhs_to_constant
  return Exp.new("sub", { lhs = lhs, rhs = rhs })
end,

__pow = function(lhs, rhs)
  @convert_lhs_or_rhs_to_constant
  return Exp.new("pow", { lhs = lhs, rhs = rhs })
end,

@declare+=
local convert_constant

@define+=
function convert_constant(x)
  if type(x) == "number" then
    return Exp.new("constant", {constant = x})
  end
  return x
end

@convert_lhs_or_rhs_to_constant+=
lhs = convert_constant(lhs)
rhs = convert_constant(rhs)

@print_exp+=
if self.kind == "sym" then
  return self.o.name
elseif self.kind == "add" then
  @display_add_string
elseif self.kind == "sub" then
  return ("(%s - %s)"):format(tostring(self.o.lhs), tostring(self.o.rhs))
elseif self.kind == "mul" then
  @display_mul_string
elseif self.kind == "pow" then
  return ("(%s ^ %s)"):format(tostring(self.o.lhs), tostring(self.o.rhs))
elseif self.kind == "constant" then
  return tostring(self.o.constant)

@methods+=
function Exp:simplify()
  @simplify_exp
  else
    return self:clone()
  end
end

@simplify_exp+=
if self.kind == "pow" then
  local lhs = self.o.lhs:simplify()
  local rhs = self.o.rhs:simplify()

  -- Just compute if both are constants
  if rhs.kind == "constant" and lhs.kind == "constant" then
    return Exp.new("constant", { constant = lhs.o.constant ^ rhs.o.constant })
  end


  if rhs.kind == "constant" then
    local sup = rhs.o.constant

    -- Check if it is an integer
    if sup == math.floor(sup) and sup >= 0 then

      -- Transform to a series of multiplication
      @transform_pow_to_multiplication
      @simplify_multiplications
      return res
    end
  end

  return Exp.new("pow", { lhs = lhs, rhs = rhs })

@transform_pow_to_multiplication+=
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

@methods+=
function Exp:clone()
  @clone_exp
  else
    print(("Error! Cannot clone kind = %s."):format(self.kind))
  end
end

@clone_exp+=
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

@simplify_multiplications+=
res = res:simplify()

@simplify_exp+=
elseif self.kind == "mul" then
  local lhs = self.o.lhs:simplify()
  local rhs = self.o.rhs:simplify()

  @collect_rhs_term_in_list
  @collect_lhs_term_in_list

  if #lhs_term > 1 or #rhs_term > 1 then
    @expand_mul
    return res
  end

@methods+=
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

@collect_rhs_term_in_list+=
local rhs_term = rhs:collectTerm()

@collect_lhs_term_in_list+=
local lhs_term = lhs:collectTerm()

@expand_mul+=
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

@methods+=
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

@display_mul_string+=
local factors = self:collectFactors()
local factors_str = {}
local factors_str_list = {}

for i=1,#factors do
  local str = tostring(factors[i])
  factors_str[str] = factors_str[str] or 0
  factors_str[str] = factors_str[str] + 1
end

factors_str_list = vim.tbl_keys(factors_str)
table.sort(factors_str_list)

local str = ""
for i, fac in ipairs(factors_str_list) do
  sup = factors_str[fac]
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
return str

@display_add_string+=
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

@methods+=
function M.cos(x)
  return Exp.new("cos", { arg = x })
end

function M.sin(x)
  return Exp.new("sin", { arg = x })
end

@print_exp+=
elseif self.kind == "cos" then
  return ("cos(%s)"):format(tostring(self.o.arg))
elseif self.kind == "sin" then
  return ("sin(%s)"):format(tostring(self.o.arg))

@methods+=
function M.inf()
  return Exp.new("inf", {})
end

@print_exp+=
elseif self.kind == "inf" then
  return "inf"

@clone_exp+=
elseif self.kind == "cos" then
  return Exp.new(self.kind, { arg = self.o.arg:clone() })
elseif self.kind == "sin" then
  return Exp.new(self.kind, { arg = self.o.arg:clone() })
elseif self.kind == "inf" then
  return Exp.new(self.kind, {})
