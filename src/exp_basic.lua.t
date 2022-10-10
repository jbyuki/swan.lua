##swan
@define+=
function M.sym(name)
  return Exp.new("sym", { name = name })
end

@variables+=
local Exp = {}

@metatables+=
local mt = { __index = Exp,
  @metamethods
}

@methods+=
function Exp.new(kind, opts)
  local o = { kind = kind, o = opts }
  return setmetatable(o, mt)
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
  rhs = Exp.new("mul", { lhs = M.constant(-1), rhs = rhs })
  return Exp.new("add", { lhs = lhs, rhs = rhs })
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

@print_exp-=
if self.kind == "sym" then
  return self.o.name

@print_exp+=
elseif self.kind == "add" then
  @display_add_string
elseif self.kind == "sub" then
  return ("%s - %s"):format(tostring(self.o.lhs), tostring(self.o.rhs))
elseif self.kind == "mul" then
  @display_mul_string
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

@methods+=
function Exp:expand()
  @expand_exp
  else
    return self:clone()
  end
end

@expand_exp-=
if self.kind:is_atomic() then
  return self.clone()

@expand_exp+=
elseif self.kind == "pow" then
  local lhs = self.o.lhs:expand()
  local rhs = self.o.rhs:expand()

  -- Just compute if both are constants
  if rhs.kind == "constant" and lhs.kind == "constant" then
    return Exp.new("constant", { constant = lhs.o.constant ^ rhs.o.constant })
  end

  @put_pow_inside_if_minus

  if rhs.kind == "constant" then
    local sup = rhs.o.constant

    -- Check if it is an integer

    if sup == math.floor(sup) and sup >= 0 then

      -- Transform to a series of multiplication
      @transform_pow_to_multiplication
      @expand_multiplications
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

@clone_exp-=
if self.kind == "constant" then
  return Exp.new(self.kind, { constant = self.o.constant })

@clone_exp+=
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

@expand_multiplications+=
res = res:expand()

@expand_exp+=
elseif self.kind == "mul" then
  local lhs = self.o.lhs:expand()
  local rhs = self.o.rhs:expand()

  @collect_rhs_term_in_list
  @collect_lhs_term_in_list

  if #lhs_term > 1 or #rhs_term > 1 then
    @expand_mul
    return res
  end

  return self:clone()

@methods+=
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

@collect_rhs_term_in_list+=
local rhs_term = rhs:collect_terms()

@collect_lhs_term_in_list+=
local lhs_term = lhs:collect_terms()

@expand_mul+=
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

@methods+=
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

@display_mul_string+=
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


@methods+=
function Exp:is_atomic()
  return self.kind == "sym" or 
    self.kind == "constant" or 
    self.kind == "inf" or 
    self.kind == "named_constant" or
    self.kind == "i"
end

@display_add_string+=
local lhs = tostring(self.o.lhs)
local rhs = tostring(self.o.rhs)

return lhs .. " + " .. rhs

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

@metamethods+=
__mul = function(lhs, rhs)
  @convert_lhs_or_rhs_to_constant
  return Exp.new("mul", { lhs = lhs, rhs = rhs })
end,


@define+=
function M.constant(num)
  return Exp.new("constant", { constant = num })
end

@define+=
function M.named_constant(name)
  return Exp.new("named_constant", { name = name })
end

@define_named_constants

@define_named_constants+=
M.e = M.named_constant("e")
M.pi = M.named_constant("pi")

@print_exp+=
elseif self.kind == "named_constant" then
  return self.o.name

@clone_exp+=
elseif self.kind == "named_constant" then
  return self

@metamethods+=
__unm = function(lhs)
  return Exp.new("mul", { 
    lhs = M.constant(-1),
    rhs = lhs
  })
end,

@expand_exp+=
elseif self.kind == "div" then
  local lhs = self.o.lhs:expand()
  local rhs = self.o.rhs:expand()

  return Exp.new("div", { lhs = lhs, rhs = rhs })

@expand_exp+=
elseif self.kind == "add" then
  local lhs = self.o.lhs:expand()
  local rhs = self.o.rhs:expand()

  return Exp.new("add", { lhs = lhs, rhs = rhs })
