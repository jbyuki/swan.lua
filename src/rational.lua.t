##swan
@define+=
function M.frac(num, den)
  num = convert_constant(num)
  den = convert_constant(den)
  return Exp.new("div", { lhs = num, rhs = den })
end

@print_exp+=
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

@methods+=
function Exp:is_integer()
  return self.kind == "constant" and math.floor(self.o.constant) == self.o.constant
end

@declare+=
local gcd

@define+=
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

@simplify_exp+=
elseif self.kind == "div" then
  local lhs = self.o.lhs:simplify()
  local rhs = self.o.rhs:simplify()

  @if_zero_divided_by_something_simplify

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

@clone_exp+=
elseif self.kind == "div" then
  return Exp.new(self.kind, { lhs = self.o.lhs:clone(), rhs = self.o.rhs:clone() })


@metamethods+=
__div = function(lhs, rhs)
  lhs = convert_constant(lhs)
  rhs = convert_constant(rhs)

	@create_constant_div_if_both_constants

  return Exp.new("div", { lhs = lhs, rhs = rhs })
end,

@if_zero_divided_by_something_simplify+=
if lhs:is_zero() then
  return M.constant(0)
end
