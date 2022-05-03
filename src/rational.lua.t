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
