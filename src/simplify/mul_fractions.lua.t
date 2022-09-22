##../swan
@handle_mul_simplify+=
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
