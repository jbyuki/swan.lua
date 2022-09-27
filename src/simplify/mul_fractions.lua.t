##../swan
@handle_mul_both_div+=
if lhs.kind == "div" and rhs.kind == "div" then
  return (lhs.o.lhs * rhs.o.lhs):simplify() / (lhs.o.rhs * rhs.o.rhs):simplify()
end
