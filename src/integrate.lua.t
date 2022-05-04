##swan
@methods+=
function Exp:integrate(dx)
  if false then
  @simplify_exp
  else
    print("Unsupported!")
  end
end

@simplify_exp+=
elseif self.kind == "add" then
  local rhs = self.o.rhs:integrate(dx)
  local lhs = self.o.lhs:integrate(dx)

  return Exp.new("add", { rhs = rhs, lhs = lhs })

@simplify_exp+=
elseif self.kind == "sym" then
  if self == dx then
    pow_exp = Exp.new("pow", { lhs = dx, rhs = M.constant(2) })
    coeff = Exp.new("div", { lhs = M.constant(1), rhs = M.constant(2) })
    return Exp.new("mul", { lhs = coeff, rhs = pow_exp })
  else
    return Exp.new("mul", { lhs = self, rhs = dx })
  end
