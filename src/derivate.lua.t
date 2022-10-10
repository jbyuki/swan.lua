##swan
@methods+=
function Exp:derivate(dx)
  @derivate_exp
  else
    print("ERROR! Cannot derivate of " .. self.kind)
  end
  return exp
end

@derivate_exp-=
if self.kind == "constant" then
  return Exp.new("constant", { constant = 0 })

@derivate_exp+=
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
  @special_case_for_exp_differentation

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

@methods+=
function Exp:is_one()
  return self.kind == "constant" and self.o.constant == 1
end

function Exp:is_zero()
  return self.kind == "constant" and self.o.constant == 0
end

function Exp:is_constant()
  return self.kind == "constant"
end

@derivate_exp+=
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

@derivate_exp+=
elseif self.kind == "sin" then
  return M.cos(self.o.arg:clone()) * self.o.arg:derivate(dx)

@derivate_exp+=
elseif self.kind == "cos" then
  return M.constant(-1) * M.sin(self.o.arg:clone()) * self.o.arg:derivate(dx)

@derivate_exp+=
elseif self.kind == "ln" then
  return self.o.arg:derivate(dx) / self.o.arg:clone()

@special_case_for_exp_differentation+=
if self.o.lhs.kind == "named_constant" and self.o.lhs.o.name == "e" then
  return self:clone() * self.o.rhs:derivate(dx)
end
