##../swan
@simplify_exp+=
elseif self.kind == "pow" then
  local lhs = self.o.lhs:simplify()
  local rhs = self.o.rhs:simplify()

  if lhs.kind == "constant" and rhs.kind == "constant" then
    return M.constant(lhs.o.constant ^ rhs.o.constant)
  elseif lhs.kind == "i" and rhs.kind == "constant" and rhs:is_integer() then
    return M.pow_i(rhs.o.constant) or M.constant(1)
  elseif rhs.kind == "constant" and rhs:is_integer() then
    if rhs.o.constant == 0 then
      return M.constant(1)
    elseif rhs.o.constant == 1 then
      return lhs
    end
  end

  return lhs ^ rhs
