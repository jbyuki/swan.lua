##swan
@create_constant_div_if_both_constants+=
if lhs:is_constant() and rhs:is_constant() then
  return Exp.new("constant_div", { lhs = lhs, rhs = rhs })
end

@print_exp+=
elseif self.kind == "constant_div" then
  local lhs = tostring(self.o.lhs)
  local rhs = tostring(self.o.rhs)
  return lhs .. "/" .. rhs

@clone_exp+=
elseif self.kind == "constant_div" then
  return Exp.new(self.kind, { lhs = self.o.lhs:clone(), rhs = self.o.rhs:clone() })
