##../swan
@handle_if_both_pow_same+=
if lhs.kind == "pow" and rhs.kind == "pow" then
  local base_lhs = lhs.o.lhs
  local base_rhs = rhs.o.lhs

  local same_base = false
  @check_if_base_is_same
  end

  if same_base then
    @add_both_sup_and_simplify
    @create_new_pow_with_combined_sup
  end
end

@check_if_base_is_same+=
if base_lhs.kind == "constant" and base_rhs.kind == "constant" then
  same_base = base_lhs.o.constant == base_rhs.o.constant
elseif base_lhs.kind == "named_constant" and base_rhs.kind == "named_constant" then
  same_base = base_lhs.o.name == base_rhs.o.name

@add_both_sup_and_simplify+=
local sup = Exp.new("add", { lhs = lhs.o.rhs:clone(), rhs = rhs.o.rhs:clone() })
sup = sup:simplify()

return Exp.new("pow", { lhs = base_lhs:clone(), rhs = sup })

