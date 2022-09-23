##../swan
@handle_if_one_is_pow+=
if lhs.kind == "pow" or rhs.kind == "pow" then
  @collect_factors_and_classify_with_base
  @output_result_simplify_same_base
end

@collect_factors_and_classify_with_base+=
local factors = self:collectFactors()
local pow_base = {}
local list_fac = {}

for _, fac in ipairs(factors) do
  fac = fac:expand()
  if fac.kind == "pow" and fac.o.lhs:is_atomic() then
    local lhs = fac.o.lhs
    if not pow_base[lhs.kind] then
      pow_base[lhs.kind] = {}
    end
    local tpow_base = pow_base[lhs.kind]
    @append_to_same_base
    else
      table.insert(list_fac, fac)
    end
  else
    table.insert(list_fac, fac)
  end
end

@append_to_same_base-=
if lhs.kind == "constant" then
  c = lhs.o.constant
  if tpow_base[c] then
    @add_to_already_existing_factor_constant
  else
    tpow_base[c] = fac
    table.insert(list_fac, fac)
  end

@add_to_already_existing_factor_constant+=
tpow_base[c].o.rhs = Exp.new("add", {lhs=tpow_base[c].o.rhs, rhs=fac.o.rhs}):simplify()

@append_to_same_base+=
elseif lhs.kind == "named_constant" then
  n = lhs.o.name
  if tpow_base[n] then
    @add_to_already_existing_factor_named_constant
  else
    tpow_base[n] = fac
    table.insert(list_fac, fac)
  end

@add_to_already_existing_factor_named_constant+=
tpow_base[n].o.rhs = Exp.new("add", {lhs=tpow_base[n].o.rhs, rhs=fac.o.rhs}):simplify()

@methods+=
function M.reduce_mul(exp_list)
  local result = nil
  for _, exp in ipairs(exp_list) do
    if not result then
      result = exp
    else
      result = Exp.new("mul", {lhs = result, rhs = exp})
    end
  end
  return result
end

@output_result_simplify_same_base+=
return M.reduce_mul(list_fac)
