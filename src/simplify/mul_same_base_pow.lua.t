##../swan
@collect_factors_and_classify_with_base+=
local pow_base = {}
local list_fac = {}

for _, fac in ipairs(factors) do
  local lhs, rhs
  if fac.kind == "pow" then
    lhs = fac.o.lhs
    rhs = fac.o.rhs
  else
    lhs = fac
    rhs = nil
  end

  @append_to_same_base
  @if_same_base_not_found_just_append
end

@append_to_same_base+=
local found = false
for _, elem in ipairs(pow_base) do
  if elem[1] == lhs then
    elem[2] = elem[2] or M.constant(1)
    elem[2] = (elem[2] or M.constant(1)) + (rhs or M.constant(1))
    found = true
    break
  end
end

@if_same_base_not_found_just_append+=
if not found then
  table.insert(pow_base, { lhs, rhs })
end

@reconstructor_from_list_of_pow_base+=
local result = nil
for _, elem in ipairs(pow_base) do
  local new_elem
  if not elem[2] then
    new_elem = elem[1]
  else
    new_elem = elem[1] ^ (elem[2]:simplify())
  end
  result = (result and (result * new_elem)) or new_elem
end

if i_fac then
  result = result and i_fac * result or i_fac
end
if not coeff:is_one() then
  result = result and coeff * result or coeff
end
result = result or M.constant(1)
return result
