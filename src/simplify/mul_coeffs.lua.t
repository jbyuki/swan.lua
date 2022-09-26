##../swan
@collect_factors_for_simplify+=
local factors = self:collect_factors()

for i=1,#factors do
  factors[i] = factors[i]:simplify()
end

@methods+=
function M.split_i(facs)
  local rest = {}
  local num_i = 0
  for _, elem in ipairs(facs) do
    if elem.kind == "i" then
      num_i = num_i + 1
    else
      table.insert(rest, elem)
    end
  end

  local fac_i = M.pow_i(num_i)
  return fac_i, rest
end

@methods+=
function M.pow_i(num)
  local fac_i = nil
  if num % 4 == 1 then
    fac_i = M.i
  elseif num % 4 == 2 then
    fac_i = M.constant(-1)
  elseif num % 4 == 3 then
    fac_i = M.constant(-1) * M.i
  end
  return fac_i
end

@combine_factors_for_simplify+=
local coeff, factors = M.split_coeffs(factors)
local coeff_i, factors = M.split_i(factors)
if coeff_i then
  coeff, coeff_i = M.split_coeffs({M.constant(coeff), coeff_i})
  coeff_i = coeff_i[1]
end

@collect_factors_and_classify_with_base

local result = nil
result = M.reduce_all("mul", list_fac)
if coeff_i then
  result = M.reduce_all("mul", {coeff_i}, result)
end

if coeff ~= 1 then
  result = M.reduce_all("mul", {M.constant(coeff)}, result)
end

result = result or M.constant(1)
return result
