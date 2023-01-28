##../swan
@collect_factors_for_simplify+=
local factors = self:collect_factors()

for i=1,#factors do
  factors[i] = factors[i]:simplify()
end

@handle_if_one_factor_is_div

@methods+=
function M.split_kind(kind, facs)
	if type(kind) ~= "table" then
		kind = { kind }
	end

  local first = {}
  local second = {}

  for _, elem in ipairs(facs) do
		local is_first = false
		for _, ki in ipairs(kind) do
			if elem.kind == ki then
				is_first = true
				break
			end
		end

		if is_first then
			table.insert(first, elem)
		else
			table.insert(second, elem)
		end
  end
  return first, second
end

@methods+=
function M.pow_i(num)
  if num % 4 == 0 then
    return 1, nil
  elseif num % 4 == 1 then
    return 1, M.i
  elseif num % 4 == 2 then
    return -1, nil
  elseif num % 4 == 3 then
    return -1, M.i
  end
end

function M.reduce_const(facs) 
  local coeff = 1
  for _, fac in ipairs(facs) do
		if fac.type == "constant" then
			coeff = coeff * fac.o.constant
		elseif fac.type == "constant_div" then
			@reduce_constant_div
		end
  end
  return coeff
end

@combine_factors_for_simplify+=
local elem_i, factors = M.split_kind("i", factors)
local i_const, i_fac = M.pow_i(#elem_i)
local elem_consts, factors = M.split_kind("constant", factors)
table.insert(elem_consts, M.constant(i_const))
local coeff = M.reduce_const(elem_consts)

if coeff == 0 then
  return M.constant(0)
end

@collect_factors_and_classify_with_base
@reconstructor_from_list_of_pow_base
