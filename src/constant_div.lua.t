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

@methods+=
function Exp:is_constant_div()
  return self.kind == "constant_div"
end

@simpify_rhs_constant_div_with_constant_add+=
return M.add_constant_div(lhs / 1, rhs)

@simpify_lhs_constant_div_with_constant_add+=
return M.add_constant_div(lhs, rhs / 1)

@simpify_both_constant_div_add+=
return M.add_constant_div(lhs, rhs)

@methods+=
function M.add_constant_div(lhs, rhs)
	@find_common_denominator
	@add_numerators
	@simplify_numerator_and_denominator_by_gcd
	@return_added_constant_div
end

@find_common_denominator+=
local lhs_den = lhs.o.rhs.o.constant
local rhs_den = rhs.o.rhs.o.constant

local common_den = M.constant(lhs_den * rhs_den)

local lhs_num = lhs.o.lhs.o.constant * rhs_den
local rhs_num = rhs.o.lhs.o.constant * lhs_den

@add_numerators+=
local common_num = M.constant(lhs_num + rhs_num)

@methods+=
function M.find_gcd(A, B)
	if A > B then
		return M.find_gcd(A - B, B)
	elseif B > A then
		return M.find_gcd(A, B - A)
	else
		return A
	end
end

@simplify_numerator_and_denominator_by_gcd+=
if common_den:is_integer() and common_num:is_integer() then
	local gcd = M.find_gcd(common_den.o.constant, common_num.o.constant)
	common_den.o.constant = common_den.o.constant / gcd
	common_num.o.constant = common_num.o.constant / gcd
end

@return_added_constant_div+=
if common_den.o.constant == 1 then
	return common_num
end

return common_num / common_den

