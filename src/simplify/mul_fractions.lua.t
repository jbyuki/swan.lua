##../swan
@handle_if_one_factor_is_div+=
local has_div = false
for _, factor in ipairs(factors) do
  if factor.kind == "div" then
    has_div = true
    break
  end
end

if has_div then
  @collect_num_and_den
  @simplify_separately_num_and_den
end

@collect_num_and_den+=
local num = {}
local den = {}

for _, factor in ipairs(factors) do
  if factor.kind == "div" then
    table.insert(num, factor.o.lhs)
    table.insert(den, factor.o.rhs)
  else
    table.insert(num, factor)
  end
end

num = M.reduce_all("mul", num)
den = M.reduce_all("mul", den)


@simplify_separately_num_and_den+=
return num:simplify() / den:simplify()
