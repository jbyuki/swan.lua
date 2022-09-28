##../swan
@collect_terms_for_simplify+=
local terms = self:collect_terms()
for i=1,#terms do
  terms[i] = terms[i]:simplify()
end

@combine_terms_for_simplify+=
local atomics = {}
for _, term in ipairs(terms) do
  local facs = term:collect_factors()
  local const, facs = M.split_kind("constant", facs)
  local coeff = M.reduce_const(const)
  table.sort(facs) 
  @append_coeff_and_facs
end

@methods+=
function M.is_atomic_all(tbl)
  for _, el in ipairs(tbl) do
    if not el:is_atomic() then
      return false
    end
  end
  return true
end

@variables+=
local kind_order = {
  @kind_order
}

@kind_order-=
["constant"] = 1,
["inf"] = 1.5,
["named_constant"] = 2,
["i"] = 3,
["sym"] = 4,
["add"] = 5,
["sub"] = 5,
["mul"] = 6,
["div"] = 7,
["pow"] = 8,
["cos"] = 9,
["sin"] = 10,
["matrix"] = 11,

@metamethods+=
__lt = function(lhs, rhs)
  if lhs.kind ~= rhs.kind then
    return kind_order[lhs.kind] < kind_order[rhs.kind]
  else
    @order_same_kind
    end
  end
  return tostring(lhs) < tostring(rhs)
end,

@order_same_kind-=
if lhs.kind == "constant" then
  return lhs.o.constant < rhs.o.constant

@order_same_kind+=
elseif lhs.kind == "sym" then
  return lhs.o.name < rhs.o.name
elseif lhs.kind == "named_constant" then
  return lhs.o.name < rhs.o.name
elseif lhs.kind == "pow" then
  return lhs.o.lhs < rhs.o.lhs

@metamethods+=
__eq = function(lhs, rhs) 
  if lhs.kind == rhs.kind then
    @compare_exp
    end
  else
    return false
  end
end,

@compare_exp-=
if lhs.kind == "constant" then
  return lhs.o.constant == rhs.o.constant

@compare_exp+=
elseif lhs.kind == "named_constant" then
  return lhs.o.name == rhs.o.name
elseif lhs.kind == "sym" then
  return lhs.o.name == rhs.o.name
elseif lhs.kind == "i" then
  return true
elseif lhs.kind == "pow" then
  return lhs.o.lhs == rhs.o.lhs and lhs.o.rhs == rhs.o.rhs
elseif lhs.kind == "mul" then
  return lhs.o.lhs == rhs.o.lhs and lhs.o.rhs == rhs.o.rhs
elseif lhs.kind == "add" then
  return lhs.o.lhs == rhs.o.lhs and lhs.o.rhs == rhs.o.rhs

@append_coeff_and_facs+=
local added = false
for i, atomic in ipairs(atomics) do
  if M.is_same_all(atomic[2], facs) then
    atomic[1] = atomic[1] + (coeff or 1)
    added = true
    break
  end
end

if not added then
  table.insert(atomics, { (coeff or 1), facs })
end

@methods+=
function M.is_same_all(tbl_a, tbl_b)
  if #tbl_a ~= #tbl_b then
    return false
  end

  for i=1,#tbl_a do
    if tbl_a[i] ~= tbl_b[i] then
      return false
    end
  end
  return true
end

@construct_resulting_addition+=
atomics = vim.tbl_filter(function(atomic)
  return atomic[1] ~= 0
end, atomics)

atomics = vim.tbl_map(function(atomic)
  local rhs = M.reduce_all("mul", atomic[2])
  
  if not rhs then
    return M.constant(atomic[1])
  end

  if atomic[1] == 1 then
    return rhs
  else
    return M.constant(atomic[1]) *  rhs
  end
end, atomics)

local result = M.reduce_all("add", atomics)
result = result or M.constant(0)

return result

@methods+=
function M.reduce_all(kind, tbl, prev)
  for _, elem in ipairs(tbl) do
    if not prev then
      prev = elem
    else
      prev = Exp.new(kind, { lhs = prev, rhs = elem})
    end
  end
  return prev
end
