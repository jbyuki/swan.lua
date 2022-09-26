##../swan
@collect_terms_for_simplify+=
local terms = self:collect_terms()

@combine_terms_for_simplify+=
local atomics = {}
local rest = {}
for _, term in ipairs(terms) do
  local fac_term = term:collect_factors()
  if M.is_atomic_all(fac_term) then
    local fac_term = M.reorder_factors(fac_term)
    local coeff, facs = M.split_coeffs(fac_term)
    @append_coeff_and_facs
  else
    table.insert(rest, term)
  end
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
local kind_priority = {
  @kind_priority
}

@kind_priority-=
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

@methods+=
-- Mainly designed for atomic factors
function M.reorder_factors(tbl) 
  table.sort(tbl, function(a, b)
    if a.kind ~= b.kind then
      return kind_priority[a.kind] < kind_priority[b.kind]
    else
      @order_same_kind
      end
      return true
    end
  end)
  return tbl
end

@order_same_kind-=
if a.kind == "constant" then
  return a.o.constant < b.o.constant

@order_same_kind+=
elseif a.kind == "sym" then
  return a.o.name < b.o.name
elseif a.kind == "named_constant" then
  return a.o.name < b.o.name

@methods+=
function M.split_coeffs(tbl)
  local coeff = 1
  local facs = {}
  local idx = 1
  for _, term in ipairs(tbl) do
    if term.kind == "constant" then
      coeff = coeff * term.o.constant
      idx = idx + 1
    else
      break
    end
  end

  for i=idx,#tbl do
    table.insert(facs, tbl[i])
  end
  return coeff, facs
end

@methods+=
function Exp:is_same(other) 
  if self.kind == other.kind then
    @compare_exp
    end
    return false
  end
  return false
end

@compare_exp-=
if self.kind == "constant" then
  return self.o.constant == other.o.constant

@compare_exp+=
elseif self.kind == "named_constant" then
  return self.o.name == other.o.name
elseif self.kind == "sym" then
  return self.o.name == other.o.name
elseif self.kind == "i" then
  return true

@append_coeff_and_facs+=
local added = false
for i, atomic in ipairs(atomics) do
  if M.is_same_all(atomic[2], facs) then
    atomic[1] = atomic[1] + coeff
    added = true
    break
  end
end

if not added then
  table.insert(atomics, { coeff, facs })
end

@methods+=
function M.is_same_all(tbl_a, tbl_b)
  if #tbl_a ~= #tbl_b then
    return false
  end

  for i=1,#tbl_a do
    if not tbl_a[i]:is_same(tbl_b[i]) then
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
  if atomic[1] == 1 then
    return rhs
  else
    return Exp.new("mul", { lhs = M.constant(atomic[1]), rhs = rhs })
  end
end, atomics)

local result = M.reduce_all("add", atomics)
result = M.reduce_all("add", rest, result)
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
