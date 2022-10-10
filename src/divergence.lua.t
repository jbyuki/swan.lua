##swan
@define+=
function Exp:divergence()
  @check_type_for_divergence
  @derivate_for_each_coordinates_and_sum
  return exp
end

@check_type_for_divergence+=
assert(self.kind == "matrix", "divergence must be called on a matrix")
assert(self:cols() == 1, "divergence must be called on a matrix with one column")
assert(self:rows() == 3, "divergence must be called on a matrix with three rows")

@derivate_for_each_coordinates_and_sum+=
local sym_coord = { 
  sym_table["x"], sym_table["y"], sym_table["z"] 
}

local terms = {}
for i=1,3 do
  if sym_coord[i] then
    local term
    term = self.o.rows[i][1]:derivate(sym_coord[i])
    table.insert(terms, term)
  end
end

local exp = M.reduce_all("add", terms)
