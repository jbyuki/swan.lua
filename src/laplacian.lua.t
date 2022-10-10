##swan
@define+=
function Exp:laplacian()
  @sum_to_make_laplacian
  return exp
end

@sum_to_make_laplacian+=
local sym_coord = { 
  sym_table["x"], sym_table["y"], sym_table["z"] 
}

local terms = {}
for i=1,3 do
  if sym_coord[i] then
    local term
    term = self:derivate(sym_coord[i]):derivate(sym_coord[i])
    table.insert(terms, term)
  end
end

local exp = M.reduce_all("add", terms)
