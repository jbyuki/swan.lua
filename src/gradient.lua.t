##swan
@define+=
function Exp:gradient()
  @check_type_for_gradient
  @collect_all_unknowns
  @derivate_for_each_row
  return exp
end

@check_type_for_gradient+=
assert(self.kind == "matrix", "gradient must be called on a matrix")
assert(self:cols() == 1, "gradient must be called on a matrix with one column")

@define+=
function Exp:collect_unknowns(unknowns)
  if self.kind == "sym" then
    unknowns[self] = true
  @collect_unknowns_exp
  end
end

@collect_unknowns_exp+=
elseif self.kind == "ln" then
  self.o.arg:collect_unknowns(unknowns)
elseif self.kind == "add" then
  self.o.lhs:collect_unknowns(unknowns)
  self.o.rhs:collect_unknowns(unknowns)
elseif self.kind == "sub" then
  self.o.lhs:collect_unknowns(unknowns)
  self.o.rhs:collect_unknowns(unknowns)
elseif self.kind == "pow" then
  self.o.lhs:collect_unknowns(unknowns)
  self.o.rhs:collect_unknowns(unknowns)
elseif self.kind == "cos" then
  self.o.arg:collect_unknowns(unknowns)
elseif self.kind == "sin" then
  self.o.arg:collect_unknowns(unknowns)
elseif self.kind == "inf" then
elseif self.kind == "named_constant" then
elseif self.kind == "i" then
elseif self.kind == "matrix" then
  for i=1,#self.o.rows do
    for j=1,#self.o.rows[i] do
      self.o.rows[i][j]:collect_unknowns(unknowns)
    end
  end
elseif self.kind == "div" then
  self.o.lhs:collect_unknowns(unknowns)
  self.o.rhs:collect_unknowns(unknowns)
elseif self.kind == "mul" then
  self.o.lhs:collect_unknowns(unknowns)
  self.o.rhs:collect_unknowns(unknowns)

@collect_all_unknowns+=
local unknowns = {}
self:collect_unknowns(unknowns)

@derivate_for_each_row+=
unknowns = vim.tbl_keys(unknowns)
table.sort(unknowns, function(a, b)
  return a.o.name < b.o.name
end)

local rows = {}
for i=1,#self.o.rows do
  local row = {}
  for j=1,#unknowns do
    table.insert(row, self.o.rows[i][1]:derivate(unknowns[j]))
  end
  table.insert(rows, row)
end

local exp = Exp.new("matrix", { rows = rows })
