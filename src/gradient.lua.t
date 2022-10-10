##swan
@define+=
function Exp:gradient()
  @collect_all_unknowns
  local exp
  if self.kind == "matrix" then
    @derivate_for_each_row
  else
    @derivate_for_each_unknown
  end
  return exp
end

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

unknowns = vim.tbl_keys(unknowns)
table.sort(unknowns, function(a, b)
  return a.o.name < b.o.name
end)

@derivate_for_each_row+=
local rows = {}
for i=1,#self.o.rows do
  local row = {}
  for j=1,#unknowns do
    table.insert(row, self.o.rows[i][1]:derivate(unknowns[j]))
  end
  table.insert(rows, row)
end

exp = Exp.new("matrix", { rows = rows })

@derivate_for_each_unknown+=
local rows = {}
for j=1,#unknowns do
  table.insert(rows, { self:derivate(unknowns[j])} )
end

exp = Exp.new("matrix", { rows = rows })
