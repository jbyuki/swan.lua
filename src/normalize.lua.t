##swan
@methods+=
function Exp:normalize()
  @normalize_exp
  end
  return self:clone()
end

@normalize_exp-=
if self:is_atomic() then
  return self:clone()

@normalize_exp+=
elseif self.kind == "mul" then
  local facs = self:collect_factors()
  for i=1,#facs do
    facs[i] = facs[i]:normalize()
  end
  table.sort(facs)
  return M.reduce_all("mul", facs)

elseif self.kind == "add" then
  local terms = self:collect_terms()
  for i=1,#terms do
    terms[i] = terms[i]:normalize()
  end
  table.sort(terms)
  return M.reduce_all("add", terms)
  
elseif self.kind == "pow" then
  return self.o.lhs:normalize() ^ self.o.rhs:normalize()

elseif self.kind == "div" then
  return self.o.lhs:normalize() / self.o.rhs:normalize()

@normalize_exp+=
elseif self.kind == "matrix" then
  local rows = {}
  for i=1,#self.o.rows do
    local row = {}
    for j=1,#self.o.rows[i] do
      table.insert(row, self.o.rows[i][j]:normalize())
    end
    table.insert(rows, row)
  end
  return Exp.new("matrix", { rows = rows })
