##swan
@define+=
function M.mat(array)
  @convert_array_elements_to_exp
  @create_matrix_exp
  return exp
end

@convert_array_elements_to_exp+=
assert(type(array) == "table", "Argument array must be table")

for i=1,#array do
  assert(type(array[i]) == "table", "Argument array elements must be table")
  for j=1,#array[i] do
    local el = array[i][j]
    if type(el) == "number" then
      array[i][j] = Exp.new("constant", { constant = el })
    end
  end
end

@create_matrix_exp+=
local exp = Exp.new("matrix", { rows = array })

@print_exp+=
elseif self.kind == "matrix" then
  local rows = {}
  for i=1,#self.o.rows do
    local row = {}
    for j=1,#self.o.rows[i] do
      table.insert(row, tostring(self.o.rows[i][j]))
    end
    table.insert(rows, "  " .. table.concat(row, ", "))
  end
  return "[\n" .. table.concat(rows, "\n") .. "\n]"

@clone_exp+=
elseif self.kind == "matrix" then
  local rows = {}
  for i=1,#self.o.rows do
    local row = {}
    for j=1,#self.o.rows[i] do
      table.insert(row, self.o.rows[i][j]:clone())
    end
    table.insert(rows, row)
  end

  return Exp.new("matrix", { rows = rows })

@methods+=
function Exp:simplify()
  if false then
  @simplify_exp
  end
  return self:clone()
end

@methods+=
function Exp:is_matrix()
  return self.kind == "matrix"
end

function Exp:rows()
  assert(self.kind == "matrix", "rows must be called on a matrix")
  return #self.o.rows
end

function Exp:cols()
  assert(self.kind == "matrix", "cols must be called on a matrix")
  return #self.o.rows[1]
end

@simplify_exp+=
elseif self.kind == "mul" then
  local lhs = self.o.lhs:simplify()
  local rhs = self.o.rhs:simplify()

  if lhs:is_constant() and rhs:is_constant() then
    return M.constant(lhs.o.constant * rhs.o.constant)
  end

  if lhs:is_matrix() and rhs:is_matrix() then
    assert(lhs:cols() == rhs:rows(), "Matrix multiplication dimensions mismatch")

    local rows = {}
    -- lhs:rows is only called once btw
    for i=1,lhs:rows() do
      local row = {}
      for j=1,rhs:cols() do
        local cell
        for k=1,rhs:rows() do
          @compute_mul_cell_matrix
        end
        table.insert(row, cell:simplify())
      end
      table.insert(rows, row)
    end

    return Exp.new("matrix", { rows = rows })
  end

@compute_mul_cell_matrix+=
local mul_exp = Exp.new("mul", { lhs = lhs.o.rows[i][k], rhs = rhs.o.rows[k][j] })

if not cell then
  cell = mul_exp
else
  cell = Exp.new("add", { lhs = cell, rhs = mul_exp })
end

@simplify_exp+=
elseif self.kind == "add" then
  local lhs = self.o.lhs:simplify()
  local rhs = self.o.rhs:simplify()

  if lhs:is_constant() and rhs:is_constant() then
    return M.constant(lhs.o.constant + rhs.o.constant)
  end
