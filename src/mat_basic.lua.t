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
