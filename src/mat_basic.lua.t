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

@simplify_exp-=
if self.kind == "mul" then
  local lhs = self.o.lhs
  local rhs = self.o.rhs

  if lhs:is_matrix() and rhs:is_matrix() then
    lhs = self.o.lhs:simplify()
    rhs = self.o.rhs:simplify()

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

  @handle_rhs_coeff_mul_matrix
  @handle_lhs_coeff_mul_matrix

	if lhs:is_constant() and rhs:is_constant() then
		return M.constant(lhs.o.constant * rhs.o.constant)
	elseif lhs:is_constant() and rhs:is_constant_div() then
		@simpify_rhs_constant_div_with_constant_mul
	elseif lhs:is_constant_div() and rhs:is_constant() then
		@simpify_lhs_constant_div_with_constant_mul
	elseif lhs:is_constant_div() and rhs:is_constant_div() then
		@simpify_both_constant_div_mul
	end

  -- @handle_if_one_is_pow
  -- @handle_mul_simplify

  @collect_factors_for_simplify
  @combine_factors_for_simplify
  @reconstruct_factor_for_simplify

@compute_mul_cell_matrix+=
local mul_exp = Exp.new("mul", { lhs = lhs.o.rows[i][k]:clone(), rhs = rhs.o.rows[k][j]:clone() })

if not cell then
  cell = mul_exp
else
  cell = Exp.new("add", { lhs = cell, rhs = mul_exp })
end

@simplify_exp+=
elseif self.kind == "add" then
  local lhs = self.o.lhs
  local rhs = self.o.rhs

  if lhs:is_matrix() and rhs:is_matrix() then
    lhs = lhs:simplify()
    rhs = rhs:simplify()
    @check_that_both_matrices_are_same_dimensions
    @add_matrix_elementwise
    return Exp.new("matrix", { rows = rows })
	elseif lhs:is_constant() and rhs:is_constant() then
		return M.constant(lhs.o.constant + rhs.o.constant)
	elseif lhs:is_constant() and rhs:is_constant_div() then
		@simpify_rhs_constant_div_with_constant_add
	elseif lhs:is_constant_div() and rhs:is_constant() then
		@simpify_lhs_constant_div_with_constant_add
	elseif lhs:is_constant_div() and rhs:is_constant_div() then
		@simpify_both_constant_div_add
  else
    @collect_terms_for_simplify
    @combine_terms_for_simplify
    @construct_resulting_addition
  end

  return Exp.new("add", { lhs = lhs, rhs = rhs })

@handle_rhs_coeff_mul_matrix+=
if rhs:is_matrix() then
  local rows = {}
  for i=1,rhs:rows() do
    local row = {}
    for j=1,rhs:cols() do
      local mul_exp = Exp.new("mul", { lhs = lhs:clone(), rhs = rhs.o.rows[i][j]:clone() })
      table.insert(row, mul_exp:simplify())
    end
    table.insert(rows, row)
  end

  return Exp.new("matrix", { rows = rows })
end

@handle_lhs_coeff_mul_matrix+=
if lhs:is_matrix() then
  local rows = {}
  for i=1,lhs:rows() do
    local row = {}
    for j=1,lhs:cols() do
      local mul_exp = Exp.new("mul", { rhs = rhs:clone(), lhs = lhs.o.rows[i][j]:clone() })
      table.insert(row, mul_exp:simplify())
    end
    table.insert(rows, row)
  end

  return Exp.new("matrix", { rows = rows })
end

@methods+=
function Exp:T()
  assert(self:is_matrix(), "T() argument must be a matrix.")

  local rows = {}
  for i = 1,#self.o.rows[1] do
    local row = {}
    for j = 1,#self.o.rows do
      table.insert(row, self.o.rows[j][i]:clone())
    end
    table.insert(rows, row)
  end
  return Exp.new("matrix", { rows = rows })
end

@check_that_both_matrices_are_same_dimensions+=
assert(lhs:cols() == rhs:cols(), "Matrix addition dimensions mismatch")
assert(lhs:rows() == rhs:rows(), "Matrix addition dimensions mismatch")

@add_matrix_elementwise+=
local rows = {}
for i = 1,rhs:rows() do
  local row = {}
  for j = 1,lhs:cols() do
    local cell = Exp.new("add", {
      lhs = lhs.o.rows[i][j]:clone(), 
      rhs = rhs.o.rows[i][j]:clone() })
    table.insert(row, cell:simplify())
  end
  table.insert(rows, row)
end

@simplify_exp+=
elseif self.kind == "matrix" then
  local rows = {}
  for i=1,#self.o.rows do
    local row = {}
    for j=1,#self.o.rows[i] do
      table.insert(row, self.o.rows[i][j]:simplify())
    end
    table.insert(rows, row)
  end
  return Exp.new("matrix", { rows = rows })

@methods+=
function Exp:mul_elem(other)
  @check_types_for_mul_elem
  @multiply_matrices_elementwise
  return exp
end

@check_types_for_mul_elem+=
assert(self.kind == "matrix", "lhs for mul_elem must be a matrix")
assert(other.kind == "matrix", "rhs for mul_elem must be a matrix")
assert(self:rows() == other:rows(), "lhs and rhs must have the same number of rows for mul_elem()")
assert(self:cols() == other:cols(), "lhs and rhs must have the same number of columns for mul_elem()")

@multiply_matrices_elementwise+=
local rows = {}
for i=1,#self.o.rows do
  local row = {}
  for j=1,#self.o.rows[i] do
    table.insert(row, self.o.rows[i][j] * other.o.rows[i][j])
  end
  table.insert(rows, row)
end

local exp = Exp.new("matrix", { rows = rows })
