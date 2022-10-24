##swan
@define+=
function Exp:dot(other)
	@both_are_column_vectors
	@mul_and_sum_to_make_dot_product
	return exp
end

@both_are_column_vectors+=
assert(self.kind == "matrix")
assert(other.kind == "matrix")

assert(#self.o.rows == #other.o.rows)

assert(#self.o.rows[1] == 1)
assert(#other.o.rows[1] == 1)


@mul_and_sum_to_make_dot_product+=
local terms = {}
for i=1,#self.o.rows do
	table.insert(terms, self.o.rows[i][1] * other.o.rows[i][1])
end

local exp = M.reduce_all("add", terms)
