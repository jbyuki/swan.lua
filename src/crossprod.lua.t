##swan
@define+=
function Exp:cross(other)
	@both_are_column_vectors
	@both_have_3_rows
	@create_cross_prod_vector
end

@both_have_3_rows+=
assert(#self.o.rows == 3)

@create_cross_prod_vector+=
local rows = {}
table.insert(rows, { self.o.rows[2][1]:clone() * other.o.rows[3][1]:clone() - self.o.rows[3][1]:clone() * other.o.rows[2][1]:clone() })
table.insert(rows, { self.o.rows[3][1]:clone() * other.o.rows[1][1]:clone() - self.o.rows[1][1]:clone() * other.o.rows[3][1]:clone() })
table.insert(rows, { self.o.rows[1][1]:clone() * other.o.rows[2][1]:clone() - self.o.rows[2][1]:clone() * other.o.rows[1][1]:clone() })

return Exp.new("matrix", { rows = rows })
