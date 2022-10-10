##swan
@define+=
function Exp:rotational()
  @check_type_for_rotational
  @sum_to_make_rotational_vector
  return exp
end

@check_type_for_rotational+=
assert(self.kind == "matrix", "rotational must be called on a matrix")
assert(self:cols() == 1, "rotational must be called on a matrix with one column")
assert(self:rows() == 3, "rotational must be called on a matrix with three rows")

@sum_to_make_rotational_vector+=
local Fx = self.o.rows[1][1]
local Fy = self.o.rows[2][1]
local Fz = self.o.rows[3][1]

local sym_x = sym_table["x"]
local sym_y = sym_table["y"]
local sym_z = sym_table["z"]

local dyFz = sym_y and Fz:derivate(sym_y) or M.constant(0)
local dzFy = sym_z and Fy:derivate(sym_z) or M.constant(0)

local dzFx = sym_z and Fx:derivate(sym_z) or M.constant(0)
local dxFz = sym_x and Fz:derivate(sym_x) or M.constant(0)

local dxFy = sym_x and Fy:derivate(sym_x) or M.constant(0)
local dyFx = sym_y and Fx:derivate(sym_y) or M.constant(0)

local exp = M.mat {
  { dyFz - dzFy },
  { dzFx - dxFz },
  { dxFy - dyFx },
}
