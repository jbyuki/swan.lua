local swan = require"swan"

local x = swan.sym "x"
local exp = x + x
exp = exp:simplify()
exp = exp:expand()
print(exp)
