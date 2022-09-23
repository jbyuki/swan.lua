local swan = require"swan"

local x = swan.sym "x"
local y = swan.sym "y"
local cosx = ((swan.e ^ (swan.i * x) + swan.e ^ (-swan.i * x)))/2
local sinx = ((swan.e ^ (swan.i * x) - swan.e ^ (-swan.i * x)))/(2*swan.i)
-- exp = (cosx*cosx):simplify()
exp = (sinx*sinx):simplify():expand()
num = exp.o.lhs
print(num:simplify():simplify():simplify())
