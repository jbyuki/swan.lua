local swan = require"swan"
x = swan.sym "x"
y = swan.sym "y"
exp = x*y + x
exp = exp:derivate(x)
print(exp)
