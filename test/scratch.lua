local swan = require"swan"
x = swan.sym "x"
y = swan.sym "y"
exp = (x+y)^2
exp = exp:expand()
print(exp)
