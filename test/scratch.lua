local swan = require"swan"

x = swan.sym "x"
a = x + x
print(x:integrate(x))
