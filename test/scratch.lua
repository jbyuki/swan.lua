local swan = require"swan"

-- x = swan.sym "x"
x = swan.sym "x"
A = swan.mat {
  {x, 2}, 
  {3, 4}
}
B = swan.mat {
  {2}, 
  {3}
}
C = A*B
print(C:simplify():T())
