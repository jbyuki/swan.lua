## swan.lua

A _work-in-progress_ symbolic math toolbox written in Lua.

This is written as a Neovim plugin ~~but this should be usable as a standalone
Lua library~~.

### Quickstart

```lua
local swan = require"swan"
x = swan.sym "x"
y = swan.sym "y"
exp = (x+y)^2
print(exp:expand()) -- => x^2 + 2xy + y^2

exp = x*y + x
print(exp:derivate(x)) -- => 1 + y

A = swan.mat {
  {x, 2}, 
  {3, 4}
}
B = swan.mat {
  {2, 5}, 
  {3, x}
}
C = A*B
print(C:simplify()) -- => [
                    --      2x + 6, 2x + 5x
                    --      18, 15 + 4x
                    --    ]

print(x:integrate(x)) -- => (1/2)x^2
```

More to come...

### Tests

[test/scratch.lua](https://github.com/jbyuki/swan.lua/blob/master/test/scratch.lua)
