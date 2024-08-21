## swan.lua

A _work-in-progress_ symbolic math toolbox written in Lua.

### Goal

* Fast manipulation of large polynomials

### Quickstart

```lua
local swan = require"swan"
local a, b = swan.symbols "a b"
local eq = (a+b)^3
eq = eq:expand() 
print(eq) -- => a*a*a + b*a*a + a*b*a + b*b*a + a*a*b + b*a*b + a*b*b + b*b*b
eq = eq:simplify() 
print(eq) -- => a*a*a + 3*a*a*b + 3*a*b*b + b*b*b
```

More to come...
