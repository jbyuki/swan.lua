## swan.lua

A _work-in-progress_ symbolic math toolbox written in Lua.

### Quickstart

```lua
local swan = require"swan"
local a, b = swan.symbols "a b"
local eq = (a+b)^2
eq = eq:expand()
print(eq) -- => a*a + b*a + a*b + b*b
```

More to come...
