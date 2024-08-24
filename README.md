## swan.lua

A _work-in-progress_ symbolic math toolbox written in Lua.

### Goal

* Fast manipulation of large polynomials

### Quickstart

```lua
local swan = require"swan"
local a, b = swan.symbols "a b"
local eq = (a+b)^4
eq = eq:expand():simplify()
print(eq) -- => a⁴ + 4a³b + 6a²b² + 4ab³ + b⁴
```

More to come...
