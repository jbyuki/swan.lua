## swan.lua

A _work-in-progress_ symbolic math toolbox written in Lua.

### Goals

* Fast manipulation of large polynomials
* Implement algorithms from [^1].

### Quickstart

```lua
local swan = require"swan"
local a, b = swan.symbols "a b"
local eq = (a+b)^4
eq = eq:expand():simplify()
print(eq) -- => a⁴ + 4a³b + 6a²b² + 4ab³ + b⁴
```

More to come...

[^1]: Cox, David, et al. Ideals, varieties, and algorithms. Vol. 3. New York: Springer, 1997.
