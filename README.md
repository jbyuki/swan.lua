## swan.lua

A _work-in-progress_ symbolic math toolbox written in Lua for learning purposes.

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

Also see [scratch.lua.t2](test/scratch.lua.t2).

[^1]: [Cox, David, et al. Ideals, varieties, and algorithms. Vol. 3. New York: Springer, 1997.](https://link.springer.com/book/10.1007/978-3-319-16721-3)
