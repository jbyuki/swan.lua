## swan.lua

A _work-in-progress_ symbolic math toolbox written in Lua.

This is written as a Neovim plugin but this should be usable as a standalone
Lua library.

### Quickstart

```lua
local swan = require"swan"
x = swan.sym "x"
y = swan.sym "y"
exp = (x+y)^2
print(exp:simplify()) -- => x^2 + 2xy + y^2
```

More to come...

### Tests

[test/scratch.lua](https://github.com/jbyuki/swan.lua/blob/master/test/scratch.lua)
