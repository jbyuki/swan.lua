-- Generated using ntangle.nvim
local M = {}
local constant_set = {}
M.constant_set = constant_set

local exp_methods = {}
local exp_mt = {}
local exp = {}
exp_mt.__index = exp_methods

local extendable = {}

local create_left_paren, create_right_paren

local grid_methods = {}
local grid_mt = {}
grid_mt.__index = grid_methods
local grid = {}

local matrix_set = {}
M.matrix_set = matrix_set


local mat = {}
M.mat = mat

local sym_methods = {}
local sym_mt = {}

local real_set = {}
M.real_set = real_set

local greek_etc = {
  ["Alpha"] = "Α", ["Beta"] = "Β", ["Gamma"] = "Γ", ["Delta"] = "Δ", ["Epsilon"] = "Ε", ["Zeta"] = "Ζ", ["Eta"] = "Η", ["Theta"] = "Θ", ["Iota"] = "Ι", ["Kappa"] = "Κ", ["Lambda"] = "Λ", ["Mu"] = "Μ", ["Nu"] = "Ν", ["Xi"] = "Ξ", ["Omicron"] = "Ο", ["Pi"] = "Π", ["Rho"] = "Ρ", ["Sigma"] = "Σ", ["Tau"] = "Τ", ["Upsilon"] = "Υ", ["Phi"] = "Φ", ["Chi"] = "Χ", ["Psi"] = "Ψ", ["Omega"] = "Ω",
  ["alpha"] = "α", ["beta"] = "β", ["gamma"] = "γ", ["delta"] = "δ", ["epsilon"] = "ε", ["zeta"] = "ζ", ["eta"] = "η", ["theta"] = "θ", ["iota"] = "ι", ["kappa"] = "κ", ["lambda"] = "λ", ["mu"] = "μ", ["nu"] = "ν", ["xi"] = "ξ", ["omicron"] = "ο", ["pi"] = "π", ["rho"] = "ρ", ["final"] = "ς", ["sigma"] = "σ", ["tau"] = "τ", ["upsilon"] = "υ", ["phi"] = "φ", ["chi"] = "χ", ["psi"] = "ψ", ["omega"] = "ω",
  ["nabla"] = "∇",
}

local EXP_TYPE = {
  ADD = 1,
  MUL = 2,

}

local sub_letters = { 
	["+"] = "₊", ["-"] = "₋", ["="] = "₌", ["("] = "₍", [")"] = "₎",
	["a"] = "ₐ", ["e"] = "ₑ", ["o"] = "ₒ", ["x"] = "ₓ", ["ə"] = "ₔ", ["h"] = "ₕ", ["k"] = "ₖ", ["l"] = "ₗ", ["m"] = "ₘ", ["n"] = "ₙ", ["p"] = "ₚ", ["s"] = "ₛ", ["t"] = "ₜ", ["i"] = "ᵢ", ["j"] = "ⱼ", ["r"] = "ᵣ", ["u"] = "ᵤ", ["v"] = "ᵥ",
	["0"] = "₀", ["1"] = "₁", ["2"] = "₂", ["3"] = "₃", ["4"] = "₄", ["5"] = "₅", ["6"] = "₆", ["7"] = "₇", ["8"] = "₈", ["9"] = "₉",
}

function constant_set:new(value)
  local set = {}
  set.value = value
  return setmetatable(set, { __index = self })
end

function matrix_set:new(m,n)
  local set = {}
  set.m = m
  set.n = n
  set.elems = {}
  for i=1,m do
    set.elems[i] = {}
  end

  return setmetatable(set, { __index = self })
end

function real_set:__tostring(sym)
  return self.name
end

function constant_set:__tostring()
  return self.value
end

function M.constant(value)
  local set = constant_set:new(value)
  local sym = {}
  sym.name = name
  sym.set = set
  setmetatable(sym, {
    __index = sym_mt.__index,
    __tostring = set.__tostring,
    __call = set.__call,

    __add = sym_mt.__add,
    __mul = sym_mt.__mul,
  })
  return sym
end
M.c = M.constant

function exp.new(children, type)
  local e = {}
  e.type = type
  e.children = children
  return setmetatable(e, exp_mt)
end

function exp_mt:__add(other)
  local new_children = {}
  for _, elem in ipairs({self, other}) do
    if elem.type and elem.type == EXP_TYPE.ADD then
      for _, child in ipairs(elem.children) do
        table.insert(new_children, child)
      end
    else
      table.insert(new_children, elem)
    end
  end

  return exp.new(new_children, EXP_TYPE.ADD)
end

function exp_mt:__mul(other)
  local new_children = {}
  for _, elem in ipairs({self, other}) do
    if elem.type and elem.type == EXP_TYPE.MUL then
      for _, child in ipairs(elem.children) do
        table.insert(new_children, child)
      end
    else
      table.insert(new_children, elem)
    end
  end

  return exp.new(new_children, EXP_TYPE.MUL)
end

function exp_mt:__tostring()
  local result = grid.new()
  for i=1,#self.children do
    local child = grid.new(tostring(self.children[i]))
    if self.type == EXP_TYPE.MUL and self.children[i].type and self.children[i].type == EXP_TYPE.ADD then
      child:enclose_paren()
    end
    result:right(child)

    if i <= #self.children-1 then
      if self.type == EXP_TYPE.ADD then
        result:right(grid.new(" + "))
      end
    end
  end
  return tostring(result)
end

function extendable.new(n, single, top, middle, bot)
  if n <= 0 then
    return grid.new()
  elseif n == 1 then
    return grid.new(single)
  else
    local lines = {}
    for i=1,n do
      if i == 1 then
        table.insert(lines, top)
      elseif i == n then
        table.insert(lines, bot)
      else
        table.insert(lines, middle)
      end
    end

    return grid.new(lines)
  end
end

function create_left_paren(n)
  return extendable.new(n, "(", "⎛", "⎜", "⎝")
end

function create_right_paren(n)
  return extendable.new(n, ")", "⎞", "⎟", "⎠")
end

function create_left_bracket(n)
  return extendable.new(n, "[", "⎡", "⎢", "⎣")
end

function create_right_bracket(n)
  return extendable.new(n, "]", "⎤", "⎥", "⎦")
end

function grid_methods:enclose_bracket()
  self:left(create_left_bracket(self.m))
  self:right(create_right_bracket(self.m))
end

function grid_methods:enclose_paren()
  self:left(create_left_paren(self.m))
  self:right(create_right_paren(self.m))
end
grid.new = function(arg)
  arg = arg or {}
  local lines = {}
  if type(arg) == "string" then
    local str = arg
    if str ~= "" then
      for line in vim.gsplit(str, "\n") do
        table.insert(lines, line)
      end
    end

  elseif type(arg) == "table" then
    lines = arg
  end
  local max_width = 0
  for i=1,#lines do
    max_width = math.max(max_width, vim.api.nvim_strwidth(lines[i]))
  end

  for i=1,#lines do
    local diff = max_width - vim.api.nvim_strwidth(lines[i])
    lines[i] = lines[i] .. (" "):rep(diff)
  end


  local g = {}
  g.lines = lines
  g.m = #lines
  g.n = 0
  if #lines > 0 then
    g.n = #lines[1]
  end

  return setmetatable(g, grid_mt)
end

function grid_mt:__tostring()
  return table.concat(self.lines, "\n")
end

function grid_methods:top(arg)
  if type(arg) == "number" then
    local count = arg
    for i=1,count do
      table.insert(self.lines, 1, (" "):rep(self.n))
    end

    self.m = self.m + count

  else
    local other = arg
    if other.n > self.n then
      local l_margin = math.floor((other.n - self.n)/2)
      local r_margin = (other.n - self.n) - l_margin
      self:left(l_margin)
      self:right(r_margin)
    elseif other.n < self.n then
      local l_margin = math.floor((self.n - other.n)/2)
      local r_margin = (self.n - other.n) - l_margin
      other = other:clone()
      other:left(l_margin)
      other:right(r_margin)
    end

    for i=1,other.m do
      table.insert(self.lines, i, other.lines[i])
    end
    self.m = self.m + other.m
    self.n = math.max(self.n, other.n)

  end
end

function grid_methods:down(arg)
  if type(arg) == "number" then
    local count = arg
    for i=1,count do
      table.insert(self.lines, (" "):rep(self.n))
    end
    self.m = self.m + count

  else
    local other = arg
    if other.n > self.n then
      local l_margin = math.floor((other.n - self.n)/2)
      local r_margin = (other.n - self.n) - l_margin
      self:left(l_margin)
      self:right(r_margin)
    elseif other.n < self.n then
      local l_margin = math.floor((self.n - other.n)/2)
      local r_margin = (self.n - other.n) - l_margin
      other = other:clone()
      other:left(l_margin)
      other:right(r_margin)
    end

    for i=1,other.m do
      table.insert(self.lines, other.lines[i])
    end
    self.m = self.m + other.m
    self.n = math.max(self.n, other.n)

  end
end

function grid_methods:left(arg)
  if type(arg) == "number" then
    local count = arg
    for i=1,self.m do
      self.lines[i] = (" "):rep(count) .. self.lines[i]
    end
    if self.m > 0 then
      self.n = self.n + count
    end

  else
    local other = arg
    if other.m > self.m then
      local d_margin = math.floor((other.m - self.m)/2)
      local t_margin = (other.m - self.m) - d_margin
      self:top(t_margin)
      self:down(d_margin)
    elseif other.m < self.m then
      other = other:clone()
      local d_margin = math.floor((self.m - other.m)/2)
      local t_margin = (self.m - other.m) - d_margin
      other:top(t_margin)
      other:down(d_margin)
    end

    for i=1,other.m do
      self.lines[i] = other.lines[i] .. self.lines[i]
    end
    self.n = self.n + other.n
    self.m = math.max(self.m, other.m)

  end
end

function grid_methods:right(arg)
  if type(arg) == "number" then
    local count = arg
    for i=1,self.m do
      self.lines[i] = self.lines[i] .. (" "):rep(count)
    end

    if self.m > 0 then
      self.n = self.n + count
    end

  else
    local other = arg
    if other.m > self.m then
      local d_margin = math.floor((other.m - self.m)/2)
      local t_margin = (other.m - self.m) - d_margin
      self:top(t_margin)
      self:down(d_margin)
    elseif other.m < self.m then
      other = other:clone()
      local d_margin = math.floor((self.m - other.m)/2)
      local t_margin = (self.m - other.m) - d_margin
      other:top(t_margin)
      other:down(d_margin)
    end

    for i=1,other.m do
      self.lines[i] =  self.lines[i] .. other.lines[i]
    end
    self.n = self.n + other.n
    self.m = math.max(self.m, other.m)

  end
end

function grid_methods:clone()
  return grid.new(tostring(self))
end

function grid.concat_grid(elems)
  local m = #elems
  local n = 0
  if #elems > 0 then
    n = #elems[1]
  end

  local max_height = {}
  for i=1,m do
    max_height[i] = 0
    for j=1,n do
      max_height[i] = math.max(elems[i][j].m, max_height[i])
    end
  end

  local max_width = {}
  for j=1,n do
    max_width[j] = 0
    for i=1,m do
      max_width[j] = math.max(elems[i][j].n, max_width[j])
    end
  end

  local matrix = grid.new()
  for i=1,m do
    local row = grid.new()
    for j=1,n do
      local l_margin = (max_width[j] - elems[i][j].n)/2
      local r_margin = (max_width[j] - elems[i][j].n) - l_margin
      local d_margin = (max_height[i] - elems[i][j].m)/2
      local t_margin = (max_height[i] - elems[i][j].m) - d_margin
      elems[i][j]:left(l_margin)
      elems[i][j]:right(r_margin)
      elems[i][j]:top(t_margin)
      elems[i][j]:down(d_margin)

      row:right(elems[i][j])
    end
    matrix:down(row)
  end
  return matrix
end

function matrix_set:__tostring()
  if self.assigned then
    local grid_elems = {}
    for i=1,#self.elems do
      grid_elems[i] = {}
      for j=1,#self.elems[i] do
        grid_elems[i][j] = grid.new(tostring(self.elems[i][j]) .. " ")
      end
    end

    local matrix_grid = grid.concat_grid(grid_elems)

    matrix_grid:enclose_bracket()

    return tostring(matrix_grid)

  else
    return self.name
  end
end

function matrix_set:size()
  return { self.set.m, self.set.n }
end

function matrix_set:assign(arr)
  self.assigned = true
  assert(#arr == self.m)
  for i=1,self.m do
    assert(#arr[i] == self.n)
    for j=1,self.n do
      self.elems[i][j] = arr[i][j]
    end
  end
end
function mat.identity(m,n)
  local z = M.c(0)
  local o = M.c(1)
  n = n or m

  local I = swan.syms("I", swan.matrix_set:new(m,n))
  local arr = {}
  for i=1,m do
    arr[i] = {}
    for j=1,n do
      arr[i][j] = i == j and o or z
    end
  end
  I:assign(arr)
  return I
end
function M.syms(names, set)
  local syms = {}
  local names_list = {}
  for name in vim.gsplit(names, " ") do
    local s, _ = name:find("^\\")

    local capture
    if s then
      capture = name:sub(s+1):match("^%a+")
      if capture and greek_etc[capture] then
        name = name:sub(1,s-1) .. greek_etc[capture] .. name:sub(s+1+#capture)
      end

    end


    local s, _ = name:find("_")
    if s then
      capture = name:sub(s+1):match("^%w+")
      if capture then 
        local subs = ""
        for i=1,#capture do
          if sub_letters[capture:sub(i,i)] then
            subs = subs .. sub_letters[capture:sub(i,i)]
          else
            subs = nil
            break
          end
        end

        if subs then
          name = name:sub(1,s-1) .. subs .. name:sub(s+1+#capture)
        end
      end

    end

    table.insert(names_list, name)

  end

  if not set then
    set = real_set
  end

  local syms = {}
  for _, name in ipairs(names_list) do
    local sym = {}
    sym.name = name
    setmetatable(sym, {
      __index = sym_mt.__index,
      __tostring = set.__tostring,
      __call = set.__call,

      __add = sym_mt.__add,
      __mul = sym_mt.__mul,
    })
    table.insert(syms, sym)
  end

  for _, sym in ipairs(syms) do
    sym.set = set
  end
  return unpack(syms)
end

function sym_mt:__index(key)
  return sym_methods[key] or self.set[key]
end

function sym_mt:__add(other)
  local new_children = {}
  for _, elem in ipairs({self, other}) do
    if elem.type and elem.type == EXP_TYPE.ADD then
      for _, child in ipairs(elem.children) do
        table.insert(new_children, child)
      end
    else
      table.insert(new_children, elem)
    end
  end

  return exp.new(new_children, EXP_TYPE.ADD)
end

function sym_mt:__mul(other)
  local new_children = {}
  for _, elem in ipairs({self, other}) do
    if elem.type and elem.type == EXP_TYPE.MUL then
      for _, child in ipairs(elem.children) do
        table.insert(new_children, child)
      end
    else
      table.insert(new_children, elem)
    end
  end

  return exp.new(new_children, EXP_TYPE.MUL)
end

return M
