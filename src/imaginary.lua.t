##swan
@define_named_constants+=
M.i = Exp.new("i", {})

@print_exp+=
elseif self.kind == "i" then
  return "i"

@clone_exp+=
elseif self.kind == "i" then
  return self
