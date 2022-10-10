##swan
@methods+=
function M.exp(x)
  return M.e ^ x
end

@methods+=
function M.ln(x)
  return Exp.new("ln", { arg = x })
end

@print_exp+=
elseif self.kind == "ln" then
  return ("ln(%s)"):format(tostring(self.o.arg))

@clone_exp+=
elseif self.kind == "ln" then
  return Exp.new(self.kind, { arg = self.o.arg:clone() })
