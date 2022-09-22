##../swan
@handle_mul_simplify+=
if lhs.kind == "i" and rhs.kind == "i" then
  return M.constant(-1)
end
