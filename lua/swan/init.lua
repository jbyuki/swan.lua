-- Generated using ntangle.nvim
local M = {}
local sym_methods = {}
local sym_mt = {}
sym_mt.__index = sym_methods

local real_domain = {}

local greek_etc = {
  ["Alpha"] = "Α", ["Beta"] = "Β", ["Gamma"] = "Γ", ["Delta"] = "Δ", ["Epsilon"] = "Ε", ["Zeta"] = "Ζ", ["Eta"] = "Η", ["Theta"] = "Θ", ["Iota"] = "Ι", ["Kappa"] = "Κ", ["Lambda"] = "Λ", ["Mu"] = "Μ", ["Nu"] = "Ν", ["Xi"] = "Ξ", ["Omicron"] = "Ο", ["Pi"] = "Π", ["Rho"] = "Ρ", ["Sigma"] = "Σ", ["Tau"] = "Τ", ["Upsilon"] = "Υ", ["Phi"] = "Φ", ["Chi"] = "Χ", ["Psi"] = "Ψ", ["Omega"] = "Ω",
  ["alpha"] = "α", ["beta"] = "β", ["gamma"] = "γ", ["delta"] = "δ", ["epsilon"] = "ε", ["zeta"] = "ζ", ["eta"] = "η", ["theta"] = "θ", ["iota"] = "ι", ["kappa"] = "κ", ["lambda"] = "λ", ["mu"] = "μ", ["nu"] = "ν", ["xi"] = "ξ", ["omicron"] = "ο", ["pi"] = "π", ["rho"] = "ρ", ["final"] = "ς", ["sigma"] = "σ", ["tau"] = "τ", ["upsilon"] = "υ", ["phi"] = "φ", ["chi"] = "χ", ["psi"] = "ψ", ["omega"] = "ω",
  ["nabla"] = "∇",
}

local sub_letters = { 
	["+"] = "₊", ["-"] = "₋", ["="] = "₌", ["("] = "₍", [")"] = "₎",
	["a"] = "ₐ", ["e"] = "ₑ", ["o"] = "ₒ", ["x"] = "ₓ", ["ə"] = "ₔ", ["h"] = "ₕ", ["k"] = "ₖ", ["l"] = "ₗ", ["m"] = "ₘ", ["n"] = "ₙ", ["p"] = "ₚ", ["s"] = "ₛ", ["t"] = "ₜ", ["i"] = "ᵢ", ["j"] = "ⱼ", ["r"] = "ᵣ", ["u"] = "ᵤ", ["v"] = "ᵥ",
	["0"] = "₀", ["1"] = "₁", ["2"] = "₂", ["3"] = "₃", ["4"] = "₄", ["5"] = "₅", ["6"] = "₆", ["7"] = "₇", ["8"] = "₈", ["9"] = "₉",
}

function real_domain.tostring(sym)
  return sym.name
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

  local syms = {}
  for _, name in ipairs(names_list) do
    local sym = {}
    sym.name = name
    setmetatable(sym, sym_mt)
    table.insert(syms, sym)
  end

  if not domain then
    domain = real_domain
  end

  for _, sym in ipairs(syms) do
    sym.domain = real_domain
  end

  return unpack(syms)
end

function sym_mt:__tostring()
  return self.domain.tostring(self)
end
return M
