local swan = require"swan"
r11 = swan.sym "r11"
r12 = swan.sym "r12"
r13 = swan.sym "r13"
r21 = swan.sym "r21"
r22 = swan.sym "r22"
r23 = swan.sym "r23"
r31 = swan.sym "r31"
r32 = swan.sym "r32"
r33 = swan.sym "r33"

tx = swan.sym "tx"
ty = swan.sym "ty"
tz = swan.sym "tz"

xw = swan.sym "xw"
zw = swan.sym "zw"

Rt = swan.mat {
 {r11, r12, r13, tx},
 {r21, r22, r23, ty},
 {r31, r32, r33, tz},
}

Pw = swan.mat {
 {xw},
 {0},
 {zw},
 {1}
}

print((Rt*Pw):simplify())
