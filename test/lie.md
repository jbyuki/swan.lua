# Lie algebra

## Rotation in 3D space

$SO(3)$ is the group of 3D rotation matrices. Composition is done by multiplication, inversion is done by transposition.

$R \in SO(3)$
$R^{-1} = R^T$

```lua
swan = require"swan"
G1 = swan.mat {
  {0, 0, 0}, 
  {0, 0, -1}, 
  {0, 1, 0}
}

G2 = swan.mat {
  {0,  0, 1}, 
  {0,  0, 0}, 
  {-1, 0, 0}
}

G3 = swan.mat {
  {0, -1, 0}, 
  {1,  0, 0}, 
  {0, 0, 0}
}
```
```output[1](05/08/22 21:58:18)
```

```lua
w1 = swan.sym "w1"
w2 = swan.sym "w2"
w3 = swan.sym "w3"

wx = w1*G1 + w2*G2 + w3*G3
print(wx:simplify())
```
```output[2](05/08/22 21:58:19)
[
  0, -1w3, w2
  w3, 0, -1w1
  -1w2, w1, 0
]
```

### Exponential Map

To convert a skew matrix to a rotation matrix, we take the exponential of the skew matrix.

This is an infinite sum but it can simplifed to the Rodrigues formula.

```lua
I = swan.mat {
  {1, 0, 0},
  {0, 1, 0},
  {0, 0, 1}
}

theta = swan.sym "theta"
 
exp_wx = I + (swan.sin(theta)/theta)*wx + ((1-swan.cos(theta))/(theta*theta))*wx*wx
print(exp_wx:simplify())
```
```output[7](05/08/22 21:59:28)
[
  -1((1 - cos(theta))/(theta^2))w2^2 + -1((1 - cos(theta))/(theta^2))w3^2 + 1, ((1 - cos(theta))/(theta^2))w1w2 + -1((sin(theta))/theta)w3, ((sin(theta))/theta)w2 + -1((1 - cos(theta))/(theta^2))w1w3
  ((sin(theta))/theta)w3 + -1((1 - cos(theta))/(theta^2))w1w2, -1((1 - cos(theta))/(theta^2))w1^2 + -1((1 - cos(theta))/(theta^2))w3^2 + 1, ((1 - cos(theta))/(theta^2))w2w3 + -1((sin(theta))/theta)w1
  ((1 - cos(theta))/(theta^2))w1w3 + -1((sin(theta))/theta)w2, ((sin(theta))/theta)w1 + -1((1 - cos(theta))/(theta^2))w2w3, -1((1 - cos(theta))/(theta^2))w1^2 + -1((1 - cos(theta))/(theta^2))w2^2 + 1
]
```



## References:

* "Lie Groups for 2D and 3D Transformations", Ethan Eade, 20 May 2017
