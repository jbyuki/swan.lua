## Lie algebra

### Rotation in 3D space

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

### Note:



### References:

* "Lie Groups for 2D and 3D Transformations", Ethan Eade, 20 May 2017
