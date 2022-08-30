# 2.0.3

### Feature changed

`zeroLine` inserts a 0 in to the Y domain.

# 2.0.2

### Fixed

Drawing order was reversed

# 2.0.1

### Fixed

Calculation of Layer Domain when there is only `include` data.

# 2.0.0

### New

The Domain type is exposed.
`getDomain` created to extract the Domain from a Layer.

### Breaking

`extents` is deprecated
`domain` becomes `include` with a different signature
`Layer` is now defined via the function `layer`

# 1.3.1

Fixed a bug in the extents function.

# 1.3.0

Added extents function to extract the domain from a list of Elements.

# 1.2.2

Fix the highlight position when a graph is resized

# 1.2.1

Improve performance of lazy by breaking the Box into primitives.

# 1.2.0

Charter.active returns true when the mouse is down over a listening Layer.

# 1.1.4

Use Svg.Lazy for rendering the layer

# 1.1.2

Fix clearing the selection

# 1.1.1

Use correct function for `area` and adjust the clamping logic.

# 1.1.0

Added step function in Chater.Extra

# 1.0.0

First release. Evolution of the jweir/sparkline library.
