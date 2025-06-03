# 2.1.2

Only include the event layer if there are events.

# 2.1.1

Removed the onAnimationFrameDelta.  It forced too many updates and was unperformant with high point count charts.

# 2.1.0
stacked area charts supported

# 2.0.7

onAnimationFrameDelta now used to process events for a smoother experience

This was done as a simple debounce.  Production testing with high point count has shown it needs to be better.


# 2.0.6

### Fixed

Removed a Debuger

# 2.0.5

### Fixed

Previous fix broke hovering

# 2.0.4

### Fixed

Detecting mouseup during a drag operation

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
