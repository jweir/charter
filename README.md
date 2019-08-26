# Charter

A package for quickly drawing sparklines, line charts and a few other charts. Supports eventing for selecting data within a chart.


## A simple inline graph that is 60 pixels by 20
```elm
sparkline 
  (Size 60 20) 
  [ Line [] [(0, 1), (1, 5), (2, 10), (3, 8), (4, 5), (5, 6)] ]
```


See `examples/Example.elm` for more detailed example with events.
