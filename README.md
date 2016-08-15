# heventsourced
Haskell implementation of the Event Sourcing architectural pattern

Event Sourcing is an architectural pattern that force to store the application state as a sequence of events.

This package provides basic framework to implement event sourcing in haskell. It is based in the design from
https://gist.github.com/Fristi/7327904. This package provides the typeclasses to implement event sourcing and
a dummy in-memory implementation of the framework. For a real use you probably want to use a concrete
implementation like heventsourced-mongo along with this.
