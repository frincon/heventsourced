[![Travis](https://img.shields.io/travis/frincon/heventsourced.svg?maxAge=2592000)](https://travis-ci.org/frincon/heventsourced)
# heventsourced
Haskell implementation of the Event Sourcing architectural pattern

## Introduction
Event Sourcing is an architectural pattern that force to store the application state as a sequence of events.

This package provides basic framework to implement event sourcing in haskell. It is based in the design from
https://gist.github.com/Fristi/7327904. 

The project contains two packages: 
- heventsourced: Is the foundation package where the common api is placed. If you are looking for implementing an application that uses Event Sourcing you probably need to use the functions in this package. It additionally contains a Dummy persistor and state manager that works in memory.
- heventsourced-mongo: An implementation of event persistor that use mongo as a store for events.
