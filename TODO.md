# TODO list

 - [x] INIT
 - [x] Simple BD process
 - [x] BD process with traits

## TODO traits

 - [x] make list of traits
 - [x] `make.traits`
 - [x] document list of traits (Rd)
 - [x] document list of traits (gitbook)
 - [x] test `make.traits`
 - [x] add and test `print.traits`
 - [x] add and test `update` argument
 - [ ] test list of traits

## TODO modifiers

 - [x] make list of modifiers
 - [x] `make.modifiers`
 - [x] document list of modifiers (Rd)
 - [x] test `make.modifiers`
 - [x] add and test `print.modifiers`
 - [ ] add and test `update` argument

## TODO events

 - [ ] make list of events
 - [ ] `make.events`
 - [ ] document list of events (Rd)
 - [ ] document list of events (gitbook)
 - [ ] test `make.events`
 - [ ] add and test `print.events`
 - [ ] test list of events

### Make the following events work:
Test the following events types
 - [x] taxa event: mass extinction based on % at time t
 - [x] taxa event: mass extinction based on traits at time t
 - [x] bd.params events: adding extinction parameter after reaching n taxa
 - [x] bd.params events: reducing speciation after reaching time t
 - [x] traits events: changing trait process at time t
 - [x] traits events: changing traits correlation when reaching trait value x for trait 1
 - [ ] modifiers events: adding a speciation condition after reaching time t
 - [ ] modifiers events: adding a branch length condition when reaching n taxa
 - [ ] modifiers events: changing the condition of a modifier after reaching trait value x
 - [ ] modifiers events: changing the modify of a modifier after reaching time t
 - [ ] founding events: generate a new birth-death process for the first node to reach trait value x

## TODO utilities
 
 - [x] add utilities
 - [ ] add manual for utilities
 - [ ] add test for `parent.traits`

## TODO package

 - [ ] add examples in functions

## TODO plot

 - [ ] add a 2/3D plot option for traits (points coloured as a function of time) + optional links
 - [ ] add a 3D plot version of plot dads with X Y being traits and Z being time
 - [ ] add a time gradient option for col.
 - [ ] distinguish tips from nodes with a circle option around the tips (in 2D)