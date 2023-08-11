---
name: Simulation template
about: Share your simulation template
title: Simulate something
labels: simulation template
assignees: ''

---

<!-- Templates are all shared under the same open license as the package (GPLv3) -->
<!-- Please edit the sections below to share your simulation template -->
<!-- Don't worry if you don't fill all the sections, you can edit them later or the community can help you filling them -->

# My simulation 

Insert your brief description here.

## What does it do?

<!-- Fill the table with ticks ✅ and ❌ -->

What function does it uses? | yes/no | comments
----------------------|----|--------
Uses `make.bd.params` | ✅ | 
Uses `make.traits` | ✅ | 
Uses `make.modifications` | ✅ | 
Uses `make.events` | ✅ | 


<!-- Fill the following sections appropriately (or delete) -->

### `bd.params`

Describe the customised `bd.params` object

```
my_bd.params <- make.bd.params()
```

### `traits`

Describe the customised `traits` object

```
my_traits <- make.traits()
```

### `modifier`

Describe the customised `modifier` object

```
my_traits <- make.modifier()
```

### `events`

Describe the customised `modifier` object

```
my_traits <- make.events()
```

## A running example

```
## My favorite seed
set.seet(42)

## Some stopping rules
my_stop.rule <- list(max.taxa = 50, max.living = 50, max.time = 5)

## The simulation
my_simulation <- treats(stop.rule = my_stop.rule,
                        bd.params = my_bd.params,
                        traits    = my_traits,
                        modifiers = my_modifiers,
                        events    = my_events)
```

## Reference 

<!-- You are more than welcome to share your paper here if you want users to cite your awesome work! -->

If you use this template in a publication, please cite:

 * `treats`
 * `R`