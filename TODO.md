# TODO list: finish package!

## bd.params

 - [x] Add plotting to manual for `make.bd.params` (in chapter 05)

## traits

 - [x] test list of traits
 - [x] add + test `repulsion.process`
 - [x] add bits about save.steps and background in the manual
 - [x] add bit about "what is a trait?"

## modifiers

 - [x] example for: `make.modifiers`: Write examples (write manual at the same time)
 - [x] example for: `modifiers`: Write examples (write manual at the same time)

## events

 - [x] make list of events
 - [x] document list of events (Rd)
 - [x] document list of events (gitbook)
 - [x] test list of separate: conditions and modifications
 - [x] make manual for events
 - [x] example for: `events.conditions`: Write examples (write manual at the same time)
 - [x] example for: `events.modifications`: Write examples (write manual at the same time)
 - [x] example for: `make.events`: Write examples (write manual at the same time)

## dads

 - [x] Also add a "snapshot" mechanism, compared to the "instant trait" one above, this one only has a look at what's happening everywhere in the tree at specific requested time (e.g. with conditions) and not at specific regular given time point (e.g. every 0.1 time unit).
    - [x] test for `background` in `make.traits`
    - [x] add manual for `background`
 - [x] add a verbose option when using `null.errors` (something like "building tree:... Done.") 
 - [x]] example for: `dads`: Run a birth-death tree with a modifier
 - [x] example for: `dads`: Run a birth-death tree with a modifier + event

## print

 - [x] example for: `print.dads`: add example for each category:
   - [x] `dads`
   - [x] `bd.params`
   - [x] `traits`
   - [x] `modifiers`
   - [x] `events`

## plot

 - [x] add trait labels + handeling colours for plotting traits
 - [x] adding legend options
 - [x] add option to switch the tree age (time to present to time since past): done through `xlim`.
 - [x] do the manual section

## utilities

 - [x] add manual for utilities
 - [x] example for: `utilities::parent.traits`: Write examples (write manual at the same time)
 - [ ] add test for `parent.traits`

## Manual

 - [x] in Index, fix illustration (pdf to eps)
 - [x] do specific examples

## Package

 - [ ] compile package with CRAN check
 - [ ] compile manual
 - [ ] update README

Selling point: dads is so modular that you can simulate a dads object that simulates a dads object.



## TODO: whishlist
 - [ ] Add lineage tracking mechanism (i.e. allow `traits`, `modifiers` or `events` to be applied to specific lineages); also allow giving prefixes to the names of tips and nodes in the lineage.
 - [ ] allow events per clade (see TreeSim)
 - [ ] add abiotic events (see RPANDA)
 - [ ] add the `dispRity` smart plot option handling
 - [ ] have a function that tracks lineages through time when using `save.steps`
 - [ ] add option to colour the elements sequentially (e.g. col = rainbow(10))
