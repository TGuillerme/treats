# TODO list: finish package!

## bd.params

 - [x] Add plotting to manual for `make.bd.params` (in chapter 05)

## traits

 - [x] test list of traits
 - [x] add + test `repulsion.process`
 - [x] add bits about save.steps and background in the manual

## modifiers

 - [x] example for: `make.modifiers`: Write examples (write manual at the same time)
 - [x] example for: `modifiers`: Write examples (write manual at the same time)

## events

 - [ ] make list of events
 - [ ] document list of events (Rd)
 - [ ] document list of events (gitbook)
 - [ ] test list of separate: conditions and modifications
 - [ ] allow events per clade (see TreeSim)
 - [ ] add abiotic events (see RPANDA)
 - [ ] make manual for events
 - [ ] example for: `events.conditions`: Write examples (write manual at the same time)
 - [ ] example for: `events.modifications`: Write examples (write manual at the same time)
 - [ ] example for: `make.events`: Write examples (write manual at the same time)

## dads

 - [x] Also add a "snapshot" mechanism, compared to the "instant trait" one above, this one only has a look at what's happening everywhere in the tree at specific requested time (e.g. with conditions) and not at specific regular given time point (e.g. every 0.1 time unit).
    - [ ] test for `background` in `make.traits`
    - [ ] add manual for `background`
 - [ ] add a verbose option when using `null.errors` (something like "building tree:... Done.") 
 - [ ] example for: `dads`: Run a birth-death tree with a modifier
 - [ ] example for: `dads`: Run a birth-death tree with a modifier + event

## print

 - [ ] example for: `print.dads`: add example for each category:
   - [ ] `dads`
   - [ ] `bd.params`
   - [ ] `traits`
   - [ ] `modifiers`
   - [ ] `events`

## plot

 - [ ] adding legend options
 - [ ] add option to switch the tree age (time to present to time since past).
 - [ ] do the manual section

## utilities

 - [ ] add manual for utilities
 - [ ] add test for `parent.traits`
 - [ ] example for: `utilities::parent.traits`: Write examples (write manual at the same time)







Selling point: dads is so modular that you can simulate a dads object that simulates a dads object.


## TODO: whishlist
 - [ ] Add lineage tracking mechanism (i.e. allow `traits`, `modifiers` or `events` to be applied to specific lineages); also allow giving prefixes to the names of tips and nodes in the lineage.
