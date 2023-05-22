# TODO list: finish package!

## New function
 - [x] allow inputing a tree and simulating from there (handled separately through `map.trait` (map trait on tree) with `map.trait(tree = tree, trait = traits)`.

## New traits
 - [x] `discrete.process`
    - [x] test
    - [x] doc
    - [x] man

## utilities

 - [x] add manual for utilities
 - [x] example for: `utilities::parent.traits`: Write examples (write manual at the same time)
 - [ ] add test for `parent.traits`
 - [ ] doc for `crude.bd.est`
 - [ ] dic for `dispRitreats`
 - [ ] add test for `dispRitreats`

## Package

 - [x] compile package with CRAN check
    - [x] clean S3
    - [x] clean codoc
 - [x] compile manual
 - [x] update README

## TODO: whishlist
 - [ ] Add lineage tracking mechanism (i.e. allow `traits`, `modifiers` or `events` to be applied to specific lineages); also allow giving prefixes to the names of tips and nodes in the lineage.
 - [ ] allow events per clade (see TreeSim)
 - [ ] add abiotic events (see RPANDA)
 - [ ] add the `dispRity` smart plot option handling
 - [ ] have a function that tracks lineages through time when using `save.steps`
 - [ ] `plot`: add option to colour the elements sequentially (e.g. col = rainbow(10))
 - [ ] add option to simulate multiple trees for one or more traits (gene tree vs species tree)
 - [ ] adding budding simulation (see paleo buddy).
 - [ ] some age dependency modifier (if lineage is old, more or less speciation)
 - [ ] add a function to make `tree` and `data` in `treats` format manually
 - [ ] `discretise.traits`,
  * [ ] either applied a posteriori on the whole `treats` object (to discretise the output)
  * [ ] or converts a process into `discrete.process` where the rate sampler is the one from the previous process.
  * [ ] allow `discretise.traits` and `discrete.process` to handle any states names (not just 0, 1, 2, ...)

## Feedback

### Manual suggestions
 - [x] add a schematic for each subsection as well (not only the main one): or maybe just do it for a specific example
 - [ ] Get some standard examples and link to the once the the end of the vignette. BM and OU with one traits that people standardly do and use. Like get 20 basic scenarios. e.g. `geiger::fitcontinuous`. Do that into the last part (example gallery).
 - [ ] put correlated traits in the documentation earlier in the vignette
 - [ ] What it does: simulates trees and traits. throughout the manual: change `disparity` to `traits` and `diversity` to `trees` 
 - [ ] Maybe a cheat sheet table for what affects what. "I want to change my extinction rate?" "where do I change that (is it in modifiers, etc...)" (kind of mini FAQ)