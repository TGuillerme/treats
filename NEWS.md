treats v1.0 (2023-11-22) 
=========================

### NEW FEATURES

 * Completely reworked `dispRitreats` to now work just like the `dispRity` function on `treats` objects.
 * The package is now released on CRAN!

### MINOR IMPROVEMENTS

 * `events` now generates singleton nodes (and associated trait values if needed) at the time of the event before applying the modification.
 * added `verbose` option to `treats` for when using the option `null.error = integer()`.
 * printing now works for `treats` outputs with `replicates = <integer>`.
 * `drop.things` now works with replicated `treats` objects (or `multiPhylo`)

## BUG FIXES
 
 * Fixed many simulation corner cases (mainly ones for special trees with always only one node, one living and one fossil).
 * Fixed printing issues with S3 `"treats"` sub-classes

### DEPRECATED AND DEFUNCT

 * `time.condition` is now changed to `age.condition`.


treats v0.2 (2023-05-25) *CRAN friendly pre-release*
=========================

### NEW FEATURES

 * New utility function `dispRitreats` to convert `"treats"` output as input for `dispRity`
 * New utility function `crude.bd.est` for a crude estimation of speciation and extinction rates from an input tree.
 * New utility function `map.traits` for mapping a `"traits"` onto a tree.
 * New utility function `transition.matrix` for designing transition matrices for discrete characters.
 * New `traits` process: `discrete.process` to generate discrete characters.
 * Changed package name from `dads` to `treats`
 * Full polished version of the manual!
 * `treats` has now a `save.steps` option to create internal nodes at regular intervals or specified ones and a `replicates` option to automatically replicate the simulations.
 * Added a `make.bd.parms` utility function for sampling from distributions (jointly or not).
 * Trees can now be generated with birth-death parameters sampled from functions.
 * added a list of pre-made `modification` and `condition` for events.
 * `traits` can now have a background component that triggers trait generation in the background each time traits are generated.
 * `events` are now fully handled and modular.
 * `make.events` function for helping to make events.
 * `make.modifiers` now has a `select.taxa` argument.
 * `make.treats` to create `"treats"` objects from a tree and a dataset (e.g. matrix). This can be useful for using the the `plot.treats` function on non `"treats"` objects. 
 * `"events"` class objects are now implemented and fully functional with the `make.events` function and the pre-made `mass.extinction` and `founding.event` events.
 * `drop.things` to drop fossils or livings species or internal nodes from `"treats"` objects.

### MINOR IMPROVEMENTS

 * Many clarifications and rewording to the manual.

### BUG FIXES

 * Corrected the `BM.process` and `OU.process` to now be exact (thanks to Rachel Warnock and Louie Rombaut for spotting that one).
 * Fixed `bd.params` print display when inputs are numeric.

treats v0.1.0 (2020-11-13) *first release*
=========================

### NEW FEATURES
 
 * `treats` function first release.
 * `make.traits` function first release.
 * `make.modifiers` function first release.
 * `print` and `plot` functions for `"treats"` objects.
 * `parent.traits` utility function first release.
 * Manual first release

<!-- ### MINOR IMPROVEMENTS

 * INIT

### BUG FIXES

 * INIT
 -->