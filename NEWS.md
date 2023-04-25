treats v0.1.12 (2023-04-28) 
=========================

### BUG FIXES

 * Corrected the `BM.process` and `OU.process` to now be exact (thanks to Rachel Warnock and Louie Rombaut for spotting that one).
 * Fixed `bd.params` print display when inputs are numeric.

### NEW FEATURES

 * Changed package name from `dads` to `treats`
 * Full polished version of the manual!
 * `treats` has now a `save.steps` option to create internal nodes at regular intervals or specified ones.
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