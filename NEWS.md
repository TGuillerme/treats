dads v0.1.6 (2023-01-12) 
=========================

### BUG FIXES

 * Corrected the `BM.process` to now be to `rnorm(n = 1, mean = x0, sd = sqrt(sd^2 * edge.length))` (thanks to Rachel Warnock for spotting that one).
 * Fixed `bd.params` print display when inputs are numeric.

### NEW FEATURES
 
 * Added a `make.bd.parms` utility function for sampling from distributions (jointly or not).
 * Trees can now be generated with birth-death parameters sampled from functions.
 * added a list of pre-made `modification` and `condition` for events.
 * `events` are now fully handled and modular.
 * `make.events` function for helping to make events.
 * `make.modifiers` now has a `select.taxa` argument.
 * `"events"` class objects are now implemented and fully functional with the `make.events` function and the pre-made `mass.extinction` and `founding.event` events.

dads v0.1.0 (2020-11-13) 
=========================

### NEW FEATURES
 
 * `dads` function first release.
 * `make.traits` function first release.
 * `make.modifiers` function first release.
 * `print` and `plot` functions for `"dads"` objects.
 * `parent.traits` utility function first release.
 * Manual first release

<!-- ### MINOR IMPROVEMENTS

 * INIT

### BUG FIXES

 * INIT
 -->