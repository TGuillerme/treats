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
 - [ ] discrete traits evolution
 - [ ] some age dependency modifier (if lineage is old, more or less speciation)
 - [ ] allow inputing a tree and simulating from there
 - [ ] add a function to make `tree` and `data` in `dads` format manually

## Feedback

### Manual suggestions
 - [ ] See list of issues from NC
 - [ ] From NC: I think my main feedback is similar to with the manual for dispRity. I think it needs a simple introduction to the problem(s) the package can solve early on. This can be very informal, but I think needs to focus on the biology rather than the practicalities (which I know you're more interested in so it makes sense that you'd jump right into those). It could refer to a specific Q or paper (if you have examples where you have used dads already that would be great). For example "Fred is writing a paper about the evolution of tetrapods. He's got some really interesting results and it looks like his traits are evolving differently before and after a mass extinction event. However, Fred can't be sure this is not just the effect of the method he's using. Maybe he'd expect this result? How can he tell whether his results truly are interesting? A solution would be to simulate the very situation Fred is interested in and then to see if the methods give the same results. However, existing packages only let fred simulate traits or a tree. This is where dads comes in"
 - [ ] Get some standard examples and link to the once the the end of the vignette. BM and OU with one traits that people standardly do and use. Like get 20 basic scenarios. e.g. `geiger::fitcontinuous`
      [ ] Make that at a higher level question type "Did the K-Pg extinction had an effect of mammalian disparity?". "Given a scenario, would I detect something anyways (puttick style)?" Maybe use more to do: simulate a pattern and see if that pattern is discernabble in a way or another (rather than doing it from an empirical scenario).
 - [ ] add a schematic for each subsection as well (not only the main one): or maybe just do it for a specific example
 - [ ] put correlated traits in the documentation earlier in the vignette
 - [ ] What it does: simulates trees and traits.
 - [ ] throughout the manual: change `disparity` to `traits` and `diversity` to `trees` 
 - [ ] Maybe a cheat sheet table for what affects what. "I want to change my extinction rate?" "where do I change that (is it in modifiers, etc...)" (kind of mini FAQ)
 - [ ] Rename to `treats` https://docs.github.com/en/repositories/creating-and-managing-repositories/renaming-a-repository
 
### Bug fixes
 - [ ] make the stop.rule warn or stop if max time is really high. (> 5)
    - [ ] add to tests
 - [ ] error when installing `rgl`. Maybe remove it from the dependencies
 - [ ] error from verbose example with background traits.
