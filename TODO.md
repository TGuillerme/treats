# TODO list: finish package!

## New function
 - [x] allow inputing a tree and simulating from there (handled separately through `map.trait` (map trait on tree) with `map.trait(tree = tree, trait = traits)`.



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
 - [ ] discrete traits evolution
 - [ ] some age dependency modifier (if lineage is old, more or less speciation)
 - [ ] add a function to make `tree` and `data` in `treats` format manually

## Feedback

### Manual suggestions
 - [ ] See list of issues from NC
    - [ ] change and standardise throughout the manuscript `"traits"` object to `"traits"`, etc.
    - [ ] Add Louie's example
 - [ ] Create an introduction step by step story example: For example "Fred is writing a paper about the evolution of tetrapods. He's got some really interesting results and it looks like his traits are evolving differently before and after a mass extinction event. However, Fred can't be sure this is not just the effect of the method he's using. Maybe he'd expect this result? How can he tell whether his results truly are interesting? A solution would be to simulate the very situation Fred is interested in and then to see if the methods give the same results. However, existing packages only let fred simulate traits or a tree. This is where treats comes in" But change this to a higher level: if scenario X happens would I be able to detect X? "Given a scenario, would I detect something anyways (puttick style)?" Maybe use more to do: simulate a pattern and see if that pattern is discernabble in a way or another (rather than doing it from an empirical scenario).

@name@ is studying the evolution of tetrapods disparity (i.e. diversity of shapes) across the K-Pg extinction (K-Pg - @@@dates).
They observe a decrease of morphological disparity after the extinction event followed by a sharp increase of disparity.
However @name@ is also prudent and wants to test whether their observed pattern is just an artifact of the data or actually showing some effect of the K-Pg extinction on mammalian evolution.
First they need to check whether changes in disparity can actually be discernables due to an extinction event (i.e. if an extinction event happens, would it be possible to detect a change in disparity)?
Second they want to know whether these changes are due to changes are due to the mass extinction event being selective or not.

This will require @name@ to simulate both trees and traits and measure disparity from the resulting simulations and comparing it to their observed results.

Let's see how to do that with the treats package


 - [ ] add a schematic for each subsection as well (not only the main one): or maybe just do it for a specific example
 - [ ] Get some standard examples and link to the once the the end of the vignette. BM and OU with one traits that people standardly do and use. Like get 20 basic scenarios. e.g. `geiger::fitcontinuous`. Do that into the last part (example gallery).
 - [ ] put correlated traits in the documentation earlier in the vignette
 - [ ] What it does: simulates trees and traits. throughout the manual: change `disparity` to `traits` and `diversity` to `trees` 
 - [ ] Maybe a cheat sheet table for what affects what. "I want to change my extinction rate?" "where do I change that (is it in modifiers, etc...)" (kind of mini FAQ)