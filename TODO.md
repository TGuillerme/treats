# TODO list: finish package!

## New function
 - [x] allow inputing a tree and simulating from there (handled separately through `map.trait` (map trait on tree) with `map.trait(tree = tree, trait = traits)`.

## utilities

 - [x] add manual for utilities
 - [x] example for: `utilities::parent.traits`: Write examples (write manual at the same time)
 - [ ] add test for `parent.traits`

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
    - [ ] In the example in 01_getting-started from line 191 you mention the staying alive function but instead code the going extinct function. suspect you switched examples at some point? Fix this
    - [ ] Line 234 01-getting-started refers to an object called biased_data. At this stage you haven't explained what you mean by biased, so I think I'd change this to modified_data or something more closely linked to the text surrounding the example? Biased means bad to most people so this might cause confusion. This is then used later in the examples too so all need to be modified
    - [ ] Also in 03-make.modifiers from line 317 onwards
    - [ ] In 02-make-traits: I don't understand the bit from line 145-167 about distributing processes and traits. Rephrase.
    - [ ] I don't think you need so much explanation of this in the 02-make-traits bit so I cut it down. Only matters what dads thinks a trait is
    - [ ] I think the explanation for the plot colours needs to come earlier in the text, perhaps in some early examples before going into the full details?
    - [ ] dads function manual - add link to vignette
    - [ ] 03-make.mofidiers: Line 49: object 'lineage' not found; Line 56: object 'lineage' not found This is when running one chapter at a time, so it might be defined in a previous chapter? But probably worth fixing this. Ah update - I see this code is not evaluated, so you might not care
    - [ ] Add Louie's example
 - [ ] Create an introduction step by step story example: For example "Fred is writing a paper about the evolution of tetrapods. He's got some really interesting results and it looks like his traits are evolving differently before and after a mass extinction event. However, Fred can't be sure this is not just the effect of the method he's using. Maybe he'd expect this result? How can he tell whether his results truly are interesting? A solution would be to simulate the very situation Fred is interested in and then to see if the methods give the same results. However, existing packages only let fred simulate traits or a tree. This is where treats comes in" But change this to a higher level: if scenario X happens would I be able to detect X? "Given a scenario, would I detect something anyways (puttick style)?" Maybe use more to do: simulate a pattern and see if that pattern is discernabble in a way or another (rather than doing it from an empirical scenario).
 - [ ] add a schematic for each subsection as well (not only the main one): or maybe just do it for a specific example
 - [ ] Get some standard examples and link to the once the the end of the vignette. BM and OU with one traits that people standardly do and use. Like get 20 basic scenarios. e.g. `geiger::fitcontinuous`. Do that into the last part (example gallery).
 - [ ] put correlated traits in the documentation earlier in the vignette
 - [ ] What it does: simulates trees and traits. throughout the manual: change `disparity` to `traits` and `diversity` to `trees` 
 - [ ] Maybe a cheat sheet table for what affects what. "I want to change my extinction rate?" "where do I change that (is it in modifiers, etc...)" (kind of mini FAQ)