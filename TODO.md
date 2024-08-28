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
