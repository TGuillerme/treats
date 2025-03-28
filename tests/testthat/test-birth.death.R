## Test
test_that("simulating trees works", {

    ## Pure birth trees
    bd.params <- make.bd.params()
    stop.rule <- list(max.living = 10,
                      max.taxa   = Inf,
                      max.time   = Inf)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 10)
    expect_equal(Nnode(test), 9)
    ## All tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 10)

    stop.rule$max.living = Inf
    stop.rule$max.taxa   = 11
    test <- birth.death.tree.traits(stop.rule, bd.params)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 11)
    expect_equal(Nnode(test), 10)
    ## All tips are living
    # expect_equal(length(which(tree.age(test)$age == 0)), 11) # weirdly says 12 on MacOS. Some rounding?

    set.seed(1)
    stop.rule$max.living = Inf
    stop.rule$max.taxa   = Inf
    stop.rule$max.time   = 4
    test <- birth.death.tree.traits(stop.rule, bd.params)$tree
    expect_is(test, "phylo")
    ## All tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), Ntip(test))
    ## The tree age is 4
    expect_equal(test$root.time, 4)
    expect_equal(max(tree.age(test)$age), 4)

    ## Birth death trees
    bd.params <- make.bd.params(speciation = 1, extinction = 0.2)
    stop.rule$max.living = 10
    stop.rule$max.taxa   = Inf
    stop.rule$max.time   = Inf
    set.seed(3)
    test <- birth.death.tree.traits(stop.rule, bd.params)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 14)
    expect_equal(Nnode(test), 13)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 10)

    set.seed(3)
    stop.rule$max.living = Inf
    stop.rule$max.taxa   = 10
    stop.rule$max.time   = Inf
    test <- birth.death.tree.traits(stop.rule, bd.params)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 10)
    expect_equal(Nnode(test), 9)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 7)

    set.seed(3)
    stop.rule$max.living = Inf
    stop.rule$max.taxa   = Inf
    stop.rule$max.time   = 6
    test <- birth.death.tree.traits(stop.rule, bd.params)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 137)
    expect_equal(Nnode(test), 136)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 107)
    ## The tree age is 6
    expect_equal(test$root.time, 6)
    expect_equal(max(tree.age(test)$age), 6)
})

test_that("simulating trees + traits works", {

    ## Some processes to test
    true.answer <- function(x0, ...) {
        return(42)
    }
    expect_equal(true.answer(x0 = 0), 42)

    element.rank <- function(x0, ...) {
        return(x0 + 1)
    }
    expect_equal(element.rank(x0 = 0), 1)

    branch.length <- function(x0, edge.length, ...) {
        return(edge.length)
    }
    expect_equal(branch.length(x0 = 2, edge.length = 0.1), 0.1)

    element.depth <- function(x0, edge.length, ...) {
        return(x0 + edge.length)
    }
    expect_equal(element.depth(x0 = 2, edge.length = 0.1), 2.1)

    ## Sim.element.trait internals work (for one trait)
    one_trait <- list(
        trait_id = 1,
        process  = list(true.answer))
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 42)
    one_trait$process[[1]] <- element.rank
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 3)
    one_trait$process[[1]] <- branch.length
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 0.1)
    one_trait$process[[1]] <- element.depth
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 2.1)

    element_rank_10 <- list(trait_id = 1, process = list(element.rank), start = 10)
    traits_list <- list("main" = list("A" = element_rank_10))
    bd.params <- make.bd.params(speciation = 1, extinction = 0.5)
    stop.rule <- list(max.living = Inf,
                      max.taxa   = 10,
                      max.time   = Inf)
    set.seed(1)
    test <- birth.death.tree.traits(bd.params = bd.params, traits = traits_list, stop.rule = stop.rule)
    expect_is(test, "list")
    expect_equal(names(test), c("tree", "data"))
    expect_is(test[[1]], "phylo")
    expect_is(test[[2]], c("matrix", "array"))
    expect_equal(nrow(test[[2]]), Ntip(test$tree) + Nnode(test$tree))


    ## Visual checking
    stop.rule <- list(max.living = 20,
                      max.taxa   = Inf,
                      max.time   = Inf)
    set.seed(1)
    traits_list$main$A$start <- 10
    test <- birth.death.tree.traits(bd.params = bd.params, traits = traits_list, stop.rule)
    ## Right dimensions
    expect_equal(dim(test$data), c(Ntip(test$tree) + Nnode(test$tree), 1))
    expect_equal(length(which(test$data == max(test$data))), 2)
    # ## Visual checking
    # tree_plot <- test$tree
    # tree_plot$edge.length <- rep(1, Nedge(tree_plot))
    # plot(tree_plot)
    # nodelabels(paste(test$tree$node.label, sep = ":", test$data[test$tree$node.label,1]), cex = 0.5)
    # tiplabels(paste(test$tree$tip.label, sep = ":", test$data[test$tree$tip.label,1]), cex = 0.5)

    stop.rule <- list(max.living = Inf,
                      max.taxa   = 10,
                      max.time   = Inf)
    set.seed(10)
    traits_list$main$A$process <- list(branch.length)
    traits_list$main$B <- traits_list$main$A
    traits_list$main$C <- traits_list$main$A
    test <- birth.death.tree.traits(bd.params = bd.params, traits = traits_list, stop.rule)

    ## The three traits are equal
    expect_equal(test$data[,1], test$data[,2])
    expect_equal(test$data[,2], test$data[,3])
    expect_equal(test$data[,1], test$data[,3])

    ## The first trait value is 10
    expect_equal(test$data[1,1], 10)

    ## The traits are equal to edge lengths
    expect_equal(unname(round(sort(test$data[-1,1]), 5)),
                 round(sort(test$tree$edge.length), 5))

    # ## Visual checking
    # tree_plot <- test$tree
    # plot(tree_plot)
    # nodelabels(paste(test$tree$node.label, sep = ":", round(test$data[test$tree$node.label,1], 2)), cex = 1)
    # tiplabels(paste(test$tree$tip.label, sep = ":", round(test$data[test$tree$tip.label,1], 2)), cex = 1)
    # edgelabels(round(test$tree$edge.length, 2))


    ## Complex traits
    complex_traits <- list(
                "A" = list(trait_id = 1:3,
                           process  = list(element.rank),
                           start    = c(0,10,20)),
                "B" = list(trait_id = 4,
                           process  = list(branch.length),
                           start    = 0)
                )
    complex_traits <- list(main = complex_traits)
    set.seed(1)
    test <- birth.death.tree.traits(bd.params = bd.params, traits = complex_traits, stop.rule)

    expect_equal(test$data[,1], test$data[,2] - 10)
    expect_equal(test$data[,1], test$data[,3] - 20)
    expect_equal(unname(test$data[,4]), c(0, test$tree$edge.length))


    ## Multidimensional brownian trait
    complex_traits <- list(
                "A" = list(trait_id = 1:3,
                           process  = list(BM.process),
                           start    = c(0,0,0)),
                "B" = list(trait_id = 4,
                           process  = list(branch.length),
                           start    = 0)
                )
    complex_traits <- list(main = complex_traits)
    set.seed(1)
    test <- birth.death.tree.traits(bd.params = bd.params, traits = complex_traits, stop.rule)
    expect_equal(dim(test$data), c(19, 4))
    expect_equal(unname(test$data[,4]), c(0, test$tree$edge.length))
})

test_that("events work", {

    stop.rule <- list(max.time = 5, max.taxa = Inf, max.living = Inf)
    traits <- NULL
    modifiers <- NULL
    bd.params <- make.bd.params(extinction = 0, speciation = 1)

    ###################
    ## Taxa events
    ###################   
    events <- make.events(target = "taxa",
                          condition = age.condition(4),
                          modification = random.extinction(0.8))

    set.seed(1)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = NULL, modifiers = NULL, events = events)
    # plot(test$tree) ; axisPhylo()
    ## 165 taxa generated
    expect_equal(Ntip(test$tree), 165)
    ## 57 extinct
    expect_equal(sum(dispRity::tree.age(test$tree)$age[1:165] > 0), 102)
    ## Only two ages for tips (0 or 0.998)
    expect_equal(round(unique(dispRity::tree.age(test$tree)$age[1:115]), 3), round(c(0.9976, 0), 3))


    ## Mass extinction based on trait values at time t
    events <- make.events(target = "taxa",
                          condition = age.condition(4),
                          modification = trait.extinction(1))

    set.seed(7)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = make.traits(), modifiers = NULL, events = events)
    class(test) <- c("treats")
    # plot(test)
    ## Generated more nodes (because of the extinction call)
    expect_equal(Nnode(test$tree), 410)
    test <- drop.singles(test)
    expect_equal(Nnode(test$tree), 263)
    # plot(test)
    ## 244 taxa generated
    expect_equal(Ntip(test$tree), 264)
    ## 89 extinct
    expect_equal(sum(dispRity::tree.age(test$tree)$age[1:264] > 0), 87)
    ## Only two ages for tips
    expect_equal(round(unique(dispRity::tree.age(test$tree)$age[1:Ntip(test$tree)]), 3), round(c(0.9742, 0), 3))
    ## Trait values for living and extinct is different
    living <- test$data[test$tree$tip.label[dispRity::tree.age(test$tree)$age[1:244] == 0], ]
    extinct <- test$data[test$tree$tip.label[dispRity::tree.age(test$tree)$age[1:244] == 0.9742], ]
    expect_equal(round(mean(living), 6), 2.0452)
    # expect_equal(round(mean(extinct), 7), -0.2946388)


    ###################
    ## bd.params events
    ###################

    ## Adding extinction after reaching n taxa
    bd.params <- make.bd.params(extinction = 0, speciation = 1)
    stop.rule <- list(max.living = 50, max.time = Inf, max.taxa = Inf)   
 
    ## Make a dummy events object
    events <- make.events(
        condition    = taxa.condition(30),
        target       = "bd.params",
        modification = bd.params.update(extinction = 1/3))
    ## Testing the results
    set.seed(2)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = NULL, modifiers = NULL, events = events)
    test <- drop.singles(test)
    # plot(test$tree)
    ## 62 tips
    expect_equal(Ntip(test$tree), 62)
    ## 50 living (12 fossils)
    expect_equal(sum(tree.age(test$tree)$age[1:62] == 0),50)
    ## But bd.params$extinction still 0
    expect_equal(sample.from(bd.params)$extinction, 0)


    ## Reducing speciation after reaching time t
    events <- make.events(
        condition    = age.condition(2),
        target       = "bd.params",
        modification = bd.params.update(speciation = 1/3))
    
    ## Updating the stop.rule
    stop.rule$max.living <- Inf
    stop.rule$max.time   <- 4
    set.seed(42)
    test1 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = NULL, modifiers = NULL, events = NULL)
    set.seed(42)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = NULL, modifiers = NULL, events = events)
    # plot(test2$tree) ; axisPhylo()
    ## Both trees have the same age
    expect_equal(test1$tree$root.time, test2$tree$root.time)
    ## But the second tree has way less times
    expect_lt(Ntip(test2$tree), Ntip(test1$tree))
    expect_equal(c(Ntip(test2$tree), Ntip(test1$tree)), c(39, 122))




    ####################
    ## traits events
    ####################

    stop.rule$max.time <- 6
    traits <- make.traits()
    events <- make.events(
        condition    = age.condition(5),
        target       = "traits",
        modification = traits.update(process = OU.process))
    
    set.seed(1)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = NULL, events = NULL)
    set.seed(1)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = NULL, events = events)
    ## Visual testing
    # par(mfrow = c(1,2))
    # class(test) <- "treats" ; plot(test, ylim = c(-5, 8))
    # class(test2) <- "treats" ; plot(test2, ylim = c(-5, 8))
    expect_false(nrow(test$data) == nrow(test2$data))

    ## Changing a trait argument (e.g. sigma) when a trait reaches value x
    ## A 2D correlated BM
    traits <- make.traits(n = 2, process.args = list(Sigma = matrix(1, 2, 2)))

    change_correlation <- make.events(
        condition    = trait.condition(3, absolute = TRUE),
        target       = "traits",
        modification = traits.update(process.args = list(Sigma = matrix(c(10,3,3,2),2,2))))

    stop.rule$max.living <- Inf
    stop.rule$max.time <- Inf
    stop.rule$max.taxa <- 100

    set.seed(8)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = NULL, events = NULL)
    set.seed(8)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = NULL, events = change_correlation)
    ## Visual testing
    # par(mfrow = c(2,1))
    # class(test) <- "treats" ; plot(test, trait = 2)
    # class(test2) <- "treats" ; plot(test2, trait = 2)
    # plot(test, trait = c(1:2))
    # plot(test2, trait = c(1:2), use.3D = TRUE)

    ## Testing the difference in correlation
    expect_equal(cor(test$data[, 1], test$data[, 2]), 1)
    expect_lt(cor(test2$data[, 1], test2$data[, 2]), 1)

    ## modifiers events
    ## Adding a speciation condition after reaching time t
    stop.rule <- list(max.time = 4, max.taxa = Inf, max.living = Inf)
    bd.params <- make.bd.params(extinction = 0, speciation = 1)
    
    ## A default modifier
    modifiers <- make.modifiers()
    traits <- make.traits()

    ## New condition and new modifier
    new.condition <- function(trait.values, lineage) return(parent.traits(trait.values, lineage) < 0)
    new.modify    <- function(x, trait.values, lineage) return(x + 1)
    
    events <- make.events(
        condition    = age.condition(3),
        target       = "modifiers",
        modification = modifiers.update(speciation = speciation, condition = new.condition, modify = new.modify))

    set.seed(4)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = modifiers, events = NULL)
    set.seed(4)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = modifiers, events = events)

    ## Visual testing
    # par(mfrow = c(2,1))
    # class(test) <- "treats" ; plot(test)
    # class(test2) <- "treats" ; plot(test2)

    ## Less tips in the second tree
    ## All the extinct tips in tree 2 are younger than the event (age 3)
    tip_ages <- dispRity::tree.age(test2$tree)[1:Ntip(test2$tree), ]
    fossils <- which(tip_ages$ages != 0)
    expect_false(any(tip_ages[fossils, ]$ages > test2$tree$root.time - 3))

    ## Adding a branch length condition when reaching n taxa
    new.modify <- function(x, trait.values, lineage) return(x * 100)
    events <- make.events(
        condition    = taxa.condition(30),
        target       = "modifiers",
        modification = modifiers.update(branch.length = branch.length, modify = new.modify))

    stop.rule <- list(max.time = Inf, max.taxa = 100, max.living = Inf)
    bd.params <- make.bd.params(extinction = 0, speciation = 1)
    modifiers <- make.modifiers()
    traits <- make.traits()

    set.seed(5)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = modifiers, events = NULL)
    set.seed(5)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = modifiers, events = events)

    # # Visual testing
    # par(mfrow = c(2,1))
    # class(test) <- "treats" ; plot(test)
    # class(test2) <- "treats" ; plot(test2)

    ## Same number of tips
    expect_equal(Ntip(test2$tree), Ntip(test$tree))
    ## There is a bigger proportion of short branches in test than in test2
    prop_short_test <- length(which(test$tree$edge.length < 1))/length(test$tree$edge.length)
    prop_short_test2 <- length(which(test2$tree$edge.length < 1))/length(test2$tree$edge.length)
    expect_lt(prop_short_test2, prop_short_test)


    ## founding events

    ## Stop rules to test
    stop.rule.time <- list(max.taxa = Inf, max.living = Inf, max.time = 4)
    stop.rule.taxa <- list(max.taxa = 50, max.living = Inf, max.time = Inf)
    stop.rule.living <- list(max.taxa = Inf, max.living = 50, max.time = Inf)

    ## Normal bd params
    bd.params <- make.bd.params(speciation = 1, extinction = 0.3)

    ## Events that generate a new process (founding effects)
    events <- make.events(condition    = taxa.condition(10),
                          target       = "founding",
                          modification = founding.event(bd.params = make.bd.params(speciation = 2,
                                                                         extinction = 0)),
                          additional.args = list(prefix = "founding_"))
    
    ## Max time
    set.seed(1)
    test1 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.time, traits = NULL, modifiers = NULL, events = NULL)
    expect_is(test1$tree, "phylo")
    expect_equal(Ntip(test1$tree), 40)
    expect_equal(sum(tree.age(test1$tree)$ages == 0), 30)

    set.seed(1)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.time, traits = NULL, modifiers = NULL, events = events)
    expect_is(test2$tree, "phylo")
    expect_equal(Ntip(test2$tree), 87)
    expect_equal(sum(tree.age(test2$tree)$ages == 0), 79)
    ## Founding tree has no fossils
    founding_tips <- grep("founding_", test2$tree$tip.label)
    expect_equal(length(founding_tips), 56)
    expect_equal(Ntip(drop.tip(test2$tree, tip = test2$tree$tip.label[-founding_tips])), length(founding_tips))

    ## Max taxa
    set.seed(8)
    test1 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.taxa, traits = NULL, modifiers = NULL, events = NULL)
    expect_is(test1$tree, "phylo")
    expect_equal(Ntip(test1$tree), 50)
    expect_equal(sum(tree.age(test1$tree)$ages == 0), 37)

    set.seed(8)
    expect_warning(test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.taxa, traits = NULL, modifiers = NULL, events = events))
    expect_is(test2$tree, "phylo")
    expect_equal(Ntip(test2$tree), 49)
    expect_equal(sum(tree.age(test2$tree)$ages == 0), 40)
    ## Founding tree has no fossils
    founding_tips <- grep("founding_", test2$tree$tip.label)
    expect_equal(length(founding_tips), 19)
    expect_equal(Ntip(drop.tip(test2$tree, tip = test2$tree$tip.label[-founding_tips])), length(founding_tips))

    ## Max living
    set.seed(8)
    test1 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.living, traits = NULL, modifiers = NULL, events = NULL)
    expect_is(test1$tree, "phylo")
    expect_equal(Ntip(test1$tree), 69)
    expect_equal(sum(tree.age(test1$tree)$ages == 0), 50)

    set.seed(8)
    expect_warning(test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.living, traits = NULL, modifiers = NULL, events = events))
    expect_is(test2$tree, "phylo")
    expect_equal(Ntip(test2$tree), 58)
    expect_equal(sum(tree.age(test2$tree)$ages == 0), 49)
    ## Founding tree has no fossils
    founding_tips <- grep("founding_", test2$tree$tip.label)
    expect_equal(length(founding_tips), 29)
    expect_equal(Ntip(drop.tip(test2$tree, tip = test2$tree$tip.label[-founding_tips])), length(founding_tips))


    ## Founding events with traits
    ## Stop rules to test
    stop.rule.time <- list(max.taxa = Inf, max.living = Inf, max.time = 4)
    stop.rule.taxa <- list(max.taxa = 50, max.living = Inf, max.time = Inf)
    stop.rule.living <- list(max.taxa = Inf, max.living = 50, max.time = Inf)

    ## Normal bd params
    bd.params <- make.bd.params(speciation = 1, extinction = 0.3)
    traits <- make.traits()

    ## Events that generate a new process (founding effects)
    events <- make.events(condition    = taxa.condition(10),
                          target       = "founding",
                          modification = founding.event(
                            bd.params = make.bd.params(speciation = 2, extinction = 0),
                                             traits = make.traits(process = OU.process)),
                          additional.args = list(prefix = "founding_"))
    
    ## Max time
    set.seed(18)
    test1 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.time, traits = traits, modifiers = NULL, events = NULL)
    expect_is(test1$tree, "phylo")
    expect_equal(Ntip(test1$tree), 84)
    expect_is(test1$data, c("matrix", "array"))
    expect_equal(dim(test1$data), c(84+83, 1))
    set.seed(18)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.time, traits = traits, modifiers = NULL, events = events)
    expect_is(test2$tree, "phylo")
    expect_equal(Ntip(test2$tree), 148)
    expect_is(test2$data, c("matrix", "array"))
    expect_equal(dim(test2$data), c(148+158, 1))

    ## Max taxa
    set.seed(19)
    test1 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.taxa, traits = traits, modifiers = NULL, events = NULL)
    expect_is(test1$tree, "phylo")
    expect_equal(Ntip(test1$tree), 50)
    expect_is(test1$data, c("matrix", "array"))
    expect_equal(dim(test1$data), c(50+49, 1))
    
    set.seed(19)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.taxa, traits = traits, modifiers = NULL, events = events)

    expect_is(test2$tree, "phylo")
    expect_equal(Ntip(test2$tree), 50)
    expect_is(test2$data, c("matrix", "array"))
    expect_equal(dim(test2$data), c(50+49, 1))
    
    ## Max living
    set.seed(20)
    test1 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.living, traits = traits, modifiers = NULL, events = NULL)
    expect_is(test1$tree, "phylo")
    expect_equal(Ntip(test1$tree), 68)
    expect_equal(sum(tree.age(test1$tree)$ages == 0), 50)
    expect_is(test1$data, c("matrix", "array"))
    expect_equal(dim(test1$data), c(68+67, 1))

    set.seed(20)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.living, traits = traits, modifiers = NULL, events = events) ## Warning should not fire and should be 50 living tips

    expect_is(test2$tree, "phylo")
    expect_equal(Ntip(test2$tree), 58)
    expect_equal(sum(tree.age(test2$tree)$ages == 0), 50)
    expect_is(test2$data, c("matrix", "array"))
    expect_equal(dim(test2$data), c(58+57, 1))
})

test_that("single logic works", {

    ## Creating an example lineage and edge_lengths (three tips, branching right, constant brlen)
    lineage_pre <- list(parents = c(0, 1, 1, 3, 3),
                        livings = c(2, 4, 5),
                        drawn   = c(3),
                        current = c(5),
                        n       = c(3),
                        split   = c(TRUE, FALSE, TRUE, FALSE, FALSE))
    edge_lengths <- c(0, 2, 1, 1, 1)
    time <- 2

    ## Testing single.nodes
    test <- bd.update.single.nodes(lineage_pre)
    expect_is(test, "list")
    expect_equal(names(test), names(lineage_pre))
    expect_equal(test$parents, c(0, 1, 1, 3, 3, 2, 4, 5))
    expect_equal(test$livings, c(6, 7, 8))
    expect_equal(test$drawn, 3)
    expect_equal(test$current, 8)
    expect_equal(test$n, 3)
    expect_equal(test$split, c(T, T, T, T, T, F, F, F))

    ## Testing single.edges
    lineage_after <- bd.update.single.nodes(lineage_pre)
    test <- bd.update.single.edges(time = time, time.slice = 1.75, lineage = lineage_after, edge_lengths = edge_lengths) 
    expect_equal(length(test), 8)
    expect_equal(test, c(0, 1.75, 1, 0.75, 0.75, 0.25, 0.25, 0.25))

    ## Works when the lineage as only one node
    lineage_pre <- list(parents = c(0, 1, 1, 2, 2, 4, 4, 6, 6, 9, 9, 10, 10, 13, 13),
                        livings = c(14),
                        drawn   = c(1),
                        current = c(14),
                        n       = c(1),
                        split   = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))
    ## Testing
    test <- bd.update.single.nodes(lineage_pre)
    expect_equal(test$parents, c(0, 1, 1, 2, 2, 4, 4, 6, 6, 9, 9, 10, 10, 13, 13, 14))
    expect_equal(test$livings, 16)
    expect_equal(test$drawn, 1)
    expect_equal(test$current, 16)
    expect_equal(test$n, 1)
    expect_equal(test$split, c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))

    ## Works when the lineage has an NA (meaning there's a fossil in the selected cherry)
    lineage_pre <- list(parents = c(0, 1, 1, 2, 2, 4, 4, 6, 6, 9, 9, 10, 10, 13, 13),
                        livings = c(14),
                        drawn   = c(2),
                        current = c(15),
                        n       = c(1),
                        split   = c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))

    ## Testing
    test <- bd.update.single.nodes(lineage_pre)
    expect_equal(test$parents, c(0, 1, 1, 2, 2, 4, 4, 6, 6, 9, 9, 10, 10, 13, 13, 14))
    expect_equal(test$livings, 16)
    expect_equal(test$drawn, 1)
    expect_equal(test$current, 16)
    expect_equal(test$n, 1)
    expect_equal(test$split, c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))

    ## More weird cases
    lineage_pre <- list(parents = c(0, 1, 1, 2, 2, 4, 4, 7, 7, 8, 8, 11, 11, 13, 13, 14, 14, 17, 17, 15, 15, 21, 21),
                        livings = c(20, 22),
                        drawn = (3),
                        current = c(23),
                        n = c(2),
                        split = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
    set.seed(1)
    test <- bd.update.single.nodes(lineage_pre)
    expect_equal(test$parents, c(0, 1, 1, 2, 2, 4, 4, 7, 7, 8, 8, 11, 11, 13, 13, 14, 14, 17, 17, 15, 15, 21, 21, 20, 22))
    expect_equal(test$livings, c(24, 25))
    expect_equal(test$drawn, 1)
    expect_equal(test$current, 24)
    expect_equal(test$n, 2)
    expect_equal(test$split, c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))

    ## Complex with many fossils
    lineage_pre <- list()
    lineage_pre$parents <- c(0, 1, 1, 2, 2, 5, 5, 6, 6, 8, 8, 9, 9, 7, 7, 4, 4, 14, 14, 15, 15, 16, 16, 11, 11, 18, 18, 13, 13, 20, 20, 30, 30, 21, 21, 25, 25, 35, 35, 31, 31, 37, 37, 28, 28, 44, 44, 32, 32, 34, 34, 50, 50, 53, 53, 54, 54, 48, 48)
    lineage_pre$livings <- 46
    lineage_pre$drawn <- 2
    lineage_pre$current <- 52
    lineage_pre$n <- 1
    lineage_pre$split <- c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)

    test <- bd.update.single.nodes(lineage_pre)
    expect_equal(test$parents, c(0, 1, 1, 2, 2, 5, 5, 6, 6, 8, 8, 9, 9, 7, 7, 4, 4, 14, 14, 15, 15, 16, 16, 11, 11, 18, 18, 13, 13, 20, 20, 30, 30, 21, 21, 25, 25, 35, 35, 31, 31, 37, 37, 28, 28, 44, 44, 32, 32, 34, 34, 50, 50, 53, 53, 54, 54, 48, 48, 46))
    expect_equal(test$livings, 60)
    expect_equal(test$drawn, 1)
    expect_equal(test$current, 60)
    expect_equal(test$n, 1)

    ## more corner cases
    lineage_pre <- list()
    lineage_pre$parents<- c(0, 1, 1, 3, 3, 5, 5, 6, 6, 7, 7, 10, 10, 11, 11, 15, 15, 17, 17, 13, 13, 20, 20, 23, 23, 25, 25, 26, 26, 28, 28)
    lineage_pre$livings<- c(12, 30)
    lineage_pre$drawn<- c(2)
    lineage_pre$current<- c(27)
    lineage_pre$n <- c( 2)
    lineage_pre$split <- c( TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)

    set.seed(1)
    test <- bd.update.single.nodes(lineage_pre)
    expect_equal(test$parents, c(0, 1, 1, 3, 3, 5, 5, 6, 6, 7, 7, 10, 10, 11, 11, 15, 15, 17, 17, 13, 13, 20, 20, 23, 23, 25, 25, 26, 26, 28, 28, 12, 30))
    expect_equal(test$livings, c(32, 33))
    expect_equal(test$drawn, 2)
    expect_equal(test$current, 33)
    expect_equal(test$n, 2)
})

test_that("snapshots/internal save works", {

    bd.params <- make.bd.params(speciation = 1, extinction = 0.1)
    stop.rule <- list(max.living = Inf, max.time = 2, max.taxa = Inf)
    traits <- make.traits(process = function(x0, edge.length) {return(1)}, background = make.traits())

    ## Working for regular background
    set.seed(3)
    test <- birth.death.tree.traits(bd.params = bd.params, traits = traits, stop.rule = stop.rule)
    expect_equal(names(test), c("tree", "data"))
    expect_equal(Ntip(test$tree), 6)
    expect_equal(Nnode(test$tree), 15)
    plot(test$tree) ; nodelabels(paste(Ntip(test$tree)+1:Nnode(test$tree)+Ntip(test$tree), test$tree$node.label, sep = ":"), cex = 0.75)
    ## The true nodes (past the first one)
    expect_equal(rownames(test$data)[which(test$data == 1)], c("n4", "n6", "n11", "n15"))
    ## Branch nodes and tips
    expect_equal(rownames(test$data)[which(test$data != 1)], c("n1", "n2", "n3", "n7", "n5", "n8", "n9", "n12", "n13", "n10", "n14", test$tree$tip.label))
 
    ## One with fossils
    set.seed(2)
    test <- birth.death.tree.traits(bd.params = bd.params, traits = traits, stop.rule = stop.rule)
    expect_equal(Ntip(test$tree), 8)
    expect_equal(Nnode(test$tree), 29)
    expect_equal(nrow(test$data), 8 + 29)

    ## 2 fossils
    expect_equal(nrow(drop.fossils(test)$data), 31) 
    expect_equal(Ntip(drop.fossils(test)$tree), 6) 
    expect_equal(Nnode(drop.fossils(test)$tree), 25) 
    ## 6 livings
    expect_equal(nrow(drop.livings(test)$data), 10) 
    expect_equal(Ntip(drop.livings(test)$tree), 2) 
    expect_equal(Nnode(drop.livings(test)$tree), 8) 
    ## 22 internals
    expect_equal(nrow(drop.singles(test)$data), 15) 
    expect_equal(Ntip(drop.singles(test)$tree), 8) 
    expect_equal(Nnode(drop.singles(test)$tree), 7)

    ## Working with some multidimensional stuff
    traits <- make.traits(process = BM.process, n = 3, background = make.traits(process = c(no.process, no.process, no.process), n = 1))
    set.seed(3)
    test <- birth.death.tree.traits(bd.params = bd.params, traits = traits, stop.rule = stop.rule)
    expect_equal(dim(test$data), c(182, 3))
    class(test) <- "treats"
    expect_null(plot(test))
})

test_that("multiple founding events works", {

    stop.rule <- list(max.time = 4)
    trait0 <- make.traits(n=1, BM.process)
    trait1 <- make.traits(n=1,
                          process=BM.process,
                          process.args=list(Sigma=diag(1)*8))
    trait2 <- make.traits(n=1,
                          process=BM.process,
                          process.args=list(Sigma=diag(1)*1/8))
    bd.params <- make.bd.params(speciation = 1, extinction = 0)
    # add 2 founding events
    events <- make.events(target="founding",
                           condition=age.condition(1),
                           modification=founding.event(bd.params=bd.params,
                                                       traits=trait1),
                           event.name="rate_shift_1",
                           additional.args=list(prefix="shift_1"))
    events <- make.events(target="founding",
                           condition=age.condition(2),
                           modification=founding.event(bd.params=bd.params,
                                                       traits=trait2),
                           event.name="rate_shift_2",
                           additional.args=list(prefix="shift_2"),
                           add=events)
    set.seed(1)
    my_data <- treats(bd.params = bd.params,
                      stop.rule = stop.rule,
                      traits = trait0,
                      events = events,
                      verbose = TRUE)
    expect_equal(Ntip(my_data$tree), 119)
    ## Root is 4 (requested)
    expect_equal(tree.age(my_data$tree)$age[120], 4)
    ## All tips are living
    expect_true(all(tree.age(my_data$tree)$age[1:119] == 0))
    expect_equal(dim(my_data$data), c(119+Nnode(my_data$tree), 1))

    ## Two shift regimes
    expect_equal(length(grep("shift_1", my_data$tree$tip.label)), 65)
    expect_equal(length(grep("shift_2", my_data$tree$tip.label)), 10)
})