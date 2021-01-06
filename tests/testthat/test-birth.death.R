library(dispRity)

## Test
test_that("simulating trees works", {

    ## Pure birth trees
    bd.params <- list(speciation = 1,
                      extinction = 0)
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
    test <- birth.death.tree.traits(bd.params, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 11)
    expect_equal(Nnode(test), 10)
    ## All tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 11)

    set.seed(1)
    stop.rule$max.living = Inf
    stop.rule$max.taxa   = Inf
    stop.rule$max.time   = 4
    test <- birth.death.tree.traits(bd.params, stop.rule)$tree
    expect_is(test, "phylo")
    ## All tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), Ntip(test))
    ## The tree age is 4
    expect_equal(test$root.time, 4)
    expect_equal(max(tree.age(test)$age), 4)

    ## Birth death trees
    bd.params <- list(speciation = 1,
                      extinction = 0.2)
    stop.rule$max.living = 10
    stop.rule$max.taxa   = Inf
    stop.rule$max.time   = Inf
    set.seed(3)
    test <- birth.death.tree.traits(bd.params, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 14)
    expect_equal(Nnode(test), 13)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 10)

    set.seed(3)
    stop.rule$max.living = Inf
    stop.rule$max.taxa   = 10
    stop.rule$max.time   = Inf
    test <- birth.death.tree.traits(bd.params, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 10)
    expect_equal(Nnode(test), 9)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 7)

    set.seed(3)
    stop.rule$max.living = Inf
    stop.rule$max.taxa   = Inf
    stop.rule$max.time   = 6
    test <- birth.death.tree.traits(bd.params, stop.rule)$tree
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
        process  = true.answer)
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 42)
    one_trait$process <- element.rank
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 3)
    one_trait$process <- branch.length
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 0.1)
    one_trait$process <- element.depth
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 2.1)

    element_rank_10 <- list(trait_id = 1, process = element.rank, start = 10)
    traits_list <- list("A" = element_rank_10)
    bd.params <- list(speciation = 1, extinction = 0.5)
    stop.rule <- list(max.living = Inf,
                      max.taxa   = 10,
                      max.time   = Inf)
    set.seed(1)
    test <- birth.death.tree.traits(bd.params, traits = traits_list, stop.rule = stop.rule)
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
    traits_list$A$start <- 10
    test <- birth.death.tree.traits(bd.params, traits = traits_list, stop.rule)
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
    traits_list$A$process <- branch.length
    traits_list$B <- traits_list$A
    traits_list$C <- traits_list$A
    test <- birth.death.tree.traits(bd.params, traits = traits_list, stop.rule)

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
                           process  = element.rank,
                           start    = c(0,10,20)),
                "B" = list(trait_id = 4,
                           process  = branch.length,
                           start    = 0)
                )
    set.seed(1)
    test <- birth.death.tree.traits(bd.params, traits = complex_traits, stop.rule)

    expect_equal(test$data[,1], test$data[,2] - 10)
    expect_equal(test$data[,1], test$data[,3] - 20)
    expect_equal(unname(test$data[,4]), c(0, test$tree$edge.length))


    ## Multidimensional brownian trait
    complex_traits <- list(
                "A" = list(trait_id = 1:3,
                           process  = BM.process,
                           start    = c(0,0,0)),
                "B" = list(trait_id = 4,
                           process  = branch.length,
                           start    = 0)
                )
    set.seed(1)
    test <- birth.death.tree.traits(bd.params, traits = complex_traits, stop.rule)
    expect_equal(dim(test$data), c(19, 4))
    expect_equal(unname(test$data[,4]), c(0, test$tree$edge.length))
})

# test_that("simulating trees + traits + modifiers work", {

#     ## Test with longer waiting time for traits with positive values


#     ## Test with extinction for traits with negative values


#     ## Test with longer average waiting time for traits with negative values

  
#     ## Test with average more extinction for traits with positive values


#     ## Visual tests

#     ## Setting up all parameters


# })

test_that("events work", {

    stop.rule <- list(max.time = 5, max.taxa = Inf, max.living = Inf)
    traits <- NULL
    modifiers <- NULL
    bd.params <- list(extinction = 0, speciation = 1)

    ###################
    ## Taxa events
    ###################   
    events <- make.events(target = "taxa",
                          condition = time.condition(4),
                          modification = random.extinction(0.8))

    set.seed(1)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = NULL, modifiers = NULL, events = events)
    # plot(test$tree) ; axisPhylo()
    ## 165 taxa generated
    expect_equal(Ntip(test$tree), 165)
    ## 57 extinct
    expect_equal(sum(dispRity::tree.age(test$tree)$age[1:165] > 0), 102)
    ## Only two ages for tips (0 or 0.998)
    expect_equal(unique(dispRity::tree.age(test$tree)$age[1:115]), c(0.998, 0))


    ## Mass extinction based on trait values at time t
    events <- make.events(target = "taxa",
                          condition = time.condition(4),
                          modification = trait.extinction(1))

    set.seed(7)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = make.traits(), modifiers = NULL, events = events)
    class(test) <- c("dads")
    # plot(test)
    ## 244 taxa generated
    expect_equal(Ntip(test$tree), 244)
    ## 57 extinct
    expect_equal(sum(dispRity::tree.age(test$tree)$age[1:244] > 0), 89)
    ## Only two ages for tips
    expect_equal(unique(dispRity::tree.age(test$tree)$age[1:244]), c(0.974, 0))
    ## Trait values for living and extinct is different
    living <- test$data[test$tree$tip.label[dispRity::tree.age(test$tree)$age[1:244] == 0], ]
    extinct <- test$data[test$tree$tip.label[dispRity::tree.age(test$tree)$age[1:244] == 0.974], ]
    expect_equal(round(mean(living), 6), 1.929087)
    expect_equal(round(mean(extinct), 7), -0.5327465)


    ###################
    ## bd.params events
    ###################

    ## Adding extinction after reaching n taxa
    bd.params <- list(extinction = 0, speciation = 1)
    stop.rule <- list(max.living = 50, max.time = Inf, max.taxa = Inf)   
 
    ## Make a dummy events object
    events <- make.events(
        condition    = taxa.condition(30),
        target       = "bd.params",
        modification = update.bd.params(1/3, "extinction"))
    ## Testing the results
    set.seed(2)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = NULL, modifiers = NULL, events = events)
    # plot(test$tree)
    ## 62 tips
    expect_equal(Ntip(test$tree), 62)
    ## 50 living (12 fossils)
    expect_equal(sum(tree.age(test$tree)$age[1:62] == 0),50)
    ## But bd.params$extinction still 0
    expect_equal(bd.params$extinction, 0)


    ## Reducing speciation after reaching time t
    events <- make.events(
        condition    = time.condition(2),
        target       = "bd.params",
        modification = update.bd.params(1/3, "speciation"))
    
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
        condition    = time.condition(5),
        target       = "traits",
        modification = update.traits(process = OU.process))
    
    set.seed(1)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = NULL, events = NULL)
    set.seed(1)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = NULL, events = events)
    ## Visual testing
    # par(mfrow = c(1,2))
    # class(test) <- "dads" ; plot(test, ylim = c(-5, 8))
    # class(test2) <- "dads" ; plot(test2, ylim = c(-5, 8))
    expect_false(nrow(test$data) == nrow(test2$data))





    ## Changing a trait argument (e.g. sigma) when a trait reaches value x
    ## A 2D correlated BM
    traits <- make.traits(n = 2, process.args = list(Sigma = matrix(1, 2, 2)))

    events <- make.events(
        condition    = trait.condition(3, absolute = TRUE),
        target       = "traits",
        modification = update.traits(process.args = list(Sigma = matrix(c(10,3,3,2),2,2))))

    stop.rule$max.time <- Inf
    stop.rule$max.taxa <- 100

    set.seed(8)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = NULL, events = NULL)
    set.seed(8)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = NULL, events = events)
    ## Visual testing
    # par(mfrow = c(2,1))
    # class(test) <- "dads" ; plot(test, trait = 2)
    # class(test2) <- "dads" ; plot(test2, trait = 2)
    # plot(test, trait = c(1:2))
    # plot(test2, trait = c(1:2), use.3D = TRUE)

    ## Testing the difference in correlation
    expect_equal(cor(test$data[, 1], test$data[, 2]), 1)
    expect_lt(cor(test2$data[, 1], test2$data[, 2]), 1)


    ## modifiers events
    ## Adding a speciation condition after reaching time t
    stop.rule <- list(max.time = 4, max.taxa = Inf, max.living = Inf)
    bd.params <- list(extinction = 0, speciation = 1)
    
    ## A default modifier
    modifiers <- make.modifiers()
    traits <- make.traits()

    ## New condition and new modifier
    new.condition <- function(trait.values, lineage) return(parent.traits(trait.values, lineage) < 0)
    new.modify    <- function(x, trait.values, lineage) return(x + 1)
    
    events <- make.events(
        condition    = time.condition(3),
        target       = "modifiers",
        modification = update.modifiers(speciation = speciation, condition = new.condition, modify = new.modify))

    set.seed(4)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = modifiers, events = NULL)
    set.seed(4)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = modifiers, events = events)

    ## Visual testing
    # par(mfrow = c(2,1))
    # class(test) <- "dads" ; plot(test)
    # class(test2) <- "dads" ; plot(test2)

    ## Less tips in the second tree
    expect_lt(Ntip(test2$tree), Ntip(test$tree))
    ## All the extinct tips in tree 2 are younger than the event (age 3)
    tip_ages <- dispRity::tree.age(test2$tree)[1:Ntip(test2$tree), ]
    fossils <- which(tip_ages$ages != 0)
    expect_false(any(tip_ages[fossils, ]$ages > test2$tree$root.time - 3))


    ## Adding a branch length condition when reaching n taxa
    new.modify <- function(x, trait.values, lineage) return(x * 100)
    events <- make.events(
        condition    = taxa.condition(30),
        target       = "modifiers",
        modification = update.modifiers(branch.length = branch.length, modify = new.modify))

    stop.rule <- list(max.time = Inf, max.taxa = 100, max.living = Inf)
    bd.params <- list(extinction = 0, speciation = 1)
    modifiers <- make.modifiers()
    traits <- make.traits()

    set.seed(5)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = modifiers, events = NULL)
    set.seed(5)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = traits, modifiers = modifiers, events = events)

    # # Visual testing
    # par(mfrow = c(2,1))
    # class(test) <- "dads" ; plot(test)
    # class(test2) <- "dads" ; plot(test2)

    ## Same number of tips
    expect_equal(Ntip(test2$tree), Ntip(test$tree))
    ## There is a bigger proportion of short branches in test than in test2
    prop_short_test <- length(which(test$tree$edge.length < 1))/length(test$tree$edge.length)
    prop_short_test2 <- length(which(test2$tree$edge.length < 1))/length(test2$tree$edge.length)
    expect_lt(prop_short_test2, prop_short_test)





    ## founding events

    ## Stop rules to test
    stop.rule.time <- list(max.taxa = Inf, max.living = Inf, max.time = 4)
    stop.rule.taxa <- list(max.taxa = 50, max.living = Inf, max.time = Inf)
    stop.rule.living <- list(max.taxa = Inf, max.living = 50, max.time = Inf)

    ## Normal bd params
    bd.params <- list(speciation = 1, extinction = 0.3)

    ## Events that generate a new process (founding effects)
    events <- make.events(condition    = taxa.condition(10),
                          target       = "founding",
                          modification = founding.event(bd.params = list(speciation = 2,
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
    expect_equal(Ntip(test2$tree), 88)
    expect_equal(sum(tree.age(test2$tree)$ages == 0), 81)
    ## Founding tree has no fossils
    founding_tips <- grep("founding_", test2$tree$tip.label)
    expect_equal(length(founding_tips), 53)
    expect_equal(Ntip(drop.tip(test2$tree, tip = test2$tree$tip.label[-founding_tips])), length(founding_tips))

    ## Max taxa
    set.seed(5)
    test1 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.taxa, traits = NULL, modifiers = NULL, events = NULL)
    expect_is(test1$tree, "phylo")
    expect_equal(Ntip(test1$tree), 50)
    expect_equal(sum(tree.age(test1$tree)$ages == 0), 35)

    set.seed(5)
    test2 <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule.taxa, traits = NULL, modifiers = NULL, events = events)
    expect_is(test2$tree, "phylo")
    expect_equal(Ntip(test2$tree), 50)
    expect_equal(sum(tree.age(test2$tree)$ages == 0), 81)
    ## Founding tree has no fossils
    founding_tips <- grep("founding_", test2$tree$tip.label)
    expect_equal(length(founding_tips), 53)
    expect_equal(Ntip(drop.tip(test2$tree, tip = test2$tree$tip.label[-founding_tips])), length(founding_tips))

})