context("sim.bd")

library(dispRity)

## Test
test_that("simulating trees works", {

    ## Pure birth trees
    speciation = 1
    extinction = 0

    error <- capture_error(birth.death.tree.traits(speciation, extinction, stop.rule = NULL))
    expect_equal(error[[1]], "You must provide at least one stopping rule. For example:\nstop.rule <- list(max.taxa   = 10,\n                  max.living = 10,\n                  max.time   = 10)")

    stop.rule <- list(max.living = 10)
    test <- birth.death.tree.traits(speciation = speciation, extinction = extinction, stop.rule = stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 10)
    expect_equal(Nnode(test), 9)
    ## All tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 10)

    stop.rule <- list(max.taxa = 11)
    test <- birth.death.tree.traits(speciation, extinction, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 11)
    expect_equal(Nnode(test), 10)
    ## All tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 11)

    set.seed(1)
    stop.rule <- list(max.time = 4)
    test <- birth.death.tree.traits(speciation, extinction, stop.rule)$tree
    expect_is(test, "phylo")
    ## All tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), Ntip(test))
    ## The tree age is 4
    expect_equal(test$root.time, 4)
    expect_equal(max(tree.age(test)$age), 4)

    ## Birth death trees
    speciation = 1
    extinction = 0.2

    set.seed(2)
    stop.rule <- list(max.living  = 10)
    test <- birth.death.tree.traits(speciation, extinction, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 15)
    expect_equal(Nnode(test), 14)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 10)

    set.seed(2)
    stop.rule <- list(max.taxa = 10)
    test <- birth.death.tree.traits(speciation, extinction, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 10)
    expect_equal(Nnode(test), 9)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 6)

    set.seed(2)
    stop.rule <- list(max.time = 6)
    test <- birth.death.tree.traits(speciation, extinction, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 139)
    expect_equal(Nnode(test), 138)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 105)
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
        n = 1,
        process = true.answer)
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 42)
    one_trait$process <- element.rank
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 3)
    one_trait$process <- branch.length
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 0.1)
    one_trait$process <- element.depth
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 2.1)

    element_rank_10 <- list(n = 1, process = element.rank, start = 10)
    traits_list <- list("A" = element_rank_10)
    set.seed(7)
    test <- birth.death.tree.traits(speciation = 1, extinction = 0.5, traits = traits_list, stop.rule = list(max.taxa = 10))
    expect_is(test, "list")
    expect_equal(names(test), c("tree", "traits"))
    expect_is(test[[1]], "phylo")
    expect_is(test[[2]], c("matrix", "array"))
    expect_equal(nrow(test[[2]]), Ntip(test$tree) + Nnode(test$tree))


    ## Visual checking
    set.seed(7)
    traits_list$A$start <- 10
    test <- birth.death.tree.traits(speciation = 1, extinction = 0.5, traits = traits_list, stop.rule = list(max.living = 20))
    ## Right dimensions
    expect_equal(dim(test$traits), c(Ntip(test$tree) + Nnode(test$tree), 1))
    expect_equal(length(which(test$traits == max(test$traits))), 2)
    # ## Visual checking
    # tree_plot <- test$tree
    # tree_plot$edge.length <- rep(1, Nedge(tree_plot))
    # plot(tree_plot)
    # nodelabels(paste(test$tree$node.label, sep = ":", test$traits[test$tree$node.label,1]), cex = 0.5)
    # tiplabels(paste(test$tree$tip.label, sep = ":", test$traits[test$tree$tip.label,1]), cex = 0.5)

    set.seed(10)
    traits_list$A$process <- branch.length
    traits_list$B <- traits_list$A
    traits_list$C <- traits_list$A
    test <- birth.death.tree.traits(speciation = 1, extinction = 0.5, traits = traits_list, stop.rule = list(max.taxa = 10))

    ## The three traits are equal
    expect_equal(test$traits[,1], test$traits[,2])
    expect_equal(test$traits[,2], test$traits[,3])
    expect_equal(test$traits[,1], test$traits[,3])

    ## The first trait value is 10
    expect_equal(test$traits[1,1], 10)

    ## The traits are equal to edge lengths
    expect_equal(unname(round(sort(test$traits[-1,1]), 5)),
                 round(sort(test$tree$edge.length), 5))

    ## Visual checking
    # tree_plot <- test$tree
    # plot(tree_plot)
    # nodelabels(paste(test$tree$node.label, sep = ":", round(test$traits[test$tree$node.label,1], 2)), cex = 1)
    # tiplabels(paste(test$tree$tip.label, sep = ":", round(test$traits[test$tree$tip.label,1], 2)), cex = 1)
    # edgelabels(round(test$tree$edge.length, 2))


    ## Complex traits
    # traits <- list(
    #             "A" = list(n       = 3,
    #                        process = element.rank,
    #                        start   = c(0,10,20))
    #             "B" = list(n       = 1,
    #                        process = branch.length,
    #                        start   = 0)
    #             )
})

