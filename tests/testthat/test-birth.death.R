context("sim.bd")

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
    set.seed(2)
    test <- birth.death.tree.traits(bd.params, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 15)
    expect_equal(Nnode(test), 14)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 10)

    set.seed(2)
    stop.rule$max.living = Inf
    stop.rule$max.taxa   = 10
    stop.rule$max.time   = Inf
    test <- birth.death.tree.traits(bd.params, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 10)
    expect_equal(Nnode(test), 9)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 6)

    set.seed(2)
    stop.rule$max.living = Inf
    stop.rule$max.taxa   = Inf
    stop.rule$max.time   = 6
    test <- birth.death.tree.traits(bd.params, stop.rule)$tree
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
    set.seed(7)
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
    set.seed(7)
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

