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
    traits <- list(
        n = 1,
        process = true.answer)
    expect_equal(sim.element.trait(parent.trait = 2, edge.length = 0.1, traits), 42)
    traits$process <- element.rank
    expect_equal(sim.element.trait(parent.trait = 2, edge.length = 0.1, traits), 3)
    traits$process <- branch.length
    expect_equal(sim.element.trait(parent.trait = 2, edge.length = 0.1, traits), 0.1)
    traits$process <- element.depth
    expect_equal(sim.element.trait(parent.trait = 2, edge.length = 0.1, traits), 2.1)

    ## Sim.element.trait internals work (for three traits)
    traits$n <- 3
    expect_equal(sim.element.trait(parent.trait = c(1,2,3), edge.length = 0.1, traits), c(1.1, 2.1, 3.1))

    ## sapply loop works
    living <- c(1,2,3)
    trait_table <- cbind(parent = c(1,2,3), element = c(2,3,4), edge = c(5,6,7),
                        matrix(1, ncol = 3, nrow = 3))
    expect_equal(unname(sim.living.tips(2, trait_table, traits)), matrix(7, 1, 3))
    expect_equal(unname(sim.living.tips(3, trait_table, traits)), matrix(8, 1, 3))

    element_rank_10 <- list(n = 1, process = element.rank, start = 10)
    set.seed(7)
    test <- birth.death.tree.traits(speciation = 1, extinction = 0.5, traits = element_rank_10, stop.rule = list(max.taxa = 10))
    expect_is(test, "list")
    expect_equal(names(test), c("tree", "traits"))
    expect_is(test[[1]], "phylo")
    expect_is(test[[2]], c("matrix", "array"))
    expect_equal(nrow(test[[2]]), Ntip(test$tree) + Nnode(test$tree))

    expect_equal(test, "error in node values attribution!")

    ## Visual checking
    plot(tree)

    nodelabels(test$traits[tree$node.label,1])
    tiplabels(test$traits[tree$tip.label,1])


# traits <- list(
#     n = 2,
#     start = 0,
#     process = function(x0, ...) return(x0 + 1),
#     cor = diag(1, 1, 1))
# speciation = 1
# extinction = 0.5
# stop.rule = list(max.taxa = 10)

# set.seed(1)
# test <- birth.death.tree.traits(speciation = 1, extinction = 0.5, traits = traits, stop.rule = list(max.taxa = 10))
# tree <- test$tree
# test$traits
})

