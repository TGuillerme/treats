context("sim.bd")

library(dispRity)

## Test
test_that("sim.bd internal works", {

    ## Pure birth trees
    speciation = 1
    extinction = 0

    stop.rule <- list(max.live = 10)
    test <- birth.death.tree.traits(speciation, extinction, stop.rule)$tree
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
    stop.rule <- list(max.live = 10)
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
    expect_equal(length(which(tree.age(test)$age == 0)), 7)

    set.seed(2)
    stop.rule <- list(max.time = 6)
    test <- birth.death.tree.traits(speciation, extinction, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 44)
    expect_equal(Nnode(test), 43)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 32)
    ## The tree age is 6
    expect_equal(test$root.time, 6)
    expect_equal(max(tree.age(test)$age), 6)
})
