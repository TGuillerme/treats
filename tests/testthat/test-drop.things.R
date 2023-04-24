## Test
test_that("drop.fossils and drop.livings works", {
    ## Create a simple bd tree
    set.seed(1)
    phy <- treats(bd.params = list(extinction = 1/3), stop.rule = list(max.living = 20))
    expect_gt(Ntip(phy), 20)
    test <- drop.fossils(phy)
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 20)
    test <- drop.livings(phy)
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 7)

        ## Create a simple bd tree + traits
        set.seed(12)
        phy <- treats(bd.params = list(extinction = 1/3), stop.rule = list(max.living = 20), traits = make.traits())
        expect_gt(Ntip(phy$tree), 20)
        expect_gt(nrow(phy$data), 39)
        test <- drop.fossils(phy)
        expect_is(test, "treats")
        expect_equal(Ntip(test$tree), 20)
        expect_equal(nrow(test$data), 42)
        test <- drop.livings(phy)
        expect_is(test, "treats")
        expect_equal(Ntip(test$tree), 3)
        expect_equal(nrow(test$data), 12)
})

test_that("drop.singles works", {

    bd.params <- make.bd.params(speciation = 1, extinction = 0.2)
    stop.rule <- list(max.living = Inf, max.time = 5, max.taxa = Inf)
    traits <- make.traits()

    ## Working for regular steps
    set.seed(2)
    test <- treats(bd.params = bd.params, traits = traits, stop.rule = stop.rule, save.steps = 0.5)
    expect_equal(Ntip(test$tree), 44)
    expect_equal(Nnode(test$tree), 128)

    test_tree <- test$tree
    expect_equal(Nnode(test_tree), 128)
    tust <- drop.singles(test_tree)
    expect_is(tust, "phylo")
    expect_equal(Nnode(tust), 43)

    test_treats <- test
    expect_equal(Nnode(test_treats$tree), 128)
    tast <- drop.singles(test_treats)
    expect_is(tast, "treats")
    expect_equal(Nnode(tast$tree), 43)
    expect_equal(nrow(tast$data), 43+44)

})

test_that("drop.things works", {
    bd.params <- make.bd.params(speciation = 1, extinction = 0.2)
    stop.rule <- list(max.living = Inf, max.time = 5, max.taxa = Inf)
    traits <- make.traits()

    ## Working for regular steps
    set.seed(2)
    test <- treats(bd.params = bd.params, traits = traits, stop.rule = stop.rule, save.steps = 0.5)
    expect_equal(Nnode(test$tree), 128)
    expect_equal(Ntip(test$tree), 44)

    ## Drop things
    expect_equal(Ntip(drop.things(test, what = "fossils")$tree), 36)
    expect_equal(Nnode(drop.things(test, what = "fossils")$tree), 123)
    expect_equal(Ntip(drop.things(test, what = "livings")$tree), 8)
    expect_equal(Nnode(drop.things(test, what = "livings")$tree), 45)
    expect_equal(Ntip(drop.things(test, what = "singles")$tree), 44)
    expect_equal(Nnode(drop.things(test, what = "singles")$tree), 43)
})