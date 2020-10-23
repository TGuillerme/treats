context("drop.fossils.dads")

## Test
test_that("drop.fossils.dads works", {

    ## Create a simple bd tree
    set.seed(2)
    phy <- dads(bd.params = list(extinction = 1/3), stop.rule = list(max.living = 20))
    expect_gt(Ntip(phy), 20)
    test <- drop.fossil.dads(phy)
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 20)

    ## Create a simple bd tree + traits
    set.seed(12)
    phy <- dads(bd.params = list(extinction = 1/3), stop.rule = list(max.living = 20), traits = make.traits())
    expect_gt(Ntip(phy$tree), 20)
    expect_gt(nrow(phy$data), 39)
    test <- drop.fossil.dads(phy)
    expect_is(test, "dads")
    expect_equal(Ntip(test$tree), 20)
    expect_equal(nrow(test$data), 39)
})
