test_that("drop.tip.treats works", {

    set.seed(1)
    my_treats <- treats(stop.rule = list(max.taxa = 20),
                        traits = make.traits())
    ## Drop tips
    test <- drop.tip.treats(my_treats, tip = c("t1", "t2", "t3", "t4", "t5"))
    expect_is(test, "treats")
    expect_equal(Ntip(test$tree), 15)
    expect_equal(Nnode(test$tree), 14)
    expect_equal(nrow(test$data), 29)

    ## Keep internals
    test <- drop.tip.treats(my_treats, tip = c("t1", "t2", "t3", "t4", "t5"), trim.internal = FALSE)
    expect_is(test, "treats")
    expect_equal(Ntip(test$tree), 16)
    expect_equal(Nnode(test$tree), 15)
    expect_equal(nrow(test$data), 31)

    ## Keep tips
    test <- keep.tip.treats(my_treats, tip = c("t1", "t2", "t3", "t4", "t5"))
    expect_is(test, "treats")
    expect_equal(Ntip(test$tree), 5)
    expect_equal(Nnode(test$tree), 4)
    expect_equal(nrow(test$data), 9) 
})
