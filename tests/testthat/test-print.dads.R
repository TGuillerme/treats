context("print.dads")

## Test
test_that("print.dads works for traits", {

    ## Simple trait
    simple <- make.traits()
    out <- capture_output(print.traits.info(simple))
    expect_equal(out[[1]], "1 trait for 1 process (A) with one starting value (0).")

    ## Complex
    complex <- make.traits(process = c(BM.process, BM.process), n = c(2,3), process.args = list(list(Sigma = diag(2)), list(Sigma = matrix(1/3, 3, 3))), trait.names = c("bib", "bob"))
    out <- capture_output(print.traits.info(complex))
    expect_equal(out[[1]], "5 traits for 2 processes (bib:2, bob:3) with one starting value (0).\nprocess bib uses the following extra argument: Sigma;\nprocess bob uses the following extra argument: Sigma;")

    ## Simple dads
    out <- capture_output(print.dads(simple))
    expect_equal(out[[1]], " ---- dads traits object ---- \n1 trait for 1 process (A) with one starting value (0).")    

})

test_that("print.dads works for dads", {

    ## Simple birth.death tree with one BM trait
    test <- dads(stop.rule = list("max.living" = 10), traits = make.traits())
    out <- capture_output(print.dads(test))
    expect_equal(out[[1]], " ---- dads object ---- \nSimulated diversity data (x$tree):\n\nPhylogenetic tree with 10 tips and 9 internal nodes.\n\nTip labels:\n  t1, t2, t3, t4, t5, t6, ...\nNode labels:\n  n1, n2, n3, n4, n5, n6, ...\n\nRooted; includes branch lengths.\n\nSimulated disparity data (x$data):\n1 trait for 1 process (A) with one starting value (0).")

    ## A more complex trait
    complex_traits <- make.traits(process = c(BM.process, BM.process), n = c(2,3), process.args = list(list(Sigma = diag(2)), list(Sigma = matrix(1/3, 3, 3))), trait.names = c("bib", "bob"))
    test <- dads(stop.rule = list("max.living" = 10), traits = complex_traits)
    out <- capture_output(print.dads(test))
    expect_equal(out[[1]], " ---- dads object ---- \nSimulated diversity data (x$tree):\n\nPhylogenetic tree with 10 tips and 9 internal nodes.\n\nTip labels:\n  t1, t2, t3, t4, t5, t6, ...\nNode labels:\n  n1, n2, n3, n4, n5, n6, ...\n\nRooted; includes branch lengths.\n\nSimulated disparity data (x$data):\n5 traits for 2 processes (bib:2, bob:3) with one starting value (0).\nprocess bib uses the following extra argument: Sigma;\nprocess bob uses the following extra argument: Sigma;")    
})