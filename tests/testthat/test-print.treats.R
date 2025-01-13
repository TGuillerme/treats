## Test
test_that("print.treats works for traits", {

    ## Simple trait
    simple <- make.traits()
    out <- capture_output(internal.print.traits.info(simple$main))
    expect_equal(out[[1]], "1 trait for 1 process (A) with one starting value (0).")

    ## Complex
    complex <- make.traits(process = c(BM.process, BM.process), n = c(2,3), process.args = list(list(Sigma = diag(2)), list(Sigma = matrix(1/3, 3, 3))), trait.names = c("bib", "bob"))
    out <- capture_output(internal.print.traits.info(complex$main))
    expect_equal(out[[1]], "5 traits for 2 processes (bib:2, bob:3) with one starting value (0).\nprocess bib uses the following extra argument: Sigma.\nprocess bob uses the following extra argument: Sigma.")

    ## Simple treats
    out <- capture_output(print.treats(simple))
    expect_equal(out[[1]], " ---- treats traits object ---- \n1 trait for 1 process (A) with one starting value (0).")

    background <- make.traits(background = make.traits())
    out <- capture_output(print.treats(background))
    expect_equal(out[[1]], " ---- treats traits object ---- \n1 trait for 1 process (A) with one starting value (0).\nAnd a background trait (see x$background for info).")
})

test_that("print.treats works for treats", {

    ## Simple birth.death tree with one BM trait
    test <- treats(stop.rule = list("max.living" = 10), traits = make.traits())
    out <- capture_output(print.treats(test))
    expect_equal(out[[1]], " ---- treats object ---- \nSimulated phylogenetic tree (x$tree):\n\nPhylogenetic tree with 10 tips and 9 internal nodes.\n\nTip labels:\n  t1, t2, t3, t4, t5, t6, ...\nNode labels:\n  n1, n2, n3, n4, n5, n6, ...\n\nRooted; includes branch length(s).\n\nSimulated trait data (x$data):\n1 trait for 1 process (A) with one starting value (0).")
    
    ## A more complex trait
    complex_traits <- make.traits(process = c(BM.process, BM.process), n = c(2,3), process.args = list(list(Sigma = diag(2)), list(Sigma = matrix(1/3, 3, 3))), trait.names = c("bib", "bob"))
    test <- treats(stop.rule = list("max.living" = 10), traits = complex_traits)
    out <- capture_output(print.treats(test))
    expect_equal(out[[1]], " ---- treats object ---- \nSimulated phylogenetic tree (x$tree):\n\nPhylogenetic tree with 10 tips and 9 internal nodes.\n\nTip labels:\n  t1, t2, t3, t4, t5, t6, ...\nNode labels:\n  n1, n2, n3, n4, n5, n6, ...\n\nRooted; includes branch length(s).\n\nSimulated trait data (x$data):\n5 traits for 2 processes (bib:2, bob:3) with one starting value (0).\nprocess bib uses the following extra argument: Sigma.\nprocess bob uses the following extra argument: Sigma.")    
})

test_that("print.modifiers works", {

    ## The default
    test <- make.modifiers()
    out <- capture_output(print.treats(test))
    expect_equal(out[[1]], " ---- treats modifiers object ---- \nNo modifiers applied to the branch length, selection and speciation processes (default).")

    my_speciation_fun <- speciation
    test <- make.modifiers(speciation = my_speciation_fun)
    out <- capture_output(print.treats(test))
    expect_equal(out[[1]], " ---- treats modifiers object ---- \nDefault branch length process.\nDefault selection process.\nSpeciation process is set to my_speciation_fun.")

    my_condition_fun <- function() return(TRUE)
    my_modify_fun <- function(x) return(x)
    test <- make.modifiers(speciation = my_speciation_fun, modify = my_modify_fun)
    out <- capture_output(print.treats(test))
    expect_equal(out[[1]], " ---- treats modifiers object ---- \nDefault branch length process.\nDefault selection process.\nSpeciation process is set to my_speciation_fun with a modifier (my_modify_fun).")

    my_condition_fun <- function() return(TRUE)
    my_modify_fun <- function(x) return(x)
    test <- make.modifiers(speciation = my_speciation_fun, condition = my_condition_fun, modify = my_modify_fun)
    out <- capture_output(print.treats(test))
    expect_equal(out[[1]], " ---- treats modifiers object ---- \nDefault branch length process.\nDefault selection process.\nSpeciation process is set to my_speciation_fun with a condition (my_condition_fun) and a modifier (my_modify_fun).")

    my_branch_length_fun <- branch.length
    test <- make.modifiers(branch.length = my_branch_length_fun, speciation = my_speciation_fun, condition = my_condition_fun, modify = my_modify_fun)
    out <- capture_output(print.treats(test))
    expect_equal(out[[1]], " ---- treats modifiers object ---- \nBranch length process is set to my_branch_length_fun with a condition (my_condition_fun) and a modifier (my_modify_fun).\nDefault selection process.\nSpeciation process is set to my_speciation_fun with a condition (my_condition_fun) and a modifier (my_modify_fun).")

    my_branch_length_fun <- branch.length
    test <- make.modifiers(branch.length = my_branch_length_fun, speciation = my_speciation_fun)
    out <- capture_output(print.treats(test))
    expect_equal(out[[1]], " ---- treats modifiers object ---- \nBranch length process is set to my_branch_length_fun.\nDefault selection process.\nSpeciation process is set to my_speciation_fun.")
})