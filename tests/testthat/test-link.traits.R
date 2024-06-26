test_that("set.conditional.traits works", {

    ## First let's design a discrete islandness trait
    transition_matrix <- matrix(c(3, 0.2, 0.05, 3), 2, 2)

    ## The firs trait (upon which to check the condition)
    discrete_trait <- make.traits(discrete.process, process.args = list(transitions = transition_matrix), trait.name = "discrete.trait")

    ## The second and third traits
    OU_trait <- make.traits(OU.process, n = 2)
    BM_trait <- make.traits(BM.process, n = 2)

    base.trait <- discrete_trait
    next.trait <- list(OU_trait, BM_trait)

    link.type = "conditional"
    link.args = list("choose.OU" = function(x1) {x1 == 0}, "choose.BM" = function(x1) {x1 == 1}) 

    ## works
    test <- set.conditional.traits(base.trait, next.trait, link.args)
    expect_equal(names(test), c("process", "start", "trait_id", "process.args", "link"))
    expect_equal(length(test$process), 3)
    expect_equal(length(test$start), 3)
    expect_equal(length(test$trait_id), 3)
    expect_equal(length(test$process.args), 3)
    expect_equal(names(test$link), c("type", "conditional.test", "trait.names"))
    expect_equal(length(test$link$conditional.test), 2)
})

test_that("link.traits works", {

    ## First let's design a discrete islandness trait
    transition_matrix <- matrix(c(3, 0.2, 0.05, 3), 2, 2)

    ## The firs trait (upon which to check the condition)
    discrete_trait <- make.traits(discrete.process, process.args = list(transitions = transition_matrix), trait.name = "conditional.trait:A")

    ## The second and third traits
    OU_trait <- make.traits(OU.process, n = 2)
    BM_trait <- make.traits(BM.process, n = 2)

    base.trait <- discrete_trait
    next.trait <- list(OU_trait, BM_trait)

    link.type = "conditional"
    link.args = list("choose.OU" = function(x1) {x1 == 0}, "choose.BM" = function(x1) {x1 == 1}) 

    error <- capture_error(link.traits(base.trait = "base.trait", next.trait = next.trait, link.type = "conditional", link.args = link.args))
    expect_equal(error[[1]], "base.trait must be of class list or treats.")
    error <- capture_error(link.traits(base.trait = base.trait, next.trait = "next.trait", link.type = "conditional", link.args = link.args))
    expect_equal(error[[1]], "next.trait must be of class list or treats.")
    error <- capture_error(link.traits(base.trait = base.trait, next.trait = next.trait, link.type = "condidfdtional", link.args = link.args))
    expect_equal(error[[1]], "link.type must be one of the following: conditional.")
    error <- capture_error(link.traits(base.trait = base.trait, next.trait = next.trait, link.type = "conditional", link.args = link.args[[1]]))
    expect_equal(error[[1]], "next.trait and link.args must be a lists of the same lengths containing one or more traits and conditional arguments for conditional links.")
    
    ## Working
    test <- link.traits(base.trait = base.trait, next.trait = next.trait, link.type = "conditional", link.args = link.args)
    expect_equal(class(test), c("treats", "traits"))
    ## Printing
    out <- capture_output(print.treats(test))
    expect_equal(out[[1]], " ---- treats traits object ---- \n3 traits for 1 process (conditional.trait:3) with one starting value (0).\nprocess conditional.trait uses the following extra arguments: transitions, NULL, NULL.")
})

test_that("implementation works", {

    ## First let's design a discrete islandness trait
    transition_matrix <- matrix(c(3, 0.2, 0.05, 3), 2, 2)

    ## The firs trait (upon which to check the condition)
    discrete_trait <- make.traits(discrete.process, process.args = list(transitions = transition_matrix), trait.name = "conditional.trait:A")

    ## The second and third traits
    trait.1 <- function(x0 = 0, edge.length = 1) {return(1)}
    trait.0 <- function(x0 = 0, edge.length = 1) {return(0)}

    # OU_trait <- make.traits(OU.process, n = 1)
    # BM_trait <- make.traits(BM.process, n = 1)
    trait_0 <- make.traits(process = trait.0)
    trait_1 <- make.traits(process = trait.1)
    link_args <- list("choose.0" = function(x1) {x1 == 0}, "choose.1" = function(x1) {x1 == 1}) 

    traits <- link.traits(base.trait = discrete_trait, next.trait = list(trait_0, trait_1), link.type = "conditional", link.args = link_args)

    stop.rule  = list(max.taxa = 100)
    stop.rule$max.living = Inf
    stop.rule$max.time = Inf
    bd.params  = make.bd.params()
    modifiers = NULL
    events = NULL
    null.error = FALSE
    check.results = TRUE
    save.steps = NULL

    ## Test basic trait (1D)
    test <- birth.death.tree.traits(stop.rule, bd.params, traits = traits, modifiers = NULL, events = NULL, null.error = FALSE, check.results = TRUE, save.steps = NULL)
    expect_equal(test$data[, 1], test$data[, 2])

    ## The second and third traits
    trait.1 <- function(x0 = 0, edge.length = 1) {return(c(1,1))}
    trait.0 <- function(x0 = 0, edge.length = 1) {return(c(0,0))}

    # OU_trait <- make.traits(OU.process, n = 1)
    # BM_trait <- make.traits(BM.process, n = 1)
    trait_0 <- make.traits(process = trait.0, n = 2)
    trait_1 <- make.traits(process = trait.1, n = 2)
    link_args <- list("choose.0" = function(x1) {x1 == 0}, "choose.1" = function(x1) {x1 == 1}) 
    traits <- link.traits(base.trait = discrete_trait, next.trait = list(trait_0, trait_1), link.type = "conditional", link.args = link_args)

    ## Test basic trait (2D)
    test <- birth.death.tree.traits(stop.rule, bd.params, traits = traits, modifiers = NULL, events = NULL, null.error = FALSE, check.results = TRUE, save.steps = NULL)
    expect_equal(test$data[, 1], test$data[, 2])
    expect_equal(test$data[, 1], test$data[, 3])

    traits <- link.traits(base.trait = discrete_trait, next.trait = list(make.traits(n = 2), trait_1), link.type = "conditional", link.args = link_args)

    set.seed(1)
    test <- birth.death.tree.traits(stop.rule, bd.params, traits = traits, modifiers = NULL, events = NULL, null.error = FALSE, check.results = TRUE, save.steps = NULL)

    expect_equal(test$data[which(test$data[, 1] == 1), 1], test$data[which(test$data[, 1] == 1), 2])
    expect_equal(test$data[which(test$data[, 1] == 1), 1], test$data[which(test$data[, 1] == 1), 3])
    expect_lt(test$data[2, 1], test$data[2, 3])

    ## Works with treats
    test <- treats(stop.rule = stop.rule, traits = traits)
    expect_equal(length(test), 4)
    expect_equal(ncol(test$data), 3)
})

test_that("complex implementation works", {

    ## Mixing a linked trait with a normal trait
    transition_matrix <- matrix(c(3, 0.2, 0.05, 3), 2, 2)

    ## The firs trait (upon which to check the condition)
    discrete_trait <- make.traits(discrete.process, process.args = list(transitions = transition_matrix), trait.name = "discrete")

    ## The second and third traits
    trait.1 <- function(x0 = 0, edge.length = 1) {return(1)}
    trait.0 <- function(x0 = 0, edge.length = 1) {return(0)}

    # OU_trait <- make.traits(OU.process, n = 1)
    # BM_trait <- make.traits(BM.process, n = 1)
    trait_0 <- make.traits(process = trait.0, trait.names = "zero")
    trait_1 <- make.traits(process = trait.1, trait.names = "one")
    link_args <- list("choose.0" = function(x1) {x1 == 0}, "choose.1" = function(x1) {x1 == 1}) 

    linked <- link.traits(base.trait = discrete_trait, next.trait = list(trait_0, trait_1), link.type = "conditional", link.args = link_args)

    ## Adding to a normal trait
    traits <- make.traits(process = OU.process, add = linked)

    stop.rule  = list(max.taxa = 100)
    stop.rule$max.living = Inf
    stop.rule$max.time = Inf
    bd.params  = make.bd.params()
    modifiers = NULL
    events = NULL
    null.error = FALSE
    check.results = TRUE
    save.steps = NULL

    ## Test basic trait (1D)
    test <- birth.death.tree.traits(stop.rule, bd.params, traits = traits, modifiers = NULL, events = NULL, null.error = FALSE, check.results = TRUE, save.steps = NULL)
    expect_equal(test$data[, 1], test$data[, 2])

    ## Works with treats
    test <- treats(stop.rule = stop.rule, traits = traits)
    expect_equal(length(test), 4)
    expect_equal(ncol(test$data), 3)


    ## Link trait with save.steps
    set.seed(1)
    test <- treats(stop.rule = stop.rule, traits = linked, save.steps = 0.5)
    expect_equal(length(test), 4)
    expect_equal(ncol(test$data), 2)
    expect_gt(Nnode(test$tree), Ntip(test$tree)+1)

    ## Linked trait with a background
    traits <- make.traits(background = make.traits(n = 3), add = linked)
    test <- treats(stop.rule = stop.rule, traits = linked)
    expect_equal(length(test), 4)
    expect_equal(ncol(test$data), 2)

    ## Link trait with update
    # test2 <- make.traits(process = list(BM.process, OU.process), update = linked)

})
