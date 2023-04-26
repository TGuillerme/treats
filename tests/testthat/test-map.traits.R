test_that("map.traits works", {
    tree_list <- rmtree(5, 10)
    tree <- tree_list[[1]]
    traits <- make.traits()

    ## Wrong inputs
    error <- capture_error(map.traits("yap", tree))
    expect_equal(error[[1]], "traits must be of class \"traits\". You can generate such object using:\nmake.traits()")
    error <- capture_error(map.traits(traits, "tree"))
    expect_equal(error[[1]], "tree must be of class phylo or multiPhylo.")
    ## no brlen
    tr <- tree
    tr$edge.length <- NULL
    error <- capture_error(map.traits(traits, tr))
    expect_equal(error[[1]], "The input tree has no branch lengths.")
    ## Warning background
    warn <- capture_warning(test <- map.traits(traits = make.traits(background = make.traits()), tree))
    expect_equal(warn[[1]], "Background traits cannot be mapped and is ignored.")

    ## Working basic
    test <- map.traits(traits, tree)
    expect_is(test, "treats")
    expect_equal(names(test), c("tree", "data"))
    expect_equal(dim(test$data), c(Ntip(tree)+Nnode(tree), 1))
    expect_true(all(rownames(test$data) %in% c(test$tree$tip.label, test$tree$node.label)))

    ## Working with multiple trees
    test <- map.traits(traits, tree_list)
    expect_equal(length(test), length(tree_list))
    expect_equal(unique(unlist(lapply(test, class))), "treats")
    expect_equal(unique(unlist(lapply(test, names))), c("tree", "data"))

    ## Working with exact traits and multiple traits
    cumulative.count <- function(x0 = 0, edge.length = 1) {return(x0 +1)}
    counting.trait <- make.traits(process = cumulative.count, start = c(10, 0, 100), n = 3)
    set.seed(1)
    tree_coal <- rcoal(5)
    test <- map.traits(counting.trait, tree_coal)
    expect_equal(unname(test$data[, 1]), c(10, 11, 11, 12, 12, 13, 13, 12, 12))
    expect_equal(unname(test$data[, 2]), c(0, 1, 1, 2, 2, 3, 3, 2, 2))
    expect_equal(unname(test$data[, 3]), c(100, 101, 101, 102, 102, 103, 103, 102, 102))
})