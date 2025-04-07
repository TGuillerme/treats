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

test_that("map.traits events", {

    ## Events object for testing
    events <- make.events(
        condition    = age.condition(3),
        target       = "traits",
        modification = traits.update(process = OU.process))
    ## Testing get trigger
    expect_equal(get.trigger.time(events, tree = NULL, traits = NULL), 3)

    ## 1- trigger works in map.traits [TODO: TG]

    ## 2- splitting works internally [TODO: CS]

    # ## Some example data
    # tree <- rtree(5)
    # splitted_tree <- split.tree(tree)
    # expect_is(splitted_tree, "list")
    # expect_equal(names(splitted), c("parent", "orphans"))
    # expect_is(splitted$parent, "multiPhylo")
    # expect_is(splitted$orphans, "list")
    # expect_is(splitted$orphans[[1]], "multiPhylo")

    ## 2.1 - after splitting make a list of all the orphan trees and their starting values
    make.orphan.trees.list <- function(orphan_trees, sim_values)

    traits$main[[1]]$process[[1]]
    events[[1]]$modification(traits, start = one_parent_trait_value)$main[[1]]$process[[1]]



    # ## 3- apply map.traits
    # output <- map.traits(splitted$parent, traits, ...)
    # ## preparing orphans traits objects [TODO: TG]
    # orphan_traits <- prep.traits(output, traits, events)

    # expect_is(orphan_traits, "list")
    # expect_is(orphan_traits[[1]], "list")
    # expect_names(orphan_traits[[1]], c("tree", "traits"))

    # orphan_output <- do.call(map.traits, orphan_traits)

    # ## 4- merge trees together [TODO: CS]
    # expect_is(parents, "treats")
    # expect_is(orphan_output, "list")
    # expect_is(orphan_output[[1]], "treats")
    # x <- merge.parents.orphans(parents, orphan_output)
    # expect_is(x, "data.frame") # or a matrix?



})