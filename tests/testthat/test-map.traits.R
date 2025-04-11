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

    set.seed(1)
    tree <- rtree(50)
    traits <- make.traits()
    ## Events object for testing
    events <- make.events(
        condition    = age.condition(3),
        target       = "traits",
        modification = traits.update(process = OU.process))
    ## Testing get trigger
    expect_equal(get.trigger.time(events, tree = NULL, traits = NULL), 3)

    ## 1- trigger works in map.traits [TODO: TG]


    ## 2.1 - after splitting make a list of all the orphan trees and their starting values
    make.orphan.trees.list <- function(orphan_trees, sim_values)

    traits$main[[1]]$process[[1]]
    events[[1]]$modification(traits, start = one_parent_trait_value)$main[[1]]$process[[1]]

    set.seed(123)
    tree <- rtree(10)
    tree <- makeNodeLabel(tree)
    splitted <- dispRity::slice.tree(tree, age = 2, model = "acctran", keep.all.ancestors = TRUE)
    splitted_branches <- splitted$tip.label[grepl("^N", splitted$tip.label)]

    orphan_trees <- (castor::get_subtrees_at_nodes(tree, splitted_branches))$subtrees

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

test_that("add.root.edge correctly adds a root edge", {
    set.seed(123) 
    tree <- rtree(5)  # small random tree
    new_root_edge <- runif(1, min = 0, max = 2)
    
    updated_tree <- add.root.edge(tree, new_root_edge)
    
    # Check if the number of nodes increased
    expect_equal(updated_tree$Nnode, tree$Nnode + 1)
    
    # Check if the new edge was added
    expect_equal(nrow(updated_tree$edge), nrow(tree$edge) + 1)
    
    # Check if the new edge length is correct
    expect_equal(updated_tree$edge.length[1], new_root_edge)
})

test_that("add.root.edge works", {

})

test_that("tree.slice.caleb works", {
    ## test "phylo" output
    set.seed(123)
    starting_tree <- rcoal(5) # tree is ultrametric to make sure that the slices add up to the correct length
    starting_tree <- makeNodeLabel(starting_tree)
    starting_tree <- set.root.time(starting_tree)
    slice <- 0.35
    sliced_object <- tree.slice.caleb(starting_tree, slice = slice)
    expect_is(sliced_object, "list")

    ## Length of the sliced tree is 1 (slice is sliced from the root)
    expect_equal(max(node.depth.edgelength(sliced_object$parent_tree)), slice)
    ## Length of the orphan trees is root.time - 1 (the rest of the time)
    expect_equal(max(node.depth.edgelength(sliced_object$orphan_tree[[1]])), (starting_tree$root.time - slice), tolerance = 1e-7) # Not sure where the tolerance error comes from (not tree.ages could be castor. but good enough)
    expect_equal(max(node.depth.edgelength(sliced_object$orphan_tree[[2]])), (starting_tree$root.time - slice), tolerance = 1e-7)
    expect_equal(max(node.depth.edgelength(sliced_object$orphan_tree[[3]])), (starting_tree$root.time - slice))

    expect_is(sliced_object$parent, "phylo")
    expect_is(sliced_object$orphan_tree, "list")
    expect_is(sliced_object$orphan_tree[[1]], "phylo")


    ## testing complex tree
    set.seed(1)
    tree <- rtree(50)
    tree <- makeNodeLabel(tree, prefix = "n")
    tree <- set.root.time(tree)
    slice <- 2
    tree_sliced <- tree.slice.caleb(tree, slice)
    expect_is(tree_sliced, "list")
    expect_equal(names(tree_sliced), c("parent_tree", "orphan_tree"))
    expect_is(tree_sliced$parent_tree, "phylo")
    ## 9 nodes leading to subtrees
    expect_equal(length(grep("map.traits_split_node", tree_sliced$parent_tree$tip.label)), 9)
    ## 6 tips leading to singletons
    expect_equal(length(grep("map.traits_split_tip", tree_sliced$parent_tree$tip.label)), 7)
    ## Should be tree orphan trees, including 6 singletons
    expect_equal(length(tree_sliced$orphan_tree), 16)
    ## should have 22 tips
    expect_equal(Ntip(tree_sliced$parent_tree), 22)
    ## ... 16 of which are cuts (min age)
    expect_equal(sum(tree.age(tree_sliced$parent_tree)$ages == min(tree.age(tree_sliced$parent_tree)$ages)), 16)
})