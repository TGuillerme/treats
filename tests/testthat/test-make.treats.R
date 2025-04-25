## Test
test_that("make.treats works", {

    ## Simple example (works)
    my_tree <- rtree(5)
    my_tree$node.label <- letters[1:4]
    my_data <- matrix(rnorm(9), dimnames = list(c(my_tree$tip.label, my_tree$node.label)))
    my_treats <- make.treats(tree = my_tree, data = my_data)
    expect_is(my_treats, "treats")
    expect_is(my_treats$tree, "phylo")
    expect_is(my_treats$data, "matrix")

    ## Data is a data.frame
    data_frame <- as.data.frame(my_data)
    my_treats <- make.treats(tree = my_tree, data = data_frame)
    expect_is(my_treats, "treats")
    expect_is(my_treats$tree, "phylo")
    expect_is(my_treats$data, "matrix")

    ## Data is a vector
    data_vector <- c(my_data)
    names(data_vector) <- rownames(my_data)
    my_treats <- make.treats(tree = my_tree, data = data_vector)
    expect_is(my_treats, "treats")
    expect_is(my_treats$tree, "phylo")
    expect_is(my_treats$data, "matrix")

    ## Wrong data (no names)
    wrong_data <- my_data
    rownames(wrong_data) <- NULL
    error <- capture_error(make.treats(my_tree, wrong_data))
    expect_equal(error[[1]],  "data must be a matrix or a data.frame with row names or a named vector.")

    ## Wrong tree (no tips)
    wrong_tree <- rtree(5)
    error <- capture_error(make.treats(wrong_tree, my_data))
    expect_equal(error[[1]], "The input tree must have tip and node labels.")

    ## Wrong data (no match)
    wrong_data <- my_data
    rownames(wrong_data) <- letters[1:9]
    error <- capture_error(make.treats(my_tree, wrong_data))
    expect_equal(error[[1]], "The tree and data labels don't match.\nYou can use the following to make them match:\ndispRity::clean.data(data, tree)")

    ## Wrong data (no nodes)
    wrong_data <- my_data[1:5, ]
    error <- capture_error(make.treats(my_tree, wrong_data))
    expect_equal(error[[1]],  "The tree and data labels don't match.\nYou can use the following to make them match:\ndispRity::clean.data(data, tree)")

    ## Works with multiPhylo and matrix list
    set.seed(1)
    ## The tree
    trees <- replicate(3, rcoal(10), simplify = FALSE)
    trees <- lapply(trees, makeNodeLabel)
    class(trees) <- "multiPhylo"
    ## The matrix
    data <- space.maker(elements = 10, dimensions = 2, distribution = rnorm, elements.name = trees[[1]]$tip.label)

    ## Run the multi.ace on the continuous data
    expect_warning(disp_data <- multi.ace(data = data, tree = trees, output = "dispRity", verbose = FALSE))

    ## List of trees
    trees <- disp_data$tree
    datas <- disp_data$matrix

    ## Works with multiple trees and data
    test <- make.treats(data = datas, tree = trees)
    expect_is(test, "treats")
    expect_length(test, 3)
    expect_is(test[[1]], "treats")
    expect_equal(names(test[[1]]), c("tree", "data"))
    test <- make.treats(data = disp_data)
    expect_is(test, "treats")
    expect_length(test, 3)
    expect_is(test[[1]], "treats")
    expect_equal(names(test[[1]]), c("tree", "data"))
    test <- make.treats(disp_data)
    expect_is(test, "treats")
    expect_length(test, 3)
    expect_is(test[[1]], "treats")
    expect_equal(names(test[[1]]), c("tree", "data"))

})
