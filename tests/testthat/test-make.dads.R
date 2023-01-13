## Test
test_that("make.dads works", {

    ## Simple example (works)
    my_tree <- rtree(5)
    my_tree$node.label <- letters[1:4]
    my_data <- matrix(rnorm(9), dimnames = list(c(my_tree$tip.label, my_tree$node.label)))
    my_dads <- make.dads(tree = my_tree, data = my_data)
    expect_is(my_dads, "dads")
    expect_is(my_dads$tree, "phylo")
    expect_is(my_dads$data, "matrix")

    ## Data is a data.frame
    data_frame <- as.data.frame(my_data)
    my_dads <- make.dads(tree = my_tree, data = data_frame)
    expect_is(my_dads, "dads")
    expect_is(my_dads$tree, "phylo")
    expect_is(my_dads$data, "matrix")

    ## Data is a vector
    data_vector <- c(my_data)
    names(data_vector) <- rownames(my_data)
    my_dads <- make.dads(tree = my_tree, data = data_vector)
    expect_is(my_dads, "dads")
    expect_is(my_dads$tree, "phylo")
    expect_is(my_dads$data, "matrix")

    ## Wrong data (no names)
    wrong_data <- my_data
    rownames(wrong_data) <- NULL
    error <- capture_error(make.dads(my_tree, wrong_data))
    expect_equal(error[[1]],  "data must be a matrix with column names or a named vector.")

    ## Wrong tree (no tips)
    wrong_tree <- rtree(5)
    error <- capture_error(make.dads(wrong_tree, my_data))
    expect_equal(error[[1]], "The input tree must have tip and node labels.")

    ## Wrong data (no match)
    wrong_data <- my_data
    rownames(wrong_data) <- letters[1:9]
    error <- capture_error(make.dads(my_tree, wrong_data))
    expect_equal(error[[1]], "tree and data labels don't match.\nYou can use dispRity::clean.data(data, tree) to make them match.")

    ## Wrong data (no nodes)
    wrong_data <- my_data[1:5, ]
    error <- capture_error(make.dads(my_tree, wrong_data))
    expect_equal(error[[1]],  "The data does not seem to contain node values or they are not matching with the tree tips or node names.")

    ## Options that fit (todo later)
})
