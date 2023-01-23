## Testing utilities

test_that("parent.traits works", {

})

test_that("snapshot logic works", {

    ## Creating an example lineage and edge_lengths (three tips, branching right, constant brlen)
    lineage_pre <- list(parents = c(0, 1, 1, 3, 3),
                        livings  = c(2, 4, 5),
                        drawn   = c(3),
                        current = c(5),
                        n       = c(3),
                        split   = c(TRUE, FALSE, TRUE, FALSE, FALSE))
    edge_lengths <- c(0, 2, 1, 1, 1)
    time <- 2

    ## Snapshot normal
    test <- snapshot(current.time = time, first.time = 0, lineage = lineage_pre, edge.lengths = edge_lengths) 
    expect_equal(test$lineage$parents, c(0, 1, 1, 3, 3, 2, 4, 5))
    expect_equal(test$lineage$livings, c(6, 7, 8))
    expect_equal(test$lineage$drawn, 3)
    expect_equal(test$lineage$current, 8)
    expect_equal(test$lineage$n, 3)
    expect_equal(test$lineage$split, c(T, T, T, T, T, F, F, F))
    expect_equal(test$edge_lengths, c(0, 2, 1, 1, 1, 0, 0, 0))
})
