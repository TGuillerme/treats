# test_that("ace.treats implementation works", {

#     set.seed(1)
#     data <- treats(stop.rule = list(max.living = 40),
#                    trait = make.traits(),
#                    bd.params = list(speciation = 1, exctinction = 0.2),
#                    null.error = 10)
#     ## Remove the node values
#     data$data <- data$data[-c(grep("n", rownames(data$data))), , drop = FALSE]

#     ## Run some basic estimations
#     test <- ace.treats(treats = data, traits = make.traits(), replicates = 5)

#     plot(test)

# })
