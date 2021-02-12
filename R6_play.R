## Testing some R6 stuff
library(R6)


start <- Sys.time()
f1 <- function() {
    big <- matrix(rnorm(1000000), 1000, 1000)
    counter <- 0
    while(counter < 10) {
        random <- sample(1:1000000, 1)
        big[random] <- NA
        counter <- counter + 1
    }
    sum(is.na(big))
}
end <- Sys.time()
end-start


r6.test <- R6Class("test",
  public = list(
    matrix = NULL,
    random = NULL,
    initialize = function() {
      self$matrix <- matrix(rnorm(1000000), 1000, 1000)
      self$random <- 1
    },
    get.random = function() {
      self$random <- sample(1:1000000, 1)
    },
    add.NA = function() {
      self$matrix[self$random] <- NA
    }
  )
)


f2 <- function(){
    test <- r6.test$new()
    random <- matrix <- NULL
    counter <- 0    
    while(counter < 10) {
        test$get.random()
        test$add.NA()
        counter <- counter + 1
    }
}
sum(is.na(test$matrix))


library(microbenchmark)
microbenchmark(f1, f2)