## Testing class.check
## examples
class_1<-NA ; class(class_1) <- 'class_1'
class_2<-NA ; class(class_2) <- 'class_2'
class_3<-NA ; class(class_3) <- 'class_3'
class_list<-c("class_1", "class_2")

## Test
test_that("check.class works", {
    ## class - single
    expect_null(
            check.class(class_1, "class_1", 'test')
            )
    expect_error(
            check.class(class_1, "class_1", 'test', errorif=TRUE)
            )
    expect_null(
            check.class(class_2, "class_2", 'test')
            )
    expect_error(
            check.class(class_2, "class_2", 'test', errorif=TRUE)
            )
    ## class - multiple
    expect_that(
            check.class(class_1, class_list, 'test'), equals("class_1")
            )
    expect_that(
            check.class(class_2, class_list, 'test'), equals("class_2")
            )
    expect_error(
            check.class(class_3, class_list, 'test')
            )

    expect_that(
            check.class(class_2, class_list, 'test', errorif = TRUE), equals("class_2"))
})

## Check class function
test_that("check.class works with functions", {
    object_fun <- function(x) return(x+1)
    object_std <- mean

    ## Normal behavior
    expect_is(
        object_fun
        ,"function")
    expect_null(
        check.class(object_fun, "function")
    )
    expect_null(
        check.class(object_std, "function")
    )
    expect_error(
        check.class(object_std, "numeric")
    )
    expect_error(
        check.class(object_fun, "standardGeneric")
    )
})

## Testing check.length
## examples
length_1 <- 1
length_2 <- c(1, 1)
length_3 <- NA
length_4 <- "1"

## Test
test_that("check.length works", {
    expect_null(
            check.length(length_1, '1', 'test')
            )
    expect_null(
            check.length(length_3, '1', 'test')
            )
    expect_null(
            check.length(length_4, '1', 'test')
            )
    expect_error(
            check.length(length_2, '1', 'test')
            )
    expect_error(
            check.length(length_1, '1', 'test', errorif=TRUE)
            )
})

## Test methods
test_that("check.method works", {
    methods <- letters[1:2]
    expect_null(check.method("a", methods))
    expect_null(check.method("b", methods))
    expect_null(check.method(c("a", "b"), methods))
    expect_null(check.method(c("b", "a"), methods))
    expect_error(check.method("c", methods))
    expect_null(check.method(c("a", "c"), methods))
})

## Stop call
test_that("stop.call works", {

    return.match.call <- function(character, numeric, fun, formula, matrix) {
        match_call <- match.call()
        return(match_call)
    }

    my_matrix <- matrix(NA)
    call <- return.match.call("a", 1, mean, ~ a + b, my_matrix)

    test <- capture_error(stop.call(call$character, " works."))
    expect_equal(test[[1]], "a works.")
    test <- capture_error(stop.call(call$numeric, " works."))
    expect_equal(test[[1]], "1 works.")
    test <- capture_error(stop.call(call$fun, " works."))
    expect_equal(test[[1]], "mean works.")
    test <- capture_error(stop.call(call$formula, " works."))
    expect_equal(test[[1]], "~a + b works.")
    test <- capture_error(stop.call(call$matrix, " works."))
    expect_equal(test[[1]], "my_matrix works.")

    test <- capture_error(stop.call(call$character, " works.", "look: "))
    expect_equal(test[[1]], "look: a works.")
})

## Check class
test_that("check.class works", {
    list <- list("a" = "a", "1" = 1, "tree" = rtree(5))
    test <- check.list(list, function(x) !is.null(x))
    expect_equal(test, c("a" = TRUE, "1" = TRUE, "tree" = TRUE))

    test <- check.list(list, is.null, condition = any)
    expect_equal(test, c("a" = FALSE, "1" = FALSE, "tree" = FALSE))

    test <- check.list(list, function(x) (x == "a"), condition = any)
    expect_equal(test, c("a" = TRUE, "1" = FALSE, "tree" = FALSE))    
})