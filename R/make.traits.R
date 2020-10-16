#' @title make.traits
#'
#' @description Making traits objects for dads
#'
#' @param process      The trait process(es) (default is \code{step.BM}).
#' @param n            Optional, the number of traits per process (default is \code{1}).
#' @param start        Optional, the starting values for each traits (default is {0}).
#' @param process.args Optional, a named list of optional arguments for the trait process.
#' @param names        Optional, the name(s) of the trait(s) (if missing the name of the process is used to generate the names).
#' @param add          Optional, another previous \code{"dads"} traits object to which to add the trait.
#' @param test         Logical, whether to test if the traits object will work with \code{\link{dads}} (\code{TRUE} - default).
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

# traits <- list(
#             "A" = list(process = element.rank,
#                        start   = c(0,10,20))
#             "B" = list(process = branch.length,
#                        start   = 0)
#             )


make.traits <- function(process = step.BM, n, start, process.args, names, add, test = TRUE) {


    match_call <- match.call()

    ## Check the process(es)
    process_class <- check.class(process, c("function", "list"))
    n_processes <- length(process)
    if(process_class == "list") {
        silent <- lapply(process, check.class, "function")
    }

    ## Check the number of traits
    if(!missing(n)) {
        check.class(n, c("integer", "numeric"))
        if(n_processes > 1 && length(n) == 1) { 
            n <- rep(n, times = n_processes)
        }
    } else {
        ## Default number of traits
        n <- n_processes
    }
    ## Generate the traits ids
    n_temp <- n
    previous_n <- index <- 0
    trait_id <- vector("list", length(n))
    while(length(n_temp) > 0) {
        index <- index + 1
        trait_id[[index]] <- seq(from = previous_n + 1, to = previous_n + n_temp[1])
        previous_n <- max(trait_id[[index]])
        n_temp <- n_temp[-1]
    }

    ## Check the starting values
    if(!missing(start)) {
        check.class(start, c("integer", "numeric"))
        if(length(start) != sum(n)) {
            start <- c(start, rep(0, sum(n)-length(start)))
        }
    } else {
        ## Default start values
        start <- rep(0, n)
    }

    ## Check the additional arguments
    add_process_args <- FALSE
    if(!missing(process.args)) {
        add_process_args <- TRUE
        check.class(process.args, "list")
        if(is.null(names(process.args))) {
            stop("process.args must be a named list of arguments.")
        }
    } 

    ## Check the names
    warning("DEBUG: handle names in make.traits")
    if(!missing(names)) {
        check.class(names, c("character", "numeric", "integer"))
        check.length(names, n, " must be the same length as the number of characters")
    } else {
        names <- match_call$process
    }


    ## Test
    check.class(test, "logical")

    ## Create the traits objects
    add.to.list <- function(list, add, add_name) {
        ## Get the output names
        add_names <- c(names(list), add_name)
        ## Add the new element
        list <- c(list, add)
        ## Update the names
        names(list) <- add_names
        return(list)
    }
    
    ## Add the process
    traits <- lapply(process, function(x) return(list(process = x)))
    names(traits) <- names
    ## Add the start values
    traits <- mapply(add.to.list, traits, start, MoreArgs = list(add_name = "start"), SIMPLIFY = FALSE)
    ## Add the id
    traits <- mapply(add.to.list, traits, trait_id, MoreArgs = list(add_name = "trait_id"), SIMPLIFY = FALSE)
    ## Add optional arguments
    if(add_process_args) {
        traits <- mapply(add.to.list, traits, process_args, MoreArgs = list(add_name = "trait_id"), SIMPLIFY = FALSE)
    }

    ## check
    warning("TODO in make.traits: check")

    class(traits) <- c("dads", "traits")
    return(traits)
}