#' @title make.traits
#'
#' @description Making traits objects for dads
#'
#' @param process      The trait process(es) (default is \code{\link{BM.process}}).
#' @param n            Optional, the number of traits per process (default is \code{1}).
#' @param start        Optional, the starting values for each traits (default is {0}).
#' @param process.args Optional, a named list of optional arguments for the trait process.
#' @param names        Optional, the name(s) of the trait(s) (if missing the name of the process is used to generate the names).
#' @param add          Optional, another previous \code{"dads"} traits object to which to add the trait.
#' @param test         Logical, whether to test if the traits object will work with \code{\link{dads}} (\code{TRUE} - default).
#' 
#' 
#' @examples
#' ## A simple Brownian motion trait (default)
#' make.traits()
#' 
#' ## Two independent Brownian motion traits
#' make.traits(n = 2)
#' 
#' ## Two different traits with different process
#' ## (Brownian motion and Ornstein-Uhlenbeck)
#' make.traits(process = list(BM.process, step.OU))
#' 
#' ## A multidimensional Brownian motion trait with correlation
#' ## and different starting points
#' (my_traits <- make.traits(n = 4, start = c(0, 1, 2, 3),
#'                           process.args = list(Sigma = diag(1))))
#' 
#' ## Adding a Ornstein-Uhlenbeck trait to the previous trait object
#' make.traits(process = step.OU, names = "OU_trait",
#'             add = my_traits)
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

make.traits <- function(process = BM.process, n, start, process.args, names, add, test = TRUE) {

    match_call <- match.call()

    ## Check the process(es)
    process_class <- check.class(process, c("function", "list"))
    n_processes <- length(process)
    if(process_class == "list") {
        silent <- lapply(process, check.class, "function")
    } else {
        process <- list(process)
    }

    ## Check the number of traits
    if(!missing(n)) {
        check.class(n, c("integer", "numeric"))        
        if(n_processes > 1 && length(n) == 1) { 
            ## Multiply the number of traits per processes
            n <- rep(n, times = n_processes)
        } else {
            if(length(n) != n_processes) {
                stop(paste0("n must be an object of the same length as the number of process(es) (", n_processes, ")."), call. = FALSE)
            }
        }
    } else {
        ## Default number of traits (one trait per process)
        n <- rep(1, n_processes)
    }

    ## Check the starting values
    if(!missing(start)) {
        check.class(start, c("integer", "numeric"))
        if(length(start) != sum(n)) {
            warning(paste0("Only the first ", length(start), " starting values were supplied for a required ", sum(n), " traits. The missing start values are set to 0."))
            start <- c(start, rep(0, length(unlist(trait_id))-length(start)))
        }
    } else {
        ## Default start values
        start <- rep(0, sum(n))
    }

    ## Generate the traits ids and starting values
    n_temp <- n
    previous_n <- index <- 0
    trait_id <- start_values <- vector("list", length(n))
    while(length(n_temp) > 0) {
        index <- index + 1
        trait_id[[index]] <- seq(from = previous_n + 1, to = previous_n + n_temp[1])
        start_values[[index]] <- start[trait_id[[index]]] 
        previous_n <- max(trait_id[[index]])
        n_temp <- n_temp[-1]
    }

    ## Check the additional arguments
    add_process_args <- rep(FALSE, n_processes)
    if(!missing(process.args)) {
        add_process_args <- TRUE
        check.class(process.args, "list")
        if(is.null(names(process.args))) {
            stop("process.args must be a named list of arguments.")
        }
    } 

    ## Check the names
    if(!missing(names)) {
        check.class(names, c("character", "numeric", "integer"))
        check.length(names, n, " must be the same length as the number of characters.")
    } else {
        names <- match_call$process
    }

    ## Check add
    if(!missing(add)) {
        if(!(is(add, "dads") && is(add, "traits"))) {
            stop("traits can only be added to objects of class dads and traits.")
        }
    }

    ## Test
    check.class(test, "logical")
    
    ## Add the process
    traits <- lapply(process, function(x) return(list(process = x)))
    ## Add the ids and the starts
    for(one_process in 1:n_processes)  {
        traits[[one_process]]$start    <- start_values[[one_process]]
        traits[[one_process]]$trait_id <- trait_id[[one_process]]
        if(add_process_args[[one_process]]) {
            stop("TODO in make.traits: add process.args")
            traits[[one_process]] <- c(traits[[one_process]], process.args[[one_process]])
        }
    }

    ## check
    if(test) {
        success <- check.traits(traits)
    }

    class(traits) <- c("dads", "traits")
    return(traits)
}