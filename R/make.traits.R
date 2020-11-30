#' @title make.traits
#'
#' @description Making traits objects for dads
#'
#' @param process      The trait process(es) (default is \code{\link{BM.process}}).
#' @param n            Optional, the number of traits per process (default is \code{1}).
#' @param start        Optional, the starting values for each traits (default is {0}).
#' @param process.args Optional, a named list of optional arguments for the trait process.
#' @param trait.names  Optional, the name(s) of the process(s).
#' @param add          Optional, another previous \code{"dads"} traits object to which to add the trait.
#' @param test         Logical, whether to test if the traits object will work with \code{\link{dads}} (\code{TRUE} - default).
#' 
#' 
#' @details
#' 
#' More details about the \code{"dads"} \code{"traits"} objects is explained in the \code{dads} manual: \url{http://tguillerme.github.io/dads}.
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
#' make.traits(process = list(BM.process, OU.process))
#' 
#' ## A multidimensional Brownian motion trait with correlation
#' ## and different starting points
#' my_correlations <- matrix(1/3, ncol = 3, nrow = 3)
#' (my_traits <- make.traits(n = 3, start = c(0, 1, 3),
#'                           process.args = list(Sigma = my_correlations)))
#' 
#' ## Adding a Ornstein-Uhlenbeck trait to the previous trait object
#' make.traits(process = OU.process, trait.names = "OU_trait",
#'             add = my_traits)
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

make.traits <- function(process = BM.process, n, start, process.args, trait.names, add, test = TRUE) {

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
            if(length(start) > 1) {
                warning(paste0("Only the first ", length(start), " starting values were supplied for a required ", sum(n), " traits. The missing start values are set to 0."))
            } else {
                start <- rep(start, sum(n))
            }
            start <- c(start, rep(0, sum(n)-length(start)))
        }
    } else {
        ## Default start values
        start <- rep(0, sum(n))
    }

    ## Check add
    add_traits <- FALSE
    if(!missing(add)) {
        if(!(is(add, "dads") && is(add, "traits"))) {
            stop("traits can only be added to objects of class dads and traits.")
        }
        add_traits <- TRUE
        ## Find the previous names (to not duplicate names)
        previous_names <- names(add)
        ## Find the previous trait_id (to not duplicate IDs)
        previous_n <- tail(add[[length(add)]]$trait_id, n = 1)
        ## Update all the starting values
        start <- unname(c(unlist(lapply(add, `[[`, "start")), start))
    } else {
        ## Initialise the names and n
        previous_names <- NULL
        previous_n <- 0
    }

    ## Generate the traits ids and starting values
    n_temp <- n
    index <- 0
    trait_id <- start_values <- vector("list", length(n))
    while(length(n_temp) > 0) {
        index <- index + 1
        trait_id[[index]] <- seq(from = previous_n + 1, to = previous_n + n_temp[1])
        start_values[[index]] <- start[trait_id[[index]]] 
        previous_n <- max(trait_id[[index]])
        n_temp <- n_temp[-1]
    }

    ## Check the additional arguments
    add_process_args <- FALSE
    if(!missing(process.args)) {
        add_process_args <- TRUE
        check.class(process.args, "list")
        if(n_processes == 1) {
            ## Check if arguments are named
            if(is.null(names(process.args))) {
                stop("process.args must be a named list of arguments.")
            }
            ## Make the process into a list (if there is only one process)
            process.args <- list(process.args)
        } else {
            ## Make sure the process correspond to the right list
            if(length(process.args) != n_processes) {
                stop(paste0("You must provide additional arguments for every process (", n_processes, "). You can provide NULL arguments for processes that don't need extra arguments e.g.\n\n    process.args = list(list(NULL),\n                        list(extra.arg = some_extra_argument))\n\nwill only provide extra arguments to the second process."))
            }
            ## Check if each arguments are named (unless NULL)
            for(one_process in 1:n_processes) {
                if(is.null(names(process.args[[one_process]])) && !is.null(process.args[[one_process]][[1]])) {
                    stop("process.args must be a named list of arguments.")
                }
            }
        }
    } 

    ## Check the names
    if(!missing(trait.names)) {
        check.class(trait.names, c("character", "numeric", "integer"))
        check.length(trait.names, n_processes, " must be the same length as the number of process(es).")
        ## Making the trait names (+ the previous ones)
        trait_names <- c(previous_names, trait.names)
    } else {
        ## Default names are letters (or numeric if > 26 processes)
        if((n_processes + length(previous_names)) <= 26) {
            ## Naming the process alphabetically (letters used in previous names)
            trait_names <- c(previous_names, LETTERS[!(LETTERS %in% previous_names)][1:n_processes])
        } else {
            ## Just giving number
            trait_names <- c(previous_names, as.character(1:n_processes))
            ## Removing duplicates
            if(any(duplicates <- duplicated(trait_names))) {
                trait_names[duplicates] <- paste0(trait_names[duplicates], ".bis")
            }
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
        if(add_process_args) {
            ## Adding optional arguments if not NULL
            if(!is.null(process.args[[one_process]][[1]])) {
                traits[[one_process]] <- c(traits[[one_process]], process.args[[one_process]])
            }
        }
    }

    ## Add previous traits
    if(add_traits) {
        traits <- c(add, traits)
    }
    
    ## Add names
    names(traits) <- trait_names

    ## check
    if(test) {
        success <- check.traits(traits)
    }

    class(traits) <- c("dads", "traits")
    return(traits)
}