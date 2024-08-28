#' @title make.traits
#'
#' @description Making traits objects for treats
#'
#' @param process      The trait process(es) (default is \code{\link{BM.process}}).
#' @param n            Optional, the number of traits per process (default is \code{1}).
#' @param start        Optional, the starting values for each traits (default is \code{0}).
#' @param process.args Optional, a named list of optional arguments for the trait process.
#' @param trait.names  Optional, the name(s) of the process(s).
#' @param add          Optional, another previous \code{"treats"} traits object to which to add the trait.
#' @param update       Optional, another previous \code{"treats"} traits object to update (see details).
#' @param background   Optional, another \code{"treats"} \code{"traits"} object to simulate background trait evolution (see details).
#' @param test         Logical, whether to test if the traits object will work with \code{\link{treats}} (\code{TRUE} - default).
#' 
#' 
#' @details
#' When using \code{update}, the provided arguments (to \code{make.traits}) will be the ones updated in the \code{"traits"} object.
#' If the \code{"traits"} object contains multiple processes, you can specify which ones should be affected with the \code{trait.names} argument.
#' Note that you cannot update the \code{traits.names} or the number of traits per processes (\code{n}) not use the \code{add} argument when updating a \code{"traits"} object.
#'
#' If a \code{background} \code{"traits"} object is given, this object is then applied to all living edges at the same in the background while the main \code{"traits"} is computed. 
#' 
#' More details about the \code{"treats"} \code{"traits"} objects is explained in the \code{treats} manual: \url{http://tguillerme.github.io/treats}.
#' 
#' @return
#' This function outputs a \code{treats} object that is a named list of elements handled internally by the \code{\link{treats}} function.
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
#' @seealso \code{\link{treats}} \code{\link{trait.process}}
#' 
#' @author Thomas Guillerme
#' @export

make.traits <- function(process = BM.process, n = NULL, start = NULL, process.args = NULL, trait.names = NULL, add = NULL, update = NULL, test = TRUE, background) {

    match_call <- match.call()

    ## Check whether the process needs to be updated
    conditional_update <- FALSE
    if(is.null(update)) {
        do_update <- FALSE
    } else {
        if(is(update, "treats") && is(update, "traits")) {
            do_update <- TRUE
            ## Check if the trait is conditional
            conditional_update <- "conditional.trait" %in% names(update$main) 
        } else {
            stop("You can only update a \"treats\" \"traits\" object. Check the documentation from the following function for helping designing such objects:\n    ?make.traits", call. = FALSE)
        }
    }

    ## Set the process to default if update and process is null
    if(do_update && is.null(process)) {
        ## Set the process to the previous one(s)
        if(!conditional_update) {
            process <- lapply(lapply(update$main, `[[`, "process"), `[[`, 1)
        } else {
            process <- update$main$conditional.trait$process
        }
        n_processes <- length(process)
    } else {
        ## Check the process(es)
        process_class <- check.class(process, c("function", "list"))
        n_processes <- length(process)
        if(process_class == "list") {
            ## Check the processes class (ignore nulls)
            silent <- lapply(process[!unlist(lapply(process, is.null))], check.class, "function")
        } else {
            process <- list(process)
        }

        ## TODO:
        ## Add the internal arguments to the process
    }

    ## Check the number of traits
    if(!is.null(n) && !do_update) {
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
        if(!do_update) {
            ## Default number of traits (one trait per process)
            n <- rep(1, n_processes)
        } else {
            n <- length(unlist(lapply(update$main, `[[`, "trait_id")))
        }
    }

    ## Check the starting values
    if(!is.null(start)) {
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
    if(!is.null(add)) {
        if(do_update) {
            stop("Impossible to add and update a traits object at the same time.")
        }
        if(!(is(add, "treats") && is(add, "traits"))) {
            stop("You can only add to a \"treats\" \"traits\" object. Check the documentation from the following function for helping designing such objects:\n    ?make.traits", call. = FALSE)
        }
        add_traits <- TRUE
        ## Find the previous names (to not duplicate names)
        previous_names <- names(add$main)
        ## Find the previous trait_id (to not duplicate IDs)
        previous_n <- tail(add$main[[length(add$main)]]$trait_id, n = 1)
        ## Update all the starting values
        start <- unname(c(unlist(lapply(add$main, `[[`, "start")), start))
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
        trait_id[[index]] <- as.integer(seq(from = previous_n + 1, to = previous_n + n_temp[1]))
        start_values[[index]] <- start[trait_id[[index]]] 
        previous_n <- max(trait_id[[index]])
        n_temp <- n_temp[-1]
    }

    ## Check the additional arguments
    add_process_args <- FALSE
    if(!is.null(process.args)) {
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
            if(!do_update) {
                if(length(process.args) != n_processes) {
                    stop(paste0("You must provide additional arguments for every process (", n_processes, "). You can provide NULL arguments for processes that don't need extra arguments e.g.\n\n    process.args = list(list(NULL),\n                        list(extra.arg = some_extra_argument))\n\nwill only provide extra arguments to the second process."))
                }
                ## Check if each arguments are named (unless NULL)
                for(one_process in 1:n_processes) {
                    if(is.null(names(process.args[[one_process]])) && !is.null(process.args[[one_process]][[1]])) {
                        stop("process.args must be a named list of arguments.")
                    }
                }
                # process.args <- list(process.args = process.args)
            }
        }
    } 

    ## Check the names
    if(!is.null(trait.names)) {
        check.class(trait.names, c("character", "numeric", "integer"))
        # if(!is.null(process)) {
        #     check.length(trait.names, n_processes, " must be the same length as the number of process(es).")
        # }
        ## Making the trait names (+ the previous ones)
        trait_names <- c(previous_names, trait.names)
    } else {
        ## Default names are letters (or numeric if > 26 processes)
        if((n_processes + length(previous_names)) <= length(LETTERS)) {
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

    ## Check the update argument
    do_update <- FALSE
    if(!is.null(update)) {
        do_update <- TRUE
        ## Check if any names is given and unchanged
        if(is.null(trait.names)) {
            update_process <- names(update$main)
        } else {
            if(any(wrong_names <- !(trait.names %in% names(update$main)))) {
                stop(paste0("No process(es) called ", paste0(trait.names[wrong_names], collapse = ", "), " to update."))
            } else {
                update_process <- trait.names
            }
        }
    }

    ## Test
    check.class(test, "logical")

    traits <- list(main = NULL, background = NULL)
    
    if(!do_update) {
        ## Add the process
        traits$main <- lapply(process, function(x) return(list(process = list(x))))
        ## Add the ids and the starts
        for(one_process in 1:n_processes) {
            traits$main[[one_process]]$start    <- start_values[[one_process]]
            traits$main[[one_process]]$trait_id <- trait_id[[one_process]]
            if(add_process_args) {
                ## Adding optional arguments if not NULL
                if(!is.null(process.args[[one_process]][[1]])) {
                    traits$main[[one_process]] <- c(traits$main[[one_process]], process.args = list(process.args[one_process]))
                }
            }
        }
    
        ## Add previous traits
        if(add_traits) {
            traits$main <- c(add$main, traits$main)
        }

        ## Add names
        names(traits$main) <- trait_names
    } else {
        ## Update the traits
        traits$main <- update$main

        ## Match the processes to update to the previous traits object
        updates <- which(names(traits$main) %in% update_process)

        ## Add the ids and the starts
        for(i in seq_along(updates)) {

            if(!conditional_update) {
                ## Update the process
                traits$main[[updates[[i]]]]$process[[1]] <- process[[i]]
            } else {
                ## Update the process
                for(j in 1:length(process)) {
                    if(!is.null(process[[j]])) {
                        traits$main$conditional.trait$process[[j]] <- process[[j]]
                    }
                }
            }

            ## Update the starting value
            traits$main[[updates[[i]]]]$start   <- start_values[[i]][1:length(traits$main[[updates[[i]]]]$start)]

            if(add_process_args) {

                if(conditional_update) {
                    stop("Process arguments update is not yet implemented for conditional traits.")
                }

                ## Replacing/adding optional arguments if not NULL
                if(!is.null(process.args[[updates[[i]]]][[1]])) {
                    ## Get the available names in the object to update
                    to_update <- names(process.args[[i]])
                    present_arguments <- unlist(lapply(traits$main[[updates[[i]]]]$process.args, names))
                    ## Detect whether to add new process.args
                    to_add <- id_to_add <- which(!(to_update %in% present_arguments))
                    ## Sort which arguments needs adding and which ones need updating
                    if(length(id_to_add) != 0) {
                        to_add <- to_update[id_to_add]
                        to_update <- to_update[-id_to_add]
                    } 
                    ## Update the arguments
                    if(length(to_update) != 0) {
                        ## Replace the existing arguments
                        traits$main[[updates[[i]]]]$process.args[[1]][[to_update]] <- process.args[[i]][[to_update]]
                    }
                    ## Add the arguments
                    if(length(to_add) != 0) {
                        ## Add new arguments
                        traits$main[[updates[[i]]]]$process.args[[1]] <- c(traits$main[[updates[[i]]]]$process.args[[1]], process.args[[i]][to_add])
                    }
                }
            }
        }
    }
        
    ## check
    if(test) {
        success <- check.traits(traits$main, events = FALSE)
    }

    ## Add the background trait
    if(!missing(background)) {
        # Check background
        if(!is(background, "treats") && !is(background, "traits")) {
            stop("background must be a treats traits object. You can use the following function to generate one:\nmake.traits()")
        }

        ## Check the length of the background trait
        process_lengths <- unname(unlist(lapply(lapply(traits$main, `[[`, "trait_id"), length)))
        main_trait_length <- sum(process_lengths)
        background_trait_length <- sum(unname(unlist(lapply(lapply(background$main, `[[`, "trait_id"), length))))
        if(main_trait_length != background_trait_length) {
            stop(paste0("The background must have the same number of traits than the main process: ", main_trait_length, ifelse(length(process_lengths > 1), paste0(" (",paste(process_lengths, collapse = " + "), ")."))))
        }

        # Add it to the traits
        traits$background <- background
    }

    class(traits) <- c("treats", "traits")
    return(traits)
}