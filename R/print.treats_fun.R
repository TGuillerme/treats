## Function for printing the traits info
internal.print.traits.info <- function(x) {

    ## Get the trait object info
    n_processes   <- length(x)
    trait_process <- lapply(x, function(X) X$trait_id)
    n_traits      <- length(unlist(trait_process))
    start_values  <- unlist(lapply(x, function(X) X$start))
    n_start       <- length(unique(start_values))
    extra_options <- lapply(x, function(X) lapply(X$process.args, names))
    with_extra    <- unlist(lapply(extra_options, function(x) length(x) != 0))

    ## Number of traits
    cat(paste0(n_traits, " trait", ifelse(n_traits > 1, "s ", " ")))
    ## Number of processes
    cat(paste0("for ", n_processes, " process", ifelse(n_processes > 1, "es ", " ")))
    if(n_traits != n_processes) {
        cat(paste0("(", paste0(paste(names(x), unlist(lapply(trait_process, length)), sep = ":"), collapse = ", ") , ") "))
    } else {
        cat(paste0("(", paste0(names(x), collapse = ", ") , ") "))
    }
    ## Starting values
    cat(paste0("with "))
    if(n_start == 1) {
        cat(paste0("one starting value (", unique(start_values), ")"))
    } else {
        cat(paste0("different starting values (", paste0(start_values, collapse = ",") ,")"))
    }
    cat(paste0(".\n"))
    ## Extra options
    if(any(with_extra)) {
        for(one_process in seq_along(with_extra)) {
            if(with_extra[one_process]) {
                cat(paste0("process ", names(x)[one_process], " uses the following extra argument", ifelse(length(extra_options[[one_process]]) > 1, "s: ", ": "), paste0(extra_options[[one_process]], collapse = ", "), ".\n"))
            }
        }
        paste0("\n")
    }
    return(invisible())
}

## Function for printing the modifiers info
internal.print.modifiers.info <- function(x) {

    ## Check of is null or default
    not.null.default <- function(x) {
        if(is.null(x)) {
            return(FALSE)
        }
        if(x[[1]] == "default") {
            return(FALSE)
        }
        return(TRUE)
    }

    ## See if everything is default
    if(all(unlist(x$call) == "default")) {
        cat(paste0("No modifiers applied to the branch length, selection and speciation processes (default).\n"))
    } else {
        ## Waiting process
        if(not.null.default(x$call$waiting$fun)) {
            cat(paste0("Branch length process is set to ", x$call$waiting$fun))
        } else {
            cat(paste0("Default branch length process"))
        }
        if(not.null.default(x$call$waiting$condition)) {
            cat(paste0(" with a condition (", x$call$waiting$condition, ")"))
        }
        if(not.null.default(x$call$waiting$modify)) {
            cat(ifelse(not.null.default(x$call$waiting$condition), " and ", " with "))
            cat(paste0("a modifier (", x$call$waiting$modify, ")"))
        }
        cat(".\n")

        ## Selecting process
        if(not.null.default(x$call$selecting$fun)) {
            cat(paste0("Selection process is set to ", x$call$selecting$fun))
        } else {
            cat(paste0("Default selection process"))
        }
        if(not.null.default(x$call$selecting$condition)) {
            cat(paste0(" with a condition (", x$call$selecting$condition, ")"))
        }
        if(not.null.default(x$call$selecting$modify)) {
            cat(ifelse(not.null.default(x$call$selecting$condition), " and ", " with "))
            cat(paste0("a modifier (", x$call$selecting$modify, ")"))
        }
        cat(".\n")

        ## Speciating process
        if(not.null.default(x$call$speciating$fun)) {
            cat(paste0("Speciation process is set to ", x$call$speciating$fun))
        } else {
            cat(paste0("Default speciation process"))
        }
        if(not.null.default(x$call$speciating$condition)) {
            cat(paste0(" with a condition (", x$call$speciating$condition, ")"))
        }
        if(not.null.default(x$call$speciating$modify)) {
            cat(ifelse(not.null.default(x$call$speciating$condition), " and ", " with "))
            cat(paste0("a modifier (", x$call$speciating$modify, ")"))
        }
        cat(".\n")
    }
}

## Function for printing the events info
internal.print.events.info <- function(x) {
    cat(paste0(ifelse(is.null(x$call$event.name), "E", paste0(x$call$event.name, " e")), "vent targeting \"", x$target, "\" to be triggered ", x$trigger+1, " time", ifelse(x$trigger < 0, "s.", ".")))
    cat("\n")
    condition_fun <- if(is(x$call$condition, "name")) {x$call$condition} else {x$call$condition[[1]]}
    cat(paste0("The condition function is: ", condition_fun, "\n"))
    modification_fun <- if(is(x$call$modification, "name")) {x$call$modification} else {x$call$modification[[1]]}
    cat(paste0("The modification function is: ", modification_fun, "\n"))
    cat("\n")
}

## Function for printing bd.params info
internal.print.bd.params.info <- function(x) {
    if(x$joint) {
        cat("joint sampling for:\n")
    }
    cat("speciation: ")
    if(is.null(x$call$speciation)) {
        ## Default
        cat("1")
    } else {
        if(length(did_grep <- grep("c", x$call$speciation)) > 0 &&  did_grep == 1) {
            ## Remove the "c(" from the expression
            call_val <- x$call$speciation[-1]
        } else {
            call_val <- x$call$speciation
        }
        if(is(call_val, "numeric")) {
            call_val <- round(call_val, digits = 3)
        } 
        cat(paste0(paste(call_val, collapse = ", ")))
    }
    if(!is.null(x$call$speciation.args)) {
        cat(" (with optional arguments)")
    }    
    cat(".\n")

    cat("extinction: ")
    if(is.null(x$call$extinction)) {
        ## Default
        cat("0")
    } else {
        if(length(did_grep <- grep("c", x$call$extinction)) > 0 &&  did_grep == 1) {
            ## Remove the "c(" from the expression
            call_val <- x$call$extinction[-1]
        } else {
            call_val <- x$call$extinction
        }
        if(is(call_val, "numeric")) {
            call_val <- round(call_val, digits = 3)
        } 
        cat(paste0(paste(call_val, collapse = ", ")))
    }
    if(!is.null(x$call$extinction.args)) {
        cat(" (with optional arguments)")
    } 
    cat(".\n")
    if(x$absolute) {
        cat("(using absolute values)\n")
    }
}