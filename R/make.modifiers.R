#' @title make.modifiers
#'
#' @description Making modifiers objects for dads based on an ancestor's (parent) trait.
#'
#' @param branch.length A function for the waiting time generating branch length (can be left empty for the defeault branch length function; see details).
#' @param speciation    A function for triggering the speciation events (can be left empty for the default speciation function; see details).
#' @param condition     A function giving the condition on which to modify the output of \code{branch.length} or \code{speciation} (see details). If missing the condition is always met.
#' @param modify        A function giving the rule of how to modify the output of \code{branch.length} or \code{speciation} (see details). If missing no modification is used.
#' @param add           Whether to add this modifier to a \code{"dads"} \code{"modifier"} object.
#' @param test          Logical whether to test if the modifiers object will work (default is TRUE).
#' 
#' @details
#' 
#' \code{branch.length} and \code{speciation} must be a functions that intakes the following arguments: \code{bd.params, n.taxa, parent.lineage, trait.values, modify.fun} (even if they are not used in the function).
#' 
#' The default \code{branch.length} function is drawing a random number from the exponantial distribution with a rate equal to the current number of taxa multiplied by the speciation and extinction (\code{rexp(1, n.taxa * (speciation + extinction))}).
#' 
#' The default \code{speciation} function is drawing a random number from a uniform distribution (0,1) and starts a speciation event if this random number is lower than the ration of speciation on speciation and extinction (\code{runif(1) < (speciation/(speciation + extinction))}). If the random number is greater, the lineage goes extinct.
#' 
#' \code{condition} must be a function with unambiguous input (the inputs listed about for \code{branch.length} and \code{speciation}) and must output a single \code{logical} value. 
#'
#' For example a conditional on the number of taxa:
#' 
#'     \code{condition = function(n.taxa) return(n.taxa < 1)}
#' 
#' or a conditional on the trait values:
#' 
#'     \code{condition = function(trait.values, parent.lineage)}
#'     \code{    \{}
#'     \code{    parent.traits(trait.values, parent.lineage) < mean(trait.values)}
#'     \code{    \}}
#' 
#' \code{modify} must be a function with at least one input named \code{x} (which will be the branch length or the speciation trigger to value depending on the modifier) and must return a \code{numeric} value.
#' For example a constant modification of the input:
#' 
#'     \code{modify = function(x) return(x * 2)}
#' 
#' or a modifier depending on the number of taxa:
#' 
#'     \code{modify = function(x, n.taxa) return(x/n.taxa)}
#' 
#' More details about the \code{modifiers} functions is explained in the \code{dads} manual: \url{http://tguillerme.github.io/dads}.
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

make.modifiers <- function(branch.length, speciation, condition, modify, add, test = TRUE) {

    ## required arguments
    required_args <- c("bd.params", "n.taxa", "parent.lineage", "trait.values", "modify.fun")
    ## Get the call
    match_call <- match.call()
    # return(match_call)
    ## Internal function for returning default calls
    call.default <- function(x) {
        return(ifelse(is.null(x), "default", as.character(x)))
    }

    ## Check branch length
    do_branch_length <- FALSE
    if(!missing(branch.length)) {
        check.class(branch.length, "function")
        ## Check if it has the right arguments
        check_args <- names(formals(branch.length))
        if(any(missing <- is.na(match(required_args, check_args)))) {
            stop(paste0("The branch.length function is missing the following argument", ifelse(sum(missing) > 1, "s: ", ": "),  paste(required_args[missing], collapse = ", "), ". If ", ifelse(sum(missing) > 1, "they are", "it is"), " not required, you can set ", ifelse(sum(missing) > 1, "them", "it"), " to NULL."), call. = FALSE)
        }
        do_branch_length <- TRUE
    }

    ## Check speciation
    do_speciation <- FALSE
    if(!missing(speciation)) {
        check.class(speciation, "function")
        ## Check if it has the right arguments
        check_args <- names(formals(speciation))
        if(any(missing <- is.na(match(required_args, check_args)))) {
            stop(paste0("The speciation function is missing the following argument", ifelse(sum(missing) > 1, "s: ", ": "),  paste(required_args[missing], collapse = ", "), ". If ", ifelse(sum(missing) > 1, "they are", "it is"), " not required, you can set ", ifelse(sum(missing) > 1, "them", "it"), " to NULL."), call. = FALSE)
        }
        do_speciation <- TRUE
    }

    ## Check condition
    do_condition <- FALSE
    if(!missing(condition)) {
        check.class(condition, "function")
        do_condition <- TRUE
        ## Check the arguments
        check_args <- names(formals(condition))
        if(any(incorrect <- is.na(match(check_args, required_args)))) {
            stop(paste0("The condition function cannot recognise the ", paste(check_args[incorrect], collapse = ", "), " argument", ifelse(sum(incorrect) > 1, "s.", ".")), call. = FALSE)
        }
        ## Make sure the arguments match the required
        used_args <- methods::formalArgs(condition)
        if(any(missing_args <- !(required_args[-c(1,5)] %in% used_args))) {
            ## Add the argument to the function
            formals(condition) <- alist("n.taxa" = , "parent.lineage" = , "trait.values" = )
        }
    } else {
        ## Default condition
        condition <- function(n.taxa, parent.lineage, trait.values, modify.fun) return(TRUE)
    }

    ## Check modify
    do_modify <- FALSE
    if(!missing(modify)) {
        check.class(modify, "function")
        do_modify <- TRUE
        ## Check the arguments
        check_args <- names(formals(modify))
        if(!("x" %in% check_args)) {
            stop(paste0("The modify function must have at least one x argument (you can use x = NULL)."))
        }
        if(any(incorrect <- is.na(match(check_args, c("x", required_args))))) {
            stop(paste0("The modify function cannot recognise the ", paste(check_args[incorrect], collapse = ", "), " argument", ifelse(sum(incorrect) > 1, "s.", ".")), call. = FALSE)
        }
        used_args <- methods::formalArgs(modify)
        if(any(missing_args <- !(c("x", required_args[-c(1,5)]) %in% used_args))) {
            ## Add the argument to the function
            formals(modify) <- alist("x" = , "n.taxa" = , "parent.lineage" = , "trait.values" = )
        }
    } else {
        ## Default modify
        modify <- function(x, n.taxa, parent.lineage, trait.values, modify.fun) return(x)
    }

    ## test
    check.class(test, "logical")

    ## add
    add_modifiers <- FALSE
    if(!missing(add)) {
        ## Check input
        if(!(is(add, "dads") && is(add, "modifiers"))) {
            stop("modifiers can only be added to objects of class dads and modifiers.")
        }

        ## Update the modifiers
        modifiers <- add

        ## Check which things to change
        if(do_branch_length) {
            message("branch.length function was overwritten.")
            init_branch_length <- TRUE
        } else {
            init_branch_length <- FALSE
        }
        if(do_speciation) {
            message("speciation function was overwritten.")
            init_speciation <- TRUE
        } else {
            init_speciation <- FALSE
        }

        if(!do_branch_length && !do_speciation) {
            ## Only update the modify and function
            if(!do_modify && !do_speciation) {
                stop("Nothing to update. Specify at least one branch.length, speciation, condition or modify function.")
            } else {
                update_condition <- do_condition
                update_modify <- do_modify
            }
        }

    } else {
        ## Build an empty modifiers list
        modifiers <- list("waiting" = NULL, "speciating" = NULL, "call" = NULL)
        init_branch_length <- init_speciation <- TRUE
        update_condition <- update_modify <- FALSE
    }



    ## Making the waiting modifier
    if(init_branch_length) {
        if(!do_branch_length) {
            modifiers$waiting <- list(fun = branch.length.fast,
                                        internal = NULL)
            ## Update the call
            modifiers$call$waiting$fun <- "default"
        } else {
            modifiers$waiting <- list(fun = branch.length,
                                      internal = list(condition = condition,
                                                      modify    = modify))
            ## Update the call
            modifiers$call$waiting$fun       <- call.default(match_call$branch.length)
            modifiers$call$waiting$condition <- call.default(match_call$condition)
            modifiers$call$waiting$modify    <- call.default(match_call$modify)
        }
    } else {
        if(do_branch_length) {
            if(update_condition) {
                modifiers$waiting$internal$condition <- condition
                ## Update the call
                modifiers$call$waiting$condition <- call.default(match_call$condition)
            }
            if(update_modify) {
                modifiers$waiting$internal$modify <- modify
                ## Update the call
                modifiers$call$waiting$modify    <- call.default(match_call$modify)
            }
        }
    }

    ## Making the speciating modifier
    if(init_speciation) {
        if(!do_speciation) {
            modifiers$speciating <- list(fun = speciation.fast,
                                         internal = NULL)
            ## Update the call
            modifiers$call$speciating$fun <- "default"
        } else {
            modifiers$speciating <- list(fun = speciation,
                                         internal = list(condition = condition,
                                                         modify    = modify))
            ## Update the call
            modifiers$call$speciating$fun       <- call.default(match_call$speciation)
            modifiers$call$speciating$condition <- call.default(match_call$condition)
            modifiers$call$speciating$modify    <- call.default(match_call$modify)
        }
    } else {
        if(do_speciation) {
            if(update_condition) {
                modifiers$speciating$internal$condition <- condition
                ## Update the call
                modifiers$call$speciating$condition <- call.default(match_call$condition)
            }
            if(update_modify) {
                modifiers$speciating$internal$modify <- modify
                ## Update the call
                modifiers$call$speciating$modify    <- call.default(match_call$modify)
            }
        }
    }

    if(test) {
        check.modifiers(modifiers)
    }

    class(modifiers) <- c("dads", "modifiers")
    return(modifiers)
}
