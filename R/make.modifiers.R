#' @title make.modifiers
#'
#' @description Making modifiers objects for treats based on an ancestor's (parent) trait.
#'
#' @param branch.length A function for the waiting time generating branch length (can be left empty for the defeault branch length function; see details).
#' @param selection     A function for selecting the lineage(s) affected by speciation (can be left empty for the default selection function; see details).
#' @param speciation    A function for triggering the speciation events (can be left empty for the default speciation function; see details).
#' @param condition     A function giving the condition on which to modify the output of \code{branch.length} or \code{speciation} (see details). If \code{NULL} the condition is always met.
#' @param modify        A function giving the rule of how to modify the output of \code{branch.length} or \code{speciation} (see details). If \code{NULL} no modification is used.
#' @param add           Whether to add this modifier to a \code{"treats"} \code{"modifier"} object.
#' @param update        Optional, another previous \code{"treats"} modifiers object to update (see details).
#' @param test          Logical whether to test if the modifiers object will work (default is TRUE).
#' 
#' @details
#' 
#' \code{branch.length}, \code{selection} and \code{speciation} must be a functions that intakes the following arguments: \code{bd.params, lineage, trait.values, modify.fun}. If left empty, any of these arguments is considered as NULL.
#' 
#' The default \code{branch.length} function is drawing a random number from the exponantial distribution with a rate equal to the current number of taxa multiplied by the speciation and extinction (\code{rexp(1, n_taxa * (speciation + extinction))}).
#' 
#' The default \code{selection} function is randomly drawing a single lineage among the ones present at the time of the speciation (\code{sample(n_taxa, 1)}).
#' 
#' The default \code{speciation} function is drawing a random number from a uniform distribution (0,1) and starts a speciation event if this random number is lower than the ration of speciation on speciation and extinction (\code{runif(1) < (speciation/(speciation + extinction))}). If the random number is greater, the lineage goes extinct.
#' 
#' \code{condition} must be a function with unambiguous input (the inputs listed about for \code{branch.length} and \code{speciation}) and must output a single \code{logical} value. 
#'
#' For example a conditional on the number of taxa:
#' 
#'     \code{condition = function(lineage) return(lineage$n < 1)}
#' 
#' or a conditional on the trait values:
#' 
#'     \code{condition = function(trait.values, lineage)}
#'     \code{    \{}
#'     \code{    parent.traits(trait.values, lineage) < mean(trait.values)}
#'     \code{    \}}
#' 
#' \code{modify} must be a function with at least one input named \code{x} (which will be the branch length or the speciation trigger to value depending on the modifier) and must return a \code{numeric} value.
#' For example a constant modification of the input:
#' 
#'     \code{modify = function(x) return(x * 2)}
#' 
#' or a modifier depending on the number of taxa:
#' 
#'     \code{modify = function(x, lineage) return(x/lineage$n)}
#' 
#' 
#' When using \code{update}, the provided arguments (to \code{make.modifiers}) will be the ones updated in the \code{"modifiers"} object.
#' If the \code{"modifiers"} object contains multiple modifiers (\code{branch.length}, \code{selection} or \code{speciation}), only the called arguments will be updated (e.g. \code{make.modifiers(update = previous_modifiers, speciation = new_speciation)} will only update the speciation process).
#' 
#' More details about the \code{modifiers} functions is explained in the \code{treats} manual: \url{http://tguillerme.github.io/treats}.
#'
#' @return
#' This function outputs a \code{treats} object that is a named list of elements handled internally by the \code{\link{treats}} function.
#'
#' @examples
#' ## These functions should be fed to the make.modifiers function to create
#' ## modifiers for treats objects. For example, the following sets specifies that
#' ## the branch length should be generated using the branch.length.trait function
#' ## the selection using the selection function and the speciation using the
#' ## speciation.trait function:
#' my_modifiers <- make.modifiers(branch.length = branch.length.trait,
#'                                selection     = selection,
#'                                speciation    = speciation.trait)
#'
#' ## Creating a treats simulation using these modifiers
#' treats(stop.rule = list(max.taxa = 20),
#'      traits = make.traits(),
#'      modifiers = my_modifiers)
#'
#' @seealso \code{\link{treats}} \code{\link{modifiers}}
#' 
#' @author Thomas Guillerme
#' @export

make.modifiers <- function(branch.length = NULL, selection = NULL, speciation = NULL, condition = NULL, modify = NULL, add = NULL, update = NULL, test = TRUE) {

    ## Get the call
    match_call <- match.call()
    # return(match_call)
    ## Internal function for returning default calls
    call.default <- function(x) {
        return(ifelse(is.null(x), "default", as.character(x)))
    }

    ## Check branch length
    do_branch_length <- FALSE
    if(!is.null(branch.length)) {
        ## Checking the arguments
        branch.length <- check.args(branch.length, fun_name = "branch length", required_args = c("bd.params", "lineage", "trait.values", "modify.fun"))
        do_branch_length <- TRUE
    }

    ## Check selection
    do_selection <- FALSE
    if(!is.null(selection)) {
        ## Checking the arguments
        selection <- check.args(selection, fun_name = "selection", required_args = c("bd.params", "lineage", "trait.values", "modify.fun"))
        do_selection <- TRUE
    }

    ## Check speciation
    do_speciation <- FALSE
    if(!is.null(speciation)) {
        ## Checking the arguments
        speciation <- check.args(speciation, fun_name = "speciation", required_args = c("bd.params", "lineage", "trait.values", "modify.fun"))
        do_speciation <- TRUE
    }

    ## Check condition
    do_condition <- FALSE
    if(!is.null(condition)) {
        ## Checking the arguments
        condition <- check.args(condition, fun_name = "condition", required_args = c("bd.params", "lineage", "trait.values", "modify.fun"))
        do_condition <- TRUE
    } else {
        ## Default condition
        condition <- function(lineage, trait.values, modify.fun) return(TRUE)
    }

    ## Check modify
    do_modify <- FALSE
    if(!is.null(modify)) {
        ## Checking the arguments
        modify <- check.args(modify, fun_name = "modify", required_args = c("x", "bd.params", "lineage", "trait.values", "modify.fun"))
        do_modify <- TRUE
    } else {
        ## Default modify
        modify <- function(x, lineage, trait.values, modify.fun) return(x)
    }

    ## test
    check.class(test, "logical")

    ## Update a modifier
    do_update <- FALSE
    if(!is.null(update)) {
        if(is(update, "treats") && is(update, "modifiers")) {
            do_update <- TRUE
        } else {
            stop("You can only update a \"treats\" \"modifiers\" object. Check the documentation from the following function for helping designing such objects:\n    ?make.modifiers", call. = FALSE)
        }
    }

    ## add
    add_modifiers <- FALSE
    if(!is.null(add)) {

        ## Only run if no update
        if(do_update) {
            stop("Impossible to add and update a modifiers object at the same time.")
        }

        ## Check input
        if(!(is(add, "treats") && is(add, "modifiers"))) {
            stop("You can only add to a \"treats\" \"modifiers\" object. Check the documentation from the following function for helping designing such objects:\n    ?make.modifiers", call. = FALSE)
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
        if(do_selection) {
            message("selection function was overwritten.")
            init_selection <- TRUE
        } else {
            init_selection <- FALSE
        }
        if(do_speciation) {
            message("speciation function was overwritten.")
            init_speciation <- TRUE
        } else {
            init_speciation <- FALSE
        }

        if(!do_branch_length && !do_selection && !do_speciation) {
            ## Only update the modify and function
            if(!do_modify && !do_selection && !do_speciation) {
                stop("Nothing to update. Specify at least one branch.length, selection, speciation, condition or modify function.")
            } else {
                update_condition <- do_condition
                update_modify <- do_modify
            }
        }

    } else {
        ## Build an empty modifiers list
        modifiers <- list("waiting" = NULL, "selecting" = NULL, "speciating" = NULL, "call" = NULL)
        init_branch_length <- init_selection <- init_speciation <- TRUE
        update_condition <- update_modify <- FALSE
    }

    if(!do_update) {
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

        ## Making the selecting modifier
        if(init_selection) {
            if(!do_selection) {
                modifiers$selecting <- list(fun = selection.fast,
                                             internal = NULL)
                ## Update the call
                modifiers$call$selecting$fun <- "default"
            } else {
                modifiers$selecting <- list(fun = selection,
                                             internal = list(condition = condition,
                                                             modify    = modify))
                ## Update the call
                modifiers$call$selecting$fun       <- call.default(match_call$selection)
                modifiers$call$selecting$condition <- call.default(match_call$condition)
                modifiers$call$selecting$modify    <- call.default(match_call$modify)
            }
        } else {
            if(do_selection) {
                if(update_condition) {
                    modifiers$selecting$internal$condition <- condition
                    ## Update the call
                    modifiers$call$selecting$condition     <- call.default(match_call$condition)
                }
                if(update_modify) {
                    modifiers$selecting$internal$modify <- modify
                    ## Update the call
                    modifiers$call$selecting$modify     <- call.default(match_call$modify)
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
                    modifiers$call$speciating$condition     <- call.default(match_call$condition)
                }
                if(update_modify) {
                    modifiers$speciating$internal$modify <- modify
                    ## Update the call
                    modifiers$call$speciating$modify     <- call.default(match_call$modify)
                }
            }
        }
    } else {
        ## Update the previous modifiers
        modifiers <- update

        ## Check what to update
        if(do_branch_length) {
            ## Update only the branch length
            modifiers$waiting$fun <- branch.length
            ## Update the call
            modifiers$call$waiting$fun <- call.default(match_call$selection)

            ## Update the condition
            modifiers$waiting$internal$condition <- condition
            ## Update the modify
            modifiers$waiting$internal$modify <- modify

            ## Update the call
            modifiers$call$waiting$condition <- call.default(match_call$condition)
            modifiers$call$waiting$modify <- call.default(match_call$modify)

            ## Don't update the condition and the modify further
            do_condition <- do_modify <- FALSE
        }

        if(do_selection) {
            ## Update only the selection
            modifiers$selecting$fun <- selection
            ## Update the call
            modifiers$call$selecting$fun <- call.default(match_call$selection)

            ## Update the condition
            modifiers$selecting$internal$condition <- condition
            ## Update the modify
            modifiers$selecting$internal$modify <- modify
            
            ## Update the call
            modifiers$call$selecting$condition <- call.default(match_call$condition)
            modifiers$call$selecting$modify <- call.default(match_call$modify)

            ## Don't update the condition and the modify further
            do_condition <- do_modify <- FALSE
        }

        if(do_speciation) {
            ## Update only the speciation
            modifiers$speciating$fun <- speciation
            ## Update the call
            modifiers$call$speciating$fun <- call.default(match_call$speciation)

            ## Update the condition
            modifiers$speciating$internal$condition <- condition
            ## Update the modify
            modifiers$speciating$internal$modify <- modify
            
            ## Update the call
            modifiers$call$speciating$condition <- call.default(match_call$condition)
            modifiers$call$speciating$modify <- call.default(match_call$modify)

            ## Don't update the condition and the modify further
            do_condition <- do_modify <- FALSE
        }

        if(do_condition) {
            ## Update all the conditions
            modifiers$waiting$internal$condition <- modifiers$selecting$internal$condition <- modifiers$speciating$internal$condition <- condition
            ## Update the call
            modifiers$call$waiting$condition <- modifiers$call$selecting$condition <- modifiers$call$speciating$condition <- call.default(match_call$condition)
        }

        if(do_modify) {
            ## Update all the modifiers
            modifiers$waiting$internal$modify <- modifiers$selecting$internal$modify <- modifiers$speciating$internal$modify <- modify
            ## Update the call
            modifiers$call$waiting$modify <- modifiers$call$selecting$modify <- modifiers$call$speciating$modify <- call.default(match_call$modify)
        }
    }

    if(test) {
        check.modifiers(modifiers, events = FALSE)
    }

    class(modifiers) <- c("treats", "modifiers")
    return(modifiers)
}
