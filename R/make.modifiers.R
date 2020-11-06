#' @title make.modifiers
#'
#' @description Making modifiers objects for dads based on an ancestor's (parent) trait.
#'
#' @param branch.length A function for the waiting time generating branch length (see details).
#' @param speciation    A function for triggering the speciation events (see details).
#' @param condition     A function giving the condition on which to modify the output of \code{branch.length} or \code{speciation} (see details). If missing the condition is always met (set to \code{condition = function() TRUE})
#' @param modify        A function giving the rule of how to modify the output of \code{branch.length} or \code{speciation} (see details). If missing no modification is used (set to \code{modify = function(x) x}).
#' @param add           Whether to add this modifier to a \code{"dads"} \code{"modifier"} object.
#' @param test          Logical whether to test if the modifiers object will work (default is TRUE),
#' @param ...           Optional, any extra argument to be passed to the modifier function.
#' 
#' @details
#' 
#' \code{branch.length} and \code{speciation} must be a functions that intakes the following arguments: \code{bd.params} = NULL, n.taxa = NULL, parent.lineage = NULL, trait.values = NULL, modify.fun}.
#' 
#' \code{condition} must be a function with unambiguous input (the inputs listed about for \code{branch.length} and \code{speciation}) and must output a single \code{logical} value. For example a conditional on the number of taxa:
#' 
#' \code{condition = function(n.taxa) return(n.taxa < 1)}
#' 
#' or a conditional on the trait values:
#' 
#' \code{condition = function(trait.values, parent.lineage)}
#' \code{    \{}
#' \code{    get.parent.traits(trait.values, parent.lineage) < mean(trait.values)}
#' \code{    \}}
#' 
#' \code{modify} must be a function with at least one input named \code{x} (which will be the branch length or the speciation trigger to value depending on the modifier) and must return a \code{numeric} value.
#' For example a constant modification of the input:
#' 
#' \code{modify = function(x) return(x * 2)}
#' 
#' or a modifier depending on the number of taxa:
#' 
#' \code{modify = function(x, n.taxa) return(x/n.taxa)}
#' 
#' More details about the \code{modifiers} functions is explained in the \code{dads} manual: \url{http://tguillerme.github.io/dads}.
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

make.modifiers <- function(branch.length, speciation, condition, modify, add, test = TRUE, ...) {

    ## required arguments
    required_args <- c("bd.params", "n.taxa", "parent.lineage", "trait.values", "modify.fun")

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
    } else {
        ## Default condition
        condition <- function() return(TRUE)
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
    } else {
        ## Default modify
        modify <- function(x) return(x)
    }

    ## test
    check.class(test, "logical")
    
    ## Set the defaults
    default_branch_length <- !do_branch_length && !do_condition && !do_modify
    default_speciation    <- !do_speciation    && !do_condition && !do_modify

    ## add
    add_modifiers <- FALSE
    if(!missing(add)) {
        ## Check input
        if(!(is(add, "dads") && is(add, "modifiers"))) {
            stop("modifiers can only be added to objects of class dads and modifiers.")
        }
        add_modifiers <- TRUE
        ## Check what's done already
        already_done <- names(add)

        ## Check if waiting time existed
        if("waiting" %in% names(add)) {
            if(do_branch_length) {
                warning("branch.length modifier overwritten.", call. = FALSE)
            } else {
                default_branch_length <- FALSE
            } 
        }

        ## Check if speciating existed
        if("speciating" %in% names(add)) {
            if(do_branch_length) {
                warning("speciation modifier overwritten.", call. = FALSE)
            } else {
                default_speciation <- FALSE
            } 
        }
    }

    ## Build the object
    modifiers <- list()
    ## Making the waiting modifier
    if(default_branch_length) {
        modifiers$waiting <- list(fun = branch.length.fast,
                                    internal = NULL)
    } else {
        if(do_branch_length) {
            modifiers$waiting <- list(fun = branch.length,
                                      internal = list(condition = condition,
                                                      modify    = modify))
        }
    }

    ## Making the speciating modifier
    if(default_speciation) {
        modifiers$speciating <- list(fun = speciation.fast,
                                     internal = NULL)
    } else {
        if(do_speciation) {
            modifiers$speciating <- list(fun = speciation,
                                         internal = list(condition = condition,
                                                         modify    = modify))
        }
    }

    if(test) {
        check.modifiers(modifiers)
    }

    class(modifiers) <- c("dads", "modifiers")
    return(modifiers)
}
