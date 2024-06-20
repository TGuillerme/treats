#' @title link.traits
#'
#' @description Linking traits objects together to simulate simulate them sequentially.
#'
#' @param base.trait    One or more \code{"treats"} \code{"traits"} object(s) to be considered first.
#' @param next.trait    One or more \code{"treats"} \code{"traits"} object(s) to be considered sequentially.
#' @param link.type     The type of link between the traits. Can be \code{"conditional"}.
#' @param link.args     Optional arguments to interpret the link between the objects (based on the \code{link.type}).
# @param trait.names   Optional, the name(s) of the process(s). 
#'
#' @details
#' This function allows to link several traits together in the simulations. The current link types implemented are:
#' \itemize{
#'      \item{"conditional"}: this allows to link the \code{next.trait} traits conditionally to the \code{base.trait} one. For example if \code{base.trait} is a \code{\link{discrete.process}} with two states \code{0} and \code{1} and \code{next.trait} is a list of two traits with two different processes \code{\link{OU.process}} and \code{\link{BM.process}}. The simulations generates a first trait using \code{base.trait} and then a second one using one of the two processes in \code{next.trait} depending on the results of \code{base.trait}. The link arguments \code{link.args} must be a list of logical functions to interpret \code{x1}, the results of the \code{base.trait}. For example, \code{list(function(x1){x1 == 0}, function(x1){x1 == 1})} will generate a trait using the first \code{next.trait} if \code{x1} is equal to \code{0} or using the second \code{next.trait} if \code{x1} is equal to \code{1}.
# }
#'
#' @return
#' This function outputs a \code{treats} object that is a named list of elements handled internally by the \code{\link{treats}} function.
#'
#' @examples
#'
#' @seealso \code{\link{treats}} \code{\link{trait.process}} \code{\link{make.traits}}
#' 
#' @author Thomas Guillerme
#' @export

link.traits <- function(base.trait, next.trait, link.type, link.args) {
    
    match_call <- match.call()

    ## Sanitizing
    ## Traits must be traits
    base_class <- check.class(base.trait, c("list", "treats"))
    next_class <- check.class(next.trait, c("list", "treats"))
    if(base_class == "list") {
        lapply(base.trait, check.class, c("treats"))
    }
    if(next_class == "list") {
        lapply(next.trait, check.class, c("treats"))
    }

    ## types
    implemented_methods <- c("conditional")
    check.method(link.type, all_arguments = implemented_methods, msg = "link.type")

    ## link.args
    if(link.type == "conditional") {
        ## Must be the same length of next.trait and be functions
        error_msg <- "next.trait and link.args must be a lists of the same lengths containing one or more traits and conditional arguments for conditional links."
        if(next_class != "list" || !is(link.args, "list")) {
            stop(error_msg,  call. = FALSE)
        }
        if(!(length(link.args) == length(next.trait))) {
            stop(error_msg, call. = FALSE)
        }
        if(any(!unlist(lapply(link.args, is, "function")))) {
            stop(error_msg, call. = FALSE)
        }
    }

    ## Generate the linked trait
    set.traits <- switch(link.type, {
        "conditional" = set.conditional.traits
    }) 

    ## Create the traits
    linked_traits <- set.traits(base.trait, next.trait, link.args)
    linked_traits <- list("main" = linked_traits, "background" = NULL)
    class(linked_traits) <- c("treats", "traits")

    return(linked_traits)
}

set.conditional.traits <- function(base.trait, next.trait, link.args) {
    ## Combine the base trait and the next traits
    conditional_trait <- list("conditional" = base.trait$main)
    
    ## Trait ID base
    id_base <- conditional_trait$conditional[[1]]$trait_id
    ## Set to 1 for now
    if(id_base != 1) {
        conditional_trait$conditional[[1]]$trait_id <- seq(1:length(id_base))
        id_base <- seq(1:length(id_base))
    }

    ## Get the conditioned traits (keep the names)
    conditioned_traits <- lapply(next.trait, function(x) return(x$main))

    ## Check if conditioned traits have the same IDs length
    id_next <- lapply(conditioned_traits, function(x) x[[1]]$trait_id)
    ## Check lengths
    if(length(dimensions <- unique(unlist(lapply(id_next, length)))) != 1) {
        stop("The next.traits must have the same number of dimensions.")
    }
    ## Set to the same IDs
    new_ids <- seq(1:dimensions)+ max(id_base)

    ## Get the conditions
    update.conditionals <- function(trait, condition, id) {
        trait[[1]]$trait_id <- id
        trait[[1]]$condition.test <- condition
        return(trait)
    }
    conditioned_traits <- mapply(update.conditionals, conditioned_traits, link.args, MoreArgs = list(id = new_ids), SIMPLIFY = FALSE)

    ## Merge conditional and conditioned
    conditional_trait$conditioned <- conditioned_traits

    return(conditional_trait)
}