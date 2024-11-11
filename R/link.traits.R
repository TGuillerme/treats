#' @title link.traits
#'
#' @description Linking traits objects together to simulate simulate them sequentially.
#'
#' @param base.trait    One or more \code{"treats"} \code{"traits"} object(s) to be considered first.
#' @param next.trait    One or more \code{"treats"} \code{"traits"} object(s) to be considered sequentially.
#' @param link.type     The type of link between the traits. Can be \code{"conditional"}.
#' @param link.args     Optional arguments to interpret the link between the objects (based on the \code{link.type}).
#' @param trait.name    Optional, a \code{character}, the name the resulting trait. 
#'
#' @details
#' This function allows to link several traits together in the simulations. The current link types implemented are:
#' \itemize{
#'      \item{"conditional"}: this allows to link the \code{next.trait} traits conditionally to the \code{base.trait} one. For example if \code{base.trait} is a \code{\link{discrete.process}} with two states \code{0} and \code{1} and \code{next.trait} is a list of two traits with two different processes \code{\link{OU.process}} and \code{\link{BM.process}}. The simulations generates a first trait using \code{base.trait} and then a second one using one of the two processes in \code{next.trait} depending on the results of \code{base.trait}. The link arguments \code{link.args} must be a list of logical functions to interpret \code{x1}, the results of the \code{base.trait}. For example, \code{list(function(x1){x1 == 0}, function(x1){x1 == 1})} will generate a trait using the first \code{next.trait} if \code{x1} is equal to \code{0} or using the second \code{next.trait} if \code{x1} is equal to \code{1}.
#'}
#'
#' @return
#' This function outputs a \code{treats} object that is a named list of elements handled internally by the \code{\link{treats}} function.
#'
#' @examples
#' ## Setting up a discrete trait
#' discrete_trait <- make.traits(discrete.process,
#'        process.args = list(transitions = matrix(c(3, 0.2, 0.05, 3), 2, 2)),
#'        trait.names  = "discrete")
#'
#' ## Setting up one dummy trait (always outputs 1)
#' always_one <- make.traits(process = function(x0 = 0, edge.length = 1) {return(1)},
#'                           trait.names = "one")
#' ## Setting up a Brownian motion trait
#' BM_trait <- make.traits(trait.names = "BM")
#'
#' ## Setting a condition list to link all traits
#' ## (if discrete trait is 0, simulate a BM trait)
#' ## (if discrete trait is 1, simulate the always one trait)
#' conditions <- list("choose.BM"  = function(x1) {x1 == 0},
#'                    "choose.one" = function(x1) {x1 == 1}) 
#'
#' ## Creating the linked trait
#' conditional <- link.traits(base.trait = discrete_trait,
#'                            next.trait = list(BM_trait, always_one),
#'                            link.type  = "conditional",
#'                            link.args  = conditions)
#'
#' ## Simulating a tree using this trait
#' treats(stop.rule = list(max.living = 200),
#'        traits    = conditional)
#'
#' @seealso \code{\link{treats}} \code{\link{trait.process}} \code{\link{make.traits}}
#' 
#' @author Thomas Guillerme
#' @export

link.traits <- function(base.trait, next.trait, link.type, link.args, trait.name) {
    
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
        ## Check if IDs are the same
        trait_IDs <- lapply(next.trait, function(x) x$main[[1]]$trait_id)
        if(length(unique(unlist(lapply(trait_IDs, length)))) > 1) {
            stop(error_msg, call. = FALSE)
        } else {
            if(!all(unlist(unique(trait_IDs)) %in% trait_IDs[[1]])) {
                stop(error_msg, call. = FALSE)
            }    
        }
        
    }

    if(!missing(trait.name)) {
        check.class(trait.name, "character")
        check.length(trait.name, 1)
    } else {
        trait.name <- paste0(link.type, ".trait")
    }

    ## Generate the linked trait
    set.traits <- switch(link.type, {
        "conditional" = set.conditional.traits
    }) 

    ## Create the traits
    linked_traits <- set.traits(base.trait, next.trait, link.args)
    linked_traits <- list("main" = list(linked_traits), "background" = NULL)
    names(linked_traits$main) <- trait.name
    class(linked_traits) <- c("treats", "traits")

    return(linked_traits)
}

set.conditional.traits <- function(base.trait, next.trait, link.args) {
    
    ## Set the base trait
    conditional_trait <- base.trait$main[[1]]
    base_name <- names(base.trait$main)
    next_names <- unlist(lapply(next.trait, function(x) names(x$main)))
    conditional_trait$link <- list(type = "conditional",
                                   conditional.test = link.args,
                                   trait.names = c(base_name, next_names))

    ## Add the conditional processes
    conditional_trait$process <- c(conditional_trait$process[[1]], lapply(next.trait, function(x) x$main[[1]]$process[[1]]))
    
    ## Add the conditional starts and ids
    conditional_trait$start <- c(conditional_trait$start, next.trait[[1]]$main[[1]]$start)
    conditional_trait$trait_id <- c(conditional_trait$trait_id, (next.trait[[1]]$main[[1]]$trait_id) + base.trait$main[[1]]$trait_id)

    ## Add the conditional process.args
    base_has_no_args <- is.null(conditional_trait$process.args)
    next_process_args <- lapply(next.trait, function(x) x$main[[1]]$process.args)
    next_has_no_args <- unlist(lapply(next_process_args, is.null))
    if(!all(c(base_has_no_args, next_has_no_args))) {
        ## Add base if not present
        if(base_has_no_args) {
            conditional_trait$process.args <- list(NULL)
        }
        ## Add the nexts empties
        for(one_trait in 1:length(next.trait)) {
            conditional_trait$process.args[[one_trait + 1]] <- list(NULL)
        }
        if(!all(next_has_no_args)) {
            not_empties <- which(!next_has_no_args)
            for(one_trait in not_empties) {
                conditional_trait$process.args[one_trait+1] <- next.trait[[one_trait]]$main[[1]]$process.args
            }   
        }
    }
        
    return(conditional_trait)
}