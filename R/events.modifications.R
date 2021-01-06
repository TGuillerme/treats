#' @name events.modifications
#' @aliases modification, mass.extinction, trait.extinction, update.bd.params, update.traits, update.modifiers, founding.event
#' @title events.modifications
#'
#' @description Inbuilt modifications functions for helping designing events
#'
#' @usage modification(x, ...)
#' 
#' @param x   a numerical value to update.
#' @param ... any specific argument for the modification (see details).
#' 
#' @details
#' The following functions allow to design specific modifications for events:
#' 
#' \itemize{
#'      
#' \item modifications for the target \code{"taxa"}
#'      \itemize{
#'          \item \code{random.extinction}: this function removes (makes extinct) a proportion of living taxa when the event is triggered. The proportion of taxa to remove can be changed with the argument \code{x}. 
#'          \item \code{trait.extinction}: this function removes (makes extinct) a number of living taxa based on their trait(s) values when the event is triggered. The trait value is specified with the argument \code{x}. You can specify the condition in relation to that trait value with \code{condition} (the default is \code{condition = `<`} meaning taxa with a trait value lower that \code{x} will go extinct) and which trait(s) to consider using \code{trait} (the default is \code{trait = 1}, meaning it will only consider the first trait).
#'      }
#' 
#' \item modifications for the target \code{"bd.params"}
#'      \itemize{
#'          \item \code{update.bd.params}: this function updates the birth death parameters within the birth death process. The value of the parameter to change is specified with the argument \code{x} and the argument to change is specified with the argument \code{parameter} (e.g. \code{parameter = "speciation"} will attribute the value \code{x} to \code{bd.params$speciation}).
#'      }
#'
#' \item modifications for the target \code{"traits"} 
#'      \itemize{
#'          \item \code{update.traits}: this function updates a \code{"dads"} \code{"traits"} object. This function takes as arguments any arguments that can be updated in \code{\link{make.traits}}, namely \code{process}, \code{process.args} and \code{trait.names}.
#'      }
#' 
#' \item modifications for the target \code{"modifiers"}
#'      \itemize{
#'          \item \code{update.modifiers}: this function updates a \code{"dads"} \code{"modifiers"} object. This function takes as arguments any arguments that can be updated in \code{\link{make.modifiers}}, namely \code{branch.length}, \code{selection}, \code{speciation}, \code{condition} and \code{modify}.
#'      }
#' }
#' 
#' More details about the \code{events} functions is explained in the \code{dads} manual: \url{http://tguillerme.github.io/dads}.
#' 
#' @examples
#' 
#' @seealso \code{make.events}
#' 
#' @author Thomas Guillerme

## The list of conditions
modification <- function(x) {
    cat("List of inbuilt modification functions in dads:\n")
    cat("For the taxa target:\n")
    cat("   ?mass.extinction\n")
    cat("   ?trait.extinction\n")
    cat("For the bd.params target:\n")
    cat("   ?update.bd.params\n")
    cat("For the traits target:\n")
    cat("   ?update.traits\n")
    cat("For the modifiers target:\n")
    cat("   ?update.modifiers\n")
    cat("For the founding targer:\n")
    cat("   ?founding.event\n")
    return(invisible())
} 

## Random mass extinction modification
random.extinction <- function(x) {
    ## The function prototype
    extinction.variable <- function(bd.params, lineage, trait.values) {
        ## Set the variable
        extinction_strength <- NULL

        ## Select a portion of the living species to go extinct
        extinct <- sample(lineage$n, round(lineage$n * extinction_strength))

        ## Update the lineage object
        lineage$livings <- lineage$livings[-extinct]
        lineage$n       <- lineage$n - length(extinct)
        return(lineage)
    }

    ## Editing the extinction strength
    body(extinction.variable)[[2]][[3]] <- eval(substitute(x))
    return(extinction.variable)
}

## Mass extinction based on traits modification
trait.extinction <- function(x, condition = `<`, trait = 1) {

    ## Function for extinction trait
    extinction.trait <- function(bd.params, lineage, trait.values) {
        ## Set the variable and the selector
        trait_limit <- NULL
        selector    <- NULL
        which_trait <- NULL

        ## Select the nodes be traits
        parent_traits <- parent.traits(trait.values, lineage, current = FALSE)
        selected_nodes <- as.numeric(names(which(selector(parent_traits[, which_trait], trait_limit))))

        ## Select the descendants that'll go extinct
        extinct <- which(lineage$parents %in% selected_nodes)

        ## Update the lineage object
        lineage$livings <- lineage$livings[!lineage$livings %in% extinct]
        lineage$n       <- length(lineage$livings)
        return(lineage)
    }

    ## Editing the extinction trait
    body(extinction.trait)[[2]][[3]] <- eval(substitute(x))
    body(extinction.trait)[[3]][[3]] <- eval(substitute(condition))
    body(extinction.trait)[[4]][[3]] <- eval(substitute(trait))
    return(extinction.trait)
}

## Updating the bd.params
update.bd.params <- function(x, parameter) {
    change.bd.param <- function(bd.params, lineage, trait.values) {
        ## Change the death parameter
        value <- NULL
        parameter  <- NULL
        bd.params[parameter] <- value
        return(bd.params)
    }

    ## Editing the update bd.params function
    body(change.bd.param)[[2]][[3]] <- eval(substitute(x))
    body(change.bd.param)[[3]][[3]] <- eval(substitute(parameter))
    return(change.bd.param)
}

## Updating a traits object
update.traits <- function(x, process = NULL, process.args = NULL, trait.names = NULL) {

    change.traits <- function(traits, bd.params, lineage, trait.values) {
        ## Setting the varianbes
        up_process <- NULL
        up_args    <- NULL
        up_names   <- NULL

        return(make.traits(update       = traits,
                           process      = up_process,
                           process.args = up_args,
                           trait.names  = up_names))
    }

    if(!is.null(process)) {
        body(change.traits)[[2]][[3]] <- eval(substitute(process))
    }
    if(!is.null(process.args)) {
        body(change.traits)[[3]][[3]] <- eval(substitute(process.args))
    }
    if(!is.null(trait.names)) {
        body(change.traits)[[4]][[3]] <- eval(substitute(trait.names))
    }
    return(change.traits)
}

## Updating a modifiers object
update.modifiers <- function(x, branch.length = NULL, selection = NULL, speciation = NULL, condition = NULL, modify = NULL) {

    change.modifiers <- function(modifiers, bd.params, lineage, trait.values) {
        ## Setting the varianbes
        up_branch.length <- NULL
        up_selection     <- NULL
        up_speciation    <- NULL
        up_condition     <- NULL
        up_modify        <- NULL

        return(make.modifiers(update        = modifiers,
                              branch.length = up_branch.length,
                              selection     = up_selection,
                              speciation    = up_speciation,
                              condition     = up_condition,
                              modify        = up_modify))
    }

    if(!is.null(branch.length)) {
        body(change.modifiers)[[2]][[3]] <- branch.length
    }
    if(!is.null(selection)) {
        body(change.modifiers)[[3]][[3]] <- selection
    }
    if(!is.null(speciation)) {
        body(change.modifiers)[[4]][[3]] <- speciation
    }
    if(!is.null(condition)) {
        body(change.modifiers)[[5]][[3]] <- condition
    }
    if(!is.null(modify)) {
        body(change.modifiers)[[6]][[3]] <- modify
    }
    return(change.modifiers)
}

## Founding events
founding.event <- function(x, bd.params = NULL, traits = NULL, modifiers = NULL, events = NULL) {

    ## founding events
    founding.fun <- function(stop.rule, time, lineage) {

        bd.params <- NULL
        traits    <- NULL
        modifiers <- NULL
        events    <- NULL

        ## Update the stop rule
        stop_rule_updated <- stop.rule
        if(stop_rule_updated$max.time != Inf) {
            stop_rule_updated$max.time <- stop_rule_updated$max.time - time
        }
        if(stop_rule_updated$max.living != Inf) {
            stop_rule_updated$max.living <- stop_rule_updated$max.living - lineage$n
        }
        if(stop_rule_updated$max.taxa != Inf) {
            stop_rule_updated$max.living <- stop_rule_updated$max.taxa - sum(!lineage$split)
        }

        ## Run the founding event
        return(birth.death.tree.traits(stop.rule = stop_rule_updated, bd.params, traits, modifiers, events, check.results = FALSE))
    }

    if(!is.null(bd.params)) {
        body(founding.fun)[[2]][[3]] <- bd.params
    }
    if(!is.null(traits)) {
        body(founding.fun)[[3]][[3]] <- traits
    }
    if(!is.null(modifiers)) {
        body(founding.fun)[[4]][[3]] <- modifiers
    }
    if(!is.null(events)) {
        body(founding.fun)[[5]][[3]] <- events
    }

    return(founding.fun)
}