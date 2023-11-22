#' @title Diversity and disparity simulator
#'
#' @description Simulating phylogenetic trees and traits. See full manual here: https://github.com/TGuillerme/treats
#'
#' @param stop.rule  The rules on when to stop the simulation (see details).
#' @param bd.params  A \code{"bd.params"} object or a named list of parameters for the birth-death process (see details or \code{\link{make.bd.params}}).
#' @param traits     A \code{"traits"} object (see \code{\link{make.traits}}).
#' @param modifiers  A \code{"modifiers"} object (see \code{\link{make.modifiers}}).
#' @param events     A \code{"events"} object (see \code{\link{make.events}}).
#' @param save.steps Optional, \code{"numeric"} value to save the simulations at specific internal points (this can slow down the algorithm significantly for large trees). 
#' @param null.error Logical, whether to return an error when the birth-death parameters fails to build a tree (\code{FALSE}; default and highly recommended) or whether to return \code{NULL} (\code{TRUE}). Can also be set to an integer value for the numbers of trials (see details).
#' @param replicates Optional, the number of replicates for the simulation.
#' @param verbose Logical, whether to be verbose (\code{TRUE}; default) or not (\code{FALSE}). 
#'
#' @details
#' \code{stop.rule}: The rule(s) for when to stop the simulation. When multiple rules are given, the simulation stops when any rule is broken. The allowed rules are:
#' \itemize{
#'   \item \code{max.taxa}   The maximum number of taxa (including extinct ones).
#'   \item \code{max.living} The maximum number of living taxa (i.e. non extinct).
#'   \item \code{max.time}   The maximum amount of phylogenetic (in arbitrary units).
#' }
#' 
#' \code{bd.params}: This can be either a \code{"treats"} \code{"bd.params"} object (see \code{\link{make.bd.params}}) or a list of named parameters. The allowed parameters are:
#' \itemize{
#'   \item \code{speciation} The speciation parameter value.
#'   \item \code{extinction} The extinction parameter value.
#' }
#' By default, this parameter is set to \code{bd.params = list(speciation = 1)}
#' 
#' If \code{null.error} is set to a numeric value, the function will run multiple times until a correct tree is generated. Using this option can greatly increase computational time!
#' 
#' @return
#' This function outputs either a \code{"phylo"} object if no traits where generated or a \code{treats} object that is a list of at least two elements: \code{$tree}, a \code{"phylo"} object and \code{$data}, a \code{"matrix"} of the trait values.
#'
#' @examples
#' ## Setting pure birth tree (no extinction) parameters
#' my_bd_params <- list(speciation = 1)
#' ## Setting a stopping rule: stop when reaching 10 taxa.
#' my_stop_rule <- list(max.taxa = 10) 
#' 
#' ## Run a birth tree without traits
#' a_tree <- treats(bd.params = my_bd_params,
#'                stop.rule = my_stop_rule)
#' ## Plot the results
#' plot(a_tree)
#' 
#' ## Add an extinction parameter
#' my_bd_params$extinction <- 1/3
#' 
#' ## Add a simple trait simulation (default Brownian motion)
#' my_trait <- make.traits()
#' 
#' ## Run a birth-death tree with traits simulation
#' treats(bd.params = my_bd_params,
#'      stop.rule = my_stop_rule,
#'      traits    = my_trait)
#' 
#' ## Simulating a tree using modifiers
#' ## Making a modifier to make speciation trait dependent
#' my_modifiers <- make.modifiers(branch.length = branch.length.trait,
#'                                selection     = selection,
#'                                speciation    = speciation.trait)
#'
#' ## Simulating the tree
#' treats(stop.rule = list(max.taxa = 20),
#'      traits = make.traits(),
#'      modifiers = my_modifiers)
#'
#' ## Run a birth death tree with an event
#' ## 80% mass extinction at time 4
#' mass_extinction <- make.events(
#'                       target       = "taxa",
#'                       condition    = age.condition(4),
#'                       modification = random.extinction(0.8))
#' 
#' ## Set the simulation parameters
#' stop.rule <- list(max.time = 5)
#' bd.params <- list(extinction = 0, speciation = 1)
#' 
#' ## Run the simulations
#' set.seed(123)
#' results <- treats(bd.params = bd.params,
#'                 stop.rule = stop.rule,
#'                 events    = mass_extinction)
#' ## Plot the results
#' plot(results, show.tip.label = FALSE)
#' axisPhylo()
#'
#'
#' @seealso \code{\link{plot.treats}} \code{\link{make.traits}} \code{\link{make.modifiers}} \code{\link{make.events}}
#' 
#' @author Thomas Guillerme
#' @export

treats <- function(stop.rule, bd.params, traits = NULL, modifiers = NULL, events = NULL, save.steps = NULL, null.error = FALSE, replicates, verbose = TRUE) {

    ## verbose
    check.class(verbose, "logical")

    ## Replicates
    if(!missing(replicates)) {
        check.class(replicates, c("integer", "numeric"))
        outputs <- replicate(replicates, treats(stop.rule  = stop.rule,
                                            bd.params  = bd.params,
                                            traits     = traits,
                                            modifiers  = modifiers,
                                            events     = events,
                                            save.steps = save.steps,
                                            null.error = null.error,
                                            verbose    = verbose),
                        simplify = FALSE)
        ## If outputs are treats
        if(is(outputs[[1]], "treats")) {
            class(outputs) <- "treats"
        } else {
            class(outputs) <- "multiPhylo"
        }
        return(outputs)
    }

    ## Sanitizing
    ## stop.rule
    if(missing(stop.rule)) {
        stop("You must provide at least one stopping rule. For example:\nstop.rule <- list(max.taxa   = 10,\n                  max.living = 10,\n                  max.time   = 10)")
    } else {
        check.class(stop.rule, "list")
        if(is.null(names(stop.rule))) {
            stop("stop.rule must be a named list of stopping rules. For example:\nstop.rule <- list(max.taxa   = 10,\n                  max.living = 10,\n                  max.time   = 10)")
        }
        if(is.null(stop.rule$max.taxa) && is.null(stop.rule$max.living) && is.null(stop.rule$max.time)) {
            stop("You must provide at least one stopping rule. For example:\nstop.rule <- list(max.taxa   = 10,\n                  max.living = 10,\n                  max.time   = 10)")
        }
    }
    ## Set the missing stopping rules to infinite
    stop.rule$max.taxa    <- ifelse(is.null(stop.rule$max.taxa),   Inf, stop.rule$max.taxa)
    stop.rule$max.living  <- ifelse(is.null(stop.rule$max.living), Inf, stop.rule$max.living)
    stop.rule$max.time    <- ifelse(is.null(stop.rule$max.time),   Inf, stop.rule$max.time)

    ## Warning message for stop rule

    ## bd.params
    if(missing(bd.params)) {
        ## Default pure birth tree
        bd.params <- make.bd.params()
    } else {
        error_msg <- "must be a named list of arguments or a \"treats\" \"bd.params\" object. You can use make.bd.params() to format the object correctly."

        if(!(is(bd.params, "treats") && is(bd.params, "bd.params"))) {

            ## Must be a named list
            check.class(bd.params, "list", msg = paste0(" ", error_msg))
            if(is.null(names(bd.params))) {
                stop(paste0("bd.params ", error_msg), call. = FALSE)
            }

            ## Create the bd.params object
            bd.params <- make.bd.params(speciation = ifelse(is.null(bd.params$speciation), 1, bd.params$speciation),
                                        extinction = ifelse(is.null(bd.params$extinction), 0, bd.params$extinction))
        }
    }

    if(stop.rule$max.time != Inf && stop.rule$max.time > 6 && bd.params$speciation() > 0.5) {
        warning_message <- "Stop rule was step higher than 6 time units.\nThis can take a considerable amount of time and RAM to simulate"
        additional_message <- ifelse((stop.rule$max.taxa == Inf && stop.rule$max.living == Inf), " if no other stop rules are given", "")
        message(paste0(warning_message, additional_message, "."))
    }

    ## traits
    if(!is.null(traits)) {
        if(is(traits, "treats") && is(traits, "traits")) {
            check.traits(traits)
        } else {
            stop("traits must be of class \"treats\" \"traits\". Use make.traits() to format the object correctly.")
        }
    }

    ## modifiers
    if(!is.null(modifiers)) {
        if(is(modifiers, "treats") && is(modifiers, "modifiers")) {
            check.modifiers(modifiers)
        } else {
            stop("modifiers must be of class \"treats\" \"modifiers\". Use make.modifiers() to format the object correctly.")
        }
    }

    ## events
    if(!is.null(events)) {
        if(is(events, "treats") && is(events, "events")) {
            lapply(events, check.events)
        } else {
            stop("events must be of class \"treats\" \"events\". Use make.events() to format the object correctly.")
        }
    }

    ## Check null error
    error_class <- check.class(null.error, c("logical", "integer", "numeric"))
    if(error_class == "logical") {
        max.counter <- 1
        null.error <- null.error
    } else {
        max.counter <- round(null.error)
        null.error <- TRUE
    }
    counter <- 0
    output <- NULL

    ## Check save steps
    if(!is.null(save.steps)) {
        check.class(save.steps, "numeric")    
    }

    if(max.counter > 1 && verbose) message("Building the tree:", appendLF = FALSE)
    while(is.null(output) && counter < max.counter) {
        ## Simulating the traits and tree
        output <- birth.death.tree.traits(stop.rule, bd.params = bd.params, traits = traits, modifiers = modifiers, events = events, save.steps = save.steps, null.error = null.error)

        ## Update the counter
        counter <- counter + 1
        ## 
        if(max.counter > 1 && verbose) message(".", appendLF = FALSE)
    }
    if(max.counter > 1 && verbose) message("Done.")

    if(is.null(output)) {
        ## Should only fire if null.error = TRUE
        return(output)
    }

    if(is.null(output$data)) {
        output <- output$tree
    } else {
        ## Adding the bd.params, modifiers and events to the object
        if(!is.null(bd.params)) {
            output$bd.params <- bd.params
        }
        if(!is.null(traits)) {
            output$traits <- traits
        }
        if(!is.null(modifiers)) {
            output$modifiers <- modifiers
        }
        if(!is.null(events)) {
            output$events <- events
        }
        class(output) <- "treats"
    }
    return(output)
}