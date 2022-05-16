#' @title Diversity and disparity simulator
#'
#' @description Simulating diversity and trait disparity
#'
#' @param stop.rule  The rules on when to stop the simulation (see details).
#' @param bd.params  A \code{"dads"} \code{"bd.params"} object or a named list of parameters for the birth death process (see details).
#' @param traits     A \code{"dads"} \code{"traits"} object (see \code{\link{make.traits}}).
#' @param modifiers  A \code{"dads"} \code{"modifiers"} object (see \code{\link{make.modifiers}}).
#' @param events     A \code{"dads"} \code{"events"} object (see \code{\link{make.events}}).
#' @param null.error Logical, whether to return an error when the birth death parameters fails to build a tree (\code{FALSE}; default and highly recommended) or whether to return \code{NULL} (\code{TRUE}). Can also be set to a integer value for the numbers of trials (see details).
#' 
#' @details
#' \code{stop.rule}: The rule(s) for when to stop the simulation. When multiple rules are given, the simulation stops when any rule is broken. The allowed rules are:
#' \itemize{
#'   \item \code{max.taxa}   The maximum number of taxa to reach (including extinct ones).
#'   \item \code{max.living} The maximum number of living (i.e. non extinct) to reach.
#'   \item \code{max.time}   The maximum amount of phylogenetic time to reach.
#' }
#' 
#' \code{bd.params}: This can be either a \code{"dads"} \code{"bd.params"} object (see \code{\link{make.bd.params}}) or a list of named parameters. The allowed parameters are:
#' \itemize{
#'   \item \code{speciation} The speciation parameter value.
#'   \item \code{extinction} The extinction parameter value.
#' }
#' By default, this parameter is set to \code{bd.params = list(speciation = 1)}
#' 
#' If \code{null.error} is set to a numeric value, the function will run multiple times until a correct tree is generated. Using this option can greatly increase computational time!
#' 
#' 
#' @examples
#' ## Setting some pure birth tree (no extinction) parameters
#' my_bd_params <- list(speciation = 1)
#' ## Setting some stopping rule (stop when reaching 10 taxa)
#' my_stop_rule <- list(max.taxa = 10) 
#' 
#' ## Run a birth tree without traits
#' a_tree <- dads(bd.params = my_bd_params,
#'                stop.rule = my_stop_rule)
#' ## Plotting the results
#' plot(a_tree)
#' 
#' ## Adding some extinction parameter
#' my_bd_params$extinction <- 1/3
#' 
#' ## Getting a simple trait (default Brownian motion)
#' my_trait <- make.traits()
#' 
#' ## Run a birth-death tree with traits
#' dads(bd.params = my_bd_params,
#'      stop.rule = my_stop_rule,
#'      traits    = my_trait)
#' 
#' ## TODO: more examples
#'
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

dads <- function(stop.rule, bd.params, traits = NULL, modifiers = NULL, events = NULL, null.error = FALSE) {

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

    ## bd.params
    if(missing(bd.params)) {
        ## Default pure birth tree
        bd.params <- make.bd.params()
    } else {

        error_msg <- "must be a named list of arguments or a \"dads\" \"bd.params\" object. You can use make.bd.params() to format the object correctly."

        if(!(is(bd.params, "dads") && is(bd.params, "bd.params"))) {

            ## Must be a named list
            check.class(bd.params, "list", msg = paste0(" ", error_msg))
            if(is.null(names(bd.params))) {
                stop(paste0("bd.params ", error_msg), call. = FALSE)
            }

            ## Set the speciation
            if(is.null(bd.params$speciation)) {
                make_spec <- 1
            } else {
                make_spec <- bd.params$speciation
            }
            ## Set the extinction
            if(is.null(bd.params$extinction)) {
                make_exti <- 0
            } else {
                make_exti <- bd.params$extinction
            }

            ## Create the bd.params object
            bd.params <- make.bd.params(speciation = make_spec, extinction = make_exti)
        }
    }

    ## traits
    if(!is.null(traits)) {
        if(is(traits, "dads") && is(traits, "traits")) {
            check.traits(traits)
        } else {
            stop("traits must be of class \"dads\" \"traits\". Use make.traits() to format the object correctly.")
        }
    }

    ## modifiers
    if(!is.null(modifiers)) {
        if(is(modifiers, "dads") && is(modifiers, "modifiers")) {
            check.modifiers(modifiers)
        } else {
            stop("modifiers must be of class \"dads\" \"modifiers\". Use make.modifiers() to format the object correctly.")
        }
    }

    ## events
    if(!is.null(events)) {
        if(is(events, "dads") && is(events, "events")) {
            lapply(events, check.events)
        } else {
            stop("events must be of class \"dads\" \"events\". Use make.events() to format the object correctly.")
        }
    }

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

    while(is.null(output) || counter < max.counter) {
        ## Simulating the traits and tree
        output <- birth.death.tree.traits(stop.rule, bd.params = bd.params, traits = traits, modifiers = modifiers, events = events, null.error = null.error)

        ## Update the counter
        counter <- counter + 1
    }

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
        class(output) <- "dads"
    }
    return(output)
}