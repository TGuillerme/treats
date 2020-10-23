#' @title Diversity and disparity simulator
#'
#' @description Simulating diversity and trait disparity
#'
#' @param bd.params  A list of named parameters for the birth death process (see details).
#' @param stop.rule  The rules on when to stop the simulation (see details).
#' @param traits     The dads traits object (see \code{\link{make.traits}}).
#' @param modifiers  The dads modifiers object (see \code{\link{make.modifiers}}).
#' @param events     The dads events object (see \code{\link{make.events}}).
#' @param null.error Logical, whether to return an error when the birth death parameters fails to build a tree (\code{FALSE}; default and highly recommended) or whether to return \code{NULL} (\code{TRUE}). Can also be set to a integer value for the numbers of trials (see details).
#' 
#' @details
#' \code{bd.params} and \code{stop.rule} should me named lists to parametrise the birth-death tree. The names of the current handled parameters for each argument are:
#' 
#' \code{bd.params}: The birth death parameters are the parameters used to simulate the birth death process. The allowed parameters are:
#' \itemize{
#'   \item \code{speciation} The constant speciation rate (sometimes called lambda or birth parameter).
#'   \item \code{extinction} The constant extinction rate (sometimes called mu, death or background extinction parameter).
#' }
#' 
#' \code{stop.rule}: The rule(s) for when to stop the simulation. When multiple rules are given, the simulation stops when any rule is broken. The allowed rules are:
#' \itemize{
#'   \item \code{max.taxa}   The maximum number of taxa to reach (including extinct ones).
#'   \item \code{max.living} The maximum number of living (i.e. non extinct) to reach.
#'   \item \code{max.time}   The maximum amount of phylogenetic time to reach.
#' }
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
#'      traits    = my_traits)
#'
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

dads <- function(bd.params, stop.rule, traits = NULL, modifiers = NULL, events = NULL, null.error = FALSE) {

    ## Sanitizing
    if(missing(bd.params)) {
        ## Default pure birth tree
        bd.params <- list(speciation = 1,
                          extinction = 0)
    } else {
        ## Must be a named list
        check.class(bd.params, "list")
        if(is.null(names(bd.params))) {
            stop("bd.params must be a named list.", call. = FALSE)
        }

        if(is.null(bd.params$speciation)) {
            ## Set default speciation
            bd.params$speciation <- 1
        } else {
            ## Speciation parameter must a single numeric (or integer) between 0 and 1
            if(any(!(c(is(bd.params$speciation, "numeric") ||
                       is(bd.params$speciation, "integer"),
                       length(bd.params$speciation) == 1,
                       !bd.params$speciation <= 0,
                       !bd.params$speciation > 1)))) {
                stop("bd.params$speciation must be a single numeric value in the (0, 1] interval.", call. = FALSE)
            }
        }
        if(is.null(bd.params$extinction)) {
            ## Set default extinction
            bd.params$extinction <- 0
        } else {
            ## Extinction parameter must a single numeric (or integer) between 0 and 1
            if(any(!(c(is(bd.params$extinction, "numeric") ||
                       is(bd.params$extinction, "integer"),
                       length(bd.params$extinction) == 1,
                       !bd.params$extinction < 0,
                       !bd.params$extinction >= 1)))) {
                stop("bd.params$extinction must be a single numeric value in the [0, 1) interval.", call. = FALSE)
            }
        }
    }

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
            check.events(events)
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

    while(is.null(output) || counter <= max.counter) {
        ## Simulating the traits and tree
        output <- birth.death.tree.traits(bd.params, stop.rule, traits = traits, modifiers = modifiers, events = events, null.error = null.error)
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
        ## Adding the traits, modifiers and events to the object
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