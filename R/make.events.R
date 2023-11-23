#' @title make.events
#'
#' @description Making events objects for treats
#'
#' @param target        What to modify, can be \code{"taxa"}, \code{"bd.params"}, \code{"traits"} or \code{"modifiers"} (see details).
#' @param condition     A \code{function} returning a logical to trigger the event (see details).
#' @param modification  A \code{function} bringing the modification to the event (see details).
#' @param add           Another \code{"events"} to object to add this event.
#' @param test          A \code{logical}, whether to test if the events object will work (default is \code{TRUE})
#' @param event.name    Optional, a \code{"character"} string to name the event.
#' @param replications  A numeric or integer value for repeating the event (by default, the event is not repeated: \code{replications = 0}).
#' @param additional.args Optional, a named \code{list} of additional arguments to be used in the event.
#' 
#' @details 
#' 
#' \code{target} is a \code{character} to designate what will be affected by the event. It can be either \code{"taxa"}, \code{"bd.params"}, \code{"traits"} or \code{"modifiers"}. This means that the \code{condition} and \code{modification} functions will target this specific part of the algorithm.
#' 
#' \code{condition} must be a function that returns a \code{logical} value and intakes any of the following arguments: \code{bd.params}, \code{lineage}, \code{traits} and \code{time}. See \code{\link{events.conditions}} for examples.
#'
#' \code{modification} must be a function that intakes a first argument named \code{"x"} an returns any specific type of class that can be handled internally by treats. For example, if \code{target = "bd.params"} the \code{modification} function should typically return an updated \code{bd.params} object (see \code{\link{make.bd.params}}). See \code{\link{events.modifications}} for examples.
#' 
#' 
#' @return
#' This function outputs a \code{treats} object that is a named list of elements handled internally by the \code{\link{treats}} function.
#'
#' @examples
#' ## Generating a mass extinction
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
#' ## Changing the trait process
#' ## The 95% upper quantile value of a distribution
#' upper.95 <- function(x) {
#'     return(quantile(x, prob = 0.95))
#' } 
#' ## Create an event to change the trait process
#' change_process <- make.events(
#'                   target       = "traits",
#'                   ## condition is triggered if(upper.95(x) > 3)
#'                   condition    = trait.condition(3, condition = `>`, what = upper.95),
#'                   modification = traits.update(process = OU.process))
#' 
#' ## Set the simulation parameters
#' bd.params <- list(extinction = 0, speciation = 1)
#' stop.rule <- list(max.time = 6)
#' traits    <- make.traits()
#' 
#' ## Run the simulations
#' set.seed(1)
#' no_change <- treats(bd.params = bd.params,
#'                   stop.rule = stop.rule,
#'                   traits    = traits)
#' set.seed(1)
#' process_change <- treats(bd.params = bd.params,
#'                        stop.rule = stop.rule,
#'                        traits    = traits,
#'                        events    = change_process)
#' ## Plot the results
#' oldpar <- par(mfrow = c(1,2))
#' plot(no_change, ylim = c(-7, 7))
#' plot(process_change, ylim = c(-7, 7))
#' par(oldpar)
#' 
#' @seealso \code{\link{treats}} \code{\link{make.bd.params}} \code{\link{make.traits}} \code{\link{make.modifiers}} \code{\link{events.conditions}} \code{\link{events.modifications}}
#' @author Thomas Guillerme
#' @export

make.events <- function(target, condition, modification, add, test = TRUE, event.name, replications = 0, additional.args) {

    match_call <- match.call()

    ## Test target
    allowed_targets <- c("taxa", "bd.params", "traits", "modifiers", "founding")
    check.method(target, allowed_targets, "target argument")

    ## Test condition
    condition <- check.args(condition, fun_name = "condition", required_args = c("bd.params", "lineage", "trait.values", "time"))

    ## Test modification
    need_args <- switch(target,
        taxa      = c("bd.params", "lineage", "trait.values"),
        bd.params = c("traits", "bd.params", "lineage", "trait.values"),
        traits    = c("traits", "bd.params", "lineage", "trait.values"),
        modifiers = c("modifiers", "bd.params", "lineage", "trait.values"),
        founding  = c("stop.rule", "time", "lineage"))
    modification <- check.args(modification, fun_name = "modification", required_args = need_args)

    ## Check add
    if(missing(add)) {
        do_add <- FALSE
    } else {
        ## Add must be a treats events
        check.class(add, c("treats", "events"))
        do_add <- TRUE
    }

    ## Check replications
    check.class(replications, c("numeric", "integer"))
    trigger <- as.integer(0L - replications)

    ## Check additional.args
    if(!missing(additional.args)) {
        more_args <- TRUE
        check.class(additional.args, "list")
        if(is.null(names(additional.args))) {
            stop("additional.args argument must be a named list.", call. = FALSE)
        }
    } else {
        more_args <- FALSE
    }

    ## Creating the events object
    if(!do_add) {
        events <- list(
                  list(target       = target,
                       trigger      = trigger,
                       condition    = condition,
                       modification = modification,
                       args         = NULL,
                       call         = match_call)
                  )
    } else {
        events <- add
        events[[length(events) + 1]] <-
                  list(target       = target,
                       trigger      = trigger,
                       condition    = condition,
                       modification = modification,
                       args         = NULL,
                       call         = match_call)
    }

    ## Naming the event
    if(!missing(event.name)) {
        names(events)[[length(events)]] <- event.name
    }

    ## Adding additional arguments
    if(more_args) {
        events[[length(events)]]$args <- additional.args
    }

    ## Testing the event object
    if(test) {
        check.events(events[[length(events)]])
    }

    class(events) <- c("treats", "events")
    return(events)
}