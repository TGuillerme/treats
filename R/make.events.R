#' @title make.events
#'
#' @description Making events objects for dads
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
#' \code{target} is a \code{character} to designate what will be affected by the event. It can be either \code{"taxa"}, \code{"bd.params"}, \code{"traits"} or \code{"modifiers"}.
#' 
#' \code{condition} must be a function that returns a \code{logical} value and intakes any of the following arguments: \code{bd.params}, \code{lineage}, \code{traits} and \code{time}.
#' 
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

make.events <- function(target, condition, modification, add, test = TRUE, event.name, replications = 0, additional.args) {

    ## Test target
    allowed_targets <- c("taxa", "bd.params", "traits", "modifiers", "founding")
    check.method(target, allowed_targets, "target argument")

    ## Test condition
    condition <- check.args(condition, fun_name = "condition", required_args = c("bd.params", "lineage", "trait.values", "time"))

    ## Test modification
    need_args <- switch(target,
        taxa      = c("bd.params", "lineage", "trait.values"),
        bd.params = c("bd.params", "lineage", "trait.values"),
        traits    = c("traits", "bd.params", "lineage", "trait.values"),
        modifiers = c("modifiers", "bd.params", "lineage", "trait.values"),
        founding  = c("stop.rule", "time", "lineage"))
    modification <- check.args(modification, fun_name = "modification", required_args = need_args)

    ## Check add
    if(missing(add)) {
        do_add <- FALSE
    } else {
        ## Add must be a dads events
        check.class(add, c("dads", "events"))
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
        events <- list(target       = target,
                       trigger      = trigger,
                       condition    = condition,
                       modification = modification,
                       args         = NULL)
    }
    if(more_args) {
        events$args <- additional.args
    }

    ## Testing the event object
    if(test) {
        check.events(events)
    }

    class(events) <- c("dads", "events")
    return(events)
}