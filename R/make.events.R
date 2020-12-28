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

make.events <- function(target, condition, modification, add, test = TRUE, event.name, replications = 0) {

    ## Test target
    allowed_targets <- c("taxa", "bd.params", "traits", "modifiers")
    check.method(target, allowed_targets, "target argument ")

    ## Test condition
    condition <- check.args.events(condition, fun_name = "condition", condition = TRUE)

    ## Test modification
    modification <- check.args.events(modification, fun_name = "modification")

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

    ## Creating the events object
    if(!do_add) {
        events <- list(target       = target,
                       trigger      = trigger,
                       condition    = condition,
                       modification = modification)
    }

    ## Testing the event object
    if(test) {
        check.events(events)
    }

    class(output) <- c("dads", "events")
    return(output)
}

# ## Change in the traits:
# trait.event <- list(
#     when = ...,
#     what = ...,
#     event = ...)
# #TG: for this one, just change traits$process or traits$cor when the event occurs
