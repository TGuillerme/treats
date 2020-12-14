#' @title make.events
#'
#' @description Making events objects for dads
#'
#' @param target       What to modify, can be \code{"taxa"}, \code{"bd.params"}, \code{"traits"} or \code{"modifiers"} (see details).
#' @param condition    A function returning a logical to trigger the event (see details).
#' @param modification The modification to bring through the event (see details).
#' @param add   Whether to add this modifier to an \code{"events"} object.
#' @param test  Logical whether to test if the events object will work (default is \code{TRUE})
#' @param replications A numeric or integer value for repeating the event (by default, the event is not repeated: \code{repeat = 0}).
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

make.events <- function(target, condition, modification, add, test = TRUE, replications = 0) {

    ## Test target
    allowed_targets <- c("taxa", "bd.params", "traits", "modifiers")
    check.method(target, allowed_targets, "target argument ")

    ## Test condition
    # condition(bd.params, lineage, trait.values, time)

    ## Check add
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

    ## Creating the events object
    if(!do_add) {
        events <- list(target  = target,
                       trigger = trigger)
    }

    ## Test different modifications
    switch(events$target,
           taxa      = {
                ## Modify the lineage object
                lineage   <- events$modification(bd.params, lineage, trait.values)
           },
           bd.params = {
                ## Modify the birth death parameters
                bd.params <- events$modification(bd.params, lineage, trait.values)
           },
           traits    = {
                ## Modify the traits
                traits    <- events$modification(process, n, start, process.args, trait.names)
           },
           modifiers = {
                ## Modify the modifiers
                modifiers <- events$modification(branch.length, selection, speciation, condition, modify)
           })


                ## Toggle the trigger tracker
                events$trigger <- events$trigger + 1L



    class(output) <- c("dads", "events")
    return(output)
}

# ## Change in the traits:
# trait.event <- list(
#     when = ...,
#     what = ...,
#     event = ...)
# #TG: for this one, just change traits$process or traits$cor when the event occurs
