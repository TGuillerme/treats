#' @title make.modifiers
#'
#' @description Making modifiers objects for dads based on an ancestor's (parent) trait.
#'
#' @param what       What to modify: can be either \code{"waiting"} or \code{"speciating"}.
#' @param when       A logical function of how to apply the modification (see details).
#' @param how        A function of what to return when the condition is met (see details).
#' @param speciating A function for the speciating events (ignores all other arguments but \code{add} and \code{test}).
#' @param waiting    A function for the waiting events (ignores all other arguments but \code{add} and \code{test})..
#' @param add        Whether to this modifier to a \code{"dads"} \code{"modifier"} object
#' @param test       Logical whether to test if the modifiers object will work (default is TRUE)
#' 
#' @details
#' 
#' \code{when} must be a function that inputs at least the parent trait value(s) as \code{x} and should return a logical value. For example, to use the condition "If the parent's first trait value is negative, do \code{how} (i.e. return \code{TRUE}):
#' 
#'     \code{when = function(x) return(x < 0)} 
#' 
#' \code{how} must be a function that inputs at least the parent trait value(s) as \code{x} and should return the value associated to \code{what} (i.e. a \code{numeric} value for \code{"waiting"} or a \code{logical} value for \code{"speciating"}. For example, to always allow speciation if \code{when} is \code{TRUE}:
#' 
#'      \code{how = function(x) return(TRUE)}
#' 
#' NOTE: modifiers only affect the birth death process after the first speciation event.
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

make.modifiers <- function(what, when, how, speciating = NULL, waiting = NULL, add, test = TRUE) {

    ## add
    add_modifiers <- FALSE
    if(!missing(add)) {
        if(!(is(add, "dads") && is(add, "modifiers"))) {
            stop("modifiers can only be added to objects of class dads and modifiers.")
        }
        add_modifiers <- TRUE
    }

    ## test
    check.class(test, "logical")

    ## Speciating
    if(!is.null(speciating)) {
        ## Check the input
        check.class(speciating, "function")
        modifiers <- list("speciating" = speciating)

        ## Add to previous (if needed)
        if(add_modifiers) {
            add$speciating <- modifiers$speciating
            modifiers <- add
        }

        ## Check object
        if(test) {
            check.modifiers(modifiers)
        }
        return(modifiers)
    }

    ## Waiting
    if(!is.null(waiting)) {
        ## Check the input
        check.class(waiting, "function")
        modifiers <- list("waiting" = waiting)

        ## Add to previous (if needed)
        if(add_modifiers) {
            add$waiting <- modifiers$waiting
            modifiers <- add
        }

        ## Check object
        if(test) {
            check.modifiers(modifiers)
        }
        return(modifiers)
    }

    ## Else go to the normal modifiers
    allowed_modifiers <- c("waiting", "speciating")
    check.method(what, allowed_modifiers, "what must be")



    output <- 42
    class(output) <- c("dads", "modifiers")
    return(output)
}


