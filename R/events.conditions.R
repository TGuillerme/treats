#' @name events.conditions
#' @aliases age.condition taxa.condition trait.condition
#' @title events.conditions
#'
#' @description Inbuilt conditions functions for helping designing events
#'
#' @usage events.condition(x, condition, ...)
#'
#' @param x         the variable to reach for satisfying a condition (see details)
#' @param condition the logical function for triggering the condition (e.g. `<`, `==`, `!>`, etc...).
#' @param ...       any optional argument specific for that condition (see details)
#' 
#' @return
#' This function outputs a \code{"function"} to be passed to \code{\link{make.events}}.
#'
#' @details
#' The following functions allow to design specific conditions for events:
#' 
#' \itemize{
#'      \item{\code{age.condition}}: a conditional function based on the time \code{x}. Typically this can be translated into "when time reaches the value x, trigger a condition" (see \code{\link{make.events}}). There is no optional argument for the function.
#' 
#'      \item{\code{taxa.condition}}: a conditional function based on the number of taxa \code{x}. Typically this can be  translated into "when the number of taxa reaches the value x, trigger a condition" (see \code{\link{make.events}}). This function has one optional argument:
#'      \itemize{
#'          \item{living}, a \code{logical} argument whether to consider the number of taxa alive when the condition is checked (default: \code{living = TRUE}) or whether to consider all the taxa simulated so far (\code{living = FALSE}).
#'      }
#' 
#'      \item{\code{trait.condition}}: a conditional function based on the value \code{x} of one or more traits. Typically this can be  translated into "when a trait reaches a value x, trigger a condition" (see \code{\link{make.events}}). This function has three optional argument:
#'      \itemize{
#'          \item{trait}, one or more \code{integer} or \code{numeric} value designating the trait(s) to consider. By default, \code{trait = 1}, thus considering only the first trait to trigger the condition.
#'          \item{what}, a \code{function} designating what to select from the trait values. By default \code{what = max} to select the maximal value of the trait when the condition is triggered (but you can use any function like \code{\link[base]{min}}, \code{\link[base]{mean}}, \code{\link[stats]{sd}}, etc. or provide your own function).
#'          \item{absolute}, a \code{logical} designating to consider absolute trait values (\code{TRUE}) or not (default; \code{FALSE}).
#'      }
#' }
#' 
#' More details about the \code{events} functions is explained in the \code{treats} manual: \url{http://tguillerme.github.io/treats}.
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
#' @seealso \code{\link{treats}} \code{\link{make.events}} \code{\link{events.modifications}}
#' 
#' @author Thomas Guillerme

## The list of conditions
events.condition <- function(x, condition, ...) {
    message("List of inbuilt condition functions in treats:")
    message("   ?taxa.condition")
    message("   ?age.condition")
    message("   ?trait.condition")
    return(invisible())
} 

## A condition based on a specific time value \code{x}
age.condition <- function(x, condition = `>`) {
    return(function(bd.params, lineage, trait.values, time) {condition(time, x)})
}

## A condition based on a specific number of taxa \code{x} (either living ones (\code{living = TRUE}) or living and fossils (\code{living = FALSE}))
taxa.condition <- function(x, condition = `>`, living = TRUE) {
    if(living) {
        return(function(bd.params, lineage, trait.values, time) {condition(lineage$n, x)})
    } else {
        return(function(bd.params, lineage, trait.values, time) {condition(sum(!lineage$split), x)})
    }
}

## A condition based on a specific trait value \code{x} for a certain trait (default is \code{trait = 1}). This value can be absolute or not (default is \code{absolute = FALSE}).
trait.condition <- function(x, condition = `>`, trait = 1, what = max, absolute = FALSE) {
    if(absolute) {
        abs.fun <- abs
    } else {
        abs.fun <- c
    }
    return(function(bd.params, lineage, trait.values, time) {return(condition(abs.fun(what(trait.values[, trait])), x))})
}
