#' @name events.conditions
#' @aliases condition, time.condition, taxa.condition, trait.condition
#' @title events.conditions
#'
#' @description Inbuilt conditions functions for helping designing events
#'
#' @usage condition(x, condition, ...)
#' 
#' @param x         the variable to reach for satisfying a condition (see details)
#' @param condition the logical function for triggering the condition (e.g. `<`, `==`, `!>`, etc...).
#' @param ...       any optional argument specific for that condition (see details)
#' 
#' @details
#' The following functions allow to design specific conditions for events:
#' 
#' \itemize{
#'      \item{\code{time.condition}}: a conditional function based on the time \code{x}. Typically this can be translated into "when time reaches the value x, trigger a condition" (see \code{\link{make.events}}). There is no optional argument for the function.
#' 
#'      \item{\code{taxa.condition}}: a conditional function based on the number of taxa \code{x}. Typically this can be  translated into "when the number of taxa reaches the value x, trigger a condition" (see \code{\link{make.events}}). This function has one optional argument:
#'      \itemize{
#'          \item{living}, a \code{logical} argument whether to consider the number of taxa alive when the condition is checked (default: \code{living = TRUE}) or whether to consider all the taxa simulated so far (\code{living = FALSe}).
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
#' More details about the \code{events} functions is explained in the \code{dads} manual: \url{http://tguillerme.github.io/dads}.
#' 
#' @examples
#' 
#' @seealso \code{make.events}
#' 
#' @author Thomas Guillerme

## The list of conditions
condition <- function(x, condition) {
    cat("List of inbuilt condition functions in dads:\n")
    cat("   ?time.condition\n")
    cat("   ?taxa.condition\n")
    cat("   ?trait.condition\n")
    return(invisible())
} 

## A condition based on a specific time value \code{x}
time.condition <- function(x, condition = `>`) {
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
        return(function(bd.params, lineage, trait.values, time) {return(condition(abs(what(trait.values[, trait])), x))})
    } else {
        return(function(bd.params, lineage, trait.values, time) {return(condition(what(trait.values[, trait]), x))})
    }
}