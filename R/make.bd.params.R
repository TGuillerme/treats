#' @title Make birth death parameters
#'
#' @description Making bd.params objects for dads.
#'
#' @param speciation The speciation parameter. Can be a single \code{numeric} value, a \code{numeric} vector or a \code{function} (default is \code{1}).
#' @param extinction The extinction parameter. Can be a single \code{numeric} value, a \code{numeric} vector or a \code{function} (default is \code{0}).
#' @param absolute Logical, whether always return absolute values (\code{TRUE}) or not (\code{FALSE}; default).
#' @param joint Logical, whether to estimate both birth and death parameter jointly with speciation > extinction (\code{TRUE}) or not (\code{FALSE}; default).
#' @param speciation.args If \code{speciation} is a function, any additional arguments to passed to the \code{speciation} function.
#' @param extinction.args If \code{speciation} is a function, any additional arguments to passed to the \code{speciation} function.
#' @param test Logical whether to test if the bd.params object will work (default is \code{TRUE}).
#' 
#' @examples
#' ## A default set of birth death parameters
#' make.bd.params()
#' 
#' ## Speciation is randomly picked between 1, 10 and 100
#' ## and extinction is always 2
#' make.bd.params(speciation = c(1,10,100), extinction = 2)
#' 
#' ## Speciation is a normal distribution(with sd = 0.75)
#' ## and extinction is a lognormal distribution always lower than
#' ## speciation (joint). Both are always positive values (absolute)
#' my_bd_params <- make.bd.params(speciation = rnorm,
#'                                speciation.args = list(sd = 0.75),
#'                                extinction = rlnorm,
#'                                joint = TRUE,
#'                                absolute = TRUE)
#' my_bd_params
#' 
#' ## Visualising the distributions
#' plot(my_bd_params)
#' 
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

make.bd.params <- function(speciation = 1, extinction = 0, joint = FALSE, absolute = FALSE, speciation.args = NULL, extinction.args = NULL, test = TRUE) {

    match_call <- match.call()

    ## Get the input classes
    spec_class <- check.class(speciation, c("integer", "numeric", "function"))
    exti_class <- check.class(extinction, c("integer", "numeric", "function"))

    ## Toggle the joint argument
    bd_params <- list(joint = joint, absolute = absolute)

    ## Default function args
    args_base <- list(n = 1)

    ## Get the speciation argument
    if(spec_class == "function") {
        speciation_args <- c(args_base, speciation.args)
        bd_params$speciation <- function() {
            do.call(speciation, speciation_args)
        }
    } else {
        if(length(speciation) == 1) {
            bd_params$speciation <- function() {
                return(speciation)
            }            
        } else {
            bd_params$speciation <- function() {
                return(sample(x = speciation, size = 1))
            }
        }
    }
    ## Get the extinction argument
    if(exti_class == "function") {
        extinction_args <- c(args_base, extinction.args)
        bd_params$extinction <- function() {
            do.call(extinction, extinction_args)
        }
    } else {
        if(length(extinction) == 1) {
            bd_params$extinction <- function() {
                return(extinction)
            }            
        } else {
            bd_params$extinction <- function() {
                return(sample(x = extinction, size = 1))
            }
        }
    }

    ## Testing
    if(test) {
        ## Testing if it works
        test <- sample.from(bd_params)
        if(length(test) != 2 || !is(test, "list")) {
            stop("Impossible to sample correct parameters from the bd.params object.", call. = FALSE)
        }
    }

    ## Save the call
    bd_params$call <- list("speciation" = spec_class, "extinction" = exti_class, "all_call" = match_call)

    ## Add the classes
    class(bd_params) <- c("dads", "bd.params")
    return(bd_params)
}