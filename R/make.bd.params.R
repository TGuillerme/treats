#' @title Make birth death parameters
#'
#' @description Making bd.params objects for treats.
#'
#' @param speciation The speciation parameter. Can be a single \code{numeric} value, a \code{numeric} vector or a \code{function} (default is \code{1}).
#' @param extinction The extinction parameter. Can be a single \code{numeric} value, a \code{numeric} vector or a \code{function} (default is \code{0}).
#' @param absolute Logical, whether always return absolute values (\code{TRUE}) or not (\code{FALSE}; default).
#' @param joint Logical, whether to estimate both birth and death parameter jointly with speciation > extinction (\code{TRUE}) or not (\code{FALSE}; default).
#' @param speciation.args If \code{speciation} is a function, any additional arguments to passed to the \code{speciation} function.
#' @param extinction.args If \code{speciation} is a function, any additional arguments to passed to the \code{speciation} function.
#' @param test Logical whether to test if the bd.params object will work (default is \code{TRUE}).
#' @param update Optional, another previous \code{"treats"} \code{"bd.params"} object to update (see details).

#' @details
#' When using \code{update}, the provided arguments (to \code{make.bd.params}) will be the ones updated in the \code{"bd.params"} object.

#' @return
#' This function outputs a \code{treats} object that is a named list of elements handled internally by the \code{\link{treats}} function.
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
#' @seealso \code{\link{treats}}
#' 
#' @author Thomas Guillerme
#' @export

make.bd.params <- function(speciation = NULL, extinction = NULL, joint = NULL, absolute = NULL, speciation.args = NULL, extinction.args = NULL, test = TRUE, update = NULL) {

    match_call <- match.call()

    ## Update
    if(is.null(update)) {
        do_update <- FALSE
    } else {
        if(!is(update, "treats") || !is(update, "bd.params")) {
            stop(paste0("This function can only update \"treats\", \"bd.params\" objects. The current object to update is of class: ", paste(class(update), collapse = ", "), "."), call. = FALSE)
        } else {
            do_update <- TRUE
        }
    }

    ## Default parameters
    if(!do_update) {
        if(is.null(speciation)) {
            speciation <- 1
        }
        if(is.null(extinction)) {
            extinction <- 0
        }
        if(is.null(joint)) {
            joint <- FALSE
        }
        if(is.null(absolute)) {
            absolute <- FALSE
        }
        do_joint <- do_absolute <- do_extinction <- do_speciation <- TRUE
    } else {
        do_joint <- do_absolute <- do_extinction <- do_speciation <- FALSE
        if(!is.null(speciation)) {
            do_speciation <- TRUE
        }
        if(!is.null(extinction)) {
            do_extinction <- TRUE
        }
        if(!is.null(joint)) {
            do_joint <- TRUE
        }
        if(is.null(absolute)) {
            do_absolute <- TRUE
        }
    }

    ## Get the input classes
    if(do_speciation) {
        spec_class <- check.class(speciation, c("integer", "numeric", "function"))
    } 
    if(do_extinction) {
        exti_class <- check.class(extinction, c("integer", "numeric", "function"))
    }

    
    ## Toggle the joint argument
    if(!do_update) {
        bd_params <- list(joint = joint, absolute = absolute)
    } else {
        ## Update the joints or absolute
        bd_params <- update
        if(!is.null(joint)) {
            bd_params$joint <- joint
        }
        if(!is.null(absolute)) {
            bd_params$absolute <- absolute
        }
    }

    ## Default function args
    args_base <- list(n = 1)

    ## Get the speciation argument
    if(do_speciation) {
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
                ## Update the call to speciation value
                match_call$speciation <- speciation
            } else {
                bd_params$speciation <- function() {
                    return(sample(x = speciation, size = 1))
                }
            }
        }
    }
    ## Get the extinction argument
    if(do_extinction) {
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
                ## Update the call to extinction value
                match_call$extinction <- extinction
            } else {
                bd_params$extinction <- function() {
                    return(sample(x = extinction, size = 1))
                }
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
    if(!do_update) {
        bd_params$call <- list("speciation" = match_call$speciation, "extinction" = match_call$extinction, "speciation.args" = match_call$speciation.args, "extinction.args" = match_call$extinction.args)
    } else {
        ## Only update the relevant call bits
        if(do_speciation) {
            bd_params$call$speciation <- match_call$speciation
            if(!is.null(speciation.args)) {
                bd_params$call$speciation.args <- match_call$speciation.args
            }
        }
        if(do_extinction) {
            bd_params$call$extinction <- match_call$extinction
            if(!is.null(extinction.args)) {
                bd_params$call$extinction.args <- match_call$extinction.args
            }
        }
    }

    ## Add the classes
    class(bd_params) <- c("treats", "bd.params")
    return(bd_params)
}