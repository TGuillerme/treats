#' @title Sample function
#'
#' @description Samples a single numeric value from a function
#'
#' @param fun A function from which to sample (default is \code{\link[stats]{rnorm}}).
#' @param ... Any additional arguments to be passed to fun
#' @param joint Logical, whether to estimate both birth and death parameter jointly with speciation > extinction.
#' 
#' @examples
#' ## Sampling a single value from a normal distribution
#' sample.fun()
#' 
#' ## Plotting a lognormal distribution
#' plot(density(replicate(100, sample.fun(rlnorm))))
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

sample.fun <- function(fun = rnorm, ..., joint = FALSE) {
    ## Set the arguments list
    args <- list(n = 1, ...)

    if(!joint) {
        ## Sampling a single value
        return(do.call(fun, args))
    } else {
        ## Sampling joint values
        first <- do.call(fun, args)
        second <- do.call(fun, args)
        counter <- 0
        ## Resample the second value until it's good
        while(second >= first && counter != 100) {
            second <- do.call(fun, args)
            counter <- counter + 1
        }
        if(counter == 100) {
            stop("Impossible to sample a joint value with the speciation > extinction.")
        }
        return(c("speciation" = first, "extinction" = second))
    }
}
