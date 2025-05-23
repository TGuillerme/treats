#' @name trait.process
#' @aliases BM.process OU.process no.process multi.peak.process repulsion.process discrete.process
#' @title Trait processes
#'
#' @description Different trait processes implemented in treats.
#'
#' @usage trait.process(x0, edge.length, ...)
#'
#' @param x0 The previous state. This can be a single value (unidimensional process) or more (multidimensional processes).
#' @param edge.length The branch length (default must be 1). This is always a single value.
#' @param ... Any optional argument for the specific process (see details).
#' 
#' @return Returns one or more \code{"numeric"} value(s).
#'
#' @details
#' The different trait processes implemented in treats are:
#' 
#' \itemize{
#'      \item{BM.process} A Brownian motion process (uni or multidimensional). This function is based on \code{\link[MASS]{mvrnorm}}.
#'      This process can take following optional arguments:
#'          \itemize{
#'               \item \code{Sigma} a positive-definite symmetric matrix specifying the covariance matrix of the variables (default is \code{diag(length(x0))}).
#'               \item \code{...} any named additional argument to be passed to \code{\link[MASS]{mvrnorm}}.
#'          }
#'
#'      \item{discrete.process}
#'      This process can take following optional arguments:
#'          \itemize{
#'               \item \code{transitions} a positive-definite squared transition matrix. If left missing, a 2 states equal rates matrix is used.
#'          }
#'      Note that for this process, 0 corresponds to state 1, 1 corresponds to state 2, etc... The current version of this process does not allow other discrete traits notation (but future versions will!).
#' 
#'      \item{OU.process} A Ornstein-Uhlenbeck process (uni or multidimensional). This function is based on \code{\link[MASS]{mvrnorm}}.
#'      This process can take following optional arguments:
#'          \itemize{
#'               \item \code{Sigma} the traits variance/covariance (default is \code{diag(length(x0))}).
#'               \item \code{alpha} the alpha parameter (default = is \code{1}).
#'               \item \code{optimum} the theta parameter (default = is \code{0}).
#'               \item \code{...} any named additional argument to be passed to \code{\link[MASS]{mvrnorm}}.
#'          }
#'      
#'      \item{no.process} An non-process unidimensional function. This function generates a trait value not depending on the branch length nor the previous state
#'      This process can take following optional arguments:
#'          \itemize{
#'               \item \code{fun} a random number function (default is \code{\link[stats]{rnorm}}).
#'               \item \code{...} any named additional argument to be passed to \code{fun}.
#'          }
#' 
#'      \item{multi.peak.process} A Ornstein-Uhlenbeck process (uni or multidimensional) with multiple optimal values. This function is based on \code{\link[MASS]{mvrnorm}}.
#'      This process can take following optional arguments:
#'          \itemize{
#'               \item \code{Sigma} the traits variance/covariance (default is \code{diag(length(x0))}).
#'               \item \code{alpha} the alpha parameter (default = is \code{1}).
#'               \item \code{peaks} the multiple optimal values to be attracted to (default = is \code{0}). This can be a \code{numeric} vector to be applied to all the values of \code{x0} or a \code{list} of the same length as \code{x0} for different multiple optimums for each \code{x0}.
#'               \item \code{...} any named additional argument to be passed to \code{\link[MASS]{mvrnorm}}.
#'          }
#' 
#'      \item{repulsion.process} An unidimensional Brownian Motion process that generates a trait value not overlapping with the other living taxa ancestral values. This function is based on \code{\link[stats]{rnorm}}.
#'      This process can take following optional arguments:
#'          \itemize{
#'               \item \code{sd} the normal distribution standard deviation.
#'               \item \code{repulsion} the minimal distance requested between trait values.
#'               \item \code{max.try} the maximum number of values to draw (if the repulsion value is to hard to achieve).
#'               \item \code{trait.values} LEAVE AS \code{NULL} (it designates the trait value table from the birth death process and is handled internally by \code{\link{treats}}).
#'               \item \code{lineage} LEAVE AS \code{NULL} (it designates the lineage object from the birth death process and is handled internally by \code{\link{treats}}).
#'               \item \code{trait} LEAVE AS \code{NULL} (it which trait to use and is analysed an is handled internally by \code{\link{treats}}).
#'          }
#' }
#' 
#' More details about the \code{trait.process} functions is explained in the \code{treats} manual: \url{http://tguillerme.github.io/treats}.
#' 
#' @examples
#' ## NOTE: You can visualise most process by making them
#' ## into a "treats" "traits" object using make.traits():
#'
#' ## The Brownian motion process
#' BM.process(x0 = 0)
#' plot(make.traits(process = BM.process))
#' ## A covariance matrix between 3 traits
#' varcovar_matrix <- matrix(c(1/3,1/3,1/3,1/3,2/3,0,1/3,0,2/3), ncol = 3)
#' BM.process(x0 = c(0,0,0), Sigma = varcovar_matrix)
#' 
#' ## The Ornstein-Uhlenbeck process
#' OU.process(x0 = 0)
#' plot(make.traits(process = OU.process))
#' 
#' ## No process
#' no.process()
#' plot(make.traits(process = no.process))
#'
#' ## Multi peaks with peaks at the values 1, 5 and 10
#' multi.peak.process(peaks = c(1, 5, 10))
#' plot(make.traits(multi.peak.process, process.args = list(peaks = c(1, 5, 10))))
#' 
#' ## Repulsion process
#' repulsion.process(x0 = 0, repulsion = 1)
#' plot(make.traits(repulsion.process, process.args = list(repulsion = 5)))
#'
#' ## Discrete trait process
#' ## Generating a stepwise transition matrix for 3 states (with an overal random transition rate)
#' stepwise_matrix <- transition.matrix(type = "stepwise", states = 3)
#' ## Generatin and plotting the the trait
#' plot(make.traits(discrete.process, process.args = list(transitions = stepwise_matrix)))
#'
#' ## 
#' 
#' @seealso \code{\link{treats}} \code{\link{make.traits}}
#' 
#' @author Thomas Guillerme
#' @export

trait.process <- function(x0 = 0, edge.length = 1, ...) {
    message("Trait processes implemented in treats:")
    message("?BM.process")
    message("?discrete.process")
    message("?OU.process")
    message("?no.process")
    message("?multi.peak.process")
    message("?repulsion.process")
    message("?conditional.process")
}

## The Brownian motion
BM.process <- function(x0 = 0, edge.length = 1, Sigma = diag(length(x0)), ...) {
    return(t(MASS::mvrnorm(n = 1, mu = x0, Sigma = Sigma * edge.length, ...)))
}

## Discrete traits
discrete.process <- function(x0 = 0, edge.length = 1, transitions = transition.matrix("ER", 2)) {
    # no_dims <- FALSE
    # if(!is.null(states <- rownames(transitions))) {
    #     no_dims <- TRUE
    #     dimnames(transitions) <- NULL
    #     ## Translate x0 to integer
    #     x0 <- match(x0, states)-1
    # }

    # ## Get the trait values
    # traits <- sapply(x0, function(x0, transitions, edge.length) sample(0:(nrow(transitions)-1), size = 1, prob = transitions[round(abs(x0))+1, ] * edge.length), transitions = transitions, edge.length = edge.length)

    return(sapply(x0, function(x0, transitions, edge.length) sample(0:(nrow(transitions)-1), size = 1, prob = transitions[round(abs(x0))+1, ] * edge.length), transitions = transitions, edge.length = edge.length))

    # ## Return the traits (and translate them back)
    # if(no_dims) {
    #     return(traits)
    # } else {
    #     return(states[(traits+1)])
    # }
}

## The OU process
OU.process <- function(x0 = 0, edge.length = 1, Sigma = diag(length(x0)), alpha = 1, optimum = 0, ...) {
    ## Calculate the means
    means <- optimum + (x0 - optimum) * exp(-alpha * edge.length)
    ## Calculate the Sigma
    Sigma <- Sigma/(2 * alpha) * (1 - exp(-2 * alpha * edge.length))
    ## Get the traits
    return(t(MASS::mvrnorm(n = 1, mu = means, Sigma = Sigma, ...)))
}

## no.process
no.process <- function(x0 = 0, edge.length = 1, fun = stats::rnorm, ...) {
    return(fun(n = 1, ...))
}

## Multi peak internals
peak.diff <- function(x, peaks) return(abs(x - peaks))
closest.peak <- function(diff, peaks) return(peaks[which(diff == min(diff)[1])])
## The multiple optimum OU process
multi.peak.process <- function(x0 = 0, edge.length = 1, Sigma = diag(length(x0)), alpha = 1, peaks = 0, ...) {
    
    ## Finding the closest optimums
    if(!is(peaks, "list")) {
        diffs <- sapply(x0, peak.diff, peaks, simplify = FALSE)
        theta <- unlist(lapply(diffs, closest.peak, peaks))
    } else {
        diffs <- mapply(peak.diff, x0, peaks, SIMPLIFY = FALSE)
        theta <- unlist(mapply(closest.peak, diffs, peaks, SIMPLIFY = FALSE))
    }
    
    ## Calculate the means
    means <- theta + (x0 - theta) * exp(-alpha * edge.length)
    ## Calculate the Sigma
    Sigma <- Sigma/(2 * alpha) * (1 - exp(-2 * alpha * edge.length))
    ## Get the traits
    return(t(MASS::mvrnorm(n = 1, mu = means, Sigma = Sigma, ...)))
}

## A repulsive process
repulsion.process <- function(x0 = 0, edge.length = 1, repulsion = 0.5, sd = 1, max.try = 100, trait.values = NULL, lineage = NULL, trait = NULL, ...) {
    
    ## Getting the parent traits of the living lineages
    if(!is.null(lineage)) {
        livings <- parent.traits(trait.values, lineage, current = FALSE)
    } else {
        livings <- x0
    }

    ## Initialise the distances and the counter
    distances <- counter <- 0
    
    ## Iteratively find a fitting value (or not if past the counter)
    while(!all(distances > repulsion) && counter < max.try) {
        ## Draw a value
        bm_value <- rnorm(1, mean = x0, sd = sd)
        ## Measure the distances from this point
        distances <- abs(c(bm_value) - c(livings))
        ## Increment the counter
        counter <- counter + 1
    }

    return(bm_value)
}