#' @name trait.process
#' @aliases BM.process OU.process
#' @title Trait processes
#'
#' @description Different trait processes implemented in dads.
#'
#' @usage trait.process(x0, edge.length, ...)
#'
#' @param x0 The previous state. This can be a single value (unidimensional process) or more (multidimensional processes).
#' @param edge.length The branch length (default must be 1). This is always a single value.
#' @param ... Any optional argument for the specific process (see details).
#' 
#' @details
#' The different trait processes implemented in dads are:
#' 
#' \itemize{
#'      \item{BM.process} A Brownian motion process (uni or multidimensional). This function is based on \code{\link[MASS]{mvrnorm}}.
#'      This process can take following optional arguments:
#'          \itemize{
#'               \item \code{Sigma} a positive-definite symmetric matrix specifying the covariance matrix of the variables (default is \code{diag(length(x0))}).
#'               \item \code{...} any named additional argument to be passed to \code{\link[MASS]{mvrnorm}}.
#'          }
#' 
#'      \item{OU.process} A Ornstein-Uhlenbeck process (uni or multidimensional). This function is based on \code{\link[MASS]{mvrnorm}}.
#'      This process can take following optional arguments:
#'          \itemize{
#'               \item \code{var} the traits variance/covariance (default is \code{diag(length(x0))}).
#'               \item \code{alpha} the alpha parameter (default = is \code{1}).
#'               \item \code{theta} the theta parameter (default = is \code{0}).
#'               \item \code{...} any named additional argument to be passed to \code{\link[MASS]{mvrnorm}}.
#'          }
#'      
#'      \item{no.process} An non-process unidimensional function. This function generates a trait value not depending on the branch length nor the previous state
#'      This process can take following optional arguments:
#'          \itemize{
#'               \item \code{fun} a random number function (default is \code{\link[stats]{rnorm}}).
#'               \item \code{...} any named additional argument to be passed to \code{fun}.
#'          }
#' }
#' 
#' More details about the \code{trait.process} functions is explained in the \code{dads} manual: \url{http://tguillerme.github.io/dads}.
#' 
#' @examples
#' ## NOTE: You can visualise most process by making them
#' ## into a "dads" "traits" object using make.traits():
#'
#' ## The Brownian motion process
#' BM.process(x0 = 0)
#' plot(make.traits(process = BM.process))
#' ## A correlation matrix between 3 traits
#' cor_matrix <- matrix(c(1/3,1/3,1/3,1/3,2/3,0,1/3,0,2/3), ncol = 3)
#' BM.process(x0 = c(0,0,0), Sigma = cor_matrix)
#' 
#' ## The Ornstein-Uhlenbeck process
#' OU.process(x0 = 0)
#' plot(make.traits(process = BM.process))
#' 
#' ## No process
#' no.process()
#' plot(make.traits(process = no.process))
#' 
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

trait.process <- function(x0, edge.length = 1, ...) {
    cat("Trait processes implemented in dads:\n")
    cat("?BM.process\n")
    cat("?OU.process\n")
    cat("?no.process\n")
}

## The Brownian motion
BM.process <- function(x0, edge.length = 1, Sigma = diag(length(x0)), ...) {
    return(t(MASS::mvrnorm(n = 1, mu = x0, Sigma = Sigma * edge.length, ...)))
}

## The OU process
OU.process <- function(x0, edge.length = 1, var = diag(length(x0)), alpha = 1, theta = 0, ...) {
    ## Calculate the means
    means <- theta + (x0 - theta) * exp(-alpha)
    ## Calculate the Sigma
    sd <- sqrt(var/(2 * alpha) * (1 - exp(-2 * alpha)))
    ## Get the traits
    return(t(MASS::mvrnorm(n = 1, mu = means, Sigma = sd * edge.length, ...)))
}

## no.process
no.process <- function(x0, edge.length = 1, fun = stats::rnorm, ...) {
    return(fun(n = 1, ...))
}
