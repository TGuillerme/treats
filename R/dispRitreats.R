#' @title dispRity to treats
#'
#' @description Converts \code{treats} data output to be passed to \code{\link[dispRity]{dispRity}}
#'
#' @param data an output from \code{treats} containing tree and traits data.
#' @param scale.tree logical, whether to scale the trees between 1 and 0 (\code{TRUE}; default) or not (\code{FALSE})
#TODO: WHISHLIST or the range to which scale the trees.
#' 
#' @details
#' The tree and data parts can be passed to the \code{dispRity} package functions \link[dispRity]{dispRity}, \code{\link[dispRity]{chrono.subsets}} or \link[dispRity]{custom.subsets} as the \code{data} argument (if needed, the \code{tree} argument is automatically recycled).
#' The \code{scale.tree} function is highly recommended if trees have various root ages.
#' 
#' @examples
#' ## Simulate a random tree with a 10 dimensional Brownian Motion trait
#' my_treats <- treats(stop.rule = list("max.taxa" = 20),
#'                     traits    = make.traits(BM.process, n = 10),
#'                     bd.params = make.bd.params(speciation = 1))
#' 
#' ## Converting the treats object to be used for a dispRity function
#' my_dispRity <- dispRitreats(my_treats)
#' 
#' ## Creating 5 time slices (dispRity)
#' time_slices <- dispRity::chrono.subsets(data      = my_dispRity$data,
#'                                         tree      = my_dispRity$tree,
#'                                         method    = "continuous",
#'                                         model     = "acctran",
#'                                         time      = 5,
#'                                         inc.nodes = TRUE)
#' 
#' ## Calculating the sum of variances for these slices
#' dispRity::dispRity(time_slices, metric = c(sum, variances))
#'
#' @seealso \code{\link{treats}} \code{\link[dispRity]{dispRity}} \code{\link[dispRity]{chrono.subsets}} \code{\link[dispRity]{custom.subsets}}
#' 
#' @author Thomas Guillerme
#' @export

dispRitreats <- function(data, scale.tree = TRUE) {
    ## Sanitizing
    is_list <- check.class(data, c("list", "treats"))
    if(is_list != "list") {
        if(is(data[[1]], "treats")) {
            class(data) <- "list"
        } else {
            data <- list(data)
        }
    }
    if(!all(unlist(lapply(data, function(x) all(c("data", "tree") %in% names(x)))))) {
        stop("data must be a list of \"treats\" objects or a \"treats\" object containing a tree and traits data.")
    }

    ## Extract the matrices and trees
    matrices <- lapply(data, function(x) return(x$data))
    trees    <- lapply(data, function(x) return(x$tree))
    
    ## scaling the trees
    if(scale.tree) {
        scale.tree.fun <- function(tree) {
            ## Scale the tree
            tree$edge.length <- tree$edge.length/tree$root.time
            tree$root.time <- 1
            return(tree)
        }
        trees <- lapply(trees, scale.tree.fun)
    }
    ## Change the tree to phylo if its alone
    if(length(trees) == 1) {
        trees <- trees[[1]]
        class(trees) <- "phylo"
    } else {
        class(trees) <- "multiPhylo"
    }

    output <- list("data" = matrices, "tree" = trees)
    class(output) <- c("dispRity", "treats")
    return(output)
}