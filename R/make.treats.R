#' @title Make a treats object
#'
#' @description Combines a tree and some associated data into a treats object (e.g. for plotting)
#'
#' @param tree a phylogenetic tree.
#' @param data a dataset of traits, either a \code{matrix} with column names or a named \code{vector}.
# @param ... additional \code{treats} objects to add (e.g. \code{traits}).
#'
#' @return
#' This function outputs a \code{treats} object that is a list of at least two elements: \code{$tree}, a \code{"phylo"} object and \code{$data}, a \code{"matrix"} of the trait values.
#'
#' @examples
#' ## Creating a random tree
#' my_tree <- rtree(5)
#' ## Adding node labels
#' my_tree$node.label <- letters[1:4]
#' ## Creating a random dataset
#' my_data <- matrix(rnorm(9),
#'     dimnames = list(c(my_tree$tip.label, my_tree$node.label)))
#' ## Creating the treats object
#' my_treats <- make.treats(tree = my_tree, data = my_data)
#' plot(my_treats)
#'
#' @seealso \code{\link{treats}} \code{\link{plot.treats}}
#' 
#' @author Thomas Guillerme
#' @export

make.treats <- function(tree, data) {#, ...) {

    ## Sanitizing

    ## First input is dispRity
    if(!missing(tree) && is(tree, "dispRity")) {
        data <- tree
    }

    ## Check the data
    data_class <- check.class(data, c("list", "data.frame", "matrix", "numeric", "integer", "dispRity"))
    row_names_error <- "data must be a matrix or a data.frame with row names or a named vector."

    ## data is dispRity
    if(data_class == "dispRity") {
        ## Check if data has tree and data
        if(is.null(data$tree[[1]])) {
            stop("make.treats can only interpret dispRity data with data and tree(s). Make sure your dispRity object contains a tree by using:\ndispRity::get.tree(data)")
        } else {
            ## Set the data and the tree to check
            tree <- data$tree
            data <- data$matrix
            data_class <- "list"
            tree_can_be_missing <- TRUE
        }
    } else {
        tree_can_be_missing <- FALSE
    }

    ## Make the data into a list
    if(data_class != "list") {
        data <- list(data)
    }

    ## Check the data validity
    data <- lapply(data, check.data.make.treats, data_class, row_names_error)

    ## Check the tree
    if(!tree_can_be_missing) {
        tree_class <- check.class(tree, c("phylo", "multiPhylo"))
        if(tree_class == "phylo") {
            tree <- list(tree)
        }

        ## Check the length of the data
        if(length(data) != length(tree)) {
            ## Replicating the tree or the data
            if(length(data) == 1) {
                data <- unlist(replicate(length(tree), data, simplify = FALSE), recursive = FALSE)
            }
            if(length(tree) == 1) {
                tree <- unlist(replicate(length(data), tree, simplify = FALSE), recursive = FALSE)
            }
            if(length(tree) != 1 && length(data) != 1) {
                stop("The tree and data don't match.")
            }
        }
        ## Check the data and the tree
        silent <- mapply(check.tree.make.treats, tree, data)
    }

    ## Make the treats object
    output <- mapply(make.treats.object, tree, data, SIMPLIFY = FALSE)

    if(length(output) == 1) {
        return(output[[1]])
    } else {
        class(output) <- "treats"
        return(output)
    }
}

## Check the data and the tree
check.tree.make.treats <- function(tree, data) {
    ## Check node and tip labels
    if(is.null(tree$tip.label) || is.null(tree$node.label)) {
        stop("The input tree must have tip and node labels.")
    }
    ## Check the data match
    if(nrow(data) != (Ntip(tree)+Nnode(tree)) || any(is.na(match(rownames(data), c(tree$tip.label, tree$node.label)))))  {
        stop("The tree and data labels don't match.\nYou can use the following to make them match:\ndispRity::clean.data(data, tree)")
    }
    return(NULL)
}
## Check the data validity
check.data.make.treats <- function(data, data_class, row_names_error) {
    if(data_class == "data.frame") {
        data <- as.matrix(data)
    }
    if(data_class %in% c("integer", "numeric")) {
        if(is.null(names(data))) stop(row_names_error, call. = FALSE)
        data <- matrix(data, ncol = 1, dimnames = list(names(data)))
    }
    ## Check for rownames
    if(is.null(rownames(data))) {
        stop(row_names_error)
    }
    return(data)
}
make.treats.object <- function(tree, data) {
    output <- list()
    output$tree <- tree
    output$data <- data
    class(output) <- "treats"
    return(output)
}