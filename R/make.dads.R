#' @title Make a dads object
#'
#' @description Combines a tree and some associated data into a dads object (e.g. for plotting)
#'
#' @param tree a phylogenetic tree.
#' @param data a dataset of traits, either a \code{"matrix"} with column names or a named \code{"vector"}.
# @param ... additional \code{"dads"} objects to add (e.g. \code{"traits"}).
#'
#' @examples
#' ## Creating a random tree
#' my_tree <- rtree(5)
#' ## Adding node labels
#' my_tree$node.label <- letters[1:4]
#' ## Creating a random dataset
#' my_data <- matrix(rnorm(9),
#'     dimnames = list(c(my_tree$tip.label, my_tree$node.label)))
#' ## Creating the dads object
#' my_dads <- make.dads(tree = my_tree, data = my_data)
#' plot(my_dads)
#'
#' @seealso plot.dads
#' 
#' @author Thomas Guillerme
#' @export

make.dads <- function(tree, data) {#, ...) {
    ## Sanitizing

    ## Check the tree
    check.class(tree, "phylo") #TODO: generalise to multiPhylo
    if(is.null(tree$tip.label) || is.null(tree$node.label)) {
        stop("The input tree must have tip and node labels.")
    }

    ## Check the data
    data_class <- check.class(data, c("data.frame", "matrix", "numeric", "integer"))
    row_names_error <- "data must be a matrix with column names or a named vector."
    if(data_class != "matrix") {
        if(data_class == "data.frame") {
            data <- as.matrix(data)
        } else {
            ## Check for rownames
            if(is.null(names(data))) stop(row_names_error)
            data <- matrix(data, ncol = 1, dimnames = list(names(data)))
        }
    } else {
        ## Check for rownames
        if(is.null(rownames(data))) {
            stop(row_names_error)
        }
    }
    ## Check the length of the data
    if(nrow(data) == Ntip(tree)) {
        stop("The data does not seem to contain node values or they are not matching with the tree tips or node names.")
    }

    ## Check the data match
    if(nrow(data) != (Ntip(tree)+Nnode(tree)) || any(is.na(match(rownames(data), c(tree$tip.label, tree$node.label)))))  {
        stop("tree and data labels don't match.\nYou can use dispRity::clean.data(data, tree) to make them match.")
    }

    ## Catch the optional info
    # options <- list(...)
    # valid_options <- unlist(lapply(lapply(options, class), function(x) return("dads" %in% x)))

    ## Making output
    # output <- list(valid_options)
    output <- list()
    output$tree <- tree
    output$data <- data
    class(output) <- "dads"
    return(output)
}