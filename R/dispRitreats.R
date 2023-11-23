#' @title dispRity interface for treats objects
#'
#' @description Pass a \code{treats} object to the \code{dispRity} function.
#'
#' @param data an output from \code{treats} containing tree and traits data.
#' @param ... any other arguments to be passed to \code{\link[dispRity]{dispRity}}, \code{\link[dispRity]{chrono.subsets}}, \code{\link[dispRity]{custom.subsets}}, and \code{\link[dispRity]{boot.matrix}}.
#' @param scale.trees logical, whether to scale the tree ages in all simulations (\code{TRUE}; default) or not (\code{FALSE}).
#' 
#' @details
#' This function applies the \code{dispRity} package pipeline to the \code{treats} output. If multiple simulations are input, the data is scaled for all the simulations.
#'
#' The \code{scale.trees} option allows the trees to have the same depth and root age. This option is recommended if \code{\link[dispRity]{chrono.subsets}} options are called to make the output results comparable.
#' 
#' Common optional arguments for the following arguments include the following (refer the the specific function for the arguments details):
#' \itemize{
#'  \item \code{\link[dispRity]{custom.subsets}}: \code{group} for the list of elements to be attributed to specific groups;
#'  \item \code{\link[dispRity]{chrono.subsets}}: \code{method} for selecting the time binning or slicing method; \code{time} for the number of time bins/slices or their specific ages; \code{model} for the time slicing method; or \code{inc.nodes} for whether to include nodes or not in the time subsets;
#'  \item \code{\link[dispRity]{boot.matrix}}: \code{bootstraps} for the number of bootstrap replicates; \code{rarefaction} for the number of elements to include in each bootstrap replicate; or \code{boot.type} for the bootstrap algorithm;
#'  \item \code{\link[dispRity]{dispRity}}: \code{metric} for the disparity, dissimilarity or spatial occupancy metric to apply to the data; or \code{dimensions} for the number of dimensions to consider.
#' }
#'
#' @return
#' Outputs a \code{"dispRity"} object that can be plotted, summarised or manipulated with the \code{dispRity} package.

# Note that because the variance in the simulations, the function \code{\link[dispRity]{summary.dispRity}} displays the average number of per subset. TODO:fix

#' 
#' @examples
#' ## Simulate a random tree with a 10 dimensional Brownian Motion trait
#' my_treats <- treats(stop.rule = list("max.taxa" = 20),
#'                     traits    = make.traits(BM.process, n = 10),
#'                     bd.params = make.bd.params(speciation = 1))
#'
#' ## Calculating disparity as the sum of variances
#' disparity <- dispRitreats(my_treats, metric = c(sum, variances))
#' summary(disparity)
#'
#' ## Calculating disparity as the mean distance from the centroid of
#' ## coordinates 42 (metric = c(mean, centroids), centroid = 42)
#' ## using 100 bootstrap replicates (bootstrap = 100) and 
#' ## chrono.subsets (method = "continuous", model = "acctran", time = 5)
#' disparity <- dispRitreats(my_treats,
#'                           metric = c(mean, centroids), centroid = 42,
#'                           bootstraps = 100,
#'                           method = "continuous", model = "acctran", time = 5)
#' plot(disparity)
#'
#' ## Simulate 20 random trees with a 10 dimensional Brownian Motion trait
#' my_treats <- treats(stop.rule = list("max.taxa" = 20),
#'                     traits    = make.traits(BM.process, n = 10),
#'                     bd.params = make.bd.params(speciation = 1))
#'
#' ## Calculating disparity on all these trees as the sum of variance
#' ## on 5 continuous proximity time subsets
#' disparity <- dispRitreats(my_treats, metric = c(sum, variances),
#'                           method = "continuous", model = "proximity", time = 5)
#' plot(disparity)
#'
#' @seealso \code{\link{treats}} \code{\link[dispRity]{dispRity}} \code{\link[dispRity]{chrono.subsets}} \code{\link[dispRity]{custom.subsets}} \code{\link[dispRity]{boot.matrix}} \code{\link[dispRity]{plot.dispRity}} \code{\link[dispRity]{summary.dispRity}}
#' 
#' @author Thomas Guillerme
#' @export

dispRitreats <- function(data, ..., scale.trees = TRUE) {

    match_call <- match.call()

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

    ## Scaling the trees
    check.class(scale.trees, "logical")
    if(scale.trees) {
        data <- lapply(data, scale.tree.fun)
    }

    ## Check all the optional arguments
    all_args <- list(...)

    ## Handling verbose across the options
    verbose <- FALSE
    if("verbose" %in% names(all_args)) {
        verbose <- all_args$verbose
        all_args$verbose <- NULL
    }

    ## Detect the type of subset required
    custom_class <- c("group")
    chrono_class <- c("method", "time", "model", "inc.nodes", "FADLAD", "t0", "bind.data")
    group_type <- "none"
    subset_args <- list()
    ## Detect the type of group
    if(any(subset_match <- chrono_class %in% names(all_args))) {
        group_type <- "chrono"
        select_class <- chrono_class
    } else {
        if(any(subset_match <- custom_class %in% names(all_args))) {
            group_type <- "custom"
            select_class <- custom_class
        }
    }

    ## Get the arguments
    if(group_type != "none") {
        for(one_arg in 1:length(select_class[subset_match])) {
            subset_args[select_class[subset_match][one_arg]] <- all_args[select_class[subset_match][one_arg]]
            all_args[[select_class[subset_match][one_arg]]] <- NULL
        }
    }

    ## Detect whether to bootstrap
    boot_class <- c("bootstraps", "rarefaction", "boot.type", "prob")
    do_bootstrap <- FALSE
    if(any(boot_match <- boot_class %in% names(all_args))) {
        do_bootstrap <- TRUE
        boot_args <- list()
        # dimension arg is handled by dispRity
        for(one_arg in 1:length(boot_class[boot_match])) {
            boot_args[boot_class[boot_match][one_arg]] <- all_args[boot_class[boot_match][one_arg]]
            all_args[[boot_class[boot_match][one_arg]]] <- NULL
        }
    }

    ## Detect the dispRity arguments
    dispRity_class <- c("metric", "dimensions", "between.groups")
    if(any(disp_match <- dispRity_class %in% names(all_args))) {
        dispRity_args <- list()
        # dimension arg is handled by dispRity
        for(one_arg in 1:length(dispRity_class[disp_match])) {
            dispRity_args[dispRity_class[disp_match][one_arg]] <- all_args[dispRity_class[disp_match][one_arg]]
            all_args[[dispRity_class[disp_match][one_arg]]] <- NULL
        }
        if(length(all_args) != 0) {
            ## Adding the arguments left
            dispRity_args <- c(dispRity_args, all_args)
        }
    } else {
        stop("No metric argument was provided.")
    }

    ## Apply the function for each dataset
    if(verbose) message("Calculating disparity:", appendLF = FALSE)
    disparity_list <- lapply(data, apply.dispRity, group_type, subset_args, do_bootstrap, boot_args, dispRity_args, verbose)
    if(verbose) message("Done.")

    ## Single simulation
    if(length(disparity_list) == 1) {
        ## Update the metric call
        disparity_list[[1]]$call$disparity$metrics$name <- match_call$metric
        return(disparity_list[[1]])
    }

    ## Combine the outputs into a dispRity object for summary, print and test
    ## Use the first one as a template
    disparity_out <- disparity_list[[1]]

    ## Combine all the matrices
    disparity_out$matrix <- lapply(lapply(disparity_list, `[[`, "matrix"), `[[`, 1)

    ## Combine all the trees
    all_trees <- lapply(lapply(disparity_list, `[[`, "tree"), `[[`, 1)
    class(all_trees) <- "multiPhylo"
    disparity_out$tree <- all_trees

    ## Combine the disparity results
    all_disparity <- lapply(disparity_list, `[[`, "disparity")
    disparity_out$disparity <- merge.disparity(all_disparity)

    ## Update the metric call
    disparity_out$call$disparity$metrics$name <- match_call$metric
    ## Toggle the dispRitreats hybrid call
    disparity_out$call$dispRitreats <- TRUE

    return(disparity_out)
}

## Wrapper function for measuring disparity across the datasets
apply.dispRity <- function(one_simulation, group_type, subset_args, do_bootstrap, boot_args, dispRity_args, verbose) {

    ## Subsets
    if(group_type != "none") {
        subset_args$data <- one_simulation$data
        subset_args$tree <- one_simulation$tree
        
        ## Set the data
        disparity_data <- switch(group_type,
                                "chrono" = do.call(chrono.subsets, subset_args),
                                "custom" = do.call(custom.subsets, subset_args))
    } else {
        ## Make the basic dispRity data
        disparity_data <- fill.dispRity(make.dispRity(data = one_simulation$data), tree = one_simulation$tree)
    }

    ## Bootstraps
    if(do_bootstrap) {
        boot_args$data <- disparity_data
        disparity_data <- do.call(boot.matrix, boot_args)
    }

    ## Calculate disparity
    dispRity_args$data <- disparity_data
    disparity_data <- do.call(dispRity, dispRity_args)

    if(verbose) message(".", appendLF = FALSE)

    return(disparity_data)
}

## Merging disparity results
merge.disparity <- function(all_disparity) {
    merge.subset.pair <- function(subset1, subset2) {
        return(mapply(FUN = function(x,y)return(matrix(c(x, y), nrow = dim(x)[1])), x = subset1, y = subset2, SIMPLIFY = FALSE))
    }
    while(length(all_disparity) != 1) {
        ## Merge all subsets
        all_disparity[[1]] <- mapply(merge.subset.pair, all_disparity[[1]], all_disparity[[2]], SIMPLIFY = FALSE)
        ## Removed merged set
        all_disparity[[2]] <- NULL
    }
    return(unlist(all_disparity, recursive = FALSE))
}


## Scaling the trees
scale.tree.fun <- function(tree) {
    ## Scale the tree
    tree$edge.length <- tree$edge.length/tree$root.time
    tree$root.time <- 1
    return(tree)
}
