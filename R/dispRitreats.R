#' @title dispRity interface for treats objects
#'
#' @description Pass a \code{treats} object to the \code{dispRity} function.
#'
#' @param data an output from \code{treats} containing tree and traits data.
#' @param ... any other arguments to be passed to \code{\link[dispRity]{dispRity}}, \code{\link[dispRity]{chrono.subsets}}, \code{\link[dispRity]{custom.subsets}}, and \code{\link[dispRity]{boot.matrix}}.
#' 
#' @details
#' This function applies the \code{dispRity} package pipeline to the \code{treats} output. If multiple simulations are input, the data is scaled for all the simulations.
#'
#' @return
#' Outputs a \code{"dispRity"} object that can be plotted, summarised or manipulated with the \code{dispRity} package.
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
#' disparity <- dispRitreats(my_treats, metric = c(sum, variances)
#'                           method = "continuous", model = "proximity", time = 5)
#' plot(disparity)
#'
#' @seealso \code{\link{treats}} \code{\link[dispRity]{dispRity}} \code{\link[dispRity]{chrono.subsets}} \code{\link[dispRity]{custom.subsets}} \code{\link[dispRity]{boot.matrix}} \code{\link[dispRity]{plot.dispRity}} \code{\link[dispRity]{summary.dispRity}}
#' 
#' @author Thomas Guillerme
#' @export

dispRitreats <- function(data, ...) {

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
    if(verbose) cat("Calculating disparity:")
    disparity_list <- lapply(data, apply.dispRity, group_type, subset_args, do_bootstrap, boot_args, dispRity_args, metric_args, verbose)
    if(verbose) cat("Done.")

    if(length(disparity_list) == 1) {
        ## Update the metric call
        disparity_list[[1]]$call$disparity$metrics$name <- match_call$metric
        return(disparity_list[[1]])
    }

    ## Combine the outputs into a dispRity object for summary, print and test
    ## 1- Take the first output
    ## 2- Combine all the matrices
    ## 3- Combine all the trees
    ## 4- Combine the disparity results
    ## 5- Update the call (metric + dispRitreats hybrid)
     

    ## Get the metric name
    # data$call$disparity$metrics$name <- c(data$call$disparity$metrics$name, match_call$metric)
    # if(!is.null(data$call$disparity$metrics$fun)) {
    #     data$call$disparity$metrics$fun <- list(unlist(data$call$disparity$metrics$fun, recursive = FALSE), metric)
    # } else {
    #     data$call$disparity$metrics$fun <- metric
    # }

    # ## Adding the between groups
    # data$call$disparity$metrics$between.groups <- ifelse(is_between.groups, TRUE, FALSE)

    # return(NULL)
}

## Wrapper function for measuring disparity across the datasets
apply.dispRity <- function(one_simulation, group_type, subset_args, do_bootstrap, boot_args, dispRity_args, metric_args, verbose) {

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

    if(verbose) cat(".")

    return(disparity_data)
}

## Scaling the trees (not used for now)
scale.tree.fun <- function(tree) {
    ## Scale the tree
    tree$edge.length <- tree$edge.length/tree$root.time
    tree$root.time <- 1
    return(tree)
}