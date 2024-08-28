#' @title Plot treats objects
#'
#' @description Plotting treats objects (either a simulated tree and trait(s) or a process for traits objects)
#'
#' @param x \code{treats} data.
#' @param col Optional, a \code{vector} of colours that can be named or a \code{function} (see details).
#' @param ... Any additional options to be passed to plot functions (from \code{graphics} or \code{rgl} if \code{use.3D = TRUE}).
#' @param trait which trait to plot (default is \code{1}; see details).
#' @param edges either a colour name to attribute to the edges or \code{NULL} to not display the edges (default is \code{"grey"}).
#' @param tips.nodes optional, a colour to circle tips and nodes (only used if \code{use.3D = FALSE}). By default \code{tips.nodes = NULL}.
#' @param use.3D logical, whether to use a 3D plot or not (default is \code{FALSE}; see details).
#' @param simulations if the input is a \code{treats} \code{traits} or \code{bd.params} object, how many replicates to run (default is \code{50}).
#' @param cent.tend if the input is a \code{treats} \code{traits}, which central tendency to plot (default is \code{mean}).
#' @param quantiles if the input is a \code{treats} \code{traits}, which quantiles to plot (default are \code{c(95, 50))}).
#' @param legend logical, whether to display the legend in 2D plots (\code{TRUE}) or not (\code{FALSE}; default)
#' @param add logical, whether to add to a previous plot.
#' @param transparency Optional, a transparency factor (\code{1} = not transparent, \code{0} = invisible). If left empty, and multiple plots are called, the transparency is set to 1 / number of plots + 0.1. 
#' 
#' @return No return value, plot \code{x}'s content.
#'
#' @details
#' The \code{col} option can be either:
#' \itemize{
#'      \item a \code{vector} of colours to be applied to \code{"treats"} \code{"traits"} objects (for respectively the median, 50% CI and 95% CI - by default this is \code{col = c("black", "grey", "lightgrey")}). This is the default option when plotting traits.
#'      \item a \code{vector} of colours to be applied to \code{"treats"} objects for the colours of different elements of the plot. This vector is applied to all the elements in the tree using the order in \code{tree$tip.label} and \code{tree$node.label}.
#'      \item an unambiguous named \code{vector} for colouring each specific elements. These can be any of the following (with default colours) \code{col = c("nodes" = "orange", "fossils" = "lightblue", "livings" = "blue")} or \code{"tips"} to designate both livings and fossils and \code{"singletons"} to designate non-bifurcating nodes.
#'      \item a \code{function} from which to sample the colours to match the time gradient for each element.
#' }
#' 
#' The \code{trait} option can intake from 1 to 3 traits (if \code{use.3D = TRUE}). If two traits are given (e.g. \code{c(1, 2)}), the default plots a correlation plot between both traits (same for 3 traits if \code{use.3D = TRUE}).
#' 
#' The \code{use.3D} option uses the \code{rgl} library to create a 3D plot. The plot displays either a time on the Z axis with two traits on the X and Y axis (if two traits are requested via \code{trait}) or three traits on the X Y and Z (if three traits a requested via \code{trait}).
#' 
#' @examples
#' ## Specifying a trait process
#' my_trait <- make.traits()
#' ## Plotting a trait process
#' plot(my_trait, main = "A Brownian Motion")
#' 
#' ## Simulating a tree with ten taxa
#' my_tree <- treats(stop.rule = list(max.taxa = 10))
#' ## Plotting a simple birth death tree (using ape::plot.phylo)
#' plot(my_tree, main = "A pure birth tree")
#' 
#' ## Simulating a tree with traits
#' my_data <- treats(stop.rule = list(max.taxa = 100),
#'                   traits    = my_trait)
#' ## Plotting the tree and traits
#' plot(my_data)
#'
#' ## Specifying a 3D trait process
#' my_3D_trait <- make.traits(n = 3)
#' ## Simulating a birth death tree with that trait
#' my_data <- treats(bd.params = list(extinction = 0.2),
#'                   stop.rule = list(max.living = 50),
#'                   traits    = my_3D_trait)
#' 
#' ## Plotting the second trait and the tree (default)
#' ## The colours are purple for nodes and blue for tips
#' ## with a black circle for highlighting the tips
#' plot(my_data, trait = 2,
#'      col = c("nodes" = "purple", "tips" = "blue"),
#'      edges = "pink", tips.nodes = "black")
#' 
#' ## Plotting the first and third trait correlation
#' ## The colours are a heat map based on the elements age
#' plot(my_data, trait = c(1,3), col = terrain.colors,
#'      edges = "grey", tips.nodes = "black")
#'
#' ## Plotting the first and third trait correlation in 3D
#' plot(my_data, trait = c(1,3), col = rainbow,
#'      edges = "grey", tips.nodes = "black", use.3D = TRUE)
#' #rglwidget() # to display the plot with non-default OpenRGL
#' 
#' ## Plotting all traits in 3D (without branch lengths)
#' plot(my_data, trait = c(1:3), col = heat.colors,
#'      edges = NULL, tips.nodes = "black", use.3D = TRUE)
#' #rglwidget() # to display the plot with non-default OpenRGL
#'
#' @seealso \code{\link{treats}}
#' 
#' @author Thomas Guillerme
#' @export

plot.treats <- function(x, col, ..., trait = 1, edges = "grey", tips.nodes = NULL, use.3D = FALSE, simulations = 20, cent.tend = mean, quantiles = c(95, 50), legend = FALSE, transparency, add = FALSE) {

    match_call <- match.call()

    ## Renaming the x parameter (data is nicer, x is just for the S3 method standards)
    data <- x

    ## Check class treats
    check.class(data, "treats")

    ## Deal with dual classes
    if(length(treats_class <- class(data)) ==  2) {
        ## Get the second class of the treats object
        second_class <- treats_class[2]

        if(second_class == "traits") {

            ## Get all the traits id
            trait_IDs <- lapply(data$main, function(x) return(x$trait_id))

            ## Check if the trait is multidimensional
            if(length(trait_IDs) != length(unlist(trait_IDs))) {
                ## Find which trait is requested
                which_trait <- lapply(trait_IDs, function(x, request) x %in% request, request = trait)
                ## Find which process is requested
                which_process <- which(unlist(lapply(which_trait, any)))
                ## Select the process and the trait
                one_trait <- data$main[[which_process]]
                # Select the trait
                if(!is.null(one_trait$start) && length(one_trait$start) != 1) {
                    one_trait$start <- one_trait$start[which_trait[[which_process]]]
                }
                if(!is.null(one_trait$trait_id) && length(one_trait$trait_id) != 1) {
                    one_trait$trait_id <- one_trait$trait_id[which_trait[[which_process]]]
                }
                ## Handle extra process arguments
                if(!is.null(one_trait$process.args)) {
                    if(any(process_args <- which(unlist(lapply(one_trait$process.args[[which_process]], length)) > 1))) {
                        for(one_arg in process_args) {
                            one_trait$process.args[[which_process]][[one_arg]] <- list(one_trait$process.args[[which_process]][[one_arg]][[trait]])
                        }
                    }
                }
            } else {
                ## Selecting the trait
                one_trait <- data$main[[trait]]                
            }

            ## Selecting the trait ids
            if(use.3D) {
                trait_ids <- seq_along(one_trait$trait_id)
            } else {
                trait_ids <- 1
            }

            ## Handle colours
            if(missing(col)) {
                col <- "default"
            }

            ## Set the trait name
            dots <- list(...)
            if(!is.null(dots$main)) {
                trait_name <- dots$main
            } else {
                trait_name <- names(data$main)[trait]
            }

            ## Plotting the results
            internal.plot.simulation(data = replicate(simulations, 
                                             sim.motion(one_trait, steps = 100),
                                             simplify = FALSE),
                            col = col, use.3D = use.3D, trait = trait_ids, trait.name = trait_name, quantiles = quantiles, cent.tend = cent.tend, ...)
        }

        if(second_class == "bd.params") {

            ## Sampling the two parameters
            simulated <- matrix(unlist(replicate(simulations, sample.from(data), simplify = FALSE)), ncol = 2, byrow = TRUE)

            ## Get the minimum values to be point estimates
            uniques <- ceiling(simulations/4)

            ## Plot the background
            hist_params <- list(...)
            hist_params$plot <- TRUE
            hist_params$x <- simulated
            hist_params$col <- grDevices::rgb(0,0,0, alpha = 0)
            hist_params$border <- grDevices::rgb(0,0,0, alpha = 0)
            if(is.null(hist_params$xlab)) {
                hist_params$xlab <- "parameter values"
            }
            if(is.null(hist_params$main)) {
                hist_params$main <- "parameters sampled"
            }
            if(is.null(hist_params$breaks)) {
                hist_params$breaks <- uniques
            }
            plot_params <- do.call(hist, hist_params)
            ## Get the colours
            if(missing(col)) {
                col <- c(grDevices::rgb(0, 0, 255, max = 255, alpha = 150), grDevices::rgb(255, 165, 0, max = 255, alpha = 150))
            } else {
                if(length(col) < 2) {
                    col <- rep(col, 2)
                } else {
                    col <- col[1:2]
                }
            }
            ## Add the histograms
            if(length(unique(simulated[,1])) < uniques) {
                breaks <- seq(from = min(plot_params$breaks), to = max(plot_params$breaks), length.out = simulations)
            } else {
                breaks <- plot_params$breaks
            }
            hist(simulated[,1], col = col[1], add = TRUE, breaks = breaks)
            
            if(length(unique(simulated[,2])) < uniques) {
                breaks <- seq(from = min(plot_params$breaks), to = max(plot_params$breaks), length.out = simulations)
            } else {
                breaks <- plot_params$breaks
            }
            hist(simulated[,2], col = col[2], add = TRUE, breaks = breaks)
            ## Add the legends
            legend("topright", col = col, legend = c("speciation", "extinction"), pch = 15, bg = "white")
        }

        return(invisible())
    }

    ## Make the data a data list
    if(is(data[[1]], "treats")) {

        ## Set transparency
        if(missing(transparency)) {
            transparency <- 1/length(data) + 0.1
        }
        
        ## Detect the plot limits for the first plot
        dots <- list(...)
        xlim <- ylim <- zlim <- NULL
        if(is.null(dots$xlim)) {
            xlim <- range(unlist(lapply(data, function(x, trait) range(x$data[, trait[1]]), trait = trait)))
        }
        if(is.null(dots$ylim)) {
            if(length(trait) > 1) {
                ## ylim is the second trait
                ylim <- range(unlist(lapply(data, function(x, trait) range(x$data[, trait[2]]), trait = trait)))
            } else {
                ## ylim is the tree depth
                get.root.time <- function(x) {
                    if(is.null(x$tree$root.time)) {
                        return(max(tree.age(x$tree)$age))
                    } else {
                        return(x$tree$root.time)
                    }
                }
                ylim <- xlim
                xlim <- c(max(unlist(lapply(data, get.root.time))), 0)
            }
        }   
        if(is.null(dots$zlim) && use.3D) {
            if(length(trait) > 2) {
                ## zlim is the second trait
                zlim <- range(unlist(lapply(data, function(x, trait) range(x$data[, trait[3]]), trait = trait)))
            } else {
                ## zlim is the tree depth
                get.root.time <- function(x) {
                    if(is.null(x$tree$root.time)) {
                        return(max(tree.age(x$tree)$age))
                    } else {
                        return(x$tree$root.time)
                    }
                }
                zlim <- ylim
                ylim <- c(max(unlist(lapply(data, get.root.time))), 0)
            }
        }

        ## First plot
        plot.treats(data[[1]], col = col, xlim = xlim, ylim = ylim, zlim = zlim, ..., trait = trait, edges = edges, tips.nodes = tips.nodes, use.3D = use.3D, legend = legend, transparency = transparency, add = FALSE)
        # plot.treats(data[[1]], col = col, xlim = xlim, ylim = ylim, zlim = zlim,  trait = trait, edges = edges, tips.nodes = tips.nodes, use.3D = use.3D, legend = legend, transparency = transparency, add = FALSE) ; warning("DEBUG")

        ## Subsequent plots
        lapply(data[-1], plot.treats, col = col, ..., trait = trait, edges = edges, tips.nodes = tips.nodes, use.3D = use.3D, legend = FALSE, transparency = transparency, add = TRUE)
        # lapply(data[-1], plot.treats, col = col, trait = trait, edges = edges, tips.nodes = tips.nodes, use.3D = use.3D, legend = FALSE, transparency = transparency, add = TRUE) ; warning("DEBUG")

        return(invisible())
    }

    ## Sanitizing
    if(missing(col)) {
        col <- "default"
    } else {
        check.class(col, c("character", "function"))
    }
    check.class(trait, c("numeric", "integer"))
    if(max(trait) > ncol(data$data)) {
        stop(paste0(as.expression(match_call$x), " contains only ", ncol(data$data), " trait", ifelse(ncol(data$data) == 1, ".", "s.")))
    }
    if(is.null(edges)) {
        do_edges <- FALSE
    } else {
        check.class(edges, "character")
        do_edges <- TRUE
    }
    if(is.null(tips.nodes)) {
        do_circles <- FALSE
    } else {
        check.class(tips.nodes, "character")
        do_circles <- TRUE
    }
    check.class(use.3D, "logical")
    check.class(add, "logical")

    ## Handle the type of plot
    if((n_traits <- length(trait)) > 3) {
        stop("Breaking physics failed. Impossible to visualise plots in more than 3D in this version.", call. = FALSE)
    } else {
        ## Selecting the plot type (TRUE is tree plots and FALSE is correlation plots)
        tree_plot <- switch(as.character(n_traits),
                            "1" = TRUE,
                            "2" = ifelse(use.3D, TRUE, FALSE),
                            "3" = ifelse(use.3D, FALSE, stop("Set use.3D = TRUE to display three traits.", call. = FALSE)))
    }

    ## Check whether to plot discrete characters
    do_discrete_plot <- FALSE
    if(!is.null(data$data) && length(trait) == 1) {
        ## Is discrete?
        if(!is.integer(data$data[, trait])) {
            do_discrete_plot <- all(as.integer(data$data[, trait]) == unname(data$data[, trait]))
            if(is.na(do_discrete_plot)) {
                do_discrete_plot <- FALSE
            }
        } else {
            do_discrete_plot <- TRUE
        }
    }

    ## Plotting discrete characters on a tree
    if(do_discrete_plot) {
        ## Isolate the components
        tree <- ladderize(data$tree)
        data <- data$data
        tips <- match(tree$tip.label, rownames(data))
        nodes <- match(tree$node.label, rownames(data))

        ## Get the number of states
        n_states <- length(unique(data[, trait]))
        
        ## Get the data to plot
        data_plot <- data[, trait]
        ## Remove 0 base (if exists)
        if(any(data_plot == 0)) {
            data_plot <- data_plot + 1
        }


        ## Set the colours
        if(col[1] == "default") {
            ## Default colours
            if(n_states <= 3) {
                col <- c("orange", "blue", "darkgreen")
            } else {
                col <- grDevices::hcl(h = seq(15, 375, length = n_states + 1), l = 65, c = 100)[1:n_states]
            }
        } else {
            if(length(col) < n_states) {
                col <- rep(col, n_states)
            }
        }
        col <- col[1:n_states]

        ## Plotting args
        plot_args <- list(...)
        plot_args$x <- tree

        ## Handle the default parameters for the tree plot
        if(is.null(plot_args$show.tip.label)) {
            plot_args$show.tip.label <- FALSE
        }
        if(is.null(plot_args$edge.color)) {
            plot_args$edge.color <-  match.tip.edge(vector = col[data_plot[c(tips, nodes)]], phylo = tree)
        }

        ## Plot the tree with the edge colours
        do.call(plot.phylo, plot_args)

        if(legend) {
            col_order <- match(sort(unique(data[, trait])), unique(data[, trait])) 
            legend("bottomleft", legend = as.character(unique(data[, trait]))[col_order], lty = 1, col = col[col_order], title = colnames(data)[trait])
        }

        ## Leave the functions
        return(invisible())
    }    

    ## Handle plot options

    ## Plot the normal data
    points_params <- lines_params <- plot_params <- list(...)
    #points_params <- lines_params <- plot_params <- list() ; warning("DEBUG plot.treats")
    
    ## Get the points IDs and ages
    points_ages     <- dispRity::tree.age(data$tree)
    points_tree_IDs <- match(rownames(data$data), points_ages[,"elements"])

    if(tree_plot) {
        ## Get the x, y (and z?) coordinates
        points_params$x <- points_ages[points_tree_IDs, "ages"]
        points_params$y <- data$data[, trait[1]]
        if(use.3D) {
            points_params$z <- data$data[, trait[2]]
        }
    } else {
        ## Get the x, y (and z?) coordinates
        points_params$x <- data$data[, trait[1]]
        points_params$y <- data$data[, trait[2]]
        if(use.3D) {
            points_params$z <- data$data[, trait[3]]
        }
    }

    ## Get the plot size parameters
    ## xlim
    if(is.null(plot_params$xlim)) {
        ## Default scale
        if(tree_plot) {
            plot_params$xlim <- rev(range(points_params$x))
        } else {
            plot_params$xlim <- range(points_params$x)
        }
    } else {
        ## Remove xlim option from points and lines
        lines_params$xlim <- points_params$xlim <- NULL
    }

    ## ylim
    if(is.null(plot_params$ylim)) {
        ## Default scale
        plot_params$ylim <- range(points_params$y)
    } else {
        ## Remove ylim option from points and lines
        lines_params$ylim <- points_params$ylim <- NULL
    }

    if(use.3D && is.null(plot_params$zlim)) {
        ## Default scale
        plot_params$zlim <- range(points_params$z)
    } else {
        ## Remove zlim from points and lines
        lines_params$zlim <- points_params$zlim <- NULL
    }

    ## main
    if(!is.null(plot_params$main)) {
        ## Remove main option from points and lines        
        lines_params$main <- points_params$main <- NULL
    }

    ## xlab
    if(is.null(plot_params$xlab)) {
        ## Default xlab
        if(tree_plot) {
            plot_params$xlab <- "Time"
        } else {
            plot_params$xlab <- colnames(data$data)[trait[1]]
        }
    } else {
        ## Remove xlab option from points and lines
        lines_params$xlab <- points_params$xlab <- NULL
    }

    ## ylab
    if(is.null(plot_params$ylab)) {
        ## Default ylab
        plot_params$ylab <- colnames(data$data)[trait[ifelse(tree_plot, 1, 2)]]
    } else {
        ## Remove ylab option from points and lines
        lines_params$ylab <- points_params$ylab <- NULL
    }

    ## zlab
    if(use.3D && is.null(plot_params$zlab)) {
        ## Default zlab
        plot_params$zlab <- colnames(data$data)[trait[ifelse(tree_plot, 2, 3)]]
    } else {
        ## Remove zlab option from points and lines
        lines_params$zlab <- points_params$zlab <- NULL
    }

    ## pch
    if(is.null(plot_params$pch)) {
        ## Add the pch for the points only
        points_params$pch <- 19
    } else {
        lines_params$pch <- plot_params$pch <- NULL
    }
    if(use.3D) {
        ## Remove pch argument and change it with "p" for rgl::plot3d
        points_params$pch <- NULL
        if(is.null(plot_params$type)) {
            points_params$type <- "p"
        } else {
            lines_params$type <- plot_params$type <- NULL
        }
    }

    ## lty
    if(is.null(plot_params$lty)) {
        ## Add the pch for the points only
        lines_params$lty <- 1
    } else {
        points_params$lty <- plot_params$lty <- NULL
    }

    ## Handle the colours
    col_handle <- handle.colours(col, points_tree_IDs, points_ages, data, legend)
    if(!missing(transparency)) {
        points_params$col <- grDevices::adjustcolor(col_handle$col, alpha.f = transparency)
    } else {
        points_params$col <- col_handle$col
    }
    legend_col <- col_handle$legend

    ## Plotting the frame
    if(!add) {
        if(!use.3D) {
            plot_params <- c(plot_params, x = list(NULL), y = list(NULL))
            do.call(plot, plot_params)
        } else {
            plot_params <- c(plot_params, x = list(NULL), y = list(NULL), z = list(NULL))
            do.call(rgl::plot3d, plot_params)
        }
    }

    ## Plotting the tree (if needed)
    if(do_edges) {

        ## Add the colours
        if(!missing(transparency)) {
            lines_params$col <- grDevices::adjustcolor(edges, alpha.f = transparency)
        } else {
            lines_params$col <- edges
        }

        if(!use.3D) {
            # ## Make the points data table
            # points_data <- cbind(points_params$x, points_params$y)[c(data$tree$tip.label, data$tree$node.labe), ]

            ## Plotting one edge
            plot.edge <- function(one_edge, points_data, params) {
                params$x <- points_data[one_edge, 1] 
                params$y <- points_data[one_edge, 2] 
                do.call(lines, params)
            }

            ## Plotting all the edges
            apply(data$tree$edge, 1, plot.edge,
                  points_data = cbind(points_params$x, points_params$y)[c(data$tree$tip.label, data$tree$node.labe), ],
                  params = lines_params)

        } else {
            ## Plotting one edge (3D)
            plot.edge3d <- function(one_edge, points_data, params) {
                params$x <- points_data[one_edge, 1] 
                params$y <- points_data[one_edge, 2] 
                params$z <- points_data[one_edge, 3] 
                do.call(rgl::segments3d, params)
            }

            ## Plotting all the edges
            apply(data$tree$edge, 1, plot.edge3d,
                  points_data = cbind(points_params$x, points_params$y, points_params$z)[c(data$tree$tip.label, data$tree$node.labe), ],
                  params = lines_params)
        }
    }

    ## Plotting the points
    if(!use.3D) {
        do.call(points, points_params)
    } else {
        ## Switch plotting type
        switch(points_params$type, 
            "p" = {points_params$type <- NULL ; do.call(rgl::points3d, points_params)},
            "s" = {points_params$type <- NULL ; do.call(rgl::spheres3d, points_params)})
    }

    ## Adding circles around the points
    if(do_circles && !use.3D) {
        ## Preparing the circles options
        circles_params <- points_params
        circles_params$pch <- 21
        circles_params$col <- tips.nodes

        ## Removing nodes
        to_remove <- which(is.na(match(points_ages[points_tree_IDs, "elements"], points_ages[1:Ntip(data$tree), "elements"])))
        circles_params$x <- circles_params$x[-to_remove]
        circles_params$y <- circles_params$y[-to_remove]

        ## Adding the circles
        do.call(points, circles_params)
    }

    ## Adding the legend
    if(legend && !use.3D) {
        ## Get the legend position
        legend_pos <- ifelse(tree_plot, "topleft", "topright")
        ## Legend type (just points or a gradient)
        is_gradient <- is(legend_col, "list")

        ## Plot the legend
        if(!is_gradient) {

            ## TODO: add tip nodes if needed

            if(!do_circles) {
                legend(legend_pos, col = legend_col, legend = names(legend_col), pch = points_params$pch)
            } else {
                legend(legend_pos, col = c(legend_col, tips.nodes), legend = c(names(legend_col), "tips"), pch = c(rep(points_params$pch, length(legend_col)), 21))
            }
        } else {
            xleft <- max(plot_params$xlim)
            xright <- xleft - abs(diff(plot_params$xlim))/25
            ytop <- max(plot_params$ylim)
            ybottom <- ytop - abs(diff(plot_params$xlim))/2
            ## Add the gradient
            graphics::rasterImage(rev(legend_col$raster), xleft, ybottom, xright, ytop)
            ## Add the text
            text(labels = legend_col$labels,
                x = rep(xright-xright*0.02, 3),
                y = c(ybottom,ybottom + (ytop-ybottom)/2, ytop))
        }
    }

    return(invisible())
}