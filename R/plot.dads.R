#' @title Plot dads objects
#'
#' @description Plotting dads objects (either a simulated tree and trait(s) or a process for traits objects)
#'
#' @param x \code{dads} data.
#' @param col Optional, a vector of colours that can be named (see details).
#' @param ... Any additional options to be passed to plot functions.
#' @param trait which trait to plot (default is \code{1}; see details).
#' @param edges either a colour name to attribute to the edges or \code{NULL} to not display the edges (default is \code{"grey"}).
#' @param tips.nodes optional, a colour to circle tips and nodes (only used if \code{use.3D = FALSE}). By default \code{tips.nodes = NULL}.
#' @param use.3D logical, whether to use a 3D plot or not (default is \code{FALSE}; see details).
#' @param simulations if the input is a \code{dads} \code{traits} object, how many replicates to run (default is \code{50}).
#' 
#' @details
#' The \code{col} option can be either:
#' \itemize{
#'      \item a \code{vector} of colours to be applied to \code{"dads"} \code{"traits"} objects (for respectively the median, 50% CI and 95% CI - by default this is \code{col = c("black", "grey", "lightgrey")}). This is the default option when plotting traits.
#'      \item a \code{vector} of colours to be applied to \code{"dads"} objects for the colours of different elements of the plot. This vector cycles through different elements of the the tree depending on the length of the vector: if one colour is given, it is applied to all elements; if two colours are given, the first one is applied to the nodes and the second to the tips; if three colours are given, they are applied to the nodes, fossils and living elements respectively. If more colours are given, they are applied in a gradient way to all elements depending on their age (see the \code{function} usage below). Note that you can always name the vector elements for avoiding ambiguities: e.g. \code{col = c("nodes" = "orange", "fossils" = "lightblue", "livings" = "blue")} (the default - you can use the name \code{"tips"} to designate both livings and fossils).
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
#' my_tree <- dads(stop.rule = list(max.taxa = 10))
#' ## Plotting a simple birth death tree (using ape::plot.phylo)
#' plot(my_tree, main = "A pure birth tree")
#' 
#' ## Simulating a tree with traits
#' my_data <- dads(stop.rule = list(max.taxa = 10),
#'                 traits    = my_trait)
#' ## Plotting the tree and traits
#' plot(my_data)
#'
#' ## Specifying a 3D trait process
#' my_3D_trait <- make.traits(n = 3)
#' ## Simulating a birth death tree with that trait
#' my_data <- dads(bd.params = list(extinction = 0.2),
#'                 stop.rule = list(max.living = 50),
#'                 traits    = my_3D_trait)
#' 
#' ## Plotting the second trait and the tree (default)
#' ## The colours are purple for nodes and blue for tips
#' plot(my_data, trait = 2, col = c("purple", "blue"),
#'      edges = "pink", tips.nodes = "black")
#' 
#' ## Plotting the first and third trait correlation
#' ## The colours are a heat map based on the elements age
#' plot(my_data, trait = c(1,3), col = heatmap,
#'      edges = "grey", tips.nodes = "black")
#'
#' ## Plotting the first and third trait correlation in 3D
#' plot(my_data, trait = c(1,3), col = rainbow,
#'      edges = "grey", tips.nodes = "black", use.3D = TRUE)
#' 
#' ## Plotting all traits in 3D (without branch lengths)
#' plot(my_data, trait = c(1:3), col = heatmap,
#'      edges = NULL, tips.nodes = "black", use.3D = TRUE)

#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

plot.dads <- function(x, col, ..., trait = 1, edges = "grey", tips.nodes = NULL, use.3D = FALSE, simulations = 50) {

    match_call <- match.call()

    ## Renaming the x parameter (data is nicer, x is just for the S3 method standards)
    data <- x

    ## Check class dads
    check.class(data, "dads")

    ## Deal with dual classes
    if(length(dads_class <- class(data)) ==  2) {
        ## Get the second class of the dads object
        second_class <- dads_class[2]

        if(second_class == "traits") {
            ## Selecting the trait
            one_trait <- data[[trait]]

            ##TODO: handle colours!

            ## Plotting the results
            plot.simulation(main = names(data)[[trait]],
                    replicate(simulations,
                        sim.motion(one_trait, steps = 100)))
        }

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

    ## Handle the type of plot
    if((n_traits <- length(trait)) > 3) {
        stop("Impossible to do plots in more than 3D.", call. = FALSE)
    } else {
        ## Selecting the plot type (TRUE is tree plots and FALSE is correlation plots)
        tree_plot <- switch(as.character(n_traits),
                            "1" = TRUE,
                            "2" = ifelse(use.3D, TRUE, FALSE),
                            "3" = ifelse(use.3D, FALSE, stop("Set use.3D = TRUE to display three traits.", call. = FALSE)))
    }


    ## Handle plot options

    ## Plot the normal data
    points_params <- lines_params <- plot_params <- list(...)
    #points_params <- lines_params <- plot_params <- list() ; warning("DEBUG plot.dads")
    
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
    ## pch
    if(is.null(plot_params$pch)) {
        ## Add the pch for the points only
        points_params$pch <- 19
    } else {
        lines_params$pch <- plot_params$pch <- NULL
    }
    ## lty
    if(is.null(plot_params$lty)) {
        ## Add the pch for the points only
        lines_params$lty <- 1
    } else {
        points_params$lty <- plot_params$lty <- NULL
    }

    ## Handle the colours
    points_params$col <- handle.colours(col, points_tree_IDs, points_ages, data)

    ## Plotting the frame
    plot_params <- c(plot_params, x = list(NULL), y = list(NULL))

    if(!use.3D) {
        do.call(plot, plot_params)
    } else {
        stop("not implemented yet")
        # rgl::plot3d
    }

    ## Plotting the tree (if needed)
    if(do_edges) {

        ## Add the colours
        lines_params$col <- edges

        ## Make the points data table
        points_data <- cbind(points_params$x, points_params$y)[c(data$tree$tip.label, data$tree$node.labe), ]

        if(!use.3D) {

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

            stop("not implemented yet")
            # rgl::segment3d

        }



    }

    ## Plotting the points
    if(!use.3D) {
        do.call(points, points_params)
    } else {
        stop("not implemented yet")

        

        x <- sort(rnorm(1000))
        y <- rnorm(1000)
        z <- rnorm(1000) + atan2(x, y)

        params_3d <- list(x = x, y = y, z = z, col = rainbow(1000))
        plot3d(x, y, z, col = rainbow(1000))

        do.call(rgl::plot3d, params_3d)



        # rgl::plot3d
    }

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

    return(invisible())
}