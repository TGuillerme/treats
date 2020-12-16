#' @title Plot dads objects
#'
#' @description Plotting dads objects (either a simulated tree and trait(s) or a process for traits objects)
#'
#' @param x \code{dads} data.
#' @param col Optional, a vector of colours that can be named (see details).
#' @param ... Any additional options to be passed to plot functions.
#' @param trait which trait to plot (default is \code{1}; see details).
#' @param use.3D logical, whether to use a 3D plot or not (default is \code{FALSE}; see details).
#' @param edges logical, whether to plot the underlying tree structure (\code{TRUE}; default) or not (\code{FALSE}).
#' @param tips.nodes optional, a colour to circle tips and nodes (only used if \code{use.3D = FALSE}).
#' @param simulations if the input is a \code{dads} \code{traits} object, how many replicates to run (default is \code{100}).
#' 
#' @details
#' The \code{col} option can be either:
#' \itemize{
#'      \item an unnamed vector of colours to be applied to \code{"dads"} \code{"traits"} objects (for respectively the median, 50% CI and 95% CI - by default this is \code{col = c("black", "grey", "lightgrey")})
#'      \item a named vector of colours to be applied to \code{"dads"} objects for the colours of different elements of the plot. By default this is \code{col = c("nodes" = "orange", "fossils" = "lightblue", "livings" = "blue", "edges" = "grey")}. To colour both living and fossil tips the same way, you can use a vector element called \code{"tips"} instead of distinguishing between \code{"fossils"} and \code{"livings"}.
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
#' ## Plotting a simple birth death tree (no traits)
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
#' ## Plotting the second trait and the tree (default)
#' ## With extra options
#' plot(my_data, trait = 2, col = rainbow,
#'      edges = "pink", tip.nodes = "black")
#' 
#' ## Plotting the first and third trait correlation
#' plot(my_data, trait = c(1,3), col = heatmap,
#'      edges = "grey", tip.nodes = "black")
#'
#' ## Plotting the first and third trait correlation in 3D
#' plot(my_data, trait = c(1,3), col = heatmap,
#'      edges = "grey", tip.nodes = "black", use.3D = TRUE)
#' 
#' ## Plotting all traits in 3D (without branch lengths)
#' plot(my_data, trait = c(1:3), col = heatmap,
#'      edges = FALSE, tip.nodes = "black", use.3D = TRUE)

#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

plot.dads <- function(x, col, ..., trait = 1, edges = "grey", simulations = 50, use.3D = FALSE, tips.nodes) {

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

    ## Plot the normal data
    points_params <- lines_params <- plot_params <- list(...)
    #points_params <- lines_params <- plot_params <- list() ; warning("DEBUG plot.dads")
    
    ## Get the x values
    points_ages     <- dispRity::tree.age(data$tree)
    points_tree_IDs <- match(rownames(data$data), points_ages[,"elements"])
    points_params$x <- points_ages[points_tree_IDs, "ages"]
    ## Get the y values
    points_params$y <- data$data[, trait]

    ## Get the plot size parameters
    ## xlim
    if(is.null(plot_params$xlim)) {
        ## Default scale
        plot_params$xlim <- rev(range(points_params$x))
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
        plot_params$xlab <- "Time"
    } else {
        ## Remove xlab option from points and lines
        lines_params$xlab <- points_params$xlab <- NULL
    }
    ## ylab
    if(is.null(plot_params$ylab)) {
        ## Default ylab
        plot_params$ylab <- colnames(data$data)[trait]
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

    ## Handeling the colours
    if(missing(col)) {
        ## Default colours
        color_scheme <- vector()
    } else {
        color_scheme <- col
    }
    ## Get the missing colour schemes
    if(is.na(color_scheme["nodes"])) {
        color_scheme["nodes"] <- "orange"
    }
    if(is.na(color_scheme["fossils"])) {
        color_scheme["fossils"] <- "lightblue"
    }
    if(is.na(color_scheme["livings"])) {
        color_scheme["livings"] <- "blue"
    }
    if(is.na(color_scheme["edges"])) {
        color_scheme["edges"] <- "grey"
    }
    if(!is.na(color_scheme["tips"])) {
        ## Overwrite fossils and livings
        color_scheme[c("fossils", "livings")] <- color_scheme["tips"]
    }

    ## Plotting the frame
    plot_params <- c(plot_params, x = list(NULL), y = list(NULL))
    do.call(plot, plot_params)

    ## Plotting the tree (if needed)
    if(plot.tree) {

        ## Add the colours
        lines_params$col <- color_scheme["edges"]

        ## Make the points data table
        points_data <- cbind(points_params$x, points_params$y)[c(data$tree$tip.label, data$tree$node.labe), ]

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
    }

    ## Plotting the points
    ## Classify the points colours
    points_params$col <- vector("character", length = (Ntip(data$tree) + Nnode(data$tree)))

    ## Fill in the colours
    ## Nodes
    points_params$col[which(points_tree_IDs > Ntip(data$tree))] <- color_scheme["nodes"]
    ## Living
    if(length(livings <- which(points_ages[points_tree_IDs, "ages"] == 0)) > 0) {
        points_params$col[livings] <- color_scheme["livings"]
    }
    ## Fossils (the ones remaining)
    if(length(fossils <- which(points_params$col == "")) > 0) {
        points_params$col[fossils] <- color_scheme["fossils"]
    }
    
    ## Add the points to the plot
    do.call(points, points_params)
    return(invisible())
}