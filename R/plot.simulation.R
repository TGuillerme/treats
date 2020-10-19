#' @title Plot simulation
#'
#' @description 
#'
#' @param data the simulated matrix
#' @param cent.tend the central tendency (default is \code{\link[stats]{mean}})
#' @param quantiles the quantiles to display (default is \code{c(95, 50)})
#' @param ... any additional argument to be passed to \code{\link[graphics]{plot}}.
#' 
#' @examples 
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

plot.simulation <- function(data, cent.tend = mean, quantiles = c(95, 50), ...) {

    ## Get the data central tendency
    central_tendency <- apply(data, 1, cent.tend)
    ## Get the quantiles
    cis <- CI.converter(quantiles)
    n_quantiles <- length(quantiles)
    quantiles_data <- t(apply(data, 1, quantile, probs = cis))
    ## Get the length of the simulations
    n_points <- nrow(data)

    ## Set the plotting parameters
    plot_params <- list(...)
    if(is.null(plot_params$ylim)) {
        plot_params$ylim <- range(data)
    }
    if(is.null(plot_params$xlim)) {
        plot_params$xlim <- c(1, n_points)
    }
    if(is.null(plot_params$col)) {
        ## Handle colours if input col is < length(quantiles) + 1
        colfun <- grDevices::colorRampPalette(c("grey", "lightgrey"))
        plot_params$col <- c("black", rev(colfun(length(quantiles))))
    }
    if(is.null(plot_params$xlab)) {
        plot_params$xlab <- c("Time")
    }
    if(is.null(plot_params$ylab)) {
        plot_params$ylab <- c("Simulated traits")
    }


    ## Adding the empty plot:
    empty_plot <- list(x = NULL, y = NULL)
    empty_plot <- c(empty_plot, plot_params)
    do.call(plot, empty_plot)

    ## Adding the polygons
    poly_args <- list(border = "NA", density = NULL)
    poly_args <- c(poly_args, plot_params)
    poly_args$x <- c(1:n_points)
    poly_args$x <- c(poly_args$x, rev(poly_args$x))

    ## Loop through the polygons
    for (one_ci in 1:n_quantiles) {
        ## Select the quantiles columns
        quantiles_col <- get.quantile.col(one_ci, n_quantiles)
        ## Set the y values
        poly_args$y <- quantiles_data[, quantiles_col[1]]
        poly_args$y <- c(poly_args$y, rev(quantiles_data[, quantiles_col[2]]))
        ## Set up the colour
        poly_args$col <- plot_params$col[one_ci+1]

        ## Plot the polygon
        do.call(polygon, poly_args)
    }

    ## Add the central tendency
    line_args <- list(x = 1:n_points, y = central_tendency)
    if(is.null(plot_params$lty)) {
        line_args$lty <- 1
    } else {
        line_args$lty <- plot_params$lty
    }
    line_args$col <- plot_params$col[1]

    ## Add the central tendency
    do.call(lines, line_args)
    
    return(invisible())
}


CI.converter <- function(CI) {
    sort(c(50-CI/2, 50+CI/2)/100)
}

get.quantile.col <- function(cis, n_quantiles) {
    return(c(
        ## Lower quantile
        cis,
        ## Higher quantile
        (n_quantiles*2) - (cis-1)
        ))
}
