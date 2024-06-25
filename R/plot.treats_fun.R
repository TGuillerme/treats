## Simulates a process based on a traits object
sim.motion <- function(one_trait, steps) {    
    ## Initialising the simulation
    output <- one_trait$start
    count <- 1
    process <- one_trait$process[[1]]
    parameters <- one_trait
    if(!is.null(parameters$process.args)) {
        parameters <- parameters$process.args[[1]]    
    }
    parameters$x0 <- one_trait$start
    parameters$process <- NULL
    parameters$trait_id <- NULL
    parameters$start <- NULL
    parameters$condition.test <- NULL
    ## First step
    output <- rbind(do.call(process, parameters), output)
    
    ## Following steps
    while(count < (steps-1)) {
        count <- count + 1
        parameters$x0 <- output[1, ]
        output <- rbind(do.call(process, parameters), output)
    }
    return(output[rev(1:nrow(output)), , drop = FALSE])
}

# ' @title Plot simulation
# '
# ' @description 
# '
# ' @param data the simulated matrix
# ' @param cent.tend the central tendency (default is \code{\link[stats]{mean}})
# ' @param quantiles the quantiles to display (default is \code{c(95, 50)})
# ' @param ... any additional argument to be passed to \code{\link[graphics]{plot}}.

internal.plot.simulation <- function(data, cent.tend, quantiles, col, use.3D, trait, trait.name, ...) {
    
    ## Whether to use 3D plots or not
    is_1D <- ncol(data[[1]]) == 1
    do_3D <- (use.3D && !is_1D)
    ## Selecting the traits
    if(do_3D) {
        trait <- trait[1:2]
    } else {
        trait <- trait[1]
    }

    ## Combine the data together per column
    data_cols <- list()
    data_cols[[1]] <- do.call(cbind, lapply(data, function(x, column) return(x[, column]), column = trait[1]))
    if(do_3D) {
        data_cols[[2]] <- do.call(cbind, lapply(data, function(x, column) return(x[, column]), column = trait[2]))
    }

    ## Get the length of the simulations
    n_points <- nrow(data_cols[[trait[1]]])

    if(!do_3D) {
        ## Get the data central tendency
        central_tendency <- apply(data_cols[[1]], 1, cent.tend)
    
        ## Get the quantiles
        cis <- CI.converter(quantiles)
        n_quantiles <- length(quantiles)
        quantiles_data <- t(apply(data_cols[[1]], 1, quantile, probs = cis))

        ## Set the plotting parameters
        plot_params <- list(...)
        if(is.null(plot_params$ylim)) {
            plot_params$ylim <- range(data_cols[[1]])
        }
        if(is.null(plot_params$xlim)) {
            plot_params$xlim <- c(1, n_points)
        }
        if(is.null(plot_params$xlab)) {
            plot_params$xlab <- c("Time")
        }
        if(is.null(plot_params$ylab)) {
            plot_params$ylab <- c("Simulated traits")
        }
        if(is.null(plot_params$main)) {
            plot_params$main <- trait.name
        }

        ## Handling colors
        if(missing(col)) {
            col <- "default"
        }
        if(col[1] == "default") {
            ## Handle colours if input col is < length(quantiles) + 1
            colfun <- grDevices::colorRampPalette(c("grey", "lightgrey"))
            plot_params$col <- c("black", rev(colfun(length(quantiles))))
        } else {
            plot_params$col <- col
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
    } else {

        ## 3D plot
        ## Set the plotting parameters
        plot_params <- list(...)
        if(is.null(plot_params$ylim)) {
            plot_params$ylim <- range(data_cols[[1]])
        }
        if(is.null(plot_params$zlim)) {
            plot_params$zlim <- range(data_cols[[2]])
        }
        if(is.null(plot_params$xlim)) {
            plot_params$xlim <- c(1, n_points)
        }
        if(is.null(plot_params$xlab)) {
            plot_params$xlab <- c("Time")
        }
        if(is.null(plot_params$ylab)) {
            plot_params$ylab <- c("Simulated traits (D1)")
        }
        if(is.null(plot_params$zlab)) {
            plot_params$zlab <- c("Simulated traits (D2)")
        }
        if(is.null(plot_params$pch)) {
            plot_params$pch <- 19
        }

        ## Get the values sorted
        plot_params$x <- rep(1:n_points, ncol(data_cols[[1]]))
        plot_params$y <- c(data_cols[[1]])
        plot_params$z <- c(data_cols[[2]])

        ## Plot the results
        do.call(rgl::plot3d, plot_params)
    }
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

handle.colours <- function(col, points_tree_IDs, points_ages, data, legend) {

    legend_col <- NULL
    do_gradient <- FALSE
    use_scheme <- FALSE
    ## Default colour scheme
    col_scheme <- character()
    col_scheme["nodes"]   <- "orange"
    col_scheme["fossils"] <- "lightblue"
    col_scheme["livings"] <- "blue"

    if(is(col, "function")) {
        ## Make a colour gradient
        do_gradient <- TRUE
        ## Check if the function works
        col.fun <- col
        test <- try(col.fun(Ntip(data$tree)+Nnode(data$tree)), silent = TRUE)
        if(is(test, "try-error") || length(test) != Ntip(data$tree)+Nnode(data$tree) || !is(test,  "character")) {
            stop("The col function failed to generate a list of colours.", call. = FALSE)
        }
    } else {
        if(col[1] != "default") {
            if(is(col,  "character") && length(col) != Ntip(data$tree)+Nnode(data$tree)) {
                ## col is an unamed vector
                if(!is.null(names(col))) {
                    allowed_names <- c("nodes", "tips", "fossils", "livings", "singletons")
                    if(!all(names(col) %in% allowed_names)) {
                        stop(paste0("If col is a named vector, it must contains the names ", paste0(allowed_names[-length(allowed_names)], collapse = ", "), " and/or ", allowed_names[length(allowed_names)], "."))
                    }
                    use_scheme <- TRUE

                    ## Attributing the colours to each element
                    if("nodes" %in% names(col)) {
                        col_scheme["nodes"] <- col["nodes"]
                    }
                    if("fossils" %in% names(col)) {
                        col_scheme["fossils"] <- col["fossils"]
                    }
                    if("livings" %in% names(col)) {
                        col_scheme["livings"] <- col["livings"]
                    }
                    if("tips" %in% names(col)) {
                        col_scheme["fossils"] <- col["tips"]
                        col_scheme["livings"] <- col["tips"]
                    }
                    if("singletons" %in% names(col)) {
                        col_scheme["singletons"] <- col["singletons"]
                    }
                }
            }
        } else {
            use_scheme <- TRUE
        }
    }

    if(!do_gradient) {
        if(use_scheme) {
            ## Creating the colour values
            col_val <- vector("character", length = (Ntip(data$tree) + Nnode(data$tree)))

            ## Nodes
            col_val[which(points_tree_IDs > Ntip(data$tree))] <- col_scheme["nodes"]
            ## Singletons
            if(!is.na(col_scheme["singletons"])) {
                ## Detect the singletons
                nodes_singles <- data$tree$edge[!(data$tree$edge[,1] %in% data$tree$edge[duplicated(data$tree$edge[,1]), 1]), 1]
                ## Adjust the colour values
                col_val[which(points_tree_IDs %in% nodes_singles)] <- col_scheme["singletons"]
            }

            ## Living
            if(length(livings <- which(points_ages[points_tree_IDs, "ages"] == 0)) > 0) {
                col_val[livings] <- col_scheme["livings"]
            }
            ## Fossils (the ones remaining)
            if(length(fossils <- which(col_val == "")) > 0) {
                col_val[fossils] <- col_scheme["fossils"]
            }
            ## Set the legend
            if(legend) {
                ## Select the available data 
                legend_col <- col_scheme[col_scheme %in% unique(col_val)]
            }
        } else {
            ## Apply the colours to all elements
            if(length(col) == (Ntip(data$tree) + Nnode(data$tree))) {
                col_val <- col
            } else {
                if(length(col) > (Ntip(data$tree) + Nnode(data$tree))) {
                    col_val <- col[1:(Ntip(data$tree) + Nnode(data$tree))]
                    warning(paste0("Only the first ", (Ntip(data$tree) + Nnode(data$tree)), " colours are used."))
                }
                if(length(col) < (Ntip(data$tree) + Nnode(data$tree))) {
                    col_val <- rep(col, ceiling((Ntip(data$tree) + Nnode(data$tree))/length(col)))[1:(Ntip(data$tree) + Nnode(data$tree))]
                }
            }
            ## Reorder
            col_val <- col_val[points_tree_IDs]
            legend_col <- col
        }
    } else {
        ## Sort the data by range
        histo <- hist(points_ages[points_tree_IDs, "ages"], plot = FALSE)
        n_col <- length(histo$counts)

        ## Get the colour gradient
        avail_cols <- rev(col.fun(n_col))

        ## Fill in the first (last) colour
        col_val <- rep(avail_cols[length(avail_cols)], length(points_ages[points_tree_IDs, "ages"]))

        ## Add the other colours
        for(colour in rev(1:n_col)) {
            col_val[(points_ages[points_tree_IDs, "ages"] <= histo$breaks[colour])] <- avail_cols[colour]
        }

        ## Get the colour scheme gradient
        if(legend) {
            if(length(avail_cols) < 2) {
                warning("Impossible to plot the legend for the colour scheme (only one gradient colour available).")
                legend_col <- NULL
            } else {
                legend_col <- list()
                legend_col$raster <- as.raster(matrix(col(n_col), ncol = 1))
                legend_col$labels <- rev(sort(c(range(histo$breaks), max(histo$breaks)/2)))
            }
        }
    }

    ## Identify each point
    names(col_val) <- points_ages[points_tree_IDs, "elements"]

    return(list(col = col_val, legend = legend_col))
}
