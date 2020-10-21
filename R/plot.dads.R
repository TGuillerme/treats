#' @title plot
#'
#' @description plot
#'
#' @param x \code{dads} data.
#' @param trait which trait to plot (default is \code{1}).
#' @param plot.tree
#' @param simulations if the input is a \code{dads} \code{traits} object, how many replicates to run (default is \code{100}).
#' @param ...
#' 
#' @examples
#' plot.dads()
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

plot.dads <- function(x, trait = 1, plot.tree = TRUE, simulations = 50) {

    ## Renaming the x parameter (data is nicer, x is just for the S3 method standards)
    data <- x

    ## Check class dads
    check.class(data, "dads")

    if(length(dads_class <- class(data)) ==  2) {
        ## Get the second class of the dads object
        second_class <- dads_class[2]

        if(second_class == "traits") {
            ## Selecting the trait
            one_trait <- data[[trait]]

            ## Plotting the results
            plot.simulation(main = names(data)[[trait]],
                    replicate(simulations,
                        sim.motion(one_trait, steps = 100)))
        }

        return(invisible())
    }

    # if(is(dads, "matrix")) {#TG: And some dads class

    # }

    graphics::plot(42)
    return(invisible())
}