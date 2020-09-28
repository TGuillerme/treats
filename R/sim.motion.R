## A generalised function for simulating a motion
#' @param process the motion function (e.g. a bm)
#' @param steps the length of the simulation
#' @param parameters optional, parameters for process
sim.motion <- function(process, steps, initial.value = 0, parameters = list(var = 1)) {
    
    count <- 0
    parameters$x0 <- initial.value
    output <- do.call(process, parameters)
    
    while(count < (steps-1)) {
        count <- count + 1
        parameters$x0 <- output[1]
        output <- c(do.call(process, parameters), output)
    }
    return(rev(output))
}



## Non process function
#' @param x0 ignored
#' @param fun
#' @param options
step.no.process <- function(x0, fun, options) {
    options$n <- 1
    do.call(fun, options)
}

plot.simulation(main = "Normal",
                    replicate(500,
                        sim.motion(process = step.no.process, 
                                   parameters = list(fun = rnorm, options = list(mean = 0, sd = 1)),
                                   steps = 100)))
plot.simulation(main = "LogNormal",
                    replicate(500,
                        sim.motion(process = step.no.process, 
                                   parameters = list(fun = rlnorm, options = list(meanlog = 0, sdlog = 1)),
                                   steps = 100)))
plot.simulation(main = "Gamma",
                    replicate(500,
                        sim.motion(process = step.no.process, 
                                   parameters = list(fun = rgamma, options = list(shape = 1)),
                                   steps = 100)))

plot.simulation(main = "Uniform",
                    replicate(500,
                        sim.motion(process = step.no.process, 
                                   parameters = list(fun = runif, options = list(max = 1, min = -1)),
                                   steps = 100)))


## OU motion
#' @param x0
#' @param var
step.OU <- function(x0, var, alpha, theta = 0) {
    mean <- theta + (x0 - theta) * exp(-alpha)
    sd   <- sqrt(var/(2 * alpha) * (1 - exp(-2 * alpha)))
    return(rnorm(1, mean = mean, sd = sd))
}

plot.simulation(main = "OU (alpha = 0.05)",
                    replicate(500,
                        sim.motion(process = step.OU, 
                                   parameters = list(var = 1, alpha = 0.05),
                                   steps = 100)))
