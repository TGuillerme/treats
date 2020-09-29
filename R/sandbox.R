# http://phytools.org/eqg/Exercise_4.1/


# t <- 0:100  # time
# sig2 <- 0.01
# ## first, simulate a set of random deviates
# x <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
# ## now compute their cumulative sum
# x <- c(0, cumsum(x))
# plot(t, x, type = "l", ylim = c(-2, 2))



## A generalised function for simulating a motion
#' @param fun the motion function (e.g. a bm)
#' @param length the length of the simulation
#' @param parameters optional, parameters for fun
sim.motion <- function(fun, length, initial.value = 0, parameters = list(var = 1)) {
    
    count <- 0
    parameters$x0 <- initial.value
    output <- do.call(fun, parameters)
    
    while(count < length) {
        count <- count + 1
        parameters$x0 <- output[1]
        output <- c(do.call(fun, parameters), output)
    }
    return(rev(output))
}

## Brownian motion
#' @param x0
#' @param var
step.BM <- function(x0, var) {
    return(rnorm(1, mean = x0, sd = sqrt(var)))
}

plot(NULL, xlim = c(0, 100), ylim = c(-50, 50))
replicate(100, lines(0:100, sim.motion(fun = step.BM, length = 100)))

## 

## BM: one parameter -- s2
make.br.bm <- function(pars) {
  if ( pars <= 0 )
    stop("s2 must be positive")
  function(x0, t)
    rnorm(length(t), x0, sqrt(pars*t))
}

## OU: three parameters -- s2, alpha, theta
make.br.ou <- function(pars) {
  s2    <- pars[1]
  alpha <- pars[2]
  theta <- pars[3]
  if ( s2 <= 0 )
    stop("s2 must be positive")
  if ( alpha <= 0 )
    stop("alpha must be positive")

  function(x0, t)  
    rnorm(length(t),
          theta + (x0 - theta)*exp(-alpha*t),
          sqrt(s2/(2*alpha) * (1 - exp(-2*alpha*t))))
}

step.OUM <- function(distribution, arguments) {
    arguments$n <- 1

    
    arguments$mena <- theta + (mean - theta) * exp(-alpha * t)
    

    return(do.call(distribution, arguments))
}


