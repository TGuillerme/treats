## Sampling from a bd.params object
sample.from <- function(bd_params) {

    ## Get the absolute values (or not)
    correct <- ifelse(bd_params$absolute, abs, c)

    if(!bd_params$joint) {
        return(list("speciation" = correct(bd_params$speciation()), "extinction" = correct(bd_params$extinction())))
    } else {
        ## Sampling joint values
        first <- correct(bd_params$speciation())
        second <- correct(bd_params$extinction())
        counter <- 0
        ## Resample the second value until it's good
        while(second >= first && counter != 100) {
            first <- correct(bd_params$speciation())
            second <- correct(bd_params$extinction())
            counter <- counter + 1
        }
        if(counter == 100) {
            stop("Impossible to sample a joint value with the speciation > extinction.", call. = FALSE)
        }
        return(list("speciation" = first, "extinction" = second))
    }
}
