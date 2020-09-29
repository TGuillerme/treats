#' @title dads
#'
#' @description dads
#'
#' @param x input
#' 
#' @examples
#' dads(1)
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

dads <- function(x) {


    ## Simulating traits:
    phylocurve::sim.traits
    castor::simulate_bm_model
    caper::growTree

    http://phytools.org/eqg/Exercise_4.1/


    ## Simulating bd tree
    phytools::pbtree
    FossilSim::sim.fbd
    https://cran.r-project.org/web/packages/FossilSim/vignettes/simfbd.html
    https://lukejharmon.github.io/pcm/chapter10_birthdeath/


    return(42)
}