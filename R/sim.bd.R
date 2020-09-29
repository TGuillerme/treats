
#' @param speciation the speciation parameter (speciation)
#' @param extinction the extinction parameter (extinction)
#' @param max.taxa # change these to stop rules
#' @param max.time
sim.birth.death <- function(speciation, extinction, max.taxa, max.time) {

  ## Initialise the parameters
  extinct <- FALSE
  split   <- FALSE
  parent <- 0  



}


one.bd.event <- function(parent, speciation, extinction, other) {

}

pars <- c(1, 0)

make.tree.bd <- function(pars, max.taxa=Inf, max.t=Inf) {
  extinct <- FALSE
  split   <- FALSE
  parent <- 0

  speciation <- pars[1]
  extinction <- pars[2]
  turnover <- speciation + extinction
  len <- 0
  t <- 0
  n.taxa <- 1
  lineages <- 1

  pr.speciation <- speciation/(speciation + extinction)

  while ( n.taxa <= max.taxa && n.taxa > 0) {
    ## When does an event happen?
    r.n <- turnover * n.taxa
    r.tot <- sum(r.n)
    dt <- rexp(1, r.tot)
    time <- time + dt

    if ( time > max.t ) {
      dt <- dt - (time - max.t)
      len[lineages] <- len[lineages] + dt
      time <- max.t
      break
    }

    len[lineages] <- len[lineages] + dt

    ## Pick a lineage for the event to happen to:
    lineage.i <- sample(n.taxa, 1)
    lineage <- lineages[lineage.i]

    if ( runif(1) < pr.speciation ) {
      ## Speciating:
      if ( n.taxa == max.taxa )
        ## Don't add this one
        break
      new.i <- length(extinct) + 1:2
      split[lineage] <- TRUE
      extinct[new.i] <- split[new.i] <- FALSE
      parent[new.i] <- lineage
      len[new.i] <- 0

      n.taxa <- n.taxa + 1

      ## lineages <- which(!split & !extinct)
      lineages <- c(lineages[-lineage.i], new.i)
    } else {
      ## Extinct
      extinct[lineage] <- TRUE
      ## lineages <- which(!split & !extinct)
      lineages <- lineages[-lineage.i]
      n.taxa <- n.taxa - 1
    }

  }

  info <- data.frame(idx=seq_along(extinct), len=len, parent=parent,
                     extinct=extinct, split=split)
  attr(info, "t") <- t
  info
}
