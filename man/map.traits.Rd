% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.traits.R
\name{map.traits}
\alias{map.traits}
\title{Maps a trait on a tree}
\usage{
map.traits(traits, tree, replicates)
}
\arguments{
\item{traits}{A \code{"traits"} object (see \code{\link{make.traits}}).}

\item{tree}{A \code{"phylo"} or \code{"multiPhylo"} object.}

\item{replicates}{Optional, a number of replicated traits to map.}
}
\value{
A \code{"treats"} object containing the tree and the traits.
}
\description{
Simulates one or more trait specified through a "traits" onto one or multiple trees.
}
\details{
This function simulates the trait(s) on the tree using the tree's branch length.
}
\examples{
## Simulating a random tree with branch length
my_tree <- rtree(20)

## Creating three different traits objects:
## A Brownian Motion
bm_process <- make.traits(process = BM.process)
## An Ornstein-Uhlenbeck process
ou_process <- make.traits(process = OU.process)
## No process (just randomly drawing values from a normal distribution)
no_process <- make.traits(process = no.process)

## Mapping the three traits on the phylogeny
bm_traits <- map.traits(bm_process, my_tree)
ou_traits <- map.traits(ou_process, my_tree)
no_traits <- map.traits(no_process, my_tree)

## Plotting the topology and the different traits
oldpar <- par(mfrow = c(2,2))
plot(my_tree, main = "Base topology")
plot(bm_traits, main = "Mapped BM")
plot(ou_traits, main = "Mapped OU")
plot(no_traits, main = "Mapped normal trait")
par(oldpar)

}
