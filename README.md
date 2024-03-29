[![R-CMD-check](https://github.com/TGuillerme/treats/workflows/R-CMD-check/badge.svg)](https://github.com/TGuillerme/treats/actions)
[![codecov](https://codecov.io/gh/TGuillerme/treats/graph/badge.svg?token=NY0M0TX334)](https://codecov.io/gh/TGuillerme/treats)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![develVersion](https://img.shields.io/badge/devel%20version-1.0.3-green.svg?style=flat)](https://github.com/TGuillerme/treats/tree/master)
[![DOI](https://zenodo.org/badge/299272555.svg)](https://zenodo.org/badge/latestdoi/299272555)

<!--
[![minimal R version](https://img.shields.io/badge/R%3E%3D-4.0.0-6666ff.svg)](https://cran.r-project.org/)
[![cran version](http://www.r-pkg.org/badges/version/treats)](https://cran.r-project.org/package=treats)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/treats)](https://github.com/metacran/cranlogs.app)
![](http://cranlogs.r-pkg.org/badges/treats)
-->

# `treats`: TREes And Traits Simulation

### What is `treats`?

With `treats` you can simulate phylogenetic trees and traits at the same time.
This has can be done with other great packages such as [`FossilSim`](https://cran.r-project.org/package=FossilSim), [`PETER`](https://github.com/PuttickMacroevolution/PETER), [`RPANDA`](https://cran.r-project.org/package=RPANDA), [`TreeSim`](https://cran.r-project.org/package=TreeSim) but `treats` is designed to be super modular so you can simulate trees and traits **the way _you_ want**!
Basically you can simulate a tree depending on the traits of the tip taxa, or not, or something else!
And you can also add events to your simulations that can modify either the tree, the traits or both (or the way they influence each other).
This can be useful for simulating mass extinctions, species competition, traitspace saturation or whatever you can think of.

![](TreatYoSelf.jpg)

<a href="https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14306"><img src="http://tguillerme.github.io/images/OA.png" height="15" widht="5"/></a> 
Check out the [paper](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.14306) associated with the first version of this package.
## Still work in progress (but nearly done!)

### So what's missing?

All functionalities for the future first release (`treats v.1.0`) are now implemented and solid.
The package is now at the pre-release stage which means I am looking for minor improvements here and there before submitting it to the CRAN.

### What can you do for helping?

Please let me know if you have any suggestions about the package.
At this stage any comments are more than welcome but the main things I am looking for are:
 
 * Whether this is/could be useful for your or not?
 * Whether there is some very important functionality I've missed that you really need?
 * Whether some functionalities are cumbersome or really badly documented?
 * Typos, typos, typos (especially in the manual).

If you want to contribute, you can send me [issues](https://github.com/TGuillerme/treats/issues) or [pull requests](https://github.com/TGuillerme/treats/pulls) through GitHub or, send me an [email](mailto:guillert@tcd.ie).
And of course you will be fully acknowledged as a contributor for the package development!

## Installing `treats`

```r
if(!require(devtools)) install.packages("devtools")
library(devtools)
install_github("TGuillerme/treats")
library(treats)
```

## How does it work?

The detailed manual is available [here](http://tguillerme.github.io/treats.html).

## Latest patch notes

Patch notes can be seen [here](https://github.com/TGuillerme/treats/blob/master/NEWS.md).

## Simulation templates

If you need some inspiration, you can browse through this gallery of [simulation templates](https://github.com/TGuillerme/treats/issues?q=is%3Aopen+is%3Aissue+label%3A%22simulation+template%22).

If you want to help your fellow or future `treats` users, please share your template as a [simulation template issue](https://github.com/TGuillerme/treats/issues/new?assignees=&labels=simulation+template&projects=&template=simulation-template.md&title=Simulate+something). Don't worry about typos, language or format. The important part is that you're happy to share your template and that it's clear and simple to other users!


Authors and contributors
-------

* [Thomas Guillerme](http://tguillerme.github.io)
