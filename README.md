[![Build Status](https://travis-ci.org/TGuillerme/treats.svg?branch=master)](https://travis-ci.org/TGuillerme/treats)
[![R-CMD-check](https://github.com/TGuillerme/treats/workflows/R-CMD-check/badge.svg)](https://github.com/TGuillerme/treats/actions)
[![codecov](https://codecov.io/gh/TGuillerme/treats/branch/master/graph/badge.svg)](https://codecov.io/gh/TGuillerme/treats)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![develVersion](https://img.shields.io/badge/devel%20version-0.1.12-green.svg?style=flat)](https://github.com/TGuillerme/treats/tree/master)
[![DOI](https://zenodo.org/badge/299272555.svg)](https://zenodo.org/badge/latestdoi/299272555)


# `treats`: TREes And Traits Simulation

**REPO NAME CHANGE IN PROGRESSE**

### What is `dads`?

With `dads` you can simulate phylogenetic trees and traits at the same time time.
This has been done before but `dads` is designed to be super modular so you can simulate data the way _you_ want!
Basically you can simulate a tree depending on the traits of the tip taxa, or not, or something else!
And you can also add events to your simulations that can modify either the tree, the traits or both (or the way they influence each other).
This can be useful for simulating mass extinctions, species competition, traitspace saturation or whatever you can think of.

### So what's missing?

Basically some bits and bobs for CRAN requirements, some more unit testing coverage and a biiiiiig typo check.
But all functionalities for the future first release (`dads v.1.0`) are now implemented and solid.
The package is now at the pre-release stage which means I am looking for minor improvements here and there before submitting it to the CRAN.

### What can you do for helping?

Please let me know if you have any suggestions about the package.
At this stage any comments are more than welcome but the main things I am looking for are:
 
 * Whether this is/could be useful for your or not?
 * Whether there is some very important functionality I've missed that you really need?
 * Whether some functionalities are cumbersome or really badly documented?
 * Typos, typos, typos (especially in the manual).

If you want to contribute, you can send me [issues](https://github.com/TGuillerme/dads/issues) or [pull requests](https://github.com/TGuillerme/dads/pulls) through GitHub or, send me an [email](mailto:guillert@tcd.ie).
And of course you will be fully acknowledged as a contributor for the package development!

## Installing `dads`

```r
if(!require(devtools)) install.packages("devtools")
library(devtools)
install_github("TGuillerme/dads")
library(dads)
```

## How does it work?

The detailed manual is available [here](http://tguillerme.github.io/dads.html).

## Latest patch notes

Patch notes can be seen [here](https://github.com/TGuillerme/dads/blob/master/NEWS.md).

Authors and contributors
-------

* [Thomas Guillerme](http://tguillerme.github.io)
