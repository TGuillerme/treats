## rgl quartz deactivate
.onAttach <- function(libname = find.package("treats"), pkgname = "treats") {
    options(rgl.useNULL=TRUE)
}