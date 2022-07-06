.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is an atpolR package")
  invisible()
}

.onLoad <- function(lib, pkg){
  Rdpack::Rdpack_bibstyles(package = pkg, authors = "LongNames")
  invisible(NULL)
}
