.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the interim App!")
  addResourcePath('www', system.file("www", package = "interimApp"))
}
