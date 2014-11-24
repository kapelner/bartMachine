.onLoad = function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname)
  assign("bartMachine_globals", new.env(), envir = parent.env(environment()))
}

.onAttach = function(libname, pkgname){
  packageStartupMessage(paste("Welcome to bartMachine v", VERSION, "\n", sep = ""))
}