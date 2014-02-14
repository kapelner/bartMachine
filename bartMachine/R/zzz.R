.onLoad = function(libname, pkgname) {
  assign("bartMachine_globals", new.env(), envir = parent.env(environment()))
}
.onAttach = function(libname, pkgname){
  packageStartupMessage("Welcome to bartMachine v1.0.3b\n")
}