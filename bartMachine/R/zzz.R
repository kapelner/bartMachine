.onLoad = function(libname, pkgname) {
  #packageStartupMessage("Welcome to BART v1.0\n")
  assign("bartMachine_globals", new.env(), envir = parent.env(environment()))
 
}
.onAttach = function(libname, pkgname){
  packageStartupMessage("Welcome to bartMachine\n")
}