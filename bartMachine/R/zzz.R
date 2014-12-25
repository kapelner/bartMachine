.onLoad = function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname)
  assign("bartMachine_globals", new.env(), envir = parent.env(environment()))
}

.onAttach = function(libname, pkgname){
  num_gigs_ram_available = .jcall(.jnew("java/lang/Runtime"), "J", "maxMemory") / 1e9
  packageStartupMessage(paste("Welcome to bartMachine v", VERSION, ". ", round(num_gigs_ram_available, 2), "GB memory available\n", sep = ""))
}