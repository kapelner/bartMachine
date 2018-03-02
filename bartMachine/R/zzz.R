.onLoad = function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname)
  
  #need to check if proper Java is installed by special request of Prof Brian Ripley
  jv = .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
  if (substr(jv, 1L, 1L) == "1") {
	  jvn = as.numeric(paste0(strsplit(jv, "[.]")[[1L]][1:2], collapse = "."))
	  if (jvn < 1.7){
	  	stop("Java 7 (at minimum) is needed for this package but is not available.")
	  }		
  }
  
  assign("bartMachine_globals", new.env(), envir = parent.env(environment()))
}

.onAttach = function(libname, pkgname){
  num_gigs_ram_available = .jcall(.jnew("java/lang/Runtime"), "J", "maxMemory") / 1e9
  packageStartupMessage(
	paste("Welcome to bartMachine v", VERSION, 
			"! You have ", round(num_gigs_ram_available, 2), 
			"GB memory available.\n\n", 
			"If you run out of memory, restart R, and use e.g.\n'options(java.parameters = \"-Xmx5g\")' for 5GB of RAM before you call\n'library(bartMachine)'.\n",
			sep = "")
  )
}