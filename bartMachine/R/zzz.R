.onLoad = function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname)
  
  #need to check if proper Java is installed by special request of Prof Brian Ripley
  jv = .jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
  major_version = as.numeric(strsplit(jv, "[.]")[[1L]][1])
  if (major_version == 1){
	  major_version = as.numeric(strsplit(jv, "[.]")[[1L]][2])
  }
  if (major_version < 21){
	  warning("Java 21 (at minimum) is needed for this package but is does not seem to be available. This message may be in error; apologies if it is.")
  }
  
  assign("bartMachine_globals", new.env(), envir = parent.env(environment()))
}

.onAttach = function(libname, pkgname){
  java_params = getOption("java.parameters")
  if (is.null(java_params) || !any(grepl("--add-modules=jdk.incubator.vector", java_params))){
	  packageStartupMessage("You did not set `options` correctly so bartMachine will not work. Restart R and run\n\noptions(java.parameters = c(\"-Xmx20g\", \"--add-modules=jdk.incubator.vector\", \"-XX:+UseZGC\")))\nlibrary(bartMachine)\n")
  } else {
	  num_gigs_ram_available = .jcall(.jnew("java/lang/Runtime"), "J", "maxMemory") / 1e9
	  more_memory_message = if (num_gigs_ram_available < 10){
												  	"\n\nIf you run out of memory, restart R, and use \n\n'options(java.parameters = \"-Xmx20g\")' for 20GB of RAM before you call\n'library(bartMachine)'.\n"
												  } else {
												  	""
												  }
	  packageStartupMessage(
			paste("Welcome to bartMachine v", packageVersion("bartMachine"), 
					"! You have ", round(num_gigs_ram_available, 2), 
					"GB memory available.", 
					more_memory_message,
					"You can safely ignore the \"WARNING: Using incubator modules: jdk.incubator.vector\" above.",
					sep = "")
	  )
  }
}