.onLoad = function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname)
  assign("bartMachine_globals", new.env(), envir = parent.env(environment()))
}

# Returns the Java *feature* version as an integer (e.g., 8, 11, 17, 21),
# or NA_integer_ if it can't be parsed.
java_feature_version_from_runtime <- function(jv) {
  jv <- as.character(jv)[1L]
  if (is.na(jv) || !nzchar(jv)) return(NA_integer_)

  # Legacy: "1.8.0_402..." -> feature version is 8
  if (startsWith(jv, "1.")) {
    m <- regexec("^1\\.(\\d+)", jv)
    g <- regmatches(jv, m)[[1L]]
    if (length(g) >= 2L) return(as.integer(g[2L]))
    return(NA_integer_)
  }

  # Modern: starts with feature version, possibly followed by ".0.1", "+35", "-ea", etc.
  m <- regexec("^(\\d+)", jv)
  g <- regmatches(jv, m)[[1L]]
  if (length(g) >= 2L) return(as.integer(g[2L]))

  NA_integer_
}

.onAttach = function(libname, pkgname){
	jv <- tryCatch(
	  .jcall("java/lang/System", "S", "getProperty", "java.runtime.version"),
	  error = function(e) NA_character_
	)

	fv <- java_feature_version_from_runtime(jv)

	if (is.na(fv)) {
	  packageStartupMessage(
	    sprintf("Could not determine Java version from java.runtime.version='%s'. Java 21+ is required.", jv)
	  )
	} else if (fv < 21L) {
	  packageStartupMessage(
	    sprintf("Java 21+ is required, but detected feature version %d (java.runtime.version='%s').", fv, jv)
	  )
	}

  java_params = getOption("java.parameters")
  if (is.null(java_params) || !any(grepl("--add-modules=jdk.incubator.vector", java_params))){
	  packageStartupMessage("You did not set `options` correctly so bartMachine will not work. Restart R and run\n\noptions(java.parameters = c(\"-Xmx20g\", \"--add-modules=jdk.incubator.vector\", \"-XX:+UseZGC\"))\nlibrary(bartMachine)\n")
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