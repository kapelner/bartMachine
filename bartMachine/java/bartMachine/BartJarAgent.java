package bartMachine;

import java.io.File;
import java.io.IOException;
import java.lang.instrument.Instrumentation;
import java.util.jar.JarFile;

/**
 * Minimal Java agent whose sole purpose is to get bart_java.jar and its 
 * dependencies onto the system classpath before any user classes load.
 *
 * Problem: TornadoVM's ASMClassVisitor.getParallelAnnotations() calls
 *   ClassLoader.getSystemClassLoader().getResourceAsStream("bartMachine/GpuForestPredictor.class")
 * rJava adds bart_java.jar to a *child* URLClassLoader after JVM startup, so
 * the system classloader cannot see it and TornadoVM bails out to CPU fallback.
 *
 * Fix: per the Java Instrumentation spec, when a jar is listed as a -javaagent:,
 * the JVM appends it to the system class path (equivalent to
 * Instrumentation.appendToSystemClassLoaderSearch) before premain is called.
 * 
 * To ensure dependencies (fastutil, etc.) are also visible to classes in
 * bart_java.jar when it's on the system classpath, we manually append them
 * in premain if they are passed in agentArgs.

 *
 * Usage in R (before library(bartMachine)):
 *   deps = c(system.file("java", "fastutil-core-8.5.18.jar", package = "bartMachineJARs"), ...)
 *   agent_args = paste(deps, collapse = ":") # use ";" on Windows
 *   options(java.parameters = c(...,
 *     paste0("-javaagent:", system.file("java","bart_java.jar",package="bartMachine"), "=", agent_args)))
 */
public final class BartJarAgent {

    public static void premain(String agentArgs, Instrumentation inst) {
        System.setProperty("bartMachine.agent.active", "true");
        System.err.println("BartJarAgent: premain called with args: " + agentArgs);
        
        // The -javaagent: mechanism already added THIS jar to the system
        // classpath before this method was called.
        
        // Now add any requested dependencies to the system classpath search
        if (agentArgs != null && !agentArgs.isEmpty()) {
            for (String path : agentArgs.split(File.pathSeparator)) {
                if (path.isEmpty()) continue;
                File file = new File(path);
                if (file.exists()) {
                    try {
                        System.err.println("BartJarAgent: Adding to system classpath: " + path);
                        inst.appendToSystemClassLoaderSearch(new JarFile(file));
                    } catch (IOException e) {
                        System.err.println("BartJarAgent: Could not add " + path + " to system classpath: " + e.getMessage());
                    }
                } else {
                    System.err.println("BartJarAgent: Dependency jar not found: " + path);
                }
            }
        } else {
            System.err.println("BartJarAgent: No dependency arguments provided.");
        }
    }

    public static void agentmain(String agentArgs, Instrumentation inst) {
        premain(agentArgs, inst);
    }
}
