package bartMachine;

import java.lang.instrument.Instrumentation;

/**
 * Minimal Java agent whose sole purpose is to get bart_java.jar onto the
 * system classpath before any user classes load.
 *
 * Problem: TornadoVM's ASMClassVisitor.getParallelAnnotations() calls
 *   ClassLoader.getSystemClassLoader().getResourceAsStream("bartMachine/GpuForestPredictor.class")
 * rJava adds bart_java.jar to a *child* URLClassLoader after JVM startup, so
 * the system classloader cannot see it and TornadoVM bails out to CPU fallback.
 *
 * Fix: per the Java Instrumentation spec, when a jar is listed as a -javaagent:,
 * the JVM appends it to the system class path (equivalent to
 * Instrumentation.appendToSystemClassLoaderSearch) before premain is called.
 * The premain method itself is intentionally empty.
 *
 * Usage in R (before library(bartMachine)):
 *   options(java.parameters = c(...,
 *     paste0("-javaagent:", system.file("java","bart_java.jar",package="bartMachine"))))
 */
public final class BartJarAgent {

    public static void premain(String agentArgs, Instrumentation inst) {
        // No-op: the -javaagent: mechanism already added this jar to the system
        // classpath before this method was called.
    }

    public static void agentmain(String agentArgs, Instrumentation inst) {}
}
