// use syntax which is valid both in Groovy and in Kotlin
doLast {
     println("id=${project.group}:${project.name}:${project.version}")
     if (project.parent != null) {
       println("parent-id=${project.parent.group}:${project.parent.name}:${project.parent.version}")
     }
     println("file=${project.buildFile}")
     if (project.hasProperty("archivesBaseName")) { // defined by java plugin
         println("final-name=" + project.configurations.getByName("archives").artifacts.stream()
           .filter { it.type.equals("jar") || it.type.equals("war") }
           .map { it.file.path }
           .findFirst()
           .orElse(""))
     } else {
         println("final-name=")
     }
     println("build-dir=${project.buildDir}")
     if (project.hasProperty("sourceSets")) { // defined by java plugin
       println("source-dirs=" + project.sourceSets.stream()
         .flatMap { it.allJava.srcDirs.stream().map { it.path } }
         .collect(Collectors.joining(File.pathSeparator)))
       println("dep-jars=" + project.sourceSets.stream()
         .flatMap { it.compileClasspath.files.stream().filter { it.name.endsWith("jar") }.map { it.path } }
         .collect(Collectors.joining(File.pathSeparator)))
     } else {
       println("source-dirs=")
       println("dep-jars=")
     }
}
