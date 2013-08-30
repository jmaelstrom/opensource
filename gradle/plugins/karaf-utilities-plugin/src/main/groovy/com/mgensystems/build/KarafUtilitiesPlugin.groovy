package com.mgensystems.build

import org.gradle.api.Plugin
import org.gradle.api.Project

class KarafUtilitiesPlugin implements Plugin<Project> {   
    static FeatureGenerator featureGenerator = new FeatureGenerator()
    
    @Override
    public void apply(Project project) {
       if (project.rootProject.extensions.findByName("featureConfiguration") == null) {
           project.extensions.create("featureConfiguration", FeatureConfiguration)
       }
       
       project.afterEvaluate {
           if ( project.configurations.hasProperty('runtime')) {
               project.configurations.runtime.resolve().each { line ->
                  featureGenerator.addDependency(line.getAbsolutePath(), true)
               }
           }
       }
       
       if (project.rootProject.getTasksByName("generateFeatures", false).size() == 0) {
           project.rootProject.task('generateFeatures') << {
               if (project.featureConfiguration.outputLocation == null) {
                   project.featureConfiguration.outputLocation = rootDir
               }
               
			   if (project.featureConfiguration.featureFilename == null) {
				   project.featureConfiguration.featureFilename = "features.xml"
			   }
               println "Generating features.xml with the following configuration:"
               println "mainFeatureName: ${project.featureConfiguration.mainFeatureName}"
               println "subFeatureName: ${project.featureConfiguration.subFeatureName}"
               println "requiredFeatures: ${project.featureConfiguration.requiredFeatures}"
               println "excludedDependencies: ${project.featureConfiguration.excludedDependencies}"
               println "includedDependencies: ${project.featureConfiguration.includedDependencies}"
               println "outputLocation: ${project.featureConfiguration.outputLocation}"
               
               if (project.featureConfiguration.excludedDependencies != null) {
                   featureGenerator.exclusions = project.featureConfiguration.excludedDependencies
               }
               
               if (project.featureConfiguration.includedDependencies != null) {
                   project.featureConfiguration.includedDependencies.each { item -> 
                       featureGenerator.addDependency(item, false)
                   }
                   
               }
               
               featureGenerator.generate(project.featureConfiguration.mainFeatureName, project.featureConfiguration.subFeatureName, project.featureConfiguration.requiredFeatures, project.featureConfiguration.outputLocation, project.featureConfiguration.featureFilename)
           }
       } 
    }
    

}
