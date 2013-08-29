import groovy.xml.MarkupBuilder

import java.io.File
import java.io.StringWriter
import java.util.List
import java.util.jar.Attributes

class FeatureGenerator {
    def rawDependencies = [:]
    def exclusions = []
	def key = new Attributes.Name("Bundle-ManifestVersion")
	
    def addDependency(String line, boolean format) {
        def formattedString = ""
        
        if (format) {
            formattedString = formatLine(line)
        } else {
            formattedString = line
        }
        
        if (!formattedString.endsWith("bundle")) {
			if (format) {
				rawDependencies[formattedString] = isBundle(line)
			} else {
				rawDependencies[formattedString] = false
			}
        }
    
    }

	def isBundle (String line) {
		def jarFile = new java.util.jar.JarFile(line)
		return jarFile.manifest.mainAttributes.containsKey(key)
	}
	
    def String formatLine(String line) {
        def words
        def show = false
        def outString = ""
		
		line = line.replaceAll("\\\\","/")
		
        words=line.split('/')

        words.each { word ->
			
            if (word == "filestore" && show == false) {
                show = true
            }

            if (word == "jar" || word == "bundle") {
                show = false
                outString += "${word}"
            }

            if (show && word != "filestore") {
                outString += "${word}/"
            }
        }

        return outString;
    }

    def void generate(String mainFeatureName, String subFeatureName, List features, String outputLocation, String fileName) {
        def writer = new StringWriter()
        def xml = new MarkupBuilder(writer)
        def parts

        removeExclusions()
        
        xml.features(name:subFeatureName) {
            feature (name:subFeatureName, version:'1.0') {
                
                features.each { item ->
                    parts = item.split(':')
                    feature (version:parts[1], parts[0])
                }
               
                rawDependencies.sort().each {
                    if ("wrap:mvn:${it.key}" != "wrap:mvn:" && "mvn:${it.key}" != "mvn:") {
						if (it.value) {
							bundle("mvn:${it.key}")
						} else {
                        	bundle("wrap:mvn:${it.key}")
						}
						println "Adding dependency: ${it.key}"
                    } 
                }
            }
        }

        def destination = new File(outputLocation + File.separator + fileName)
        
        println "Writing ${fileName} to ${destination}"
        
        destination.write writer.toString()
    }
    
    def void removeExclusions() {
		exclusions.each {
			rawDependencies.remove(it)
		}
    }
}