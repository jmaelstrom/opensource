package com.oec.build

import java.util.List;

class FeatureConfiguration {
    String mainFeatureName
    String subFeatureName
    List requiredFeatures
    List excludedDependencies
    List includedDependencies
    String outputLocation
	String featureFilename
    //static FeatureGenerator featureGenerator = new FeatureGenerator()
}