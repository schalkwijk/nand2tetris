def createHDLTestStep(projectNum, filePath, fileName) {
    echo "Creating step ${fileName} for project ${projectNum}"
    return {
        stage(name: fileName) {
            sh "./tools/HardwareSimulator.sh $filePath"
        }
    }
}

def createASMTestStep(projectNum, filePath, fileName) {
    echo "Creating step ${fileName} for project ${projectNum}"
    return {
        stage(name: fileName) {
            sh "for file in `ls projects/${projectNum}/**/*.asm`; do ./tools/Assembler.sh \$file; done"
            sh "./tools/CPUEmulator.sh $filePath"
        }
    }
}

def generateStepsForProject(projectNum, extension) {
    generatedSteps = [:]
    dir("projects/${projectNum}") {
        files = sh(script: "find | grep tst", returnStdout: true).trim().split()
        for(int i = 0; i < files.length; i++) {
            filePath = "${pwd()}/${files[i]}"
            fileName = filePath.split("/").last()
            if(extension == "hdl") {
                generatedSteps[fileName]  = createHDLTestStep(projectNum, filePath, fileName);
            } else {
                generatedSteps[fileName] = createASMTestStep(projectNum, filePath, fileName);
            }
        }
    }
    return generatedSteps
}

node {
    checkout scm

    names = ["01", "hdl", "02", "hdl", "03", "hdl", "04", "asm"]

    for(int i = 0; i < names.size(); i+=2) {
        projectName = names[i]
        projectExecutable = names[i+1]
        stage(name: "Project ${projectName}") {
            parallel generateStepsForProject(projectName, projectExecutable)
        }
    }
}
