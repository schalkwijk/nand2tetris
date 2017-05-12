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

def generateStepsForProject(projectNum, executable) {
    dir("projects/${projectNum}") {
        files = sh(script: "find | grep tst", returnStdout: true).trim().split()
        generatedSteps = [:]
        for(int i = 0; i < files.length; i++) {
            filePath = "${pwd()}/${files[i]}"
            fileName = filePath.split("/").last()
            if(executable == "HardwareSimulator") {
                generatedSteps[fileName]  = createHDLTestStep(projectNum, filePath, fileName);
            } else {
                generatedSteps[fileName] = createASMTestStep(projectNum, filePath, fileName);
            }
        }
        return generatedSteps
    }
}

node {
    checkout scm

    names = ["01", "HardwareSimulator", "02", "HardwareSimulator", "03", "HardwareSimulator",
    "04", "CPUEmulator"]

    for(int i = 0; i < names.size(); i+=2) {
        projectName = names[i]
        projectExecutable = names[i+1]
        stage(name: "Project ${projectName}") {
            parallel generateStepsForProject(projectName, projectExecutable)
        }
    }
}
