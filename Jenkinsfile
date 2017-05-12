def createHDLTestStep(projectNum, file) {
    echo "Creating step ${file} for project ${projectNum}"
    return {
        stage(name: file) {
            sh "./tools/HardwareSimulator.sh projects/${projectNum}/${file}.tst"
        }
    }
}

def createASMTestStep(projectNum, file) {
    echo "Creating step ${file} for project ${projectNum}"
    return {
        stage(name: file) {
            sh "for file in `ls projects/${projectNum}/*.asm`; do ./tools/Assembler.sh \$file; done"
            sh "./tools/CPUEmulator.sh projects/${projectNum}/${file}.tst"
        }
    }
}

def generateStepsForProject(projectNum, executable) {
    files = sh(script: "ls projects/${projectNum}/*.tst | cut -d '.' -f 1 | rev | cut -d '/' -f 1 | rev", returnStdout: true).trim().split()
    generatedSteps = [:]
    for(int i = 0; i < files.length; i++) {
        file = files[i]
        if(executable == "HardwareSimulator") {
            generatedSteps[file] = createHDLTestStep(projectNum, file);
        } else {
            generatedSteps[file] = createASMTestStep(projectNum, file);
        }
    }
    return generatedSteps
}

node {
    checkout scm

    names = ["01", "HardwareSimulator", "02", "HardwareSimulator", "03/a", "HardwareSimulator", "03/b", "HardwareSimulator",
    "04/fill", "CPUEmulator", "04/mult", "CPUEmulator"]

    for(int i = 0; i < names.size(); i+=2) {
        projectName = names[i]
        projectExecutable = names[i+1]
        stage(name: "Project ${projectName}") {
            parallel generateStepsForProject(projectName, projectExecutable)
        }
    }
}
