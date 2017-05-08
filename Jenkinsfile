def createStep(projectNum, file) {
  echo "Creating step ${file} for project ${projectNum}"
  return {
    stage(name: file) {
      sh "./tools/HardwareSimulator.sh projects/${projectNum}/${file}.tst"
    }
  }
}

def generateStepsForProject(projectNum) {
  files = sh(script: "ls projects/${projectNum}/*.tst | cut -d '.' -f 1 | rev | cut -d '/' -f 1 | rev", returnStdout: true).trim().split()
  generatedSteps = [:]
  for(int i = 0; i < files.length; i++) {
    file = files[i]
    generatedSteps[file] = createStep(projectNum, file);
  }
  return generatedSteps
}

node {
  checkout scm

  projects = ["01", "02", "03/a", "03/b"]
  for(project in projects) {
    stage(name: "Project ${project}") {
      parallel generateStepsForProject(project)
    }
  }
}
