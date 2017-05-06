def createStep(file) {
  return {
    stage(name: file) {
      sh "./tools/HardwareSimulator.sh projects/01/${file}.tst"
    }
  }
}
node {
  checkout scm
  files = sh(script: 'ls projects/01/*.tst | cut -d "." -f 1 | cut -d "/" -f 3', returnStdout: true).trim().split()
  project1Jobs = [:]
  for(int i = 0; i < files.length; i++) {
    file = files[i]
    project1Jobs[file] = createStep(file);
  }

  files = sh(script: 'ls projects/02/*.tst | cut -d "." -f 1 | cut -d "/" -f 3', returnStdout: true).trim().split()
  project2Jobs = [:]
  for(int i = 0; i < files.length; i++) {
    file = files[i]
    project2Jobs[file] = createStep(file);
  }

  stage(name: "project 1") {
    parallel project1Jobs
  }

  stage(name: "project 2") {
    parallel project2Jobs
  }
}
