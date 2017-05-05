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
  work = [:]
  for(int i = 0; i < files.length; i++) {
    file = files[i]
    work[file] = createStep(file);
  }

  stage(name: "test") {
    parallel work
  }
}
