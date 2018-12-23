pipeline {
    agent none
    stages {
        stage('Lint') {
            agent {
                docker {
                    image 'obraun/fun-jenkins'
                    args '-v /home/jenkins/.stack:/home/jenkins/.stack'
                }
            }   
            steps {
                sh 'hlint .'
            }
        }
        stage('Build') {
            agent {
                docker {
                    image 'obraun/fun-jenkins'
                    args '-v /home/jenkins/.stack:/home/jenkins/.stack'
                }
            }
            steps {
                sh 'stack --local-bin-path . install'
            }
        }
        stage('Test') {
            agent {
                docker {
                    image 'obraun/fun-jenkins'
                    args '-v /home/jenkins/.stack:/home/jenkins/.stack'
                }
            }
            steps {
                timeout(20) {
                    sh 'stack test'
                }
            }
        }
        stage('Build Docker Image') {
            agent {
                label 'master'
            }
            steps {
                sh "docker-build-and-push -b ${BRANCH_NAME}"
            }
        }
    }
}
