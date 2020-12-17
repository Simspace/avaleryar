properties([
    buildDiscarder(
        logRotator(artifactDaysToKeepStr: '60', artifactNumToKeepStr: '3', daysToKeepStr: '60', numToKeepStr: '20')
    ),
])

/* Init Constants */
String haskellWorkDir="/home/remotejenkins/workspace/haskell-work-dir"

/* Init global variables for later use */
String userId=""

import jenkins.model.CauseOfInterruption.UserInterruption
import org.jenkinsci.plugins.workflow.steps.FlowInterruptedException

timestamps {

    // Cancel older builds.
    Run previousBuild = currentBuild.rawBuild.getPreviousBuildInProgress()
    while (previousBuild != null) {
        if (previousBuild.isInProgress()) {
            def executor = previousBuild.getExecutor()
            if (executor != null) {
                echo ">> Aborting older build #${previousBuild.number}"
                executor.interrupt(Result.ABORTED, new UserInterruption(
                    "Aborted by newer build #${currentBuild.number}"
                ))
            }
        }
        previousBuild = previousBuild.getPreviousBuildInProgress()
    }

    node ('pipelines') {
        try {
            stage('build') {
                parallel (
                failFast: true,
                    haskellBuild: {
                        withEnv(["HASKELL_WORK_DIR=${haskellWorkDir}"]) {
                            sh(script: 'mkdir -p $HASKELL_WORK_DIR')
                        }
                        dir(path: "${haskellWorkDir}/") {
                            def scmVars = checkout([
                                $class: 'GitSCM',
                                branches: [[ name: env.BRANCH_NAME ]],
                                userRemoteConfigs: [[credentialsId: '010e0c41-651f-4f83-8706-b5f4281d9e9c', url: 'git@github.com:Simspace/avaleryar.git']],
                                extensions: [[$class: 'CleanBeforeCheckout']],
                            ])

                         /* Notify committer that job is starting and where */
                         def committerEmail = sh(script: 'git --no-pager show -s --format="%ae"', returnStdout: true).trim()
                         userId = slackUserIdFromEmail("$committerEmail")
                         slackSend(color: 'good', notifyCommitters: true, message: "<@$userId> Your Job has started here: <$BUILD_URL>")

                         /* Build haskell binaries */
                         sh '''
                             # no -Werror until ghc 8.8 is on everywhere
                             stack test avaleryar avaleryar-repl --ghc-options='-Wall' --fast
                             # run the benchmarks with a 10-second timeout
                             stack bench avaleryar --fast --ba '-o ava-benchmarks.html --junit ava-benchmarks.xml --time-limit 10'
                         '''
                        }
                    }
                )
                junit 'criterion.xml'
            }
            stage('clean-up') {
                /* Notify committer that job succeeded */
                slackSend(color: 'good', notifyCommitters: true, message: "<@$userId> Your Job has succeeded. : <$BUILD_URL>")
            }

        // Catches the abort signal, want to skip the input()
        } catch(FlowInterruptedException caught) {
            stage('clean-up') {
                slackSend(color: 'good', notifyCommitters: true, message: "<@$userId> Your Job has been aborted: <$BUILD_URL>")
            }

        // Catches all other failure reasons
        } catch(caught) {
            currentBuild.result = "FAILED"
            stage('clean-up') {
                slackSend(color: 'bad', notifyCommitters: true, message: "<@$userId> Your Job has Failed. Do you want to keep the generated portal for further debugging? <$BUILD_URL>")
            }
            throw caught
        }
    }
}
