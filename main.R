library(plumber)
r=plumb("deploy_svm.R")
r$run(port=80,host="0.0.0.0")