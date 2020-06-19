context("SnowFor_ETA_netlogo")
skip("too long")



test_that("NetLogo", {

  result = snowFor(1:64,function(i){
    library(RNetLogo)
    nl.path <- "C:/Program Files/NetLogo 6.0.4/app"
    nl.jarname <- "netlogo-6.0.4.jar"
    NLStart(nl.path, nl.jarname=nl.jarname,gui = F)

    model.path <- "/models/Sample Models/Biology/Blood Sugar Regulation.nlogo"
    NLLoadModel(paste(nl.path,model.path,sep=""))
    NLCommand("setup")
    NLDoCommand(100, "go")
    insulins <- NLReport("count insulins")
    NLQuit()
    insulins
  },cores = 2)



})
