launchMtRApp <-
function(){
    shinyApp(ui = createUI(), server = createServer, options = list(launch.browser = T))
}
