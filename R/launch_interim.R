#' launch_interim - Launches the interim ,application
#'
#' @export
#'
#' @description
#' Starts the interim application in the client's browser.
#'
#' @param host host link (defaults to the local machine "127.0.0.1")
#' @param port port number (randomly chosen unless specified as a certain number)
#' @param browser path to browser exe (defaults to standard browser)
#'
#' @keywords interim
#'
#' @details Further information on how to use this application can be found in the vignette of this package.
#'
#' @examples
#' \dontrun{
#' ## Launch application on localhost (127.0.0.1)
#' ## -------------------------------------------
#' ## By default launch_interim starts the application on localhost
#' ## and a randomly selected port (e.g. 9876), in which case you can connect
#' ## to the running application by navigating your browser to
#' ## http://localhost:9876.
#' launch_interim()
#'
#' ## Launch application on a different host
#' ## --------------------------------------
#' ## You can also run the application on a different host
#' ## by specifying a hostname and port. Just make sure to
#' ## use an open port on your machine. Here "open" means
#' ## that the port should not be used by another service
#' ## and the port is opened by your firewall.
#' launch_interim(host="your-hostname", port=8888)
#'
#'
#' ## Make the application available to your coworkers
#' ## ------------------------------------------------
#' ## within your local area network even without a
#' ## dedicated Shiny server. The value set through the
#' ## host argument says to accept any connection (not just from localhost).
#' ## Then take note of your local IP (if you are under linux,
#' ## you can see it through ifconfig). Say your IP is 192.168.1.70.
#' ## Your colleagues can use your app by inserting in the address
#' ## bar of their browser 192.168.1.70:8888, i.e. your IP followed
#' ## by : and the port number you selected.
#' launch_interim(host="0.0.0.0", port=8888)
#'
#' ## Launch application on a different browser
#' ## ----------------------------------------
#' ## To run the shiny app on a different browser than your standard browser
#' ## use the "browser" argument to set the path to the respective .exe file.
#' launch_interim(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")
#' }
#'
#' @import stats
#' @import graphics
#' @import shiny
#' @import shinyBS
#' @import interim
#' @return A shiny app

launch_interim <- function(host = "127.0.0.1", port = NULL, browser=NULL) {

  if (!requireNamespace("interim", quietly = TRUE)) {
    message("Error: launch_interim requires the package interim to be installed")
    stop()
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    message("Error: launch_interim requires the package shiny to be installed")
    stop()
  }
  if (!requireNamespace("shinyBS", quietly = TRUE)) {
    message("Error: launch_interim requires the package shinyBS to be installed")
    stop()
  }

  #### Server ####

  server <- shinyServer(function(input, output) {

    e1Col="#81a5c9" # lightblue
    e2Col="#81a5c9" # lightblue
    coCol="#5ca754" # green
    ccCol="#cb4b41" # red
    scCol="#165b97" # darkblue
    enCol="#597dae" # midblue
    t1Col="#81a5c9" # lightblue
    t2Col="#81a5c9" # lightblue

    output$cntrPlot <- renderPlot({
      req(input$ncinf,input$nsinf,input$cw,input$sw,input$sf,input$tb,input$en)
      if((input$ncinf==TRUE)&(input$nsinf==TRUE)){
        x=recruitment(nc=Inf,ns=Inf,cw=input$cw,sw=input$sw,sf=input$sf,tb=input$tb,en=input$en)
      }
      else if((input$ncinf==FALSE)&(input$nsinf==TRUE)){
        x=recruitment(nc=input$nc,ns=Inf,cw=input$cw,sw=input$sw,sf=input$sf,tb=input$tb,en=input$en)
      }
      else if((input$ncinf==TRUE)&(input$nsinf==FALSE)){
        x=recruitment(nc=Inf,ns=input$ns,cw=input$cw,sw=input$sw,sf=input$sf,tb=input$tb,en=input$en)
      }
      else if((input$ncinf==FALSE)&(input$nsinf==FALSE)){
        x=recruitment(nc=input$nc,ns=input$ns,cw=input$cw,sw=input$sw,sf=input$sf,tb=input$tb,en=input$en)
      }

      centerPlot(r=x)

    },bg="transparent")

    output$trtPlot <- renderPlot({
      req(input$ncinf,input$nsinf,input$cw,input$sw,input$sf,input$tb,input$en)
      if((input$ncinf==TRUE)&(input$nsinf==TRUE)){
        x=recruitment(nc=Inf,ns=Inf,cw=input$cw,sw=input$sw,sf=input$sf,tb=input$tb,en=input$en)
      }
      else if((input$ncinf==FALSE)&(input$nsinf==TRUE)){
        x=recruitment(nc=input$nc,ns=Inf,cw=input$cw,sw=input$sw,sf=input$sf,tb=input$tb,en=input$en)
      }
      else if((input$ncinf==TRUE)&(input$nsinf==FALSE)){
        x=recruitment(nc=Inf,ns=input$ns,cw=input$cw,sw=input$sw,sf=input$sf,tb=input$tb,en=input$en)
      }
      else if((input$ncinf==FALSE)&(input$nsinf==FALSE)){
        x=recruitment(nc=input$nc,ns=input$ns,cw=input$cw,sw=input$sw,sf=input$sf,tb=input$tb,en=input$en)
      }

      if(input$treat==TRUE){
        y=treatment(r=x,du=input$du,dr=input$dr)
        treatmentPlot(r=x,t1=y)
      }
      else if(input$tte==TRUE){
        e=event(r=x,er=input$er,dr=input$edr,du=input$du)
        eventPlot(r=x,e1=e)
      }
      else{
        treatmentPlot(r=x)
      }

      if((input$cross == TRUE)&(input$treat==TRUE)){
        trialWeek(t=y,p=input$p)
        cross(w=trialWeek(t=y,p=input$p),p=input$p)
      }
      else if((input$cross == TRUE)&(input$tte==TRUE)){
        eventWeek(t=e,p=input$p)
        cross(w=eventWeek(t=e,p=input$p),p=input$p)
      }


    },bg="transparent")

    #' Table:
    output$table <- renderTable({
      req(input$ncinf,input$nsinf,input$cw,input$sw,input$sf,input$tb,input$en)
      if((input$ncinf==TRUE)&(input$nsinf==TRUE)){
        x=recruitment(nc=Inf,ns=Inf,cw=input$cw,sw=input$sw,sf=input$sf,tb=input$tb,en=input$en)
      }
      else if((input$ncinf==FALSE)&(input$nsinf==TRUE)){
        x=recruitment(nc=input$nc,ns=Inf,cw=input$cw,sw=input$sw,sf=input$sf,tb=input$tb,en=input$en)
      }
      else if((input$ncinf==TRUE)&(input$nsinf==FALSE)){
        x=recruitment(nc=Inf,ns=input$ns,cw=input$cw,sw=input$sw,sf=input$sf,tb=input$tb,en=input$en)
      }
      else if((input$ncinf==FALSE)&(input$nsinf==FALSE)){
        x=recruitment(nc=input$nc,ns=input$ns,cw=input$cw,sw=input$sw,sf=input$sf,tb=input$tb,en=input$en)
      }

      if(input$treat==TRUE){
        y=treatment(r=x,du=input$du,dr=input$dr)
      }
      else if(input$tte==TRUE){
        e=event(r=x,er=input$er,dr=input$edr,du=input$du)
        e$weeksOfEnrollment<-NULL
        e$enrollinstant<-NULL
        e
      }
      else{
        x
      }

    },bg="#424242")

    #' Re-active UI
    output$Center <- renderUI({
      req(input$ncinf)
      if(input$ncinf!=TRUE){
        numericInput("nc", "Maximum number of centers to be opened:", 300)
      }
    })

    output$Patients <- renderUI({
      req(input$nsinf)
      if(input$nsinf!=TRUE){
        numericInput("ns", "Maximum number of patients to be screened within each center:", 100)
      }
    })

    output$Capacity <- renderUI({
      req(input$ncinf,input$nsinf)
      if((input$ncinf==FALSE)&(input$nsinf==FALSE)){
        sliderInput("en", "Number of patients to be enrolled:",min=0, max=capacity(nc=input$nc,ns=input$ns,sf=input$sf), value=400)
      }
      else{
        numericInput("en", "Number of patients to be enrolled:", 700)
      }
    })

    output$Treatment <- renderUI({
      req(input$treat)
      if(input$treat == TRUE){
        list(
          numericInput("du", "Duration of treatment phase in weeks:", 12),
          numericInput("dr", "Drop-out rate during the treatment phase:", 0.05)
        )
      }
    })

    output$TimeToEvent <- renderUI({
      req(input$tte)
      if(input$tte == TRUE){
        list(
          numericInput("er", "Event rate:", 0.12),
          numericInput("edr", "Drop-out rate during the event phase:", 0.08),
          numericInput("du", "Duration of event phase:", 50)
        )
      }
    })

    output$InterimAnalysis <- renderUI({
      req(input$cross, input$treat)
      if((input$cross == TRUE)&(input$treat==TRUE)){
        numericInput("p", "Required number of patients:", 300)
      }
      else if((input$cross == TRUE)&(input$tte==TRUE)){
        numericInput("p", "Required number of events:", 200)
      }

    })

    output$info <- renderText({
      req(input$treat,input$tte)
      if(input$treat==TRUE){
        paste0("Week = ", input$plot_hover$x, "\nNumber of Patients = ", input$plot_hover$y)
      }
      else if(input$tte==TRUE){
        paste0("Week = ", input$plot_hover$x, "\nNumber of Events = ", input$plot_hover$y)
      }

    })

  })

  #### User Interface (UI) ####


  ui <- shinyUI(fluidPage(theme = "bootstrap.css",
                          tags$style(type = 'text/css',HTML(
                            'body {background-color: #383838; color: #6b6b6b;}',
                            '.nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {background-color: #424242;border: 1px solid #424242; color: #fff; font-weight: bold}',
                            '.nav-tabs > li > a, .nav-tabs > li > a:focus, .nav-tabs > li > a:hover {background-color: transparent; color: #6b6b6b}',
                            '.nav-tabs > li > a:hover {background-color: transparent;border: 1px solid #424242; color: #fff}',
                            '.nav-tabs {border-bottom: 1px solid #424242}',
                            '.well {background-color: #424242; border: 1px solid #424242; color: #fff}',
                            '.panel-default > .panel-heading {color: #fff; background-color: #383838; border-color: #6b6b6b}',
                            '.panel-default {border-color: #383838}',
                            '.panel-group .panel {background-color: #383838}',
                            '.panel-default > .panel-heading + .panel-collapse > .panel-body {border-top-color: #424242}',
                            '.panel-title {font-weight: bold}',
                            '.table {margin-left: 20px}',
                            '.tab-content {background-color: #424242}'
                          )),

                          # shiny::addResourcePath(prefix='www',directoryPath = system.file('www',
                          #                                                                 package = 'interimApp')),

                          titlePanel(
                            img(src='www/Logo.png', height="200px")
                          ),

                          #sidebarLayout(

                          sidebarPanel(
                            bsCollapse(
                              bsCollapsePanel("Recruitment Parameters",
                                              checkboxInput("ncinf", "Unlimited number of centers", TRUE),
                                              uiOutput("Center"),
                                              checkboxInput("nsinf", "Unlimited number of patients", TRUE),
                                              uiOutput("Patients"),
                                              numericInput("cw", "Number of center openings per week:", 1),
                                              numericInput("sw", "Number of screened patients per week and center:", 5),
                                              numericInput("sf", "Screening failure rate:", 0.1),
                                              numericInput("tb", "Time (weeks) between screening and enrollment/randomization:", 1),
                                              uiOutput("Capacity")
                              ),id = "collapse", open="Recruitment Parameters"),


                            checkboxInput("treat", "Fixed duration study", FALSE),
                            uiOutput("Treatment"),

                            checkboxInput("tte", "Time-to-event study", FALSE),
                            uiOutput("TimeToEvent"),

                            checkboxInput("cross", "Scheduling of interim analysis", FALSE),
                            uiOutput("InterimAnalysis"),

                            verbatimTextOutput("info")

                          ),


                          mainPanel(
                            tabsetPanel(
                              tabPanel("Center Plot", plotOutput("cntrPlot", height = "700px", inline = FALSE)),
                              tabPanel("Patient/Event Plot", plotOutput("trtPlot",hover = "plot_hover", height = "700px", inline = FALSE)),
                              tabPanel("Source data", tableOutput("table"))
                            )
                          )
  )
  )


  #### Run Shiny App  ####

  interim_app <- shinyApp(ui = ui, server = server)
  on_ex_browser <- options()$browser
  on.exit(options(browser = on_ex_browser))
   if (!is.null(browser)) {options(browser = browser)}
  shiny::runApp(interim_app, host = host, port = port)
}
