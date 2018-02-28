library(shiny)
library(tidyverse)
library(Cairo)
library(stringr)
library(shinyjs)
library(reader)
library(shinyFiles)

source("data/read_iop.R")

ui <- fluidPage(theme = "bootstrap.css",
                shinyjs::useShinyjs(),
                titlePanel("IOP data cleaning ~ depth timeseries"),
                sidebarLayout(
                  sidebarPanel(width = 12,
                               # get the following to get a max width for the 2 panels
                               tags$head(tags$style(type="text/css", ".well { max-width: 2000px; }")),
                               tags$style(".well {background-color:lightblue;}"),
                               HTML('<center><img src="iop.png", alt = "C-OPS wet unit",
                                    height = "200", width = "200"></center>'),
                               shinyDirButton("dir", "Chose directory", "Upload"),
                               tags$br(),
                               helpText("current DIR"),
                               verbatimTextOutput("dir"),
                               tags$br(),
                               helpText("current DIR files list"),
                               verbatimTextOutput("files"),
                               tags$br(),
                               helpText("current file being processed"),
                               verbatimTextOutput("spy"),
                               actionButton(inputId = "previous.file", label = "previous file",
                                            icon = icon(name = "arrow-circle-left"),
                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                               actionButton(inputId = "next.file", icon = icon(name = "arrow-circle-right"),
                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                            label = "next file")
                  ),
                  mainPanel(width = 12,
                            column(width = 12, offset = 0, class = "well",
                                   helpText("PLOT #1 - draw a rectangle to zoom on the selected 
                                            data (dbl click to reset, result appears in PLOT #2)"),
                                   fluidRow(
                                     column(width = 12,
                                            plotOutput("iopPlot", height = 500,
                                                       brush = brushOpts(
                                                         id = "plot1_brush",
                                                         resetOnNew = TRUE
                                                       )))),
                                   tags$br(),
                                   helpText("PLOT #2 - click or draw a rectangle and click 'toggle points' 
                                            to discard / reselect the data (click 'reset' to reset) - 
                                            kept points are black circles, discarded points are hollow circles"),
                                   fluidRow(
                                     column(width = 12,
                                            plotOutput("iopPlotZoom", height = 500,
                                                       click = "plot2_click",
                                                       brush = brushOpts(
                                                         id = "plot2_brush",
                                                         resetOnNew = FALSE
                                                       )))),
                                   fluidRow(
                                     tags$br(),
                                     column(width = 4, offset = 0,
                                            actionButton("exclude_toggle", "Toggle points",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                     column(width = 4, offset = 0,
                                            actionButton("exclude_reset", "Reset",
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                   fluidRow(
                                     column(width = 4,
                                            helpText("# of kept points"),
                                            textOutput("nb_kept_points")),
                                     column(width = 4,
                                            helpText("# of toggled points"),
                                            textOutput("nb_toggled_points")),
                                     column(width = 4,
                                            helpText("total # of points"),
                                            textOutput("nb_total_points"))
                                   ),
                                   HTML('<hr style="height: 6px;
                                        color: lightblue;
                                        background-color: lightblue;
                                        border: none"'),
                                   fluidRow(actionButton("save","Save this file"),
                                            helpText("While creating the output file, a boolean column (called 'flag') will be added to the input file ones:"),
                                            helpText(" - 'FALSE' for discarded measurements,"),
                                            helpText(" - 'TRUE' for the kept ones."))
                            )
                            
                  )
                )
)

options(shiny.maxRequestSize = 30 * 1024 ^ 2) # 50 mb file max

server <- function(input, output, session) {
  
  # set LC_TIME to "C" otherwise strptime or as.POSIXct does not recognize %p for AM/PM format
  Sys.setlocale("LC_TIME", "C")
  
  # declare the ranges of the brush zoom  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # declare the variable that will contain the count for current input file
  i.file <- reactiveValues() # counter will be i.file$cpt
  
  # will check that list.files(path()) does not contains files from
  # another folder than ./data/input.files/ or that this one if selected is not empty
  observe({
    if (!is.null(list.files(path()))){ # ./data/input.files is not empty
      extension.1er.fichier <- get.ext(list.files(path())[1])[[1]] # if another one, got tsv files?
      if (extension.1er.fichier == "txt") i.file$cpt <- 1
    }
  })
  
  # path function, used to fill in the output$files verbatim text and populate the files list
  path <- reactive({
    home <- normalizePath("./")
    file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
  })
  
  # print currently chosen dir in the top verbatim text box
  shinyDirChoose(input, 'dir', roots = c(home = './'), filetypes = c('txt'))
  dir <- reactive(input$dir)
  current.dir <- reactive({parseDirPath(c(home = './'), input$dir)})
  output$dir <- renderPrint({parseDirPath(c(home = './'), input$dir)})
  
  # print all files located in the currently chosendir in the bottom verbatim text box
  output$files <- renderPrint(unlist(list.files(path())))
  
  # get them whenever the brush is used 
  observe({
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)}
    else {
      ranges$x <- NULL
      ranges$y <- NULL}
  })
  
  # disable / enable actionButton and selectInput to avoid errors
  observe({
    if (!is.null(i.file$cpt)){
      inFile <- list.files(path())[i.file$cpt]
      output$spy <- renderPrint(inFile)
      shinyjs::enable("exclude_toggle")
      shinyjs::enable("exclude_reset")
      shinyjs::enable("save")
      if (i.file$cpt < length(list.files(path()))) {
        shinyjs::enable("next.file") }
      if (i.file$cpt > 1) {
        shinyjs::enable("previous.file")}
      if (i.file$cpt == length(list.files(path()))) {
        shinyjs::disable("next.file") }
      if (i.file$cpt == 1) {
        shinyjs::disable("previous.file")}
    }
    else {
      shinyjs::disable("exclude_toggle")
      shinyjs::disable("exclude_reset")
      shinyjs::disable("save")
      shinyjs::disable("next.file")
      shinyjs::disable("previous.file")}
  })

  # create "myData()" as reactive data to trigger all
  # dependancies. Also mutate date and time to take
  # millisecond into account, and compute time elapsed since
  # file recording start. Do that as otherwise all the points
  # within 1 sec are all on the same x value in the plots
  myData <- reactive({
    if (!is.null(i.file$cpt)){
      inFile <- list.files(path())[i.file$cpt]
      #inFile <- input$file1
      if (is.null(inFile)) return(NULL)
      data <- read_iop(paste(parseDirPath(c(home = './'), input$dir),"/", inFile,sep=""))
      data1 <- data %>%
        mutate(Time_ms = (as.numeric(Time_ms) - as.numeric(Time_ms[1])) / 60000) %>% 
        mutate(Pres_dbar = as.numeric(Pres_dbar))
      data1
      
    }})
  
  # if user click on next file or previous file,
  # change the currently processed file. No need to
  # check too many things here as the actionButton
  # "next file" and "previous file" are not enable when
  # they cannot be clicked (for example "next file" won't be
  # enable when we reach the last file of the selected folder
  
  # next file
  observeEvent(input$next.file, {
    df <- myData()
    if (!is.null(df)) 
      i.file$cpt <- i.file$cpt + 1
  })
  # previous file
  observeEvent(input$previous.file, {
    df <- myData()
    if (!is.null(df)) 
      i.file$cpt <- i.file$cpt - 1
  })
  
  
  # declare "vals" (let it empty) as the reactive value
  # that will store the points to keep and the points to discard
  vals <- reactiveValues()
  
  # create an observe item to fill "vals" with "TRUEs"
  # for that, get the size of myData()
  observe({
    df <- myData()
    if (!is.null(df)) vals$keeprows = rep(TRUE, nrow(df))
  })
  
  # Toggle points that are clicked
  # here we just update "vals"
  observeEvent(input$plot2_click, {
    df <- myData()
    if (!is.null(df)) {
      res <- nearPoints(df, input$plot2_click, allRows = TRUE)
      vals$keeprows <- xor(vals$keeprows, res$selected_)}
  })
  
  # Toggle points that are brushed, when button is clicked
  # here we just update "vals"
  observeEvent(input$exclude_toggle, {
    df <- myData()
    if (!is.null(df)) {
      res <- brushedPoints(df, input$plot2_brush, allRows = TRUE)
      vals$keeprows <- xor(vals$keeprows, res$selected_)}
  })
  
  # Reset all points
  # here we just update "vals"
  observeEvent(input$exclude_reset, {
    df <- myData()
    if (!is.null(df)) vals$keeprows <- rep(TRUE, nrow(df))
  })
  # main plot, zoom out full range
  output$iopPlot <- renderPlot({
    df <- myData()
    if (is.null(df)) {
      dat <- data.frame(x = c(-1,0,1), y = c(-1,0,1))
      #plot(dat$x, dat$y, type = "n", title = "empty plot, select a file")
      plot(0, type='n', xlab = "empty plot, select a file", ylab  = "", xlim= c(0,1), ylim = c(0,1))
    } else {
      #dat <- dplyr::filter(df, type == input$typeselect & wavelength == input$wlselect)
      dat <- df
      ggplot(dat, aes(x = Time_ms, y = Pres_dbar)) +
        geom_point() +
        scale_y_reverse() +
        xlab("Time elapsed since start of file recording (min)") +
        ylab("Depth (m)") +
        theme_bw()}
  })
  
  # second plot, zoomed out to start with then
  # can be zoomed with main plot brush
  # toggled points are "blanked" (no filling but
  # still visible)
  # text output gives number of toggled points
  output$iopPlotZoom <- renderPlot({
    df <- myData()
    if (is.null(df)) {
      dat <- data.frame(x = c(-1,0,1), y = c(-1,0,1))
      plot(0, type='n', xlab = "empty plot, select a file", ylab  = "", xlim= c(0,1), ylim = c(0,1))
    } else {
      keep    <- df[ vals$keeprows, , drop = FALSE]
      exclude <- df[!vals$keeprows, , drop = FALSE]
      nb.toggled.points <- length(exclude$Pres_dbar)
      nb.kept.points    <- length(keep$Pres_dbar)
      nb.total.points   <- length(df$Pres_dbar)
      output$nb_toggled_points <- renderText({ as.character(nb.toggled.points) })
      output$nb_kept_points <- renderText({ as.character(nb.kept.points) })
      output$nb_total_points <- renderText({ as.character(nb.total.points) })
      dat.keep    <- keep #dplyr::filter(keep, type == input$typeselect & wavelength == input$wlselect)
      dat.exclude <- exclude #dplyr::filter(exclude, type == input$typeselect & wavelength == input$wlselect)
      ggplot(df, aes(x = Time_ms, y = Pres_dbar)) +
        geom_point(data = dat.keep, aes(x = Time_ms, y = Pres_dbar)) +
        geom_point(data = dat.exclude, aes(x = Time_ms, y = Pres_dbar),
                   color = "red", alpha = 0.25) +
        scale_y_reverse() +
        theme_bw() +
        xlab("Time elapsed since start of file recording (min)") +
        ylab("Depth (m)") +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y)}
  })
  
  # save output files. Save 2 versions, one woth the flag column in "/res/flagged/",
  # the other without the flag column, but with only kept values, in "/rees/cleaned/"
  observeEvent(input$save, {
    df   <- myData()
    inFile <- list.files(path())[i.file$cpt]
    keep <- df[ vals$keeprows, , drop = FALSE]
    exclude <- df[!vals$keeprows, , drop = FALSE]
    #df <- df %>% 
    #  select(-Pres_dbar, -Time_ms)
    df <- df %>% 
      mutate(Time_ms = Time_ms * 60000)
    df.flagged <- df
    all.false = rep(FALSE, nrow(df.flagged))
    df.flagged$flag <- all.false
    df.flagged$flag[vals$keeprows] <- TRUE
    df.cleaned <- df.flagged %>% 
      filter(flag == "TRUE") %>% 
      select(-flag)
    write.table(df.flagged, file= paste("./res/flagged/",inFile,sep=""), row.names = FALSE, append = FALSE, sep = "\t")
    write.table(df.cleaned, file= paste("./res/cleaned/",inFile,sep=""), row.names = FALSE, append = FALSE, sep = "\t")
  })
  
}

shinyApp(ui = ui, server = server)