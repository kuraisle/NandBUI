#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if(!"nandb" %in% installed.packages()){
  install.packages("nandb")
}
if(!"ijtiff" %in% installed.packages()){
  install.packages("ijtiff")
}
if(!"filesstrings" %in% installed.packages()){
  install.packages("filesstrings")
}
if(!"bsplus" %in% installed.packages()){
  install.packages("bsplus")
}
if(!"EBImage" %in% installed.packages()){
  install.packages("EBImage")
}
if(!"ggplot2" %in% installed.packages()){
  install.packages("ggplot2")
}

library(shiny)
library(nandb)
library(ijtiff)
library(filesstrings)
library(EBImage)
library(bsplus)
library(htmltools)
library(detrendr)
library(ggplot2)


ui <- navbarPage(
   "Number & Brightness",
   tabPanel(
     "Import data",
     sidebarLayout(
       sidebarPanel(
         fileInput(
           inputId = "rawData",
           label = "Import data"
         ),
         numericInput("frameRate", "Frame Rate", value = 0),
         textInput("timeUnits", "Units of time", value = "seconds")
       ),
       mainPanel(
         displayOutput("initialPlot")
       )
     )
   ),
   tabPanel(
     "Number & Brightness settings",
     sidebarLayout(
       sidebarPanel(
         radioButtons(
           inputId = "brightDef",
           label = "Definition of Brightness",
           choices = c("B", "epsilon"),
           selected = "B"
         ),
         radioButtons(
           inputId = "numberDef",
           label = "Definition of Number",
           choices = c("N", "n"),
           selected = "N"
         ),
         selectInput(
           inputId = "detrendOpt",
           label = "Detrending method",
           choices = c("None", "Boxcar", "Exponential", "Polynomial", "RobinHood"),
           selected = "None"
         ),
         "On my computer, Robin Hood detrending is either astronomically slow or doesn't work.",
         br(),
         "I haven't had the patience to find out.",
         conditionalPanel(
           condition = "input.detrendOpt == 'Boxcar'",
           textInput("boxCarL", "l for Boxcar detrending", value = "auto")
         ),
         conditionalPanel(
           condition = "input.detrendOpt == 'Exponential'",
           textInput("expTau", "tau for Exponential detrending", value = "auto")
         ),
         conditionalPanel(
           condition = "input.detrendOpt == 'Polynomial'",
           textInput("polyDeg", "Degree for polynomial detrending", value = "auto")
         ),
         conditionalPanel(
           condition = "input.detrendOpt == 'RobinHood'",
           textInput("RHS", "Swaps for Robin Hood detrending", value = "auto")
         )
       ),
       mainPanel(
         h1("Brightness"),
         displayOutput("brightnessDisplay"),
         h1("Number"),
         displayOutput("numberDisplay"),
         h1("Intensity"),
         displayOutput("intensityDisplay")
       )
     )
   ),
   tabPanel(
     "Plots",
     sidebarLayout(
       sidebarPanel(
         h1("Which plots?"),
         checkboxInput("IBHist", label = "Intensity/Brightness Histogram"),
         checkboxInput("BHist", label = "Brightness density plot"),
         checkboxInput("NHist", label = "Number density plot"),
         conditionalPanel(
           condition = "input.IBHist == 1",
           checkboxInput("IBHistROI", label = "Region of Interest?")
         )
       ),
       mainPanel(
         conditionalPanel(
           condition = "input.IBHist == 1",
           plotOutput("BvIHist"),
           uiOutput("BvIxSlider"),
           uiOutput("BvIySlider"),
           uiOutput("BvIbinSet"),
           conditionalPanel(
             condition = "input.IBHistROI == 1",
             uiOutput("BvIROIxSlider"),
             uiOutput("BvIROIySlider"),
             displayOutput("gatedBrightness")
           )
         ),
         conditionalPanel(
           condition = "input.BHist == 1",
           plotOutput("BHist"),
           uiOutput("BHistYSlider"),
           uiOutput("BHistXSlider")
         ),
         conditionalPanel(
           condition = "input.NHist == 1",
           plotOutput("NHist"),
           uiOutput("NHistYSlider"),
           uiOutput("NHistXSlider")
         )
       )
     )
   ),
   tabPanel(
     "Save Data",
     "None of these work at the minute, sorry!",
     bs_accordion_sidebar(id = "Downloads") %>%
       bs_append(
         title = "Number",
         content_side = downloadButton(outputId = "numberDownload", label = "Download"),
         content_main = textInput("numberFile", label = "Filename", value = "number")) %>%
       bs_append(
         title = "Brightness",
         content_side = downloadButton("brightnessDownload", label = "Download"),
         content_main = textInput("brightFile", label = "Filename", value = "brightness")) %>%
       bs_append(
         title = "Gated",
         content_side = downloadButton("gateDownload", label = "Download"),
         content_main = textInput("gateFile", label = "Filename", value = "gatedValues"))
   )
)

server <- function(input, output) {
  # Increase the size that input files can be to 30 MB
  options(shiny.maxRequestSize = 30*1024^2)
  # Accept tif files as input
  rd <- reactive({
    raw <- read_tif(input$rawData$datapath)
    # Incorporate all the detrend options on offer
    if(input$detrendOpt == "None"){
      out <- raw
    } else if(input$detrendOpt == "Boxcar"){
      out <- img_detrend_boxcar(raw, l = input$boxCarL, purpose = "FFS")
    } else if(input$detrendOpt == "Exponential"){
      out <- img_detrend_exp(raw, tau = input$expTau, purpose = "FFS")
    } else if(input$detrendOpt == "Polynomial"){
      out <- img_detrend_polynom(raw, degree = input$polyDeg, purpose = "FFS")
    } else if(input$detrendOpt == "RobinHood"){
      out <- img_detrend_robinhood(raw, swaps = input$RHS)
    }
    out
  })
  # Calculate brightness. This is reactive. It should update if you change your definition of brightness, but I don't think it does right now
  brightDat <- reactive({
    brightness(rd(), def = input$brightDef)
  })
  # Calculate number. As with brightness, it doesn't seem to change with definitions
  numberDat <- reactive({
    number(rd(), def = input$numberDef)
  })
  # Calculate the mean pixel intensity
  intenseDat <- reactive({
    apply(rd(), c(1,2), mean)
  })
  # Show the input. EBImage doesn't seem to like reactive input
  output$initialPlot <- renderDisplay({
    req(input$rawData$datapath)
    ijtiff::display(read_tif(input$rawData$datapath), method = "browser")
  })
  # Show the brightness plot. This reacts to detrending just fine.
  output$brightnessDisplay <- renderDisplay({
    req(input$rawData$datapath)
    ijtiff::display(brightDat(), method = "browser")
  })
  # Show the number plot. Also reacts to detrending
  output$numberDisplay <- renderDisplay({
    req(input$rawData$datapath)
    ijtiff::display(numberDat(), method = "browser")
  })
  # Show mean intensity.
  output$intensityDisplay <- renderDisplay({
    req(input$rawData$datapath)
    ijtiff::display(intenseDat(), method = "browser")
  })
  # Output functions for showing brightness vs intensity
  # slider for the x axis (intensity)
  output$BvIxSlider <- renderUI({
    sliderInput(
      inputId = "BvIx", label = "X axis",
      min = min(intenseDat(), na.rm = T),
      max = max(intenseDat(), na.rm = T),
      value = range(intenseDat(), na.rm = T)
    )
  })
  # slider for the y axis (brightness)
  output$BvIySlider <- renderUI({
    sliderInput(
      inputId = "BvIy", label = "Y axis",
      min = min(brightDat(), na.rm = T),
      max = max(brightDat(), na.rm = T),
      value = range(brightDat(), na.rm = T)
    )
  })
  # slider for the x axis (intensity)
  output$BvIbinSet <- renderUI({
    numericInput(
      inputId = "BvIbin",
      label = "Number of bins",
      value = 100)
  })
  # sliders for the ROI box
  output$BvIROIySlider <- renderUI({
    sliderInput(
      inputId = "BvIROIy",
      label = "Range of ROI (y)",
      min = min(brightDat(), na.rm = T),
      max = max(brightDat(), na.rm = T),
      value = range(brightDat(), na.rm = T)
    )
  })
  output$BvIROIxSlider <- renderUI({
    sliderInput(
      inputId = "BvIROIx",
      label = "Range of ROI (x)",
      min = min(intenseDat(), na.rm = T),
      max = max(intenseDat(), na.rm = T),
      value = range(intenseDat(), na.rm = T)
    )
  })
  # plot the histogram
  output$BvIHist <- renderPlot({
    out <- ggplot(data = data.frame(Intensity = as.vector(intenseDat()),
                             Brightness = as.vector(brightDat())),
           mapping = aes(x = Intensity, y = Brightness)) + stat_bin2d(bins = input$BvIbin) + scale_x_continuous(limits = input$BvIx) + scale_y_continuous(limits = input$BvIy)
    if(input$IBHistROI == 1){
      out <- out + geom_rect(aes(xmin = input$BvIROIx[1], xmax = input$BvIROIx[2], ymin = input$BvIROIy[1], ymax = input$BvIROIy[2]), color = "red", alpha = 0)
    }
    out
  })
  # Output functions for showing a brightness density plot
  #calculate the density
  bDensDat <- reactive({
    density(brightDat()[,,1,], na.rm = T)
  })
  # slider for the y axis (frequency)
  output$BHistYSlider <- renderUI({
    d <- bDensDat()
    sliderInput(
      inputId = "BHistY",
      label = "Y axis",
      min = min(d$y, na.rm = T),
      max = max(d$y, na.rm = T),
      value = range(d$y, na.rm = T)
    )
  })
  # slider for the x axis (brightness)
  output$BHistXSlider <- renderUI({
    d <- bDensDat()
    sliderInput(
      inputId = "BHistX",
      label = "X axis",
      min = min(d$x, na.rm = T),
      max = max(d$x, na.rm = T),
      value = range(d$x, na.rm = T)
    )
  })
  # plot the density
  output$BHist <- renderPlot({
    plot(bDensDat(), xlim = input$BHistX, ylim = input$BHistY, bty = "l", main = "Brightness Density")
  })
  # Output functions for showing a number density plot
  #calculate the density
  nDensDat <- reactive({
    density(numberDat()[,,1,], na.rm = T)
  })
  # slider for the y axis (frequency)
  output$NHistYSlider <- renderUI({
    d <- nDensDat()
    sliderInput(
      inputId = "NHistY",
      label = "Y axis",
      min = min(d$y, na.rm = T),
      max = max(d$y, na.rm = T),
      value = range(d$y, na.rm = T)
    )
  })
  # slider for the x axis (number)
  output$NHistXSlider <- renderUI({
    d <- nDensDat()
    sliderInput(
      inputId = "NHistX",
      label = "X axis",
      min = min(d$x, na.rm = T),
      max = max(d$x, na.rm = T),
      value = range(d$x, na.rm = T)
    )
  })
  # plot the density
  output$NHist <- renderPlot({
    plot(nDensDat(), xlim = input$NHistX, ylim = input$NHistY, bty = "l", main = "Number Density")
  })
  # functions for displaying the gating
  # function for finding coordinates of the gated values
  gatedVals <- reactive({
    d <- brightDat()
    subY <- which(d >= min(input$BvIROIy) & d <= max(input$BvIROIy))
    d <- intenseDat()
    subX <- which(d >= min(input$BvIROIx) & d <= max(input$BvIROIx))
    intersect(subY, subX)
  })
  threeChannel <- reactive({
    brightDims <- dim(brightDat())
    threeChannel <- array(data = rep(brightDat(),3), dim = c(brightDims[1:2], 3, 1))
    threeChannel[,,1,][gatedVals()] <- (threeChannel[,,1,][gatedVals()])*2
    threeChannel[,,2,][gatedVals()] <- (threeChannel[,,2,][gatedVals()])/2
    threeChannel[,,3,][gatedVals()] <- (threeChannel[,,3,][gatedVals()])/2
    threeChannel
  })
  #function for displaying the gated values on the original image
  output$gatedBrightness <- renderDisplay({
    ijtiff::display(threeChannel(), method = "browser")
  })
  # functions for downloads
  output$numberDownload <- downloadHandler(
    filename = function() {
      paste0(input$numberFile, ".csv")
    },
    content = function(file){
      write.csv(numberDat()[,,1,], file)
    })
  output$brightnessDownload <- downloadHandler(
    filename = function() {
      paste0(input$brightFile, ".csv")
    },
    content = function(file){
      write.csv(brightDat()[,,1,], file)
    })
  output$gateDownload <- downloadHandler(
    filename = function() {
      paste0(input$gateFile, ".csv")
    },
    content = function(file){
      write.csv(threeChannel(), file)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

