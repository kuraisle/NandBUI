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
if(!"imager" %in% installed.packages()){
  install.packages("imager")
}
if(!"RColorBrewer" %in% installed.packages()){
  install.packages("RColorBrewer")
}
if(!"DT" %in% installed.packages()){
  install.packages("DT")
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
library(imager)
library(RColorBrewer)
library(DT)


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
         numericInput("frameRate", "Frame Rate", value = 1),
         textInput("timeUnits", "Units of time", value = "seconds")
       ),
       mainPanel(
         displayOutput("initialPlot"),
         plotOutput("meanInt")
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
         "Robin Hood detrending can take a while.",
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
         ),
         checkboxInput("imageROI", label = "Region of interest?", value = FALSE)
       ),
       mainPanel(
         conditionalPanel(
           condition = "input.imageROI == 1",
           plotOutput("iROI"),
           uiOutput("iROIFrameSlider"),
           uiOutput("iROIxSlider"),
           uiOutput("iROIySlider")
         ),
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
     '"Gating"',
     sidebarLayout(
       sidebarPanel(
         h2("Display"),
         selectInput(
           inputId = "gateYaxis",
           label = "Y axis",
           choices = list("Number", "Intensity", "Brightness")
         ),
         selectInput(
           inputId = "gateXaxis",
           label = "X axis",
           choices = list("Number", "Intensity", "Brightness")
         )
       ),
       mainPanel(
         plotOutput("gatePlot"),
         h2("Gate"),
         numericInput("nGates", 'How many "Gates"?', 1),
         DTOutput("gateTable"),
         actionButton("gateCalc", "Calculate"),
         DTOutput("gatedResults")
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
           uiOutput("BvIxMinput"),
           uiOutput("BvIxMaxput"),
           uiOutput("BvIyMinput"),
           uiOutput("BvIyMaxput"),
           uiOutput("BvIbinSet"),
           conditionalPanel(
             condition = "input.IBHistROI == 1",
             uiOutput("BvIROIxMinput"),
             uiOutput("BvIROIxMaxput"),
             uiOutput("BvIROIyMinput"),
             uiOutput("BvIROIyMaxput"),
             plotOutput("gatedBrightness")
           )
         ),
         conditionalPanel(
           condition = "input.BHist == 1",
           plotOutput("BHist"),
           uiOutput("BHistYMinput"),
           uiOutput("BHistYMaxput"),
           uiOutput("BHistXMinput"),
           uiOutput("BHistXMaxput")
         ),
         conditionalPanel(
           condition = "input.NHist == 1",
           plotOutput("NHist"),
           uiOutput("NHistYMinput"),
           uiOutput("NHistYMaxput"),
           uiOutput("NHistXMinput"),
           uiOutput("NHistXMaxput")
         )
       )
     )
   ),
   tabPanel(
     "Save Data",
     "If you want to save these, you have to run the app in the browser.",
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
  # A plot of mean intensity over time. Doesn't work yet
  mIntTime <- reactive({
    apply(rd(), 4, mean)
  })
  output$meanInt <- renderPlot({
    req(input$rawData$datapath)
    plot(1, type = "n",
         ylim = c(0, max(mIntTime())), xlim = c(0, length(mIntTime())*input$frameRate),
         ylab = "Mean Intensity", xlab = paste0("Time (", input$timeUnits, ")"),
         bty = "n", axes = F)
    axis(side = 1, pos = 0)
    axis(side = 2, pos = 0)
    points(x = (1:(length(mIntTime()))-1)*input$frameRate, y = mIntTime(), pch = 19)
  })
  # Calculate brightness. This is reactive. It should update if you change your definition of brightness, but I don't think it does right now
  brightDat <- reactive({
    if(input$imageROI == 1){
      d <- rd()[input$iROIy[1]:input$iROIy[2], input$iROIx[1]:input$iROIx[2],,]
    } else {
      d <- rd()
    }
    brightness(d, def = input$brightDef)
  })
  # Calculate number. As with brightness, it doesn't seem to change with definitions
  numberDat <- reactive({
    if(input$imageROI == 1){
      d <- rd()[input$iROIy[1]:input$iROIy[2], input$iROIx[1]:input$iROIx[2],,]
    } else {
      d <- rd()
    }
    number(d, def = input$numberDef)
  })
  # Calculate the mean pixel intensity
  intenseDat <- reactive({
    if(input$imageROI == 1){
      d <- rd()[input$iROIy[1]:input$iROIy[2], input$iROIx[1]:input$iROIx[2],,]
    } else {
      d <- rd()
    }
    apply(d, c(1,2), mean)
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
  # Functions for "gating" the image
  # Plotting the image upon which to draw a region of interest
  output$iROI <- renderPlot({
    req(input$rawData$datapath)
    plot(as.cimg(t(rd()[,,,input$iROIFrame])), axes = F)
    rect(input$iROIx[1], input$iROIy[1], input$iROIx[2], input$iROIy[2],
         border = "dodgerblue2", lwd = 2)
  })
  # Slider to choose which frame to display
  output$iROIFrameSlider <- renderUI({
    sliderInput(
      inputId = "iROIFrame",
      label = "Which frame to display?",
      min = 1, max = dim(rd())[4], value = 1
    )
  })
  # Sliders to choose ROI
  output$iROIxSlider <- renderUI({
    sliderInput(
      inputId = "iROIx",
      label = "X dimensions of ROI",
      min = 1, max = dim(rd())[2],
      value = c(0, dim(rd())[2])
    )
  })
  output$iROIySlider <- renderUI({
    sliderInput(
      inputId = "iROIy",
      label = "Y dimensions of ROI",
      min = 1, max = dim(rd())[1],
      value = c(0, dim(rd())[1])
    )
  })
  # numerical inputs for the x axis (intensity)
  output$BvIxMinput <- renderUI({
    numericInput(
      inputId = "BvIxMin",
      label = "X axis minimum",
      value = min(intenseDat(), na.rm = T)
    )
  })
  output$BvIxMaxput <- renderUI({
    numericInput(
      inputId = "BvIxMax",
      label = "X axis maximum",
      value = max(intenseDat(), na.rm = T)
    )
  })
  # numerical inputs for the y axis (brightness)
  output$BvIyMinput <- renderUI({
    numericInput(
      inputId = "BvIyMin",
      label = "Y axis minimum",
      value = min(brightDat(), na.rm = T)
    )
  })
  output$BvIyMaxput <- renderUI({
    numericInput(
      inputId = "BvIyMax",
      label = "Y axis maximum",
      value = max(brightDat(), na.rm = T)
    )
  })
  # slider for the number of bins
  output$BvIbinSet <- renderUI({
    numericInput(
      inputId = "BvIbin",
      label = "Number of bins",
      value = 100)
  })
  # sliders for the ROI box
  output$BvIROIxMinput <- renderUI({
    numericInput(
      inputId = "BvIROIxMin",
      label = "X axis minimum (ROI)",
      value = min(intenseDat(), na.rm = T)
    )
  })
  output$BvIROIxMaxput <- renderUI({
    numericInput(
      inputId = "BvIROIxMax",
      label = "X axis maximum (ROI)",
      value = max(intenseDat(), na.rm = T)
    )
  })
  output$BvIROIyMinput <- renderUI({
    numericInput(
      inputId = "BvIROIyMin",
      label = "Y axis minimum (ROI)",
      value = min(brightDat(), na.rm = T)
    )
  })
  output$BvIROIyMaxput <- renderUI({
    numericInput(
      inputId = "BvIROIyMax",
      label = "Y axis maximum (ROI)",
      value = max(brightDat(), na.rm = T)
    )
  })
  
  # plot the histogram
  output$BvIHist <- renderPlot({
    df <- data.frame(Intensity = as.vector(intenseDat()),
                     Brightness = as.vector(brightDat()))
    x.bin <- seq(min(na.omit(df$Intensity)), ceiling(max(na.omit(df$Intensity))), length.out = input$BvIbin)
    y.bin <- seq(min(na.omit(df$Brightness)), ceiling(max(na.omit(df$Brightness))), length.out = input$BvIbin)
    
    freq <- as.data.frame(
      table(
        findInterval(df$Intensity, x.bin),
        findInterval(df$Brightness, y.bin)
      )
    )
    freq[,1] <- as.numeric(freq[,1])
    freq[,2] <- as.numeric(freq[,2])
    freq2D <- diag(input$BvIbin)*0
    freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]
    rf <- colorRampPalette(c("white", "darkblue", "dodgerblue2", "firebrick"), bias = .1)
    r <- rf(100)
    image(x = x.bin, y = y.bin, z = log(freq2D),
          col = r, xlim = c(input$BvIxMin, input$BvIxMax), ylim = c(input$BvIyMin, input$BvIyMax),
          bty = "l", xlab = "Intensity", ylab = "Brightness")
    if(input$IBHistROI == 1){
      rect(xleft = input$BvIROIxMin, ybottom = input$BvIROIyMin, xright = input$BvIROIxMax, ytop = input$BvIROIyMax, lwd = 2)
    }
  })
  # Output functions for showing a brightness density plot
  #calculate the density
  bDensDat <- reactive({
    density(brightDat()[,,1,], na.rm = T)
  })
  # numeric inputs for the y axis (frequency)
  output$BHistYMinput <- renderUI({
    d <- bDensDat()
    numericInput(
      inputId = "BHistYMin",
      label = "Y axis minimum",
      value = min(d$y, na.rm = T)
    )
  })
  output$BHistYMaxput <- renderUI({
    d <- bDensDat()
    numericInput(
      inputId = "BHistYMax",
      label = "Y axis maximum",
      value = max(d$y, na.rm = T)
    )
  })
  # numeric inputs for the x axis (brightness)
  output$BHistXMinput <- renderUI({
    d <- bDensDat()
    numericInput(
      inputId = "BHistXMin",
      label = "X axis minimum",
      value = min(d$x, na.rm = T)
    )
  })
  output$BHistXMaxput <- renderUI({
    d <- bDensDat()
    numericInput(
      inputId = "BHistXMax",
      label = "X axis maximum",
      value = max(d$x, na.rm = T)
    )
  })
  # plot the density
  output$BHist <- renderPlot({
    plot(bDensDat(), xlim = c(input$BHistXMin, input$BHistXMax), ylim = c(input$BHistYMin, input$BHistYMax), bty = "l", main = "Brightness Density")
  })
  # Output functions for showing a number density plot
  #calculate the density
  nDensDat <- reactive({
    density(numberDat()[,,1,], na.rm = T)
  })
  # numeric inputs for the y axis (frequency)
  output$NHistYMinput <- renderUI({
    d <- nDensDat()
    numericInput(
      inputId = "NHistYMin",
      label = "Y axis minimum",
      value = min(d$y, na.rm = T)
    )
  })
  output$NHistYMaxput <- renderUI({
    d <- nDensDat()
    numericInput(
      inputId = "NHistYMax",
      label = "Y axis maximum",
      value = max(d$y, na.rm = T)
    )
  })
  # numeric inputs for the x axis (brightness)
  output$NHistXMinput <- renderUI({
    d <- nDensDat()
    numericInput(
      inputId = "NHistXMin",
      label = "X axis minimum",
      value = min(d$x, na.rm = T)
    )
  })
  output$NHistXMaxput <- renderUI({
    d <- nDensDat()
    numericInput(
      inputId = "NHistXMax",
      label = "X axis maximum",
      value = max(d$x, na.rm = T)
    )
  })
  # plot the density
  output$NHist <- renderPlot({
    plot(nDensDat(), xlim = c(input$NHistXMin, input$NHistXMax), ylim = c(input$NHistYMin, input$NHistYMax), bty = "l", main = "Number Density")
  })
  #function for displaying the gated values on the original image
  output$gatedBrightness <- renderPlot({
    d <- brightDat()
    #subY <- d > input$BvIROIyMin & d < input$BvIROIyMax & !is.na(d)
    e <- array(data = NA, dim = dim(brightDat()))
    e[,,1,1] <- intenseDat()
    #subX <- e > input$BvIROIxMin & e < input$BvIROIxMax & !is.na(e)
    #gatedVals <- subY & subX
    gatedVals <- d > input$BvIROIyMin & d < input$BvIROIyMax & !is.na(d) & e > input$BvIROIxMin & e < input$BvIROIxMax & !is.na(e)
    im <- as.cimg(rd()[,,,1])
    im <- im/max(im, na.rm = T)
    pretendT <- function(x,y){
      list(x = y, y = x)
    }
    plot(imager::imwarp(
      imager::colorise(
      im,
      gatedVals,
      "firebrick2",
      alpha = .5
    ), map = pretendT
    ),
    axes = F
    )
  })
  
  # Functions for gating
  # functions for adding gates to the data
  # Make an editable table for gates
  gt <- reactive({
    out <- matrix(rep(c(0, max(numberDat(), na.rm = T), 0, max(brightDat(), na.rm = T), 0, max(intenseDat(), na.rm = T)), input$nGates),
                  nrow = input$nGates, ncol = 6, byrow = T)
    colnames(out) <- c("Number Minimum", "Number Maximum", "Brightness Minimum", "Brightness Maximum", "Intensity Minimum", "Intensity Maximum")
    out <- as.data.frame(out)
    out
  })
  
  
  gateThat <- function(x){
      numberGate <- which(numberDat() >= x[1] & numberDat() < x[2])
      brightGate <- which(brightDat() >= x[3] & brightDat() < x[4])
      intenseGate <- which(intenseDat() >= x[5] & intenseDat() < x[6])
      gatedVals <- intersect(numberGate, intersect(brightGate, intenseGate))
      length(gatedVals)
    }
  
  initGate <- c(0, max(numberDat(), na.rm = T),
                           0, max(brightDat(), na.rm = T),
                           0, max(intenseDat(), na.rm = T))
  
  
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

