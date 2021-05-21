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
if(!"autothresholdr" %in% installed.packages()){
  install.packages("autothresholdr")
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
library(autothresholdr)


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
         plotOutput("meanInt"),
         downloadButton("meanIntImage"),
         textInput("meanIntName", "File name"),
         selectInput("meanIntFileType", "File type", list("jpg", "png", "pdf", "png")),
         uiOutput("meanIntIHOut"),
         uiOutput("meanIntIWOut")
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
         selectInput("thresholding", "Thresholding", c("None",
                                                       "Numeric",
                                                       "IJDefault",
                                                       "Huang",
                                                       "Huang2",
                                                       "Intermodes",
                                                       "IsoData",
                                                       "Li",
                                                       "MaxEntropy",
                                                       "Mean",
                                                       "MinErrorI",
                                                       "Minimum",
                                                       "Moments",
                                                       "Otsu",
                                                       "Percentile",
                                                       "RenyiEntropy",
                                                       "Shanbhag",
                                                       "Triangle",
                                                       "Yen")
                     ),
         conditionalPanel(
           condition = "input.thresholding == 'Numeric'",
           numericInput("threshNum", "Value for thresholding", 0)
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
         "Save as TIF",
         textInput("brightTifFile", label = "Filename"),
         downloadButton("brightTifDownload"),
         h1("Number"),
         displayOutput("numberDisplay"),
         "Save as TIF",
         textInput("numTifFile", label = "Filename"),
         downloadButton("numTifDownload"),
         h1("Intensity"),
         displayOutput("intensityDisplay"),
         "Save as TIF",
         textInput("intTifFile", label = "Filename"),
         downloadButton("intenseTifFile")
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
         ),
         numericInput("gateBin", "Number of bins", 100),
         numericInput("gateYaxMin", "Y axis minimum", 0),
         uiOutput("gateYaxMaxOut"),
         numericInput("gateXaxMin", "X axis minimum", 0),
         uiOutput("gateXaxMaxOut")
       ),
       mainPanel(
         plotOutput("gatePlot"),
         fluidRow(
           column(4,
                  h2("Gate"),
                  numericInput("nGateMin", "Minimum Number", value = 0),
                  uiOutput("nGateMaxOut"),
                   numericInput("bGateMin", "Minimum Brightness", value = 0),
                   uiOutput("bGateMaxOut"),
                   numericInput("iGateMin", "Minimum Intensity", value = 0),
                   uiOutput("iGateMaxOut"),
                   actionButton("gateCalc", "Calculate"),
                  fileInput("gateLoad", "Load gates")),
           column(8, plotOutput("gatedOverlay"))
         ),
         div(DTOutput("gatedResults", width = "50%"), style = "font-size: 75%; width: 726px"),
         "Download plot",
         textInput("gatedIName", "File name"),
         selectInput("gatedIFileType", "Filetype", list("jpg", "png", "pdf", "svg")),
         uiOutput("gatedIHOut"),
         uiOutput("gatedIWOut"),
         downloadButton("gatedImage")
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
  # Increase the size that input files can be to 500 MB
  options(shiny.maxRequestSize = 500*1024^2)
  # Accept tif files as input
  rd <- reactive({
    raw <- read_tif(input$rawData$datapath)
    # In Rory Nolan's original code, thresholding is done first, so I'm doing that
    if(input$thresholding == "None"){
      threshed <- raw
    } else if(input$thresholding == "Numeric"){
      threshed <- autothresholdr::mean_stack_thresh(raw, method = input$threshNum)
    } else {
      threshed <- autothresholdr::mean_stack_thresh(raw, method = input$thresholding)
    }
    # Incorporate all the detrend options on offer
    if(input$detrendOpt == "None"){
      out <- threshed
    } else if(input$detrendOpt == "Boxcar"){
      out <- img_detrend_boxcar(threshed, l = input$boxCarL, purpose = "FFS")
    } else if(input$detrendOpt == "Exponential"){
      out <- img_detrend_exp(threshed, tau = input$expTau, purpose = "FFS")
    } else if(input$detrendOpt == "Polynomial"){
      out <- img_detrend_polynom(threshed, degree = input$polyDeg, purpose = "FFS")
    } else if(input$detrendOpt == "RobinHood"){
      out <- img_detrend_robinhood(threshed, swaps = input$RHS)
    }
    out
  })
  # A plot of mean intensity over time.
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
    axis(side = 2, pos = 0, at = seq(-1, ceiling(max(mIntTime())), by = 0.5))
    points(x = (1:(length(mIntTime()))-1)*input$frameRate, y = mIntTime(), pch = 19)
    perBl <- round((max(mIntTime())-min(mIntTime()))/max(mIntTime())*100, 2)
    legend("right", paste0("Bleaching: ", perBl, "%"), bty = "n")
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
    text(x = max(bDensDat()$y), y = bDensDat()$x[which.max(bDensDat()$y)],
         labels = paste("Peak: x =", bDensDat()$x[which.max(bDensDat()$y)], "\n         y = ", max(bDensDat()$y)),
         pos = 4,
         offset = 3)
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
    text(x = max(nDensDat()$y), y = nDensDat()$x[which.max(nDensDat()$y)],
         labels = paste("Peak: x =", nDensDat()$x[which.max(nDensDat()$y)], "\n         y = ", max(nDensDat()$y)),
         pos = 4,
         offset = 3)
  })
  #function for displaying the gated values on the original image
  output$gatedBrightness <- renderPlot({
    d <- brightDat()
    e <- array(data = NA, dim = dim(brightDat()))
    e[,,1,1] <- intenseDat()
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
  # Plotting the data
  
  gateY <- reactive({
    if(input$gateYaxis == "Number"){
      yaxis <- numberDat()
    } else if (input$gateYaxis == "Brightness"){
      yaxis <- brightDat()
    } else {
      yaxis <- intenseDat()
    }
    yaxis
  })
  
  gateX <- reactive({
    if(input$gateXaxis == "Number"){
      xaxis <- numberDat()
    } else if (input$gateXaxis == "Brightness"){
      xaxis <- brightDat()
    } else {
      xaxis <- intenseDat()
    }
    xaxis
  })
  
  output$gateYaxMaxOut <- renderUI({
    d <- as.vector(gateY())
    numericInput("gateYaxMax", "Y axis maximum", max(na.omit(d)))
  })
  
  output$gateXaxMaxOut <- renderUI({
    d <- as.vector(gateX())
    numericInput("gateXaxMax", "X axis maximum", max(na.omit(d)))
  })
  
  output$gatePlot <- renderPlot({
    df <- data.frame(x = as.vector(gateX()),
                     y = as.vector(gateY()))
    x.bin <- seq(min(na.omit(df$x)), ceiling(max(na.omit(df$x))), length.out = input$gateBin)
    y.bin <- seq(min(na.omit(df$y)), ceiling(max(na.omit(df$y))), length.out = input$gateBin)
    
    freq <- as.data.frame(
      table(
        findInterval(df$x, x.bin),
        findInterval(df$y, y.bin)
      )
    )
    freq[,1] <- as.numeric(freq[,1])
    freq[,2] <- as.numeric(freq[,2])
    freq2D <- diag(input$gateBin)*0
    freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]
    rf <- colorRampPalette(c("white", "darkblue", "dodgerblue2", "firebrick"), bias = 1)
    r <- rf(100)
    gatePal <- colorRampPalette(c("dodgerblue2", "darkorange", "orchid"))
    gPal <- gatePal(nrow(gt()))
    image(x = x.bin, y = y.bin, z = log(freq2D),
          col = r, xlim = c(input$gateXaxMin, input$gateXaxMax), ylim = c(input$gateYaxMin, input$gateYaxMax),
          bty = "l", xlab = input$gateXaxis, ylab = input$gateYaxis)
    rectY <- matrix()
    rectX <- matrix()
    if(input$gateYaxis == "Number"){
      rectY <- gt()[,1:2]
    } else if(input$gateYaxis == "Brightness") {
      rectY <- gt()[,3:4]
    } else {
      rectY <- gt()[,5:6]
    }
    if(input$gateXaxis == "Number"){
      rectX <- gt()[,1:2]
    } else if(input$gateXaxis == "Brightness") {
      rectX <- gt()[,3:4]
    } else {
      rectX <- gt()[,5:6]
    }
    for(i in 1:nrow(gt())){
      rect(xleft = rectX[i,1], ybottom = rectY[i,1], xright = rectX[i,2], ytop = rectY[i,2], border = gPal[i], lwd = 2)
    } 
  })
  
  # functions for adding gates to the data
  # Make an editable table for gates
  
  gt <- reactiveVal()
  gt(data.frame("Number Minimum" = numeric(0),
               "Number Maximum" = numeric(0),
               "Brightness Minimum" = numeric(0),
               "Brightness Maximum" = numeric(0),
               "Intensity Minimum" = numeric(0),
               "Intensity Maximum" = numeric(0),
               "Pixel count" = numeric(0)))
  
  # UI elements to put the maximum values in the gates
  output$nGateMaxOut <- renderUI({
    numericInput("nGateMax", paste0("Maximum Number (", max(numberDat(), na.rm = T), ")"), max(numberDat(), na.rm = T))
  })
  
  output$bGateMaxOut <- renderUI({
    numericInput("bGateMax", paste0("Maximum Brightness (", max(brightDat(), na.rm = T), ")"), max(brightDat(), na.rm = T))
  })
  
  output$iGateMaxOut <- renderUI({
    numericInput("iGateMax", paste0("Maximum Intensity (", max(intenseDat(), na.rm = T), ")"), max(intenseDat(), na.rm = T))
  })
  
  # A function for finding the number of pixels within specified gates
  gateThat <- function(x){
        numberGate <- which(numberDat() >= x[1] & numberDat() < x[2])
        brightGate <- which(brightDat() >= x[3] & brightDat() < x[4])
        intenseGate <- which(intenseDat() >= x[5] & intenseDat() < x[6])
        gatedVals <- intersect(numberGate, intersect(brightGate, intenseGate))
        length(gatedVals)
    }
  
  # Make the calculation of the gates happen when the button is pressed
  observeEvent(input$gateCalc, {
    gateVec <- c(input$nGateMin, input$nGateMax, input$bGateMin, input$bGateMax, input$iGateMin, input$iGateMax)
    gateVec <- c(gateVec, gateThat(gateVec))
    t = rbind.data.frame(gt(), gateVec)
    names(t) <- names(gt())
    gt(t)
  })
  
  # functions for loading gates from a .csv file
  loadGates <- reactive({
    gateIn <- read.csv(input$gateLoad$datapath, header = F)
    gateFill <- function(x){
      y <- x
      if(toupper(x[1]) == "MIN"){
        y[1] <- min(numberDat(), na.rm = T)
      }
      if(toupper(x[2]) == "MAX"){
        y[2] <- max(numberDat(), na.rm = T)
      }
      if(toupper(x[3]) == "MIN"){
        y[3] <- min(brightDat(), na.rm = T)
      }
      if(toupper(x[4]) == "MAX"){
        y[4] <- max(brightDat(), na.rm = T)
      }
      if(toupper(x[5]) == "MIN"){
        y[5] <- min(intenseDat(), na.rm = T)
      }
      if(toupper(x[6]) == "MAX"){
        y[6] <- max(intenseDat(), na.rm = T)
      }
      as.numeric(y)
    }
    data.frame(t(apply(gateIn, 1, gateFill)))
  })
  
  observeEvent(input$gateLoad, {
    out <- data.frame(loadGates(), apply(loadGates(), 1, gateThat))
    names(out) <- names(gt())
    gt(rbind(gt(),out))
  })
  # render the table of gates
  output$gatedResults <- DT::renderDataTable(
    DT::datatable(
      gt(),
      extensions = 'Buttons',
      options = list(
        paging = FALSE,
        searching = FALSE,
        fixedColumns = T,
        autoWidth = F,
        ordering = T,
        dom = 'tB',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "display"
    )
  )
  output$gatedOverlay <- renderPlot({
    d <- brightDat()
    e <- array(data = NA, dim = dim(brightDat()))
    e[,,1,1] <- intenseDat()
    g <- numberDat()
    gateMultiple <- function(x){
      which(!is.na(d) & !is.na(e) & !is.na(g) &
        g >= x[1] & g < x[2] &
        d >= x[3] & d < x[4] &
        e >= x[5] & e < x[6])
    }
    gatedVals <- apply(gt(), 1, gateMultiple)
    im <- as.cimg(rd()[,,,1])
    im <- im/max(im, na.rm = T)
    pretendT <- function(x,y){
      list(x = y, y = x)
    }
    plot(imager::imwarp(
      im, map = pretendT
    ),
    axes = F
    )
    gatePal <- colorRampPalette(c(rgb(28/256, 134/256, 238/256, 0.2), rgb(255/256, 140/256, 0, 0.4), rgb(218/256, 112/256, 214/256,0.4)), alpha = T)
    gPal <- gatePal(nrow(gt()))
    if(nrow(gt()) > 0){
      for(i in 1:(length(gatedVals))){
      xCoords <- 1+floor(gatedVals[[i]]/dim(brightDat())[1])
      yCoords <- gatedVals[[i]] %% dim(brightDat())[1]
      points(xCoords, yCoords, col = gPal[i], pch = 19, cex = 0.01)
      }
    }
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
  # and image downloads
  output$brightTifDownload <- downloadHandler(
    filename = function() {
      paste0(input$brightTifFile, ".tif")
    },
    content = function(file){
      ijtiff::write_tif(brightDat(), file)
    }
  )
  output$numTifDownload <- downloadHandler(
    filename = function(){
      paste0(input$numTifFile, ".tif")
    },
    content = function(file){
      ijtiff::write_tif(numberDat(), file)
    }
  )
  output$intenseTifDownload <- downloadHandler(
    filename = function(){
      paste0(input$intTifFile, ".tif")
    },
    content = function(file) {
      ijtiff::write_tif(intenseDat(), file)
    }
  )
  # and downloads for plots of images
  output$meanIntIHOut <- renderUI({
    if(input$meanIntFileType == "pdf" || input$meanIntFileType == "svg"){
      numericInput("meanIntIHeight", "Plot Height (in)", 10)
    } else {
      numericInput("meanIntIHeight", "Plot Height (px)", 600)
    }
  })
  output$meanIntIWOut <- renderUI({
    if(input$meanIntFileType == "pdf" || input$meanIntFileType == "svg"){
      numericInput("meanIntIWidth", "Plot Width (in)", 10)
    } else {
      numericInput("meanIntIWidth", "Plot Width (px)", 600)
    }
  })
  output$meanIntImage <- downloadHandler(
    filename = function(){
      paste0(input$meanIntName, ".", input$meanIntFileType)
    },
    content = function(file){
      if(input$meanIntFileType == "jpg"){
        jpeg(filename = file, width = input$meanIntIWidth, height = input$meanIntIHeight)
      }else if(input$meanIntFileType == "png"){
        png(file, width = input$meanIntIWidth, height = input$meanIntIHeight)
      }else if(input$meanIntFileType == "pdf"){
        pdf(file, input$meanIntIWidth, input$meanIntIHeight)
      }else if(input$meanIntFileType == "svg"){
        svg(file, input$meanIntIWidth, input$meanIntIHeight)
      }
      plot(1, type = "n",
           ylim = c(0, max(mIntTime())), xlim = c(0, length(mIntTime())*input$frameRate),
           ylab = "Mean Intensity", xlab = paste0("Time (", input$timeUnits, ")"),
           bty = "n", axes = F)
      axis(side = 1, pos = 0)
      axis(side = 2, pos = 0)
      points(x = (1:(length(mIntTime()))-1)*input$frameRate, y = mIntTime(), pch = 19)
      dev.off()
    }
  )
  output$gatedIWOut <- renderUI({
    if(input$meanIntFileType == "pdf" || input$meanIntFileType == "svg"){
      numericInput("gatedIWidth", "Plot Width (in)", 10)
    } else {
      numericInput("gatedIWidth", "Plot Width (px)", 600)
    }
  })
  output$gatedIHOut <- renderUI({
    if(input$meanIntFileType == "pdf" || input$meanIntFileType == "svg"){
      numericInput("gatedIHeight", "Plot Height (in)", 10)
    } else {
      numericInput("gatedIHeight", "Plot Height (px)", 600)
    }
  })
  output$gatedImage <- downloadHandler(
    filename = function(){
      paste0(input$gatedIName, ".", input$gatedIFileType)
    },
    content = function(file){
      if(input$gatedIFileType == "jpg"){
        jpeg(file, input$gatedIWidth, input$gatedIHeight)
      } else if(input$gatedIFileType == "png") {
        png(file, input$gatedIWidth, input$gatedIHeight)
      } else if(input$gatedIFileType == "pdf") {
        pdf(file, input$gatedIWidth, input$gatedIHeight)
      } else if(input$gatedIFileType == "svg") {
        svg(file, input$gatedIWidth, input$gatedIHeight)
      }
      df <- data.frame(x = as.vector(gateX()),
                       y = as.vector(gateY()))
      x.bin <- seq(min(na.omit(df$x)), ceiling(max(na.omit(df$x))), length.out = input$gateBin)
      y.bin <- seq(min(na.omit(df$y)), ceiling(max(na.omit(df$y))), length.out = input$gateBin)
      
      freq <- as.data.frame(
        table(
          findInterval(df$x, x.bin),
          findInterval(df$y, y.bin)
        )
      )
      freq[,1] <- as.numeric(freq[,1])
      freq[,2] <- as.numeric(freq[,2])
      freq2D <- diag(input$gateBin)*0
      freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]
      rf <- colorRampPalette(c("white", "darkblue", "dodgerblue2", "firebrick"), bias = .1)
      r <- rf(100)
      gatePal <- colorRampPalette(c("darkorchid", "olivedrab4"))
      gPal <- gatePal(nrow(gt()))
      image(x = x.bin, y = y.bin, z = log(freq2D),
            col = r, xlim = c(input$gateXaxMin, input$gateXaxMax), ylim = c(input$gateYaxMin, input$gateYaxMax),
            bty = "l", xlab = input$gateXaxis, ylab = input$gateYaxis)
      rectY <- matrix()
      rectX <- matrix()
      if(input$gateYaxis == "Number"){
        rectY <- gt()[,1:2]
      } else if(input$gateYaxis == "Brightness") {
        rectY <- gt()[,3:4]
      } else {
        rectY <- gt()[,5:6]
      }
      if(input$gateXaxis == "Number"){
        rectX <- gt()[,1:2]
      } else if(input$gateXaxis == "Brightness") {
        rectX <- gt()[,3:4]
      } else {
        rectX <- gt()[,5:6]
      }
      for(i in 1:nrow(gt())){
        rect(xleft = rectX[i,1], ybottom = rectY[i,1], xright = rectX[i,2], ytop = rectY[i,2], border = gPal[i], lwd = 2)
      } 
      dev.off()
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

