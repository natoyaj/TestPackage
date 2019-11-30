#
# Example of tabulated plot inspired by Gerritsen et al. 2006
#

library(ggplot2)
library(grid)
library(gridExtra)
library(egg)

#' @noRd
makeColorMap <- function(columns, color){
  colors <- as.list(rep(color, length(columns)))
  names(colors) <- columns
  return(colors)
}

#' @noRd
panelPlotOverlay <- function(plotData, columnGroups, xVariable, yVariable, yVariableUpper, yVariableLower, xlimrow, ylimcol, showX=F, showY=F, ylabel="", basetheme=NULL, titletext="", pointcol=NULL, linecol=NULL, errorcol=NULL, tickmarks=NULL){
  
  panelplot <- ggplot(plotData, aes_string(x=xVariable, y=yVariable)) + xlim(xlimrow) + ylab(ylabel) + ylim(ylimcol)
  
  cols <- unique(plotData[,columnGroups])
  for (col in cols){
    colData <- plotData[plotData[,columnGroups] == col,]
    if (!is.null(yVariableUpper) & !is.null(yVariableLower)){
      panelplot <- panelplot + geom_linerange(data=colData, aes_string(ymin=yVariableLower, ymax=yVariableUpper), color=errorcol[[col]])
    }  
    #points and lines
    if (nrow(colData) == 0){
      panelplot <- panelplot + geom_blank()
    }
    else{
      panelplot <- panelplot + geom_line(data=colData,color=linecol[[col]]) + geom_point(shape=20, size=2, color=pointcol[[col]])
    }
    
    if (!is.null(title) & col == cols[1]){
      panelplot <- panelplot + ggtitle(titletext)
    }
    
  }

  panelplot <- panelplot + basetheme()
  panelplot <- panelplot + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
  if (!is.null(tickmarks)){
    panelplot <- panelplot + scale_y_continuous(breaks=tickmarks, limits = ylimcol)
  }
  if (!showX){
    panelplot <- panelplot + theme(axis.title.x = element_blank(), axis.text.x=element_blank())
  }
  else{
    panelplot <- panelplot + theme(axis.title.x = element_blank())
  }
  
  if (!showY){
    panelplot <- panelplot + theme(axis.title.y = element_blank(), axis.text.y=element_blank())
  }
  
  return(panelplot)
}

#' @noRd
panelPlot  <- function(plotdata, xVariable, yVariable, yVariableUpper, yVariableLower, xlimrow, ylimcol, ylabel, basetheme, showX=F, showY=F, title=NULL, pointcol="white", linecol="black", errorcol="black", tickmarks=NULL){
  
  panelplot <- ggplot(plotdata, aes_string(x=xVariable, y=yVariable)) + xlim(xlimrow) + ylab(ylabel) + ylim(ylimcol)

  #error bars
  if (!is.null(yVariableUpper) & !is.null(yVariableLower)){
    panelplot <- panelplot + geom_linerange(aes_string(ymin=yVariableLower, ymax=yVariableUpper), color=errorcol)
  }
  
  #points and lines
  if (nrow(plotdata) == 0){
    panelplot <- panelplot + geom_blank()
  }
  else{
    panelplot <- panelplot + geom_line(color=linecol) + geom_point(shape=20, size=2, color=pointcol)
  }
  
  panelplot <- panelplot + basetheme()
  panelplot <- panelplot + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
  if (!is.null(tickmarks)){
    panelplot <- panelplot + scale_y_continuous(breaks=tickmarks, limits = ylimcol)
  }
  if (!showX){
    panelplot <- panelplot + theme(axis.title.x = element_blank(), axis.text.x=element_blank())
  }
  else{
    panelplot <- panelplot + theme(axis.title.x = element_blank())
  }
  if (!showY){
    panelplot <- panelplot + theme(axis.title.y = element_blank(), axis.text.y=element_blank())
  }
  if (!is.null(title)){
    panelplot <- panelplot + ggtitle(title)
  }
  
  return(panelplot)
}

#' Table plot
#' @description 
#'  makes a tabulated plot of estimates grouped by two covariates, one for columns, one for rows. axes of all plots are the same variables.
#' @details 
#'  If 'yVariableUpper' and 'yVaraiateLower' is not provided, error bars will not be plotted
#' @param data data.table() with data, must contain columns identified by parameters 'columnGroups', 'rowGroups', 'xVariable' and 'yVariable'
#' @param columnGroups character() identifies column in data that identifies the grouping used for columns in the plot
#' @param rowGRoups character() identifies column in data that identifies the grouping used for rows in the plot
#' @param xVariable character() identifies column in data containting variable to be used for x-axis
#' @param yVariable character() identifies column in data containing variable to be used for y-axis
#' @param yVariableLower character(), optional, identifies column in data containing lower limits for error bars
#' @param yVariableUpper character(), optional,  identifies column in data containing upper limits for error bars
#' @param ylab character(), optional, label for y-axis, if not provided 'yVariable' will be used.
#' @param ymin numeric(), optional, lower bounds of y axis, if not NULL y axis will be adopted to data for each column
#' @param ymax numeric(), optional, upper bounds of y axis, if not provided y axis will be adopted to data for each column
#' @param xlim vector, optional, lower and upper bounds of x axis, if not provided y axis will be adapted to data for each row
#' @param pointcol character() or named list mapping the values in data[,columnGroups] to colors, color of points in plots
#' @param linecol character() or named list mapping the values in data[,columnGroups] to colors color of connecting lines in plots
#' @param errorcol character() or named list mapping the values in data[,columnGroups] to colors color of error bars
#' @param tickmarks numeric() specifies tickmarks for y-axis
#' @param basetheme ggplot2 - theme function to use for plotting. Default adjusts y-axis label alignments to account for variable width of tick-labels.
stackedPanels <- function(data, columnGroups, rowGroups, xVariable, yVariable, yVariableLower=NULL, yVariableUpper=NULL, ylab=NULL, xlab=NULL, xlim=NULL, ymin=0, ymax=NULL, pointcol="black", linecol="#cb181d", errorcol="#cb181d", tickmarks=NULL, basetheme=function(x){ggplot2::theme_classic() + theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(angle = 90, hjust = 1, size=6))}){
  
  if(is.numeric(columnGroups) | is.numeric(rowGroups)){
    stop("ColumnGroups and rowGroups can not be numeric variables. Covert with as.character()")
  }
  
  if (!all(c(columnGroups, rowGroups, xVariable, yVariable) %in% names(data))){
    stop("Some arguments (columnGroups, rowGroups, xVariable, yVariable) not found in data.")
  }
  
  if (any(is.na(data[,c(columnGroups, rowGroups, xVariable)]))){
    stop("NAs in grouping variables or x variable")
  }

  if (is.null(yVariableUpper) + is.null(yVariableLower) == 1){
    stop("Provide either both yVariableUpper and yVariableLower, or neither of them")
  }
  
  if (is.null(ylab)){
    ylab <- yVariable
  }
  
  if (is.null(xlab)){
    xlab <- xVariable
  }
  
  rows <- sort(unique(unlist(data[,rowGroups])), decreasing = T)
  cols <- sort(unique(unlist(data[,columnGroups])))
  
  if (is.character(linecol)){
    linecol <- makeColorMap(cols, linecol)
  }
  if (is.character(pointcol)){
    pointcol <- makeColorMap(cols, pointcol)
  }
  if (is.character(errorcol)){
    errorcol <- makeColorMap(cols, errorcol)
  }
  
  panels <- list()
  for (row in rows){
    
    #determine ylims if necessary
    minvar <- yVariable
    if (!is.null(yVariableLower)){
      minvar <- yVariableLower  
    }
    miny <- ymin
    if (is.null(miny)){
      miny <- min(data[data[,rowGroups] == row,minvar])
      miny <- miny - abs(miny) * .1
    }
    
    maxvar <- yVariable
    if (!is.null(yVariableUpper)){
      maxvar <- yVariableUpper  
    }
    maxy <- ymax
    if (is.null(maxy)){
      maxy <- max(data[data[,rowGroups] == row,maxvar])
      maxy <- maxy + abs(maxy) * .1
    }
    ylimcol <- c(miny, maxy)
    
    for (col in cols){
      xlimrow <- xlim
      if (is.null(xlimrow)){
        xlimrow <- c(min(data[data[,columnGroups] == col,xVariable]), max(data[data[,columnGroups] == col,xVariable]))
      }
      
      title <- NULL
      if (row == rows[1]){
        title = col
      }
      
      plotdata <- data[data[,columnGroups] == col & data[,rowGroups] == row,]
      panel <- panelPlot(plotdata, xVariable, yVariable, yVariableUpper, yVariableLower, xlimrow, ylimcol, showX=(row == rows[length(rows)]), showY=(col == cols[1]), ylabel=row, basetheme=basetheme, title=title, pointcol=pointcol[[col]], linecol=linecol[[col]], errorcol=errorcol[[col]], tickmarks=tickmarks)
      panels[[paste(row, col, sep="/")]] <- panel
      
    }
  }
  
  ggarrange(
    plots=panels,
    nrow=length(rows),
    left=ylab,
    bottom=xlab
    )
}

#' Makes Two column plot-table
#' Same x-axis, and row-categories, but two different y-axis for the different groups.
#' values for each of the column groups are plotted on top of each other in the column to the right for yvariable1
#' values for each of the column groups are plotted on top of each other in the column to the left for yvariable2
overlayedColumnPlot <- function(data, columnGroups, rowGroups, xVariable, yVariable1, yVariable2, yVariable1Lower=NULL, yVariable1Upper=NULL, yVariable2Lower=NULL, yVariable2Upper=NULL, ylab1=NULL, ylab2=NULL, xlab=NULL, xlim=NULL, ymin1=0, ymin2=0, ymax1=NULL, ymax2=NULL, pointcol="black", linecol="#cb181d", errorcol="#cb181d", tickmarksY1=NULL, tickmarksY2=NULL, basetheme=function(x){ggplot2::theme_classic() + theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(angle = 90, hjust = 1, size=6))}){
  if(is.numeric(columnGroups) | is.numeric(rowGroups)){
    stop("ColumnGroups and rowGroups can not be numeric variables. Covert with as.character()")
  }
  
  if (!all(c(columnGroups, rowGroups, xVariable, yVariable1, yVariable2) %in% names(data))){
    stop("Some arguments (columnGroups, rowGroups, xVariable, yVariable1, yvaraible2) not found in data.")
  }
  
  if (any(is.na(data[,c(columnGroups, rowGroups, xVariable)]))){
    stop("NAs in grouping variables or x variable")
  }
  
  if (is.null(yVariable1Upper) + is.null(yVariable1Lower) == 1){
    stop("Provide either both yVariableUpper and yVariableLower, or neither of them")
  }
  if (is.null(yVariable2Upper) + is.null(yVariable2Lower) == 1){
    stop("Provide either both yVariableUpper and yVariableLower, or neither of them")
  }
  
  if (is.null(ylab1)){
    ylab1 <- yVariable1
  }
  if (is.null(ylab2)){
    ylab2 <- yVariable2
  }
  
  if (is.null(xlab)){
    xlabel <- xVariable
  }
  else{
    xlabel <- xlab
  }
  
  rows <- sort(unique(unlist(data[,rowGroups])), decreasing = T)
  cols <- sort(unique(unlist(data[,columnGroups])))
  
  if (is.character(linecol)){
    linecol <- makeColorMap(cols, linecol)
  }
  if (is.character(pointcol)){
    pointcol <- makeColorMap(cols, pointcol)
  }
  if (is.character(errorcol)){
    errorcol <- makeColorMap(cols, errorcol)
  }
  
  panels <- list()
  for (row in rows){
    
    #determine ylims for yVariable1 if necessary
    minvar1 <- yVariable1
    if (!is.null(yVariable1Lower)){
      minvar1 <- yVariable1Lower  
    }
    miny1 <- ymin1
    if (is.null(miny1)){
      miny1 <- min(data[data[,rowGroups] == row,minvar1])
      miny1 <- miny1 - abs(miny1) * .1
    }
    
    maxvar1 <- yVariable1
    if (!is.null(yVariable1Upper)){
      maxvar1 <- yVariable1Upper  
    }
    maxy1 <- ymax1
    if (is.null(maxy1)){
      maxy1 <- max(data[data[,rowGroups] == row,maxvar1])
      maxy1 <- maxy1 + abs(maxy1) * .1
    }
    ylim1col <- c(miny1, maxy1)
    
    
    #determine ylims for yVariable2 if necessary
    minvar2 <- yVariable2
    if (!is.null(yVariable2Lower)){
      minvar2 <- yVariable2Lower  
    }
    miny2 <- ymin2
    if (is.null(miny2)){
      miny2 <- min(data[data[,rowGroups] == row,minvar2])
      miny2 <- miny2 - abs(miny2) * .1
    }
    
    maxvar2 <- yVariable2
    if (!is.null(yVariable2Upper)){
      maxvar2 <- yVariable2Upper  
    }
    maxy2 <- ymax2
    if (is.null(maxy2)){
      maxy2 <- max(data[data[,rowGroups] == row,maxvar2])
      maxy2 <- maxy2 + abs(maxy2) * .1
    }
    ylim2col <- c(miny2, maxy2)
    
    #get x-axis and title params
    xlimrow <- xlim
    if (is.null(xlimrow)){
      xlimrow <- c(min(data[,xVariable]), max(data[,xVariable]))
    }
    
    title1 <- NULL
    if (row == rows[1]){
      title1 = yVariable1
    }
    title2 <- NULL
    if (row == rows[1]){
      title2 = yVariable2
    }
    
    plotdata <- data[data[,rowGroups] == row,]
    #plot yvar1
    panel <- panelPlotOverlay(plotdata, columnGroups, xVariable, yVariable1, yVariable1Upper, yVariable1Lower, xlimrow, ylim1col, showX=T, showY=T, ylabel=row, basetheme=basetheme, title=title1, pointcol=pointcol, linecol=linecol, errorcol=errorcol, tickmarks=tickmarksY1)
    panels[[paste(row, "yvar1", sep="/")]] <- panel
    
    #plot yvar2
    panel <- panelPlotOverlay(plotdata, columnGroups, xVariable, yVariable2, yVariable2Upper, yVariable2Lower, xlimrow, ylim2col, showX=T, showY=T, ylabel=NULL, basetheme=basetheme, title=title2, pointcol=pointcol, linecol=linecol, errorcol=errorcol, tickmarks=tickmarksY2)
    panels[[paste(row, "yvar2", sep="/")]] <- panel
    
    
  }

  ggarrange(
    plots=panels,
    nrow=length(rows),
    bottom=xlabel
  )
  
}

warning("Change file reference to package location")
warning("Check which CI to use")
warning("Check axis labels")


#
# compare the estimated mCPUE from the two ALKs for each age group (showing confidence intervals)
#

alkresult <- read.csv("data/ALKs_results.csv", sep=";", stringsAsFactors = F, dec = ",", na.strings = c("#N/A"))
alkresult$bs <- NULL
alkresult[alkresult$Bootstrap=="Stratified", "bs"] <- "S"
alkresult[alkresult$Bootstrap=="ICES-IBTS", "bs"] <- "I"
alkresult[alkresult$Bootstrap=="Modified-ICES", "bs"] <- "mI"
alkresult$ALKm <- paste(alkresult$ALK, alkresult$bs, sep=", ")
alkresult$age <- paste("Age", alkresult$age)
alkresult$RSE <- alkresult$sd / alkresult$mCPUE


estimator_colors <- list()
estimator_colors[["area based, I"]] <- "#92c5de"
estimator_colors[["area based, mI"]] <- "#0571b0"
estimator_colors[["haul based, S"]] <- "#ca0020"

####
# Plot 1
# Using Q1 and all years
#
# Place legends outside of plot somewhere reasonable.
#
alkq1 <- alkresult[alkresult$Quarter=="Q1",]
alkq1 <- alkq1[alkq1$age != "Age 0",]
overlayedColumnPlot(data = alkq1, columnGroups = "ALKm", rowGroups = "age", xVariable = "Year", yVariable1 = "mCPUE", yVariable2 = "RSE", yVariable1Lower = "Q025", yVariable1Upper = "Q975", tickmarksY1 = c(0,2,5,10,15,20,25), tickmarksY2=NULL, pointcol = "black", linecol = estimator_colors, errorcol = estimator_colors)


stop()
####
# Plot 2 - for supplementary
# As plot 1, but for Q3, retaining age group 0
# Using Q3 and all years
alkq3 <- alkresult[alkresult$Quarter=="Q3",]
stackedPanels(data = alkq3, columnGroups = "ALKm", rowGroups = "age", xVariable = "Year", yVariable = "mCPUE", "Q025", "Q975", tickmarks = c(0,1,2,5,10,15,20,25))

#
# Compare expected RSE from the three bootstrap procedures for each age group
# Using Q3 and all years
#
stackedPanels(data = alkq3, columnGroups = "ALKm", rowGroups = "age", xVariable = "Year", yVariable = "RSE", tickmarks = c(0,.2,.4,.6))

#
# Compare resampling for haul based ALK
#
resamplingPrXcm <- read.csv("data/OtolithsOnly_1_per_xcm_haulBased.csv", sep=";", stringsAsFactors = F, dec=",", na.strings = c("#N/A"))
resamplingPrXcm$age <- paste("Age", resamplingPrXcm$age)
resamplingPrXcm$Year <- as.character(resamplingPrXcm$Year)
resamplingPrXcmQ1 <- resamplingPrXcm[resamplingPrXcm$Quarter=="Q1",]
resamplingPrXcmQ1 <- resamplingPrXcmQ1[resamplingPrXcmQ1$age != "Age 0",]

resamplingPrXcmQ3 <- resamplingPrXcm[resamplingPrXcm$Quarter=="Q3",]

####
# Plot 3
# Using Q1 and all years
# change order of x axis
# make B/W
#
stackedPanels(data = resamplingPrXcmQ1, columnGroups = "Year", rowGroups = "age", xVariable = "Otolith_1cm", yVariable = "cv", xlab="Length group width (cm)", ylab="RSE", ymin=0)


####
# Plot 4
# as plot 3, but for q3, and retaining age 0
# change order of x axis
# make B/W
#
stackedPanels(data = resamplingPrXcmQ3, columnGroups = "Year", rowGroups = "age", xVariable = "Otolith_1cm", yVariable = "cv", xlab="Length group width (cm)", ylab="RSE", ymin=0)



####
# Plot 5
# Need to add current sampling (C) to the right of 5 (included now as 7)
# Remove line connecting points, and add horisontal line for C.
# Put horisontal under points.
# Make B/W
#
resamplingFixedLengthGroups <- read.csv("OtolithsOnly_Results/OtolithsOnly_y_per_5cm_haulBased.csv", sep=";", stringsAsFactors = F, dec=".", na.strings = c("#N/A", "NA"))
resamplingFixedLengthGroups$age <- paste("Age", resamplingFixedLengthGroups$age)
resamplingFixedLengthGroups$Year <- as.character(resamplingFixedLengthGroups$Year)
resamplingFixedLengthGroupsQ1 <- resamplingFixedLengthGroupsQ1[resamplingFixedLengthGroupsQ1$Quarter=="Q1",]
resamplingFixedLengthGroupsQ1 <- resamplingFixedLengthGroupsQ1[resamplingFixedLengthGroupsQ1$age != "Age 0",]
resamplingFixedLengthGroupsQ1[resamplingFixedLengthGroupsQ1$Otolith_per5cm == "C", "Otolith_per5cm"] <- 7
resamplingFixedLengthGroupsQ1$Otolith_per5cm <- as.integer(resamplingFixedLengthGroupsQ1$Otolith_per5cm)

stackedPanels(data = resamplingFixedLengthGroups, columnGroups = "Year", rowGroups = "age", xVariable = "Otolith_per5cm", yVariable = "cv", xlab="Otoliths per 5cm", ylab="RSE", ymin=0)

####
# Plot 6 - supplementary
# As plot 5, but for Q3, and including Age 0
#

####
# Plot 7
# overlay 1 and 5 for otholith per 5 cm
# put legeneds outside plot somewhere reasonable. legend: 1 fish per 5 cm, 5 fish per 5 cm
# make B/W
resamplingOtolithsAndHauls <- read.csv("OtolithsAndHauls_Results_2015_2018/OtolAndHaul_1_5_per_5cm_areaBased2015_2018.csv", sep=";", stringsAsFactors = F, na.strings=c("NA"))
resamplingOtolithsAndHauls$cv <- resamplingOtolithsAndHauls$sd / resamplingOtolithsAndHauls$bootstrapMean
resamplingOtolithsAndHauls$age <- paste("Age", resamplingOtolithsAndHauls$age)
resamplingOtolithsAndHauls$Year <- as.character(resamplingOtolithsAndHauls$Year)
resamplingOtolithsAndHaulsQ1 <- resamplingOtolithsAndHauls[resamplingOtolithsAndHauls$Quarter=="Q1",]
resamplingOtolithsAndHaulsQ1 <- resamplingOtolithsAndHaulsQ1[resamplingOtolithsAndHaulsQ1$age != "Age 0",]

resamplingOtolithsAndHaulsQ1o5 <- resamplingOtolithsAndHaulsQ1[resamplingOtolithsAndHaulsQ1$Otolith_per5cm == 5,]
stackedPanels(data = resamplingOtolithsAndHaulsQ1o5, columnGroups = "Year", rowGroups = "age", xVariable = "N", yVariable = "cv", xlab="Number of hauls", ylab="RSE", ymin=0)

####
# Plot 8 - supplementary
# Like plot 7, but for Q3 and including aGE GROUP 0

####
# Plot 9  - supplementary
# Like plot 7, but for 1997 - 1999

####
# Plot 10  - supplementary
# Like plot 9, but for Q3 and including age group 0