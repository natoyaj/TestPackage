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
  
  rows <- sort(unique(unlist(data[,rowGroups])))
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
alkq1 <- alkresult[alkresult$Quarter=="Q1",]
stackedPanels(data = alkq1, columnGroups = "ALKm", rowGroups = "age", xVariable = "Year", yVariable = "mCPUE", "Q025", "Q975", tickmarks = c(0,2,5,10,15,20,25))
alkq3 <- alkresult[alkresult$Quarter=="Q3",]
stackedPanels(data = alkq3, columnGroups = "ALKm", rowGroups = "age", xVariable = "Year", yVariable = "mCPUE", "Q025", "Q975", tickmarks = c(0,1,2,5,10,15,20,25))

#
# Compare expected RSE from the three bootstrap procedures for each age group
#
stackedPanels(data = alkq3, columnGroups = "ALKm", rowGroups = "age", xVariable = "Year", yVariable = "RSE", tickmarks = c(0,.2,.4,.6))

#
# Compare resampling for haul based ALK
#
resamplingPrXcm <- read.csv("data/OtolithsOnly_1_per_xcm_haulBased.csv", sep=";", stringsAsFactors = F, dec=",")
resamplingPrXcm$Year <- as.character(resamplingPrXcm$Year)
resamplingPrXcmQ1 <- resamplingPrXcm[resamplingPrXcm$Quarter=="Q1",]
resamplingPrXcmQ3 <- resamplingPrXcm[resamplingPrXcm$Quarter=="Q3",]
stackedPanels(data = resamplingPrXcmQ3, columnGroups = "Year", rowGroups = "age", xVariable = "Otolith_1cm", yVariable = "mean", "Q025", "Q975", xlab="length group", ylab="mCPUE", tickmarks = c(0,2,5,10,15,20,25))

#some variants to higlight variances
resamplingPrXcmQ32018 <- resamplingPrXcmQ3[resamplingPrXcmQ3$Year=="2018",]
stackedPanels(data = resamplingPrXcmQ32018, columnGroups = "Year", rowGroups = "age", xVariable = "Otolith_1cm", yVariable = "mean", "Q025", "Q975", xlab="length group", ylab="mCPUE", ymin=NULL)
resamplingPrXcmQ3plusG <- resamplingPrXcmQ3[resamplingPrXcmQ3$age=="6+",]
stackedPanels(data = resamplingPrXcmQ3plusG, columnGroups = "Year", rowGroups = "age", xVariable = "Otolith_1cm", yVariable = "mean", "Q025", "Q975", xlab="length group", ylab="mCPUE", tickmarks = c(0,2,5,10,15,20,25), ymin=NULL)


