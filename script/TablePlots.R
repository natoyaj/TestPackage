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

#' generates function for passing to tickmaks
#' tickfunctions tries a range of preset scales form 0 to ymax
#' and chooses the one that leaves fewer tickmars, exceding n
genTickMarksTester <- function(n=3, steps=c(.001, 0.0025, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 25, 50, 100, 250, 500)){

  stopifnot(all(steps == sort(steps)))

  tickfun <- function(lim){
    maxl <- lim[2]
    minl <- 0
    for (step in rev(steps)){
      ticks <- seq(minl, maxl, step)

      if (length(ticks) >= 3){
        return(ticks)
      }
    }
    stop("Could not make tickmarks")
  }
}
ticks3 <- genTickMarksTester()

#' @noRd
panelPlot  <- function(plotdata, xVariable, yVariable, yVariableUpper, yVariableLower, xlimrow, ylimcol, ylabel, basetheme, showX=F, showY=F, title=NULL, pointcol="white", linecol="black", errorcol="black", tickmarks=NULL, reverseX=F, hLine=NULL, hLineCol="grey", hLineType="solid"){

  panelplot <- ggplot(plotdata, aes_string(x=xVariable, y=yVariable)) + xlim(xlimrow) + ylab(ylabel) + ylim(ylimcol)

  if (!is.null(hLine)){
    panelplot <- panelplot + geom_hline(yintercept = hLine, color = hLineCol, linetype=hLineType)
  }
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

  if (reverseX){
    panelplot <- panelplot + scale_x_reverse()
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
#' @param reverseX logical() whether to reverse X axes
#' @param hLineCol character() specify any column that should be used for horisontal reference lines
stackedPanels <- function(data, columnGroups, rowGroups, xVariable, yVariable, yVariableLower=NULL, yVariableUpper=NULL, ylab=NULL, xlab=NULL, xlim=NULL, ymin=0, ymax=NULL, pointcol="black", linecol="#cb181d", errorcol="#cb181d", tickmarks=NULL, basetheme=function(x){ggplot2::theme_classic() + theme(plot.title = element_text(hjust = 0.5,size=10), axis.title.y = element_text(size=8),  axis.title.x = element_text(size=8), axis.text.y = element_text(angle = 90, hjust = 1, size=6), axis.text.x = element_text(size=6))}, reverseX=F, hLineCol=NULL){

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

      referenceline <- NULL
      if (!is.null(hLineCol)){
        stopifnot(length(unique(unlist(plotdata[,hLineCol])))==1)
        referenceline <- unlist(plotdata[,hLineCol])[1]
      }

      panel <- panelPlot(plotdata, xVariable, yVariable, yVariableUpper, yVariableLower, xlimrow, ylimcol, showX=(row == rows[length(rows)]), showY=(col == cols[1]), ylabel=row, basetheme=basetheme, title=title, pointcol=pointcol[[col]], linecol=linecol[[col]], errorcol=errorcol[[col]], tickmarks=tickmarks, reverseX, hLine = referenceline)
      panels[[paste(row, col, sep="/")]] <- panel

    }
  }

  ggarrange(
    plots=panels,
    nrow=length(rows),
    left=textGrob(ylab, gp=gpar(fontsize=9), rot = 90),
    bottom=textGrob(xlab, gp=gpar(fontsize=9))
    )
}

warning("Change file reference to package location")
warning("Check which CI to use")
warning("Check axis labels")



#
# Compare resampling for haul based ALK
#
resamplingPrXcm <- read.csv("OtolithsOnly_Results/OtolithsOnly_1_per_xcm_haulBased.csv", sep=";", stringsAsFactors = F, dec=".", na.strings = c("#N/A", "NA"))
resamplingPrXcm$age <- paste("Age", resamplingPrXcm$age)
resamplingPrXcm$Year <- as.character(resamplingPrXcm$Year)
resamplingPrXcmQ1 <- resamplingPrXcm[resamplingPrXcm$Quarter=="Q1",]
resamplingPrXcmQ1 <- resamplingPrXcmQ1[resamplingPrXcmQ1$age != "Age 0",]

resamplingPrXcmQ3 <- resamplingPrXcm[resamplingPrXcm$Quarter=="Q3",]

#### resample 1 fish per 1cm, 2cm, 3cm, 4cm or 5cm------------------------------------------
# Plot 3
# Using Q1 and all years
#
pdf(file = "ices/figures/ALK_Bootstrap_Plots_Edvin/resamplingVariableLengthGroupQ1.pdf", width=3.35, onefile = F) #85mm in inches
stackedPanels(data = resamplingPrXcmQ1, columnGroups = "Year", rowGroups = "age", xVariable = "Otolith_xcm", yVariable = "cv", xlab="Length group width (cm)", ylab="RSE", ymin=0, reverseX = T, linecol = "black", tickmarks = ticks3)
dev.off()

####
# Plot 4
# as plot 3, but for q3, and retaining age 0
#
pdf(file = "ices/figures/ALK_Bootstrap_Plots_Edvin/suppMatResamplingVariableLengthGroupQ3.pdf", width=3.35, onefile = F) #85mm in inches
stackedPanels(data = resamplingPrXcmQ3, columnGroups = "Year", rowGroups = "age", xVariable = "Otolith_xcm", yVariable = "cv", xlab="Length group width (cm)", ylab="RSE", ymin=0, reverseX = T, linecol = "black", tickmarks = ticks3)
dev.off()


#### resample 1, 2, 3, 4 or 5 fish per 5cm length group width----------------------------------------------
# Plot 5
#
resamplingFixedLengthGroups <- read.csv("OtolithsOnly_Results/OtolithsOnly_y_per_5cm_haulBased.csv", sep=";", stringsAsFactors = F, dec=".", na.strings = c("#N/A", "NA"))
resamplingFixedLengthGroups$age <- paste("Age", resamplingFixedLengthGroups$age)
resamplingFixedLengthGroups$Year <- as.character(resamplingFixedLengthGroups$Year)
reference <- resamplingFixedLengthGroups[resamplingFixedLengthGroups$Otolith_per5cm == "C",c("Year","Quarter", "age", "cv")]
names(reference) <- c("Year","Quarter", "age", "current")
resamplingFixedLengthGroups <- resamplingFixedLengthGroups[resamplingFixedLengthGroups$Otolith_per5cm != "C",]
resamplingFixedLengthGroups$Otolith_per5cm <- as.integer(resamplingFixedLengthGroups$Otolith_per5cm)

resamplingFixedLengthGroups <- merge(resamplingFixedLengthGroups, reference)
resamplingFixedLengthGroupsQ1 <- resamplingFixedLengthGroups[resamplingFixedLengthGroups$Quarter=="Q1",]
resamplingFixedLengthGroupsQ1 <- resamplingFixedLengthGroupsQ1[resamplingFixedLengthGroupsQ1$age != "Age 0",]

pdf(file = "ices/figures/ALK_Bootstrap_Plots_Edvin/resamplingConstantLengthGroupQ1.pdf", width=3.35, onefile = F) #85mm in inches
stackedPanels(data = resamplingFixedLengthGroupsQ1, columnGroups = "Year", rowGroups = "age", xVariable = "Otolith_per5cm", yVariable = "cv", xlab="Fish sampled per 5cm", ylab="RSE", ymin=0, hLineCol="current", linecol="black", tickmarks = ticks3)
dev.off()

####
# Plot 6 - supplementary
# As plot 5, but for Q3, and including Age 0
#

resamplingFixedLengthGroupsQ3 <- resamplingFixedLengthGroups[resamplingFixedLengthGroups$Quarter=="Q3",]
pdf(file = "ices/figures/ALK_Bootstrap_Plots_Edvin/suppMatResamplingConstantLengthGroupQ3.pdf", width=3.35, onefile = F) #85mm in inches
stackedPanels(data = resamplingFixedLengthGroupsQ3, columnGroups = "Year", rowGroups = "age", xVariable = "Otolith_per5cm", yVariable = "cv", xlab="Fish sampled per 5cm", ylab="RSE", ymin=0, hLineCol="current", linecol="black", tickmarks = ticks3)
dev.off()
