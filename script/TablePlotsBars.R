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
panelPlot  <- function(plotdata, xVariable, yVariable, ylimcol, ylabel, basetheme, showX=F, showY=F, title=NULL, barcol="black", tickmarks=NULL){

  panelplot <- ggplot(plotdata, aes_string(x=xVariable, y=yVariable)) + ylab(ylabel) + ylim(ylimcol)

  #barplots
  if (nrow(plotdata) == 0){
    panelplot <- panelplot + geom_blank()
  }
  else{
    panelplot <- panelplot + geom_bar(color=barcol, stat = "identity")
  }
  
  panelplot <- panelplot + basetheme()
  panelplot <- panelplot + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
  if (!is.null(tickmarks)){
    panelplot <- panelplot + scale_y_continuous(breaks=tickmarks, limits = ylimcol)
  }
  if (!showX){
    panelplot <- panelplot + theme(axis.title.x = element_blank(), axis.text.x=element_blank(), axis.ticks = element_blank())
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
#' @param ylab character(), optional, label for y-axis, if not provided 'yVariable' will be used.
#' @param ymin numeric(), optional, lower bounds of y axis, if not NULL y axis will be adopted to data for each column
#' @param ymax numeric(), optional, upper bounds of y axis, if not provided y axis will be adopted to data for each column
#' @param xlim vector, optional, lower and upper bounds of x axis, if not provided y axis will be adapted to data for each row
#' @param barcol character() or named list mapping the values in data[,columnGroups] to colors, color of points in plots
#' @param tickmarks numeric() specifies tickmarks for y-axis
#' @param basetheme ggplot2 - theme function to use for plotting. Default adjusts y-axis label alignments to account for variable width of tick-labels.
stackedPanelsBars <- function(data, columnGroups, rowGroups, xVariable, yVariable, ylab=NULL, xlab=NULL, xlim=NULL, ymin=0, ymax=NULL, barcol="black", tickmarks=NULL, basetheme=function(x){ggplot2::theme_classic() + theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(angle = 90, hjust = 1, size=6))}){

  if(is.numeric(data[,columnGroups]) | is.numeric(data[,rowGroups])){
    stop("ColumnGroups and rowGroups can not be numeric variables. Covert with as.character()")
  }

  if (!all(c(columnGroups, rowGroups, xVariable, yVariable) %in% names(data))){
    stop("Some arguments (columnGroups, rowGroups, xVariable, yVariable) not found in data.")
  }

  if (any(is.na(data[,c(columnGroups, rowGroups, xVariable)]))){
    stop("NAs in grouping variables or x variable")
  }

  if (is.null(ylab)){
    ylab <- yVariable
  }

  if (is.null(xlab)){
    xlab <- xVariable
  }

  rows <- sort(unique(unlist(data[,rowGroups])), decreasing = T)
  cols <- sort(unique(unlist(data[,columnGroups])))
  
  if (is.character(barcol)){
    barcol <- makeColorMap(cols, barcol)
  }
  

  panels <- list()
  for (row in rows){

    #determine ylims if necessary
    minvar <- yVariable
    miny <- ymin
    if (is.null(miny)){
      miny <- min(data[data[,rowGroups] == row,minvar])
      miny <- miny - abs(miny) * .1
    }

    maxvar <- yVariable
    maxy <- ymax
    if (is.null(maxy)){
      maxy <- max(data[data[,rowGroups] == row,maxvar])
      maxy <- maxy + abs(maxy) * .1
    }
    ylimcol <- c(miny, maxy)

    for (col in cols){

      title <- NULL
      if (row == rows[1]){
        title = col
      }

      plotdata <- data[data[,columnGroups] == col & data[,rowGroups] == row,]

      panel <- panelPlot(plotdata, xVariable, yVariable, ylimcol, showX=F, showY=(col == cols[1]), ylabel=row, basetheme=basetheme, title=title, barcol=barcol[[col]], tickmarks=tickmarks)
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

countAtAge <- read.csv("Hauls_With_Age/Hauls_with_Age_data.csv", sep=";", stringsAsFactors = F)
countAtAge$Year <- as.character(countAtAge$Year)
countAtAge$Age <- paste("Age", as.character(countAtAge$Age))
countAtAgeQ1 <- countAtAge[countAtAge$Quarter == 1,]
stackedPanelsBars(data = countAtAgeQ1, columnGroups = "Year", rowGroups = "Age", xVariable = "Haul", yVariable = "Count", xlab="Haul", ylab="Count", ymin=0, barcol = "black", tickmarks = ticks3)

countAtAgeQ3 <- countAtAge[countAtAge$Quarter == 3,]
stackedPanelsBars(data = countAtAgeQ3, columnGroups = "Year", rowGroups = "Age", xVariable = "Haul", yVariable = "Count", xlab="Haul", ylab="Count", ymin=0, barcol = "black", tickmarks = ticks3)
