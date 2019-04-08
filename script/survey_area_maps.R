library(rgdal)
library(rworldxtra)
library(mapplots)
library(raster)
library(RColorBrewer)

#
# shapefiles etc
#
rfa <-
  readOGR(file.path(
    system.file("shapefiles", package = "IBTSindices"),
    "Roundfish_shapefiles"
  ))
rfa_labelpos <- coordinates(rfa)
rfa_labelpos[9,1] <- rfa_labelpos[9,1]+0.4 #adjust position for area 3
rfa_labelpos[6,1] <- rfa_labelpos[6,1]+0.2 #adjust position for area 5
rfa_labelpos[6,2] <- rfa_labelpos[6,2]+0.3 #adjust position for area 5
rfa_labelpos[6,2] <- rfa_labelpos[6,2]+0.2 #adjust position for area 5
rfa_labelpos[5,2] <- rfa_labelpos[5,2]+0.2 #adjust position for area 6
rfa_labelpos[9,1] <- rfa_labelpos[9,1]+0.3 #adjust position for area 3
rfa_labelpos[2,1] <- rfa_labelpos[2,1] #adjust position for area 9
rfa_labelpos[2,2] <- rfa_labelpos[2,2]+0.1 #adjust position for area 9

ices_rectangles <- readOGR(file.path(system.file("shapefiles", package = "IBTSindices"), "ICES_StatRec_mapto_ICES_Areas"), "StatRec_map_Areas_Full_20170124")
ices_midpoints <- SpatialPoints(as.data.frame(coordinates(ices_rectangles)), proj4string=CRS(proj4string(ices_rectangles)))
ibts_rectangles <- ices_rectangles[!is.na(unlist(sp::over(ices_midpoints, rfa))),]

ns <- readRDS(file.path(system.file("bathymetry", package = "IBTSindices"), "wgs84_raster_north_sea_crop", "ns_halfres.rds"))*-1
data(countriesHigh)
map <- countriesHigh

#Needs fiddling when final output dimensions are decided
map_labels <- c("Denm.", "France", "Germany", "Netherl.", "Norway", "UK Scotl.", "Sweden", "UK Engl.")
map_labelpoints <- t(matrix(c(
  10.9, 55.6,
  2.5, 49.3,
  10, 51,
  6.9, 52.28,
  9.4, 60.7,
  -5, 57,
  14, 59,
  -1.9, 52.4), ncol=length(map_labels), nrow=2))

#
# colors
#
batpal <- brewer.pal(9, "Blues")
batpal <- c(batpal[1:(length(batpal)-3)], rep(batpal[7],4),rep(batpal[8],4), rep(batpal[9],4))


plot_ibts_map <- function(polygons = rfa, labelcol="AreaName", labelpos=rfa_labelpos, polygonscol="black", cex.polygons=0.8, rectangles=ibts_rectangles, rectanglecol="lightgray", landmap=map, landcol="lightgrey", landbordercol="darkgrey", landlabels=map_labels, land_labelpoints=map_labelpoints, land_labelcol="black", cex.landlabels=0.6, seacol="white", bat=ns, batcol=batpal, xlim=c(-8,18), ylim=c(49,62.5), xlab="Longitude", ylab="Latitude"){

  if (!is.null(bat)){
    ext <- as(extent(xlim[1], xlim[2], ylim[1], ylim[2]), 'SpatialPolygons')
    plot(crop(bat, ext), col=batcol, xlab=xlab, ylab=ylab, legend.args=list(text="depth (m)"))
  }
  else{
    basemap(
      xlim = xlim,
      ylim = ylim,
      bg = seacol,
      xlab=xlab, ylab=ylab
    )
  }
  plot(landmap, col = landcol, add = T, border="darkgrey")
  if (!is.null(polygons)) {
    if (!is.null(rectangles)){
      plot(rectangles, add=T, border=rectanglecol)
    }
    plot(polygons, add = T, border=polygonscol)
    if (!is.null(labelcol)){
      if (is.null(labelpos)){
        labelpos <- coordinates(polygons)
      }
      text(labelpos, as.character(polygons@data[[labelcol]]), col=polygonscol, cex=cex.polygons)
    }
    if (!is.null(land_labelpoints)){
      text(land_labelpoints, landlabels, cex=cex.landlabels, col=land_labelcol)
    }
  }
}

plot_on_pdf <- function(filename){
  pdf(filename, width=8, height=4)
  old.par <- par(no.readonly = T)
  par(mfrow=c(1,2), mar=c(5.1, 4.1, 4.1, 4.1))
  plot_ibts_map(bat = NULL, land_labelpoints = NULL)
  par(mar=c(5.1, 1.1, 4.1, 7.1))
  plot_ibts_map(rectangles = NULL, ylab="", labelcol = NULL)
  par(old.par)
  dev.off()
}
plot_on_pdf("surveyarea.esp")
