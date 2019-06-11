library(IBTSindices)
path = "Papers/manuscript/results/olav/resamplingOtoliths/"
quarter =3
n = 500
#procedure = "haulBased"
procedure = "datras"
art = "cod"

if(art=="cod"){
  conf = confALK(species = "Gadus morhua", quarter = quarter)
  minAge = conf$minAge
  maxAge = conf$maxAge
  minAgePlot = minAge;maxAgePlot = maxAge
}else if(art=="Saithe"){
  conf = confALK(species = "Pollachius virens", quarter = quarter)
  minAge = conf$minAge
  maxAge = conf$maxAge
  minAgePlot = 3;maxAgePlot = 8
}

library(RColorBrewer)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
col_vector = col_vector[-c(4,6,6)] #Exclude yellow and some other similar colors.

path = "Papers/manuscript/results/olav/resamplingOtoliths/"

for(year in 2015:2018){
#  x11()
  jpeg(paste(path,"removal",art,year,"Q",quarter,".jpeg", sep = ""),height = 500,width = 500)
  plotRemoval(year,art,quarter,n,procedure ,minAge,maxAge,minAgePlot,maxAgePlot,path)
  dev.off()
}


for(year in 2015:2018){
#  x11()
  jpeg(paste(path,"removal",art,year,"Q",quarter,"DL5.jpeg", sep = ""),height = 500,width = 500)
  plotRemoval(year,art,quarter,n,procedure ,minAge,maxAge,minAgePlot,maxAgePlot,path,dl = 5)
  dev.off()
}


#Resample both N and O
path = "Papers/manuscript/results/olav/resamplingNandOtoliths/"
for(year in 2015:2018){
#  x11()
  jpeg(paste(path,"resampleNandO",art,year,"Q",quarter,"DL5",procedure ,".jpeg", sep = ""),height = 700,width = 1000)
  plotRemovalNandO(year,art,quarter,procedure ,minAge,maxAge,path,dl = 5)
  legend("topright", legend = c("1 fish per 5cm", "5 fish per 5cm"), cex=1.8, col = c("red", "black"),lwd = 3, bty ="n")
  dev.off()
}


#Resample both N and O, older years.
maxAge = 9
path = "Papers/manuscript/results/olav/resamplingNandOtoliths/"
for(year in 1997:1999){
#  x11()
  jpeg(paste(path,"resampleNandO",art,year,"Q",quarter,"DL5",procedure ,".jpeg", sep = ""),height = 700,width = 1000)
  plotRemovalNandO(year,art,quarter,procedure ,minAge,maxAge,path,dl = 5,ylim = c(0,3))
  legend("topright", legend = c("1 fish per 5cm", "5 fish per 5cm"), cex=1.8, col = c("red", "black"),lwd = 3, bty ="n")
    dev.off()
}



#calculating number of otoliths removed, sampled and percentage-----------------------------
path = "Papers/manuscript/results/olav/resamplingOtoliths/"
b =readRDS(paste(path, "RemovalCodDl2year2018Q3n500haulBased1", sep =""))
mean(attr(b, "nOtolithsTotal"))
mean(attr(b, "nOtolithsRemoved"))
c = mean(attr(b, "nOtolithsTotal")-attr(b, "nOtolithsRemoved"))
c
(c/mean(attr(b, "nOtolithsTotal")))*100

