x <- c("ggmap", "rgdal", "rgeos", "maptools", "dplyr", "tidyr", "tmap")

# install.packages(x)

lapply(x, library, character.only = TRUE) # load the required packages
