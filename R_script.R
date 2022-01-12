# Author: Lukas Gabor, 2022

# Library package -----
library (virtualspecies)


# Step 1: Generating artificial occurrences -----

# Upload the virtual species (`vs`) object
load(file = "virtual.species")

# Conversion to presence – absence
PA.raster <- convertToPA(virtual.species, alpha = -0.05, beta = 0.3, plot = T)

# Sampling occurrences with sample prevalence 0.33 (this will sample twice as many absences as presences)
PA.sampling <- sampleOccurrences(PA.raster, n = 300, type = "presence-absence", plot = T, sample.prevalence = 0.33)


# Step 2: Simulating positional error in occurrence data -----

# Upload functions
# Function limiting the shift only to study area 
inside <- function(x, y, mask){
  ins <- ifelse(x < mask@extent@xmin | x > mask@extent@xmax | y < mask@extent@ymin | y >       mask@extent@ymax, FALSE, TRUE)
  if (ins == TRUE) {
    val <- extract(mask, matrix(c(x,y), ncol=2))
    ins <- ifelse(is.na(val), FALSE, TRUE)
  }
  return(ins)
}

# Function shifting occurrences in a random direction according about specified range
shift <- function(x, y, dist, mask){
  new.x <- x
  new.y <- y
  inside <- FALSE
  while (!inside){
    angle <- runif(1,0,360)*pi/180
    new.x <- x + dist*sin(angle)
    new.y <- y + dist*cos(angle)
    inside <- inside(new.x, new.y, mask)
  }
  return(c(new.x, new.y))
}

# Preparing data frame for shifting species occurrences
DATA.Prep <- as.data.frame(PA.sampling $sample.points)
s <- nrow(DATA.Prep)
DATA.Prep$ID <- 1:s

# Example of shifting species occurrences from 100 up to 500 meters
mask <- raster("twi.tif") # Upload the raster for limiting the shift of occurrences only to study area

DATA.Prep[,c("S1x", "S1y")] <-  t(apply(DATA.Prep, 1, function(.) shift(.["x"], .["y"], dist=runif(1, 100, 500), mask=mask)))

# extracting accurate and shifted occurrences
Presence_Absence_XY <- DATA.Prep[,1:2] # unaltered data
S1.Presence_Absence_XY <- DATA.Prep[,5:6] # data shifted from 100 up to 500 metres

# Plotting accurate and shifted occurrences
plot(PA.raster$pa.raster)
points(Presence_Absence_XY, col = "blue")
points(S1.Presence_Absence_XY, col = "red")

