# https://rpubs.com/jguelat/autocorr
# https://gis.stackexchange.com/questions/372925/constructing-a-raster-with-morans-i-close-to-1

# check.packages function: install and load multiple R packages.
# Found this function here: https://gist.github.com/smithdanielle/9913897 on 2019/06/17
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}

# Check if packages are installed and loaded:
packages <- c("janitor",  "tidyverse", "utils", "here", "raster","gstat", "lattice", "ncf", "viridis", "ggthemes", "patchwork")
check.packages(packages)

library(raster)
library(gstat)
library(lattice)
library(spdep)

set.seed(45)



# Converting for Plot

plotRaster_acr <- function(title, raster){
  
  
  test_spdf <- as(raster, "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  colnames(test_df) <- c("value", "x", "y")
  
  p <-ggplot() +
    geom_tile(data= test_df, aes(x=x, y=y, fill=factor(value)), alpha=0.4) +
    scale_fill_viridis(direction = -1, discrete = TRUE) +
    coord_equal() +
    scale_x_continuous(breaks =seq(from = 0, to = 1.03, by = round(1/5,2)), limits = c(0,1.04))+
    scale_y_continuous(breaks =seq(from = 0, to = 1.03, by = round(1/5,2)), limits = c(0,1.04))+
    theme_minimal()+
    labs( x= "lat", y = "long", fill = "Variable", subtitle = paste0(title,": ",round(Moran(raster, w = rook),2)))+
    theme(legend.position="none") 
  
  return(p)
}

moran_ggplot <- function(raster) {
  spdf_2 <- rasterToPolygons(raster)
  
  sf_data <- st_as_sf(spdf_2)
  
  mp <-spdf_2 %>% 
    poly2nb() %>%
    nb2listw(zero.policy = TRUE) %>%
    moran.plot(spdf_2$layer, . , zero.policy = TRUE,
               xlab = "Variable",
               ylab = "Lagged Neighbor Variable") 
  p <- ggplot(mp, aes(x=x, y=wx)) + geom_point(shape=1) + 
    geom_smooth(formula=y ~ x, method="lm") + 
    geom_hline(yintercept=mean(mp$wx), lty=2) + 
    geom_vline(xintercept=mean(mp$x), lty=2) + theme_minimal() + 
    geom_point(data=mp[mp$is_inf,], aes(x=x, y=wx), shape=9) +
    geom_text(data=mp[mp$is_inf,], aes(x=x, y=wx, label=labels, vjust=1.5)) +
    xlab("Variable") + ylab(paste0("Spatially lagged ", "Variable"))
  
  return(p)
}



  
# Making a raster grid
# Function for raster with n x n


# Rook Neigbour Calculation | Rook Neighbor means just adjacent ones are neibhbor. Other pattern is Queen qhich takes all eight around it
# For differentiation remeber: Rook - All single movements that a rook can make, for queen all single movements that a queen can make

rook = matrix(c(0,1,0,1,0,1,0,1,0),3,3)
# Creating 5 x 5 Matrix Grid
n = 5 
r_test <- raster(nrows = n, ncols = n, xmn = 0,xmx = 1, ymn = 0, ymx=1)


# Clustered Data - +ive AutoCorrelation
values (r_test) <- 0
values (r_test) [seq (1, 10)] <- 1
p_pac <- plotRaster_acr("Positive Autocorrelation", r_test)
#m_pac <- moran_ggplot(r_test)
  
# Negative AutoCorrelation
values (r_test) <- 0
values (r_test) [seq (1, n^2, by = 2)] <- 1
p_nac <- plotRaster_acr("Negative Autocorrelation", r_test)
#m_nac <- moran_ggplot(r_test)

# No AutoCorrelation
values (r_test) <- 0
values (r_test) [sample.int (n^2, n^2 / 2)] <- 1
p_zac <- plotRaster_acr("~No Autocorrelation", r_test)
#m_zac <- moran_ggplot(r_test)

p_pac + p_nac + p_zac








# Define function to draw random samples from a multivariate normal
# distribution
rmvn <- function(n, mu = 0, V = matrix(1)) {
  p <- length(mu)
  if (any(is.na(match(dim(V), p)))) 
    stop("Dimension problem!")
  D <- chol(V)
  t(matrix(rnorm(n * p), ncol = p) %*% D + rep(mu, rep(n, p)))
}

# Set up a square lattice region
simgrid <- expand.grid(1:20, 1:20)
n <- nrow(simgrid)

# Set up distance matrix
distance <- as.matrix(dist(simgrid))
# Generate random variable
phi <- 0.05
X <- rmvn(1, rep(0, n), exp(-phi * distance))

# Visualize results
Xraster <- rasterFromXYZ(cbind(simgrid[, 1:2] - 0.5, X))
par(mfrow = c(1, 2))
#plot(1:100, exp(-phi * 1:100), type = "l", xlab = "Distance", ylab = "Correlation")
plot(Xraster, main = "Distribution")
f <- matrix(c(0,1,0,1,0,1,0,1,0), nrow=3)
plot(MoranLocal(Xraster, w=f), main = "Local Moran I")
