library(usethis)
library(raster)


# creat the 'plotsamp' data -----------------------------------------------

set.seed(123)
samp_mat <- t(matrix(seq(1,25,1),5,5))
samp_grid <- raster(samp_mat)
extent(samp_grid) <- c(0,100,0,100)
N <- 150
s <- cbind(runif(N,0,100),
           runif(N,0,100))

plot_df <- extract(samp_grid,SpatialPoints(s), df=TRUE)
plot_df$layer <- factor(plot_df$layer, levels=1:25)

plotsamp <- as.data.frame(table(plot_df$layer))
colnames(plotsamp) <- c("Plot","Count")

plotsamp$X <- coordinates(samp_grid)[,1]
plotsamp$Y <- coordinates(samp_grid)[,2]

use_data(plotsamp, name="plotsamp")
