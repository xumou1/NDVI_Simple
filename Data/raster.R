library(raster)
library(tidyverse)
library(ggpubr)
library(ggforce)
library(gridGraphics)

# NDVI function
vi <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

# EVI
evi <- function(img, r, n, b) {
  br <- img[[r]] # Band Red
  bn <- img[[n]] # Band NIR
  bb <- img[[b]] # Band Blue
  evi <- (bn - br) * 2.5 / (1 + bn + 6 * br - 7.5 * bb)
}

# NDWI
wi <- function(img, n,sw) {
  bn = img[[n]]
  bsw = img[[sw]]
  wi <- (bn - bsw)/(bn - bsw)
}

# Raster Data
filenames <- paste0('Landsat5_60m_B', 1:7, ".tif")
filenames

filenames_7 <- paste0('Landsat7_60m_B', 1:7, ".tif")
filenames_7

filenames_sentinel <- paste0('Sentinel_60m_B', 1:12, ".tif")
filenames_sentinel

landsat5 <- stack(filenames)
landsat5

landsat7 <- stack(filenames_7)
landsat7

sentinel <- stack(filenames_sentinel)
sentinel

# True color and False color
landsat5RGB <- landsat5[[c(3, 2, 1)]]
landsat5FCC <- landsat5[[c(4, 3, 2)]]
landsat7RGB <- landsat7[[c(3, 2, 1)]]
landsat7FCC <- landsat7[[c(4, 3, 2)]]
sentinelRGB <- sentinel[[c(4, 3, 2)]]
sentinelFCC <- sentinel[[c(5, 4, 3)]]

# NDVI
NDVI5 <- vi(landsat5, 4, 3)
NDVI7 <- vi(landsat7, 4, 3)
NDVI_sentinel <- vi(sentinel, 8, 4)
EVI5 <- evi(landsat5, 3, 4, 1)
EVI7 <- evi(landsat7, 3, 4, 1)
EVI_sentinel <- evi(sentinel, 4, 8, 2)
NDWI5 <- wi(landsat5, 4, 5)
NDWI7 <- wi(landsat7, 4, 5)
NDWI_sentinel <- wi(sentinel, 5, 12)

plot(NDVI5, col = rev(terrain.colors(10)), main = "NDVI for Landsat5 in 1984", asp = NA, xaxt = "n", yaxt = "n")
NDVI5_image <- recordPlot()
NDVI5_image

plot(NDVI7, col = rev(terrain.colors(10)), main = "NDVI for Landsat7 in 1999", asp = NA, xaxt = "n", yaxt = "n")
NDVI7_image <- recordPlot()
NDVI7_image

plot(NDVI_sentinel, col = rev(terrain.colors(10)), main = "NDVI for Sentinel in 2019", asp = NA, xaxt = "n", yaxt = "n")
NDVI_sentinel_image <- recordPlot()
NDVI_sentinel_image

plotRGB(landsat5FCC, axes = TRUE, stretch = "lin", main = "Landsat5 False color", asp = NA, xaxt = "n", yaxt = "n")
landsat5FCC_image <- recordPlot()
landsat5FCC_image

par(mfrow = c(1, 1))
difference_NDVI84_99 <- NDVI7 - NDVI5
plot(difference_NDVI, asp = NA, xaxt = "n", yaxt = "n")

difference_NDVI <- NDVI_sentinel - NDVI7
plot(difference_NDVI, asp = NA, xaxt = "n", yaxt = "n")

par(mfrow = c(2, 3), mar = c(8, 2, 2, 1))
plot(NDVI5, col = rev(terrain.colors(10)), main = "NDVI for Landsat5 in 1984", asp = NA, xaxt = "n", yaxt = "n")
plot(NDVI7, col = rev(terrain.colors(10)), main = "NDVI for Landsat7 in 1999", asp = NA, xaxt = "n", yaxt = "n")
plot(NDVI_sentinel, col = rev(terrain.colors(10)), main = "NDVI for Sentinel in 2019", asp = NA, xaxt = "n", yaxt = "n")

hist(NDVI5,
     # label = TRUE,
     main = "Distribution of NDVI5 values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "wheat",
     xlim = c(-0.1, 1),
     breaks = 30,
     xaxt = "n")
axis(side=1, at = seq(-0.1,1, 0.05), labels = seq(-0.1,1, 0.05))

hist(NDVI7,
     # label = TRUE,
     main = "Distribution of NDVI7 values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "wheat",
     xlim = c(-0.1, 1),
     breaks = 30,
     xaxt = "n")
axis(side=1, at = seq(-0.1,1, 0.05), labels = seq(-0.1,1, 0.05))

hist(NDVI_sentinel,
     # label = TRUE,
     main = "Distribution of NDVI_Sentinel values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "wheat",
     xlim = c(-0.1, 1),
     breaks = 30,
     xaxt = "n")
axis(side=1, at = seq(-0.1,1, 0.05), labels = seq(-0.1,1, 0.05))

Forest5 <- reclassify(NDVI5, cbind(-Inf, 0.7, NA))
Forest7 <- reclassify(NDVI7, cbind(-Inf, 0.7, NA))
Forest_sentinel <- reclassify(NDVI_sentinel, cbind(-Inf, 0.7, NA))

par(mfrow = c(2, 3))
plot(Forest5, col = rev(terrain.colors(10)), main = "NDVI for Landsat5 in 1984", asp = NA, xaxt = "n", yaxt = "n")
plot(Forest7, col = rev(terrain.colors(10)), main = "NDVI for Landsat7 in 1999", asp = NA, xaxt = "n", yaxt = "n")
plot(Forest_sentinel, col = rev(terrain.colors(10)), main = "NDVI for Sentinel in 2019", asp = NA, xaxt = "n", yaxt = "n")

Bin_NDVI5_1 <- reclassify(NDVI5, cbind(-Inf, 0.7, 0))
Bin_NDVI5 <- reclassify(Bin_NDVI5_1, cbind(0.7, Inf, 1))
Bin_NDVI7_1 <- reclassify(NDVI7, cbind(-Inf, 0.7, 0))
Bin_NDVI7 <- reclassify(Bin_NDVI7_1, cbind(0.7, Inf, 1))
Bin_NDVI_Sentinel_1 <- reclassify(NDVI_sentinel, cbind(-Inf, 0.7, 0))
Bin_NDVI_Sentinel <- reclassify(Bin_NDVI_Sentinel_1, cbind(0.7, Inf, 1))

Result <- Bin_NDVI5 + Bin_NDVI7 + Bin_NDVI_Sentinel
Result_sub1 <- Bin_NDVI5 + Bin_NDVI7
Result_sub2 <- Bin_NDVI5 + Bin_NDVI_Sentinel
Result_sub3 <- Bin_NDVI7 + Bin_NDVI_Sentinel

par(mfrow=c(1,1),mar = c(8, 2, 2, 1))

par(mfrow = c(2, 1), mar = c(1.6, 8, 2, 2))
plot(Result, main = "Result of Combination", asp = NA, xaxt = "n", yaxt = "n")
hist(Result,
     unique(values(Result)),
     label = TRUE,
     main = "Distribution of Results values",
     xlab = "Results Value",
     ylab= "Number of Cell",
     col = "green",
     # xlim = c(-0.1, 1),
     # breaks = 30,
     xaxt = "n")
axis(side=1, at = seq(0, 3, 1), labels = seq(0, 3, 1))

par(mfrow = c(2, 3),mar = c(8, 2, 2, 1))
plot(Result_sub1, main = "Result of Sub_Combination(1984-1999)", asp = NA, xaxt = "n", yaxt = "n")
plot(Result_sub2, main = "Result of Sub_Combination(1984-2019)", asp = NA, xaxt = "n", yaxt = "n")
plot(Result_sub3, main = "Result of Sub_Combination(1999-2019)", asp = NA, xaxt = "n", yaxt = "n")
hist(Result_sub1,
     unique(values(Result)),
     label = TRUE,
     main = "Distribution of Results values(1984-1999)",
     xlab = "Results Value",
     ylab= "Number of Cell",
     col = "green",
     # xlim = c(-0.1, 1),
     # breaks = 30,
     xaxt = "n")
axis(side=1, at = seq(0, 2, 1), labels = seq(0, 2, 1))
hist(Result_sub2,
     unique(values(Result)),
     label = TRUE,
     main = "Distribution of Results values(1984-2019)",
     xlab = "Results Value",
     ylab= "Number of Cell",
     col = "green",
     # xlim = c(-0.1, 1),
     # breaks = 30,
     xaxt = "n")
axis(side=1, at = seq(0, 2, 1), labels = seq(0, 2, 1))
hist(Result_sub3,
     unique(values(Result)),
     label = TRUE,
     main = "Distribution of Results values(1999-2019)",
     xlab = "Results Value",
     ylab= "Number of Cell",
     col = "green",
     # xlim = c(-0.1, 1),
     # breaks = 30,
     xaxt = "n")
axis(side=1, at = seq(0, 2, 1), labels = seq(0, 2, 1))

# par(mfrow = c(2, 2))
# plot(b1, main = "Blue", col = gray(0:100/100))
# plot(b2, main = "Green", col = gray(0:100/100))
# plot(b3, main = "Red", col = gray(0:100/100))
# plot(b4, main = "NIR", col = gray(0:100/100))

# landsatRGB <- stack(b3, b2, b1)
# plotRGB(landsatRGB, axes = TRUE, stretch = "lin", main = "Landsat5 Ture Color Composite")

# par(mfrow = c(1, 2))
# plotRGB(landsatRGB, axes = TRUE, stretch = "lin", main = "Landsat5 Ture Color Composite")
# landsatFCC <- stack(b4, b3, b2)
# plotRGB(landsatFCC, axes = TRUE, stretch = "lin", main = "Landsat5 False Color Composite")

# landsatsub1 <- subset(landsat5, 1:3)
# landsatsub2 <- landsat5[[1:3]]
# nlayers(landsat5)
# nlayers(landsatsub1)
# nlayers(landsatsub2)

# Change the band name
# landsat <- subset(landsat5, 1:7)
# names(landsat)

# names(landsat) <- c('blue', 'green', 'red', 'NIR', 'SWIR1', 'LWIR', 'SWIR2')
# names(landsat)

# Clip to extent
# extent(landsat)
# e <- extent(423000, 463000, 4804000, 4824000)

# landsatcrop <- crop(landsat, e)

# Save raster in processing
# writeRaster(landsatcrop, filename = "cropped-landsat.tif", overwrite = TRUE)

# pairs(landsatcrop[[1:2]], main = "Blue versus Green")
# pairs(landsatcrop[[3:4]], main = "Red versus NIR")

# par(mfrow = c(1, 1))
# ndvi7 <- vi(landsat7, 4, 3)
# plot(ndvi7, col = rev(terrain.colors(10)), main = "Lansat7-NDVI")

# veg <- reclassify(ndvi7, cbind(-Inf, 0.4, NA))
# plot(veg, main = 'Vegetation')

# land <- reclassify(ndvi7, c(-Inf, 0.25, NA, 0.25, 0.3, 1, 0.3, Inf, NA))
# plot(land, main = 'Reclassify vegetation')

# plotRGB()

nr5 <- getValues(NDVI5)
str(nr5)

set.seed(99)

kmncluster <- kmeans(na.omit(nr5), centers = 10, iter.max = 500, nstart = 5, algorithm = "Lloyd")
str(kmncluster)