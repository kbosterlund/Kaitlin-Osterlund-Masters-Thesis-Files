# Temperature Cycles and Spatial Synchrony
# Wavelets Analysis
# Kaitlin Osterlund

rm(list=ls(all=T))

setwd("/Users/kaitlinosterlund/Documents/Masters Files/Wavelets")

library(stats)
library(utils)
library(dplR)
library(tidyverse)

# Tetrahymena
detach(tet)
tet <- read.csv("ALL_Tetrahymena.csv", header=T)
attach(tet)

tetra <- log(tet + 1)

# Nomenclature based on jar ID
# First letter indicates temperature cycle (A=16d, B=20d, C=24d, D=28d, E=32d, F=36d)
# Second letter based on enrichment level (L=low, H=high)
# Third letter based on predation (T=tetrahymena only, E=euplotes present)
# Last number based on jar replicate (1 through 5)

# ALE2 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(ALE2)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ALE3 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(ALE3)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ALE4 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(ALE4)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ALE5 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(ALE5)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# AHE1 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(AHE1)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# AHE2 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(AHE2)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# AHE3 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(AHE3)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# AHE4 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(AHE4)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# AHE5 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(AHE5)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BLE2 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(BLE2)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BLE3 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(BLE3)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BLE4 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(BLE4)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BLE5 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(BLE5)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BHE1 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(BHE1)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BHE2 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(BHE2)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BHE3 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(BHE3)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BHE4 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(BHE4)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BHE5 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(BHE5)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CLE1 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(CLE1)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CLE2 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(CLE2)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CLE3 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(CLE3)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CLE4 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(CLE4)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CLE5 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(CLE5)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CHE1 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(CHE1)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CHE2 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(CHE2)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CHE3 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(CHE3)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CHE4 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(CHE4)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CHE5 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(CHE5)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DLE1 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(DLE1)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DLE2 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(DLE2)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DLE3 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(DLE3)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DLE4 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(DLE4)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DLE5 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(DLE5)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DHE1 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(DHE1)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DHE2 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(DHE2)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DHE3 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(DHE3)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DHE4 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(DHE4)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DHE5 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(DHE5)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ELE1 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(ELE1)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ELE2 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(ELE2)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ELE3 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(ELE3)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ELE4 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(ELE4)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ELE5 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(ELE5)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# EHE2 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(EHE2)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# EHE3 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(EHE3)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# EHE4 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(EHE4)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# EHE5 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(EHE5)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FLE1 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(FLE1)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FLE2 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(FLE2)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FLE3 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(FLE3)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FLE4 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(FLE4)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FLE5 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(FLE5)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FHE1 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(FHE1)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FHE2 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(FHE2)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FHE3 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(FHE3)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FHE4 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(FHE4)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FHE5 Tetrahymena
#####
tetra_sub <-
  
  tetra %>%
  
  select(FHE5)

tetra.rwi <- detrend(rwl = tetra_sub, method = "ModNegExp")
tetra.crn <- chron(tetra.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(tetra.crn)
CAMstd <- tetra.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####



# Euplotes
detach(eup)
eup <- read.csv("ALL_Euplotes.csv", header=T)
attach(eup)

euplotes <- log(eup + 1)

# ALE1 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(ALE1)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ALE2 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(ALE2)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ALE3 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(ALE3)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ALE4 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(ALE4)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ALE5 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(ALE5)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# AHE1 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(AHE1)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# AHE2 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(AHE2)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# AHE3 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(AHE3)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# AHE4 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(AHE4)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# AHE5 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(AHE5)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BLE1 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(BLE1)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BLE2 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(BLE2)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BLE3 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(BLE3)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BLE4 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(BLE4)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BLE5 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(BLE5)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BHE1 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(BHE1)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BHE2 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(BHE2)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BHE3 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(BHE3)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BHE4 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(BHE4)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# BHE5 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(BHE5)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CLE1 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(CLE1)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CLE2 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(CLE2)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CLE3 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(CLE3)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CLE4 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(CLE4)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CLE5 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(CLE5)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CHE1 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(CHE1)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CHE2 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(CHE2)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CHE3 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(CHE3)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CHE4 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(CHE4)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# CHE5 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(CHE5)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DLE1 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(DLE1)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DLE2 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(DLE2)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DLE3 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(DLE3)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DLE4 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(DLE4)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DLE5 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(DLE5)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DHE1 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(DHE1)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DHE2 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(DHE2)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DHE3 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(DHE3)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DHE4 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(DHE4)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# DHE5 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(DHE5)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ELE1 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(ELE1)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ELE2 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(ELE2)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ELE3 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(ELE3)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ELE4 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(ELE4)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# ELE5 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(ELE5)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# EHE1 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(EHE1)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# EHE2 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(EHE2)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# EHE3 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(EHE3)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# EHE4 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(EHE4)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# EHE5 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(EHE5)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FLE1 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(FLE1)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FLE2 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(FLE2)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FLE3 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(FLE3)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FLE4 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(FLE4)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FLE5 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(FLE5)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FHE1 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(FHE1)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FLE2 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(FHE2)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FHE3 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(FHE3)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FHE4 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(FHE4)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
# FHE5 Euplotes
#####
euplotes_sub <-
  
  euplotes %>%
  
  select(FHE5)

euplotes.rwi <- detrend(rwl = euplotes_sub, method = "ModNegExp")
euplotes.crn <- chron(euplotes.rwi, prefix = "CAM", prewhiten = FALSE)
Years <- time(euplotes.crn)
CAMstd <- euplotes.crn[, 1]
out.wave <- morlet(y1 = CAMstd, x1 = Years, dj = 0.25, siglvl = 0.95)
wavelet.plot(out.wave, useRaster = NA)
#####
