# Temperature Cycles and Spatial Synchrony
# Correlation analysis and Permutation tests
# Kaitlin Osterlund

rm(list=ls(all=T))

setwd("/Users/kaitlinosterlund/Documents/Masters Files/Correlation")

# Required packages
library(Hmisc)
library(coin)
library(tidyverse)
library(magrittr)
library(reshape2)
library(ggplot2)
library(ggcorrplot)

#####
# Tetrahymena
#####

detach(tetra)
tetra <- read.csv("ALL_Tetrahymena.csv", header = T)
attach(tetra)

names(tetra)

# log transformation
log.tetra <- log(tetra + 1)

cor.tetra <- cor(log.tetra)
round(cor.tetra,2)

cor2.tetra <- rcorr(as.matrix(log.tetra))

# extract the correlation coefficients
cor2.tetra$r

# extract the p values
cor2.tetra$P

# visualize correlation
#compute a matrix of correlation p-values
p.mat <- cor_pmat(log.tetra)
#add correlation significance level
#argument p.mat
#barring the no significant coefficients
ggcorrplot(cor.tetra, type = "lower", p.mat = p.mat, ggtheme = ggplot2::theme_grey, tl.cex = 6)

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

options(max.print = 10000)
tetra.matrix <- flattenCorrMatrix(cor2.tetra$r,cor2.tetra$P)

# Permutation test
# Factor by temperature cycle
tetra.matrix1 <-
  
  tetra.matrix%>%
  
  mutate(tempcross = paste0(str_sub(row,1,1), str_sub(column,1,1)))

tetra.matrix1$tempcross.fac <- as.factor(tetra.matrix1$tempcross)

tetra.permutation1 <- oneway_test(tetra.matrix1$cor ~ tetra.matrix1$tempcross.fac, 
                                    distribution = approximate (nresample = 100000))
tetra.permutation1

# Permutation test
# Factor by enrichment
tetra.matrix2 <-
  
  tetra.matrix%>%
  
  mutate(richcross = paste0(str_sub(row,2,2), str_sub(column,2,2)))

tetra.matrix2$richcross.fac <- as.factor(tetra.matrix2$richcross)

tetra.permutation2 <- oneway_test(tetra.matrix2$cor ~ tetra.matrix2$richcross.fac, 
                                    distribution = approximate (nresample = 100000))
tetra.permutation2

# Permutation test
# Factor by temperature cycle and enrichment
tetra.matrix3 <-
  
  tetra.matrix%>%
  
  mutate(temprichcross = paste0(str_sub(row,1,2), str_sub(column,1,2)))

tetra.matrix3$temprichcross.fac <- as.factor(tetra.matrix3$temprichcross)

tetra.permutation3 <- oneway_test(tetra.matrix3$cor ~ tetra.matrix3$temprichcross.fac, 
                                    distribution = approximate (nresample = 100000))
tetra.permutation3



#####
# Euplotes
#####

detach(eup)
eup <- read.csv("ALL_Euplotes.csv", header = T)
attach(eup)

names(eup)

all.eup <- log(eup + 1)

all.eup.cor <- cor(all.eup)
round(all.eup.cor,2)

all.eup.cor2 <- rcorr(as.matrix(all.eup))

# extract the correlation coefficients
all.eup.cor2$r

# extract the p values
all.eup.cor2$P

# visualize correlation
#compute a matrix of correlation p-values
p.mat <- cor_pmat(all.eup)
p.mat
#add correlation significance level
#argument p.mat
#barring the no significant coefficients
ggcorrplot(all.eup.cor, type = "lower", p.mat = p.mat, ggtheme = ggplot2::theme_grey, tl.cex = 6)

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

options(max.print = 10000)
all.eup.matrix <- flattenCorrMatrix(all.eup.cor2$r,all.eup.cor2$P)

# Permutation test
# Factor by temperature cycle
all.eup.matrix1 <-
  
  all.eup.matrix%>%
  
  mutate(tempcross = paste0(str_sub(row,1,1), str_sub(column,1,1)))

all.eup.matrix1$tempcross.fac <- as.factor(all.eup.matrix1$tempcross)

all.eup.permutation1 <- oneway_test(all.eup.matrix1$cor ~ all.eup.matrix1$tempcross.fac, 
                                    distribution = approximate (nresample = 100000))
all.eup.permutation1

# Permutation test
# Factor by enrichment
all.eup.matrix2 <-
  
  all.eup.matrix%>%
  
  mutate(richcross = paste0(str_sub(row,2,2), str_sub(column,2,2)))

all.eup.matrix2$richcross.fac <- as.factor(all.eup.matrix2$richcross)

all.eup.permutation2 <- oneway_test(all.eup.matrix2$cor ~ all.eup.matrix2$richcross.fac, 
                                    distribution = approximate (nresample = 100000))
all.eup.permutation2

# Permutation test
# Factor by temperature cycle and enrichment
all.eup.matrix3 <-
  
  all.eup.matrix%>%
  
  mutate(temprichcross = paste0(str_sub(row,1,2), str_sub(column,1,2)))

all.eup.matrix3$temprichcross.fac <- as.factor(all.eup.matrix3$temprichcross)

all.eup.permutation3 <- oneway_test(all.eup.matrix3$cor ~ all.eup.matrix3$temprichcross.fac, 
                                    distribution = approximate (nresample = 100000))
all.eup.permutation3


#####
# Boxplots
#####

# Tetrahymena factored by temperature cycle
box1 <- ggplot(tetra.matrix1, aes(x = tempcross.fac, y = cor))+
  geom_boxplot()+
  labs(x="Temperature Cycle Cross",y="Cross Correlation Value")
box1 + theme(plot.title = element_text(hjust = 0.5))

# Tetrahymena factored by enrichment
box2 <- ggplot(tetra.matrix2, aes(x = richcross.fac, y = cor))+
  geom_boxplot()+
  labs(x="Enrichment Level Cross",y="Cross Correlation Value")
box2 + theme(plot.title = element_text(hjust = 0.5))

# Tetrahymena factored by temperature cycle and enrichment
box3 <- ggplot(tetra.matrix3, aes(x = temprichcross.fac, y = cor))+
  geom_boxplot()+
  labs(x="Temperature Cycle and Enrichment Level Cross",y="Cross Correlation Value")
box3 + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45))

# Euplotes factored by temperature cycle full time series
box1 <- ggplot(all.eup.matrix1, aes(x = tempcross.fac, y = cor))+
  geom_boxplot()+
  labs(x="Temperature Cycle Cross",y="Cross Correlation Value")
box1 + theme(plot.title = element_text(hjust = 0.5))

# Euplotes factored by enrichment full time series
box2 <- ggplot(all.eup.matrix2, aes(x = richcross.fac, y = cor))+
  geom_boxplot()+
  labs(x="Enrichment Level Cross",y="Cross Correlation Value")
box2 + theme(plot.title = element_text(hjust = 0.5))

# Euplotes factored by temperature cycle and enrichment full time series
box3 <- ggplot(all.eup.matrix3, aes(x = temprichcross.fac, y = cor))+
  geom_boxplot()+
  labs(x="Temperature Cycle and Enrichment Level Cross",y="Cross Correlation Value")
box3 + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45))

# End Script