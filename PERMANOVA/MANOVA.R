# Temperature Cycles and Spatial Synchrony
# PERMANOVA
# Kaitlin Osterlund

rm(list=ls(all=T))

setwd("/Users/kaitlinosterlund/Documents/Masters Files/MANOVA")

library(tidyverse)
library(vegan)
library(devtools)
library(pairwiseAdonis)

### Tetrahymena no predation - Temperature Cycles ####
detach(tetra_temp_nopred)
tetra_temp_nopred <- read.csv("tetra_temp_nopred.csv", header = T)
attach(tetra_temp_nopred)

# PERMANOVA
grouped_tetra_temp_nopred <- tetra_temp_nopred[,c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8",
                                    "T9", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T17",
                                    "T18", "T19", "T20", "T21", "T22", "T23", "T24", "T25", "T26", "T27",
                                    "T28", "T29", "T30", "T31", "T32", "T33", "T34", "T35")]
adonis(grouped_tetra_temp_nopred ~ tetra_temp_nopred$Cycle, method = "euclidean", permutations = 999)

# Post-hoc
pairwise.adonis(grouped_tetra_temp_nopred, tetra_temp_nopred$Cycle, sim.method = "euclidean",
                p.adjust.m = "bonferroni")

### Tetrahymena no predation - Enrichment Levels ####
detach(tetra_enrich_nopred)
tetra_enrich_nopred <- read.csv("tetra_enrich_nopred.csv", header = T)
attach(tetra_enrich_nopred)

# PERMANOVA
grouped_tetra_enrich_nopred <- tetra_enrich_nopred[,c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8",
                                        "T9", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T17",
                                        "T18", "T19", "T20", "T21", "T22", "T23", "T24", "T25", "T26", "T27",
                                        "T28", "T29", "T30", "T31", "T32", "T33", "T34", "T35")]
adonis(grouped_tetra_enrich_nopred ~ tetra_enrich_nopred$Enrichment, method = "euclidean", permutations = 999)

# Post-hoc
pairwise.adonis(grouped_tetra_enrich_nopred, tetra_enrich_nopred$Enrichment, sim.method = "euclidean",
                p.adjust.m = "bonferroni")


### Tetrahymena no predation - Temperature Cycles and Enrichment Levels ####
detach(tetra_temp_enrich_nopred)
tetra_temp_enrich_nopred <- read.csv("tetra_temp_enrich_nopred.csv", header = T)
attach(tetra_temp_enrich_nopred)

# PERMANOVA
grouped_tetra_temp_enrich_nopred <- tetra_temp_enrich_nopred[,c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8",
                                                  "T9", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T17",
                                                  "T18", "T19", "T20", "T21", "T22", "T23", "T24", "T25", "T26", "T27",
                                                  "T28", "T29", "T30", "T31", "T32", "T33", "T34", "T35")]
adonis(grouped_tetra_temp_enrich_nopred ~ tetra_temp_enrich_nopred$Cycle_Enrichment, method = "euclidean", permutations = 999)

# Post-hoc
pairwise.adonis(grouped_tetra_temp_enrich_nopred, tetra_temp_enrich_nopred$Cycle_Enrichment, sim.method = "euclidean",
                p.adjust.m = "bonferroni")

### Tetrahymena - Temperature Cycles ####
detach(tetra_temp)
tetra_temp <- read.csv("Tetrahymena_Temperature_Cycles.csv", header = T)
attach(tetra_temp)

# PERMANOVA
grouped_tetra_temp <- tetra_temp[,c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8",
                   "T9", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T17",
                   "T18", "T19", "T20", "T21", "T22", "T23", "T24", "T25", "T26", "T27",
                   "T28", "T29", "T30", "T31", "T32", "T33", "T34", "T35")]
adonis(grouped_tetra_temp ~ tetra_temp$Cycle, method = "euclidean", permutations = 999)

### Tetrahymena - Enrichment Levels ####
detach(tetra_enrich)
tetra_enrich <- read.csv("Tetrahymena_Enrichment_Levels.csv", header = T)
attach(tetra_enrich)

# PERMANOVA
grouped_tetra_enrich <- tetra_enrich[,c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8",
                                        "T9", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T17",
                                        "T18", "T19", "T20", "T21", "T22", "T23", "T24", "T25", "T26", "T27",
                                        "T28", "T29", "T30", "T31", "T32", "T33", "T34", "T35")]
adonis(grouped_tetra_enrich ~ tetra_enrich$Enrichment, method = "euclidean", permutations = 999)

### Tetrahymena - Temperature Cycles and Enrichment Levels ####
detach(tetra_temp_enrich)
tetra_temp_enrich <- read.csv("Tetrahymena_Temperature_Cycles_and_Enrichment_Levels.csv", header = T)
attach(tetra_temp_enrich)

# PERMANOVA
grouped_tetra_temp_enrich <- tetra_temp_enrich[,c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8",
                                                  "T9", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T17",
                                                  "T18", "T19", "T20", "T21", "T22", "T23", "T24", "T25", "T26", "T27",
                                                  "T28", "T29", "T30", "T31", "T32", "T33", "T34", "T35")]
adonis(grouped_tetra_temp_enrich ~ tetra_temp_enrich$Cycle_Enrichment, method = "euclidean", permutations = 999)

### Euplotes- Temperature Cycles ####
detach(euplotes_temp)
euplotes_temp <- read.csv("Euplotes_Temperature_Cycles.csv", header = T)
attach(euplotes_temp)

# PERMANOVA
# null hypothesis: 
# groups do not differ in spread or position in 
# multivariate space.

grouped_euplotes_temp <- euplotes_temp[,c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8",
                                    "T9", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T17",
                                    "T18", "T19", "T20", "T21", "T22", "T23", "T24", "T25", "T26", "T27",
                                    "T28", "T29", "T30", "T31", "T32", "T33", "T34", "T35")]
adonis(grouped_euplotes_temp ~ euplotes_temp$Cycle, method = "euclidean", permutations = 999)

### Euplotes - Enrichment Levels ####
detach(euplotes_enrich)
euplotes_enrich <- read.csv("Euplotes_Enrichment_Levels.csv", header = T)
attach(euplotes_enrich)

# PERMANOVA
grouped_euplotes_enrich <- euplotes_enrich[,c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8",
                                          "T9", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T17",
                                          "T18", "T19", "T20", "T21", "T22", "T23", "T24", "T25", "T26", "T27",
                                          "T28", "T29", "T30", "T31", "T32", "T33", "T34", "T35")]
adonis(grouped_euplotes_enrich ~ euplotes_enrich$Enrichment, method = "euclidean", permutations = 999)

# Post-hoc
pairwise.adonis(grouped_euplotes_enrich, euplotes_enrich$Enrichment, sim.method = "euclidean",
                p.adjust.m = "bonferroni")

### Euplotes - Temperature Cycles and Enrichment Levels ####
detach(euplotes_temp_enrich)
euplotes_temp_enrich <- read.csv("Euplotes_Temperature_Cycles_and_Enrichment.csv", header = T)
attach(euplotes_temp_enrich)

# PERMANOVA
grouped_euplotes_temp_enrich <- euplotes_temp_enrich[,c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8",
                                              "T9", "T10", "T11", "T12", "T13", "T14", "T15", "T16", "T17",
                                              "T18", "T19", "T20", "T21", "T22", "T23", "T24", "T25", "T26", "T27",
                                              "T28", "T29", "T30", "T31", "T32", "T33", "T34", "T35")]
adonis(grouped_euplotes_temp_enrich ~ euplotes_temp_enrich$Cycle_Enrichment, method = "euclidean", permutations = 999)

# Post-hoc
pairwise.adonis(grouped_euplotes_temp_enrich, euplotes_temp_enrich$Cycle_Enrichment, sim.method = "euclidean",
                p.adjust.m = "bonferroni")




