# Project: Sierra Nevada Roof Pendant BMI-Microbe 
This repository contains data (supplementary tables) and novel code pertaining to the manuscript tentatively titled "Synecological response of spring benthic prokaryotes and macroinvertebrates to Paleozoic roof pendant-derived calcium". 

## Data1 folder
Description: Data 1, aka the supporting tables (in '.xlsx' format), are included within this folder. 

## KEGGCode folder
Description: Contains novel code necessary to generate a list of summarized KEGG Modules from the PICRUSt2 KO output. 

## Relevant PICRUSt2 version
Maintaining a permanent copy of the PICRUSt2 version used (v 2.5.0) for data replication purposes. 

## NovelRCode folder
Novel code filename: plot_cladogram_modified.R

Author: Cale Seymour, University of Nevada Las Vegas

Description: Modification to the plot_cladogram function from the microbiomeMarker package.  

Allows the user to:
1) specify to ignore ranks (i.e., don't plot LEFSE results at the species and genus level)
2) Exclude "unclassified" taxa when plotting

The purpose of both of these is to reduce the complexity of the LEFSE trees, especially in larger datasets.

Usage: (in R) source('plot_cladogram_modified.R').

New params: <character length 1:[number of ranks], within c('p','c','o','f','g','s')> "exclude_rank"
              <logical length 1> "drop_unclassifieds"

Usage example:
clado = plot_cladogram_mod(lef, color = col, exclude_rank = 's', drop_unclassifieds = TRUE, clade_label_level = 5)

## Please cite Friel et al., 2022 and the R package microbiomeMarker if you use this script verbatim.


