#### Libraries ---------------------------------------------------------
# General
library(tidyverse)
library(data.table)
library(dataRetrieval)
library(here)
library(styler)
library(scales)
library(knitr)
library(lubridate)
library(zoo)
library(ggpubr)
library(ggthemes)
library(ggpmisc)
library(ggforce)
library(ggrepel)
library(Gmisc)
library(patchwork)
library(RColorBrewer)

# Project Specific
library(sf)
library(terra)
library(exactextractr)
library(riverdist)
library(whitebox)
library(stars)
# mapview dev version: remotes::install_github("r-spatial/mapview")
library(mapview)

#### Options ------------------------------------------------------------
nCores <- parallel::detectCores()-1
options(mc.cores = nCores)
options(readr.show_col_types = FALSE)

#### Functions ----------------------------------------------------------
function_files <- list.files("~/Dropbox/R/Functions", full.names = TRUE)
lapply(function_files, source)

#### Paths --------------------------------------------------------------
# General
datPath <- here("Data/")
resPath <- here("Results/")
figPath <- here("Figs/")
cacheDir <- here("Data/.cache/")
# Project Specific
demDir <- here("Data/1mDEM/")
shpFiles <- here("Results/ShapeFiles/")
basins <- here("Data/.cache/basins")

# Combine all directories into a single vector
paths <- c(datPath, resPath, figPath, cacheDir, shpFiles, basins)
# Create directories if they don’t exist
lapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE)

#### Clean Up -----------------------------------------------------------
rm(paths, basins, function_files)
