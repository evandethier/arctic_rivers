#### i. LIBRARY IMPORTS ####
## Tables
library(data.table)
library(lubridate)
# library(tidyr)
library(broom)
# library(readxl)
# library(rgdal)

## Plots
library(ggplot2)
library(maps)
library(scales)
library(ggtext)
library(patchwork)
library(ggbeeswarm)
library(ggh4x) # force facet dimensions
library(ggforce)
library(cowplot)
# library(ggthemes)
# library(ggpubr)
# library(gstat)
# library(markdown)
# library(egg)
# library(zoo)


## Data download
# library(dataRetrieval)
library(tidyhydat)

## Analysis
library(glmnet)
library(changepoint)
# library(Hmisc)

# ## Tables
# library(data.table)
# library(readxl)
# # library(rgdal)
# library(lubridate)
# library(tidyr)
# library(broom)
# 
# ## Plots
# library(ggplot2)
# library(maps)
# library(scales)
# library(ggthemes)
# library(ggpubr)
# library(gstat)
# library(markdown)
# library(ggtext)
# library(patchwork)
# library(egg)
# library(zoo)
# library(ggforce)


# library(ggbeeswarm)
# 
# ## Data download
# library(dataRetrieval)
# library(tidyhydat)
# 
# ## Analysis
# library(glmnet)
# library(Hmisc)


#### ii. THEMES ####
# Custom themes
theme_hydro <- theme_bw() + theme(
  strip.background = element_blank(),
  strip.text = element_text(hjust = 0, margin = margin(0,0,0,0, unit = 'pt'))
)

theme_markdown <- theme_hydro + theme(
  strip.text = element_markdown(hjust = 0, margin = margin(0,0,0,0, unit = 'pt')),
  axis.title.x = element_markdown(),
  axis.title.y = element_markdown(),
  legend.title = element_markdown(),
  legend.position = 'none'
)

fancy_scientific_modified <- function(l) { 
  # turn in to character string in scientific notation 
  if(abs(max(log10(l), na.rm = T) - min(log10(l), na.rm = T)) > 2 | 
     # min(l, na.rm = T) < 0.01 | 
     max(l, na.rm = T) > 1e5){ 
    l <- log10(l)
    label <- parse(text = paste("10^",as.character(l),sep = ""))
  }else{
    label <- parse(text = paste(as.character(l), sep = ""))
  }
  # print(label)
  # return(parse(text=paste("'Discharge [m'", "^3* s", "^-1 ", "*']'", sep="")))
  return(label)
}

lat_dd_lab <- function(l){
  label <- c()
  for(i in 1:length(l)){
    label_sel <- ifelse(l[i] < 0, paste0(abs(l[i]), '°S'), 
                        paste0(abs(l[i]), '°N'))
    label <- c(label, label_sel)
  }
  return(label)}

long_dd_lab <- function(l){
  label <- c()
  for(i in 1:length(l)){
    label_sel <- ifelse(l[i] < 0, paste0(abs(l[i]), '°W'), 
                        paste0(abs(l[i]), '°E'))
    label <- c(label, label_sel)
  }
  return(label)}

abbrev_year <- function(l){
  label <- c() 
  for(i in 1:length(l)){
    label_sel <- paste0("'",substr(as.character(l[i]),3,4))
    label <- c(label, label_sel)  
  }
  return(label)}

# Custom labeller function
positive_latitude_labeller <- function(x) {
  return(as.character(abs(as.numeric(x))))
}

abbrev_year <- function(l){
  label <- c() 
  for(i in 1:length(l)){
    label_sel <- paste0("'",substr(as.character(l[i]),3,4))
    label <- c(label, label_sel)  
  }
  return(label)}

# Breaks for log scales
breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

#### iii. SET DIRECTORIES ####
# Set root directory
wd_root <- getwd()

# Imports folder (store all import files here)
wd_imports <- paste0(wd_root,"/imports/")
wd_code <- paste0(wd_root, '/code/')
wd_figures <- paste0(wd_root, "/figures/")

# Create folders within root directory to organize outputs if those folders do not exist
export_folder_paths <- c(wd_imports, wd_figures,wd_code)
for(i in 1:length(export_folder_paths)){
  path_sel <- export_folder_paths[i]
  if(!dir.exists(path_sel)){
    dir.create(path_sel)}
}



