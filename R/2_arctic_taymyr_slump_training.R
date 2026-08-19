#### i. LIBRARY IMPORTS ####
## Tables
library(data.table)
library(readxl)
library(rgdal)
library(lubridate)
library(tidyr)
library(broom)

## Plots
library(ggplot2)
library(maps)
library(scales)
library(ggthemes)
library(ggpubr)
library(gstat)
library(markdown)
library(ggtext)
library(patchwork)
library(egg)
library(zoo)
library(ggforce)

## Data download
library(dataRetrieval)
library(tidyhydat)

## Analysis
library(glmnet)
library(Hmisc)

#### ii. THEMES ####
theme_evan <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey70'),
    panel.grid.major.x = element_blank(),
    # panel.grid = element_blank(),
    legend.position = 'none',
    panel.border = element_rect(size = 0.5),
    text = element_text(size=8),
    axis.text = element_text(size = 8), 
    plot.title = element_text(size = 9)
  )

theme_evan_facet <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid = element_blank(),
    # legend.position = 'none',
    panel.border = element_rect(size = 0.5),
    strip.background = element_rect(fill = 'white'),
    text = element_text(size=10),
    axis.text = element_text(size = 10), 
    plot.title = element_text(size = 13)
  )
season_facet <- theme_evan_facet + theme(
  legend.position = 'none', 
  strip.background = element_blank(),
  strip.text = element_text(hjust = 0, margin = margin(0,0,0,0, unit = 'pt'))
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




slump_training_data <- fread(paste0(wd_imports, 'taymyr_slump_train_2024_20250605.csv'))

slump_training_data <- slump_training_data[,':='(
  nir = min(c(B8/2400, 1)), 
  red = min(c(B4/2400, 1)), 
  green = min(c(B3/2400,1)),
  blue = min(c(B2/2400,1))
),
by = .(`system:index`)]

ggplot(rbind(slump_training_data[slump==1][150:250],
             slump_training_data[slump==0][150:250])) +
  geom_tile(aes(x = 1, y = `system:index`, 
                fill = rgb(nir, red, green))) +
  scale_fill_identity() +
  facet_wrap(.~slump, scales = 'free', ncol = 1) |
ggplot(rbind(slump_training_data[slump==1][150:250],
             slump_training_data[slump==0][150:250])) +
  geom_tile(aes(x = 1, y = `system:index`, 
                fill = rgb(red, green, blue))) +
  scale_fill_identity() +
  facet_wrap(.~slump, scales = 'free', ncol = 1)

slump_training_data_long <- melt(slump_training_data[,':='(ndvi_scaled = (ndvi*1000)+1000,
                                                           ndwi_scaled = (ndwi*1000)+1000)], 
                                 measure.vars = c('B2', 'B3', 'B4', 'B8', 'B8A', 'B11', 'B12', 'ndvi_scaled', 'ndwi_scaled'),
                                 id.vars = c('slump'))
ggplot(slump_training_data_long, aes(x = variable, y = value, fill = as.character(slump))) +
  geom_boxplot()

