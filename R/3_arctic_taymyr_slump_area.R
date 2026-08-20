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

#### 1. IMPORT SLUMP DATA, BY-WATERSHED SUMMARY ####
slumps_all_year <- fread(paste0(wd_imports,'taymyr_slumps_validated_20250620.csv'))
slumps_by_watershed <- fread(paste0(wd_imports,'taymyr_slumps_by_wshd_20250820.csv'))

ggplot(slumps_by_watershed, aes(x = year, y = area_km2/watershed_area_km2)) + 
  geom_line() +
  theme_markdown + 
  facet_wrap(.~site_no)

ggplot(slumps_all_year, aes(x = area_km2)) +
  geom_histogram() +
  scale_x_log10(labels = fancy_scientific_modified)

labels <- unique(slumps_all_year[year == 2024 & area_km2 > 0.02,label2024])[1:100]

all_labels <- unique(slumps_all_year[,label])
all_label2024 <- unique(slumps_all_year[,label2024])
unique_years <- sort(unique(slumps_all_year[,year]))

#### 2. ANALYZE SLUMPS BY YEAR, WATERSHED ####
slump_annual_sum <- slumps_all_year[,.(
  area_km2 = sum(area_km2, na.rm = T),
  N_segments = .N
), by = .(label2024, year)]
# ), by = .(label, year)]


slump_annual_sum[year == 2016 & area_km2 > 0]
slump_annual_metadata <- slumps_all_year[,.(
  first_year = min(year, na.rm = T),
  latitude = mean(latitude, na.rm = T),
  longitude = mean(longitude, na.rm = T)
), by = .(label2024)]
# ), by = .(label)]

fill_with_zeros <- merge(data.table(label2024 = sort(rep(all_label2024, length(unique_years))),
# fill_with_zeros <- merge(data.table(label = sort(rep(all_labels, length(unique_years))),
                              # year = rep(unique_years, length(all_labels)),
                              year = rep(unique_years, length(all_label2024)),
                              area_km2_fill = 0,
                              N_segments_fill = 0),
                         slump_annual_metadata,
                         by = c('label2024')
                         # by = c('label')
)

slump_annual_sum <- merge(slump_annual_sum,
      fill_with_zeros,
      by = c('label2024','year'),
      # by = c('label','year'),
      all.y = T)[,':='(
        area_km2 = ifelse(!is.na(area_km2), area_km2, area_km2_fill),
        N_segments = ifelse(!is.na(area_km2), area_km2, N_segments_fill)
      )][
        ,':='(area_norm = area_km2/max(area_km2, na.rm = T)),
        by = .(label2024)
        # by = .(label)
      ]                      


slump_first_year_and_max_size <- slump_annual_sum[
  ,.(
    area_km2 = max(area_km2, na.rm = T)
  ),
  by = .(label2024, first_year, latitude, longitude)
]  

fwrite(slump_annual_sum, file = paste0(wd_imports, 'taymyr_slump_annual_sum_and_watershed.csv'))

fwrite(slump_first_year_and_max_size, file = paste0(wd_imports, 'slump_first_year_and_max_size.csv'))
ggplot(slump_annual_sum, aes(x = first_year)) +
  geom_bar() +
  theme_markdown

ggplot(slump_annual_sum[label2024 %chin% labels], aes(x = year, y = area_km2)) +
  geom_line(aes(group = label2024)) +
  geom_point() +
  facet_wrap(.~label2024, scales = 'free_y') +
  theme_markdown

slump_area_normalized_annual <- ggplot(slump_annual_sum, aes(x = year, y = area_norm)) +
  # stat_summary(geom = 'ribbon', alpha = 0.25) +
  stat_summary(geom = 'point', alpha = 0.25) +
  stat_summary(geom = 'line', fun = mean) +
  theme_markdown +
  # facet_grid(-(latitude - latitude%%0.25)~(longitude - longitude%%0.25)) +
  labs(
    x = 'Year',
    y = 'Normalized Slump area'
  ) +
  theme(
    axis.title.y = element_markdown()
  )

slump_avg_area_km2_annual <- ggplot(slump_annual_sum, aes(x = year, y = area_km2)) +
  # stat_summary(geom = 'ribbon', alpha = 0.25) +
  stat_summary(data = slump_annual_sum[area_km2 > 0], geom = 'point', alpha = 0.5, fun = mean, size = 3) +
  stat_summary(data = slump_annual_sum[area_km2 > 0], geom = 'line', fun = mean) +
  # stat_summary(aes(y = area_km2/100), geom = 'point', color = 'red', alpha = 0.5, fun = sum, size = 3) +
  # stat_summary(aes(y = area_km2/100), geom = 'line', color = 'red', fun = sum) +
  theme_markdown +
  scale_x_continuous(limits = c((min(unique_years)-0.5),max(unique_years)+0.45), breaks = c((min(unique_years)):(max(unique_years)))) +
  # facet_grid(-(latitude - latitude%%0.25)~(longitude - longitude%%0.25)) +
  labs(
    x = 'Year',
    y = 'Avg. slump area (km<sup>2</sup>)'
  ) +
  theme(
    axis.title.y = element_markdown()
  )

## TABLE X. SLUMP AREA AND NUMBER, BY YEAR ##
## STATISTIC: New slumps and slump area, by year 
slump_annual_sum_and_n_initiated_study_area <- merge(
  slump_annual_sum[year >= 2018][,.(
    slump_area_km2 = round(sum(area_km2, na.rm = T),1)
  ), by = .(year)],
  slump_annual_metadata[first_year >= 2018][,.(
    n_initiated = .N
  ), by = .(year = first_year)],
  by = 'year'
)[,':='(
  total_n_slumps = cumsum(n_initiated),
  slump_area_km2_mean = round(slump_area_km2/cumsum(n_initiated),3),
  new_slump_area_km2 = c('-',diff(slump_area_km2))
)][
  ,.(`Year` = year, `Total slump area (km2)` = slump_area_km2, `Increase in slump area (km2)` = new_slump_area_km2, 
    `Avg. slump area (km2)` = slump_area_km2_mean, `Initiated slumps` = n_initiated, `Total slumps` = total_n_slumps)
]

fwrite(slump_annual_sum_and_n_initiated_study_area, file = paste0(wd_imports, 'slump_annual_sum_and_n_initiated_study_area.csv'))
slump_area_km2_annual <- ggplot(slump_annual_sum[year >= 2018], aes(x = year, y = area_km2)) +
  # stat_summary(geom = 'ribbon', alpha = 0.25) +
  stat_summary(geom = 'point', alpha = 0.5, fun = sum, size = 3) +
  stat_summary(geom = 'line', fun = sum) +
  theme_markdown +
  # scale_x_continuous(limits = c((min(unique_years)-0.5),max(unique_years)+0.45), breaks = c((min(unique_years)):(max(unique_years)))) +
  scale_x_continuous(limits = c((2018-0.5),max(unique_years)+0.45), breaks = c((min(unique_years)):(max(unique_years)))) +
  # facet_grid(-(latitude - latitude%%0.25)~(longitude - longitude%%0.25)) +
  labs(
    x = '***Year***',
    y = '***Total slump area*** (km<sup>2</sup>)'
  ) +
  theme(
    axis.title.y = element_markdown()
  )



slump_count_annual <- ggplot(slump_annual_metadata[first_year >= 2018], aes(x = first_year)) +
  # stat_summary(geom = 'ribbon', alpha = 0.25) +
  geom_bar(alpha = 0.5) +
  theme_markdown +
  # scale_x_continuous(limits = c((min(unique_years)-0.5),max(unique_years)+0.45), breaks = c((min(unique_years)):(max(unique_years)))) +
  scale_x_continuous(limits = c((2018-0.5),max(unique_years)+0.45), breaks = c((min(unique_years)):(max(unique_years)))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  # facet_grid(-(latitude - latitude%%0.25)~(longitude - longitude%%0.25)) +
  labs(
    x = '***Year***',
    y = '***Number of initiated slumps***'
  ) +
  theme(
    axis.title.y = element_markdown()
  )

slump_footprint_and_initiation_by_year <-  slump_count_annual / slump_area_km2_annual +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(slump_footprint_and_initiation_by_year, filename = paste0(wd_figures, 'slump_footprint_and_initiation_by_year.pdf'),
       width = 4.5, height = 6.5, useDingbats = F)

median_slump_size_summary <- slump_annual_sum[,.(
  area_km2 = median(area_km2, na.rm = T)
), by = .(first_year, year)]

ggplot(median_slump_size_summary[year > 2017], aes(x = year - first_year, y = area_km2)) + 
  geom_line(aes(group = first_year, color = factor(first_year))) +
  geom_point() + 
  theme_markdown +
  scale_y_log10() +
  theme(legend.position = 'top') / 

ggplot(median_slump_size_summary, aes(x = year, y = area_km2)) + 
  geom_line(aes(group = first_year, color = factor(first_year))) +
  geom_point() + 
  theme_markdown +
  scale_y_log10() +
  theme(legend.position = 'top')
  

ggplot(slump_annual_sum[year > 2017 & first_year >= 2018], aes(x = year - first_year, y = area_km2)) + 
  geom_line(aes(group = label2024), alpha = 0.1) +
  # geom_point() + 
  theme_markdown +
  scale_y_log10() +
  theme(legend.position = 'top')

ggplot(slump_annual_sum[year > 2017 & first_year >= 2018], 
       aes(x = factor(year), y = area_km2)) + 
  # stat_summary(geom = 'point', fun = mean, color = 'blue') +
  # stat_summary(geom = 'point', fun = median, color = 'navy') +
  # stat_summary(geom = 'point', fun = perc, color = 'red') +
  # stat_summary(geom = 'point', fun = max, color = 'red') +
  geom_boxplot() +
  # geom_point() + 
  theme_markdown +
  scale_y_log10() +
  theme(legend.position = 'top')


#### 2. PLOT SLUMP AREA DISTRIBUTIONS OVER TIME ####
# Function for getting probability distribution function (PDF)
getPDF <- function(x, mean, sd){
  prob <- dnorm(x, 
                mean = mean, 
                sd = sd
  )
  return(prob)
}

## TO DO: APPLY PDF TO MEAN, SD TO GET SLUMP DISTRIBUTION CURVES ##
slump_size_by_year <- ggplot(slump_annual_sum[
  # first_year == 2022
  year > 2017
  & area_km2 >= 0.01
  ], aes(y = area_km2)) +
  geom_histogram(fill = '#a6cee3', color = 'black', lwd = 0.2) +
  geom_density() +
  theme_markdown +
  scale_y_log10(labels = fancy_scientific_modified) +
  scale_x_continuous(expand = expansion(mult = c(0,0.1))) +
  facet_wrap(.~year, nrow = 1) +
  # facet_grid(-(latitude - latitude%%0.25)~(longitude - longitude%%0.25)) +
  labs(
    x = 'N slumps',
    y = 'Slump area km<sup>2</sup>'
  ) +
  theme(
    axis.title.y = element_markdown()
  )

ggsave(slump_size_by_year, filename = paste0(wd_figures, 'slump_size_by_year.pdf'),
       width = 4, height = 2, useDingbats=F)

#### 3. INDIVIDUAL SLUMP EROSION ANALYSIS -- SLUMP AREA CHANGE ####
# Import slump erosion data
# Selected slumps, filtered data
slump_area <- fread(paste0(wd_imports, 'taymyr_SlumpArea_selected.csv'))
# Selected slumps, unfiltered
slump_area <- fread(paste0(wd_imports, 'taymyr_slumpArea_unfiltered.csv'))

slump_area <- slump_area[,':='(Area = RollingMedian)]
# Add date columns
slump_area <- slump_area[
  ,':='(yday = yday(Date),
        year = year(Date),
        month = month(Date))
]

# Add cumulative date columns
slump_area <- slump_area[
  ,':='(cumul_days = cumsum(yday),
        cumul_days_since_20200101 = as.numeric(ymd(Date) - ymd('2020-01-01')),
        yday3 = yday - yday%%3 - 1.5,
        yday5 = yday - yday%%5 - 2.5,
        yday7 = yday - yday%%7 - 3.5,
        yday10 = yday - yday%%10 - 5,
        yday15 = yday - yday%%15 - 7.5,
        yday20 = yday - yday%%20 - 10,
        period = ifelse(year == 2020, '2020', 'post-2020')),
  by = .(ID)
]

# Unique slumps
slump_ids = unique(slump_area[,ID])
slump_metadata = slump_area[
  ,.(max_area_m2 = max(Area, na.rm = T),
     min_area_m2 = min(Area, na.rm = T),
     N_images = .N),
  # by = .(ID, latitude, longitude)
  by = .(ID)
][order(N_images, decreasing = TRUE)]

fwrite(slump_metadata, file = paste0(wd_imports, region_name, '_metadata.csv'))
# STATISTIC -- Number of slumps in erosion rate analysis
print(paste0('N slumps in erosion rate analysis: ', length(slump_ids)))

# FIGURE: Figure SX. RGB changes in river color, pre- vs. post-slump event
# Plot of slump image day-of-year distribution
slump_image_yday_histogram <- ggplot(slump_area, aes(y = yday)) +
  geom_histogram(aes(fill = period), bins = 10, color = 'black', linewidth = 0.5) +
  scale_fill_manual(values = c('2020' = 'black', 'post-2020' = 'grey40')) +
  theme_markdown +
  facet_wrap(.~period) +
  scale_x_continuous(expand = expansion(mult = c(0,0.1))) +
  theme(legend.position = c(0.22,0.95),
        legend.background = element_blank(),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(),
        axis.line.x = element_line(linewidth = 0.5)) + 
  labs(
    x = 'N images',
    y = 'Day of year',
    color = '',
    fill = 'Period'
  )

ggsave(slump_image_yday_histogram, filename = paste0(wd_figures, region_name, '_image_yday_histogram.png'),
       width = 6.5, height = 5)
ggsave(slump_image_yday_histogram, filename = paste0(wd_figures, region_name, '_image_yday_histogram.pdf'),
       width = 6.5, height = 5, useDingbats = F)

# Calculate the number of samples in each day-of-year window
slump_area <- slump_area[
  ,':='(N_samples_per_period = .N),
  by = .(period, yday7)
][
  # N_samples_per_period > 5
]

# Summarize slump area data by day-of-year
slump_area_summary <- slump_area[
  ,.(N_samples_per_period = .N),
  by = .(period, yday7)
]

# Calculate incremental area change (successive images)
slump_area <- slump_area[
  order(cumul_days)
][
  ,':='(area_change_m2 = c(NA, diff(Area)),
        max_area_m2 = max(Area, na.rm = T),
        n_days = c(NA, diff(cumul_days))),
  by = .(ID)
]

# Calculate erosion rate (area change/days elapsed between images)
slump_area <- slump_area[
  ,':='(
    area_change_m2_d = area_change_m2/n_days,
    perc_area_change = area_change_m2/n_days/data.table::shift(Area,type='lag')*100,
    perc_area_change_vs_max = area_change_m2/n_days/max(Area,na.rm = T)*100,
    perc_area_vs_max = Area/max(Area, na.rm = T)*100
  ),
  by = .(ID)
]

# Add number of dates for each slump (ID)
slump_area <- slump_area[
  ,':='(N_dates = .N),
  by = .(ID)
]

# Add season category
slump_area <- slump_area[
  ,':='(season = ifelse(yday < 190 & n_days > 100, 'winter', 'warm season'))
]
# Add period (pre- and post-2020 + season)
slump_area <- slump_area[
  ,':='(period_season = paste0(period, ', ', season))
]

# Subset to bookend winter, but limit to late-ish fall and early-ish spring
slump_area_winter <- slump_area[
  yday < 190 & n_days > 100
]

slump_area <- slump_area[,':='(
  rolling_min_area_w5 = frollapply(Area, 5, 'min'),
  rolling_median_area_w5 = frollapply(Area, 5, 'median'),
  rolling_max_area_w5 = frollapply(Area, 5, 'max')
),
# by = .(ID, latitude, longitude)
by = .(ID)
]

test_slumps <- slump_area[ID %chin% slump_metadata[40:51,ID]]
# SUPPLEMENTAL FIGURE -- Figure SX. Example of slump erosion, limited to 16 slumps
example_slump_erosion_timeseries_plot <- 
  ggplot(test_slumps[month == 7], aes(x = month/12 + year, y = perc_area_vs_max)) +
  geom_line() +
  # ggplot(slump_area[ID %chin% slump_metadata[49:100,ID]], aes(x = year + month/12, y = rolling_median_area_w5)) +
  # stat_summary(fun = 'mean', aes(color = factor(month)), geom = 'line') +
  facet_wrap(.~ID) +
  scale_x_continuous(labels = abbrev_year) +
  theme_markdown +
  labs(
    x = 'Year',
    y = '% Maximum slump area'
  )


ggsave(example_slump_erosion_timeseries_plot, filename = paste0(wd_figures, region_name, '_example_slump_erosion_timeseries_plot.png'),
       width = 6.5, height = 5)
ggsave(example_slump_erosion_timeseries_plot, filename = paste0(wd_figures, region_name, '_example_slump_erosion_timeseries_plot.pdf'),
       width = 6.5, height = 5, useDingbats = F) 

example_slump_erosion_area_timeseries_plot <- 
  ggplot(test_slumps[month %in% c(7,8)], aes(x = month/12 + year, y = Area)) +
  geom_line() +
  # ggplot(slump_area[ID %chin% slump_metadata[49:100,ID]], aes(x = year + month/12, y = rolling_median_area_w5)) +
  # stat_summary(fun = 'mean', aes(color = factor(month)), geom = 'line') +
  facet_wrap(.~ID, scale = 'free_y') +
  scale_x_continuous(labels = abbrev_year) +
  theme_markdown +
  labs(
    x = 'Year',
    y = '% Maximum slump area'
  )


ggsave(example_slump_erosion_timeseries_plot, filename = paste0(wd_figures, region_name, '_example_slump_erosion_timeseries_plot.png'),
       width = 6.5, height = 5)
ggsave(example_slump_erosion_timeseries_plot, filename = paste0(wd_figures, region_name, '_example_slump_erosion_timeseries_plot.pdf'),
       width = 6.5, height = 5, useDingbats = F) 

# FIGURE -- Figure Xa. Slump erosion rate timeseries, showing each warm-season month since 2020
slump_area_rate_timeseries_plot <- ggplot(slump_area[
  # year == 2020
  # N_dates > 12
], aes(x = month(Date)/12 + year, y = perc_area_change,
       # group = period_season,
       # fill = period
)) +
  stat_summary(geom = 'errorbar', width = 0.1) +
  stat_summary(geom = 'line', fun = mean) +
  stat_summary(aes(fill = factor(month)), geom = 'point', pch = 21, fun = mean, size = 3.5, color = 'black') +
  # geom_text(data = slump_area_summary, aes(y = 1.5, label = N_samples_per_period), angle = 90) +
  # stat_summary(geom = 'point', fun = length) +
  theme_markdown +
  # facet_wrap(.~year) +
  # facet_wrap(.~period) +
  scale_fill_manual(values = c('#012E40','#038C8C','#F28705')) +
  # scale_fill_manual(values = c('2020' = 'black', 'post-2020' = 'grey40')) +
  labs(
    x = '',
    y = 'Slump erosion rate<br>(*% area/day*)',
    color = 'Season',
    fill = 'Month'
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_markdown(),
    legend.position = c(0.85, 0.75),
    legend.background = element_blank()
  )

ggsave(slump_area_rate_timeseries_plot, filename = paste0(wd_figures, region_name, '_area_rate_timeseries_plot.png'),
       width = 6.5, height = 5)
ggsave(slump_area_rate_timeseries_plot, filename = paste0(wd_figures, region_name, '_area_rate_timeseries_plot.pdf'),
       width = 6.5, height = 5, useDingbats = F)  


# FIGURE -- Figure Xb. Average slump erosion rate for 10-day periods during 2020 and 2021-2024.
slump_area_rate_by_yday_plot <- ggplot(slump_area[
  # year == 2020
  year %in% c(2020,2021)
  # N_dates > 12
  & season != 'winter'
], aes(x = yday7, y = perc_area_change, 
       group = period_season,
       color = season,
       fill = period)) +
  stat_summary(geom = 'errorbar', width = 2) +
  stat_summary(geom = 'line', fun = mean) +
  stat_summary(geom = 'point', pch = 21, fun = mean, size = 3.5) +
  # geom_text(data = slump_area_summary, aes(y = 0.15, label = N_samples_per_period, x = yday7), inherit.aes=F, angle = 90) +
  geom_text(data = slump_area[
    ,.(N_samples_per_period = .N),
    by = .(period, yday7)
  ], aes(y = 0.15, label = N_samples_per_period, x = yday7), inherit.aes=F, angle = 90) +
  # stat_summary(geom = 'point', fun = length) +
  theme_markdown +
  # facet_wrap(.~year) +
  facet_wrap(.~period) +
  scale_color_manual(values = c('winter' = 'blue', 'warm season' = 'grey40')) +
  scale_fill_manual(values = c('2020' = 'black', 'post-2020' = 'grey40')) +
  labs(
    x = 'Day of year',
    y = 'Slump erosion rate<br>(*% area/day*)',
    color = 'Season',
    fill = 'Period'
  ) +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.position = 'top'
  )

ggsave(slump_area_rate_by_yday_plot, filename = paste0(wd_figures, region_name, '_erosion_rate_yday.png'),
       width = 6.5, height = 5)
ggsave(slump_area_rate_by_yday_plot, filename = paste0(wd_figures, region_name, '_erosion_rate_yday.pdf'),
       width = 6.5, height = 5, useDingbats = F) 

# FIGURE -- Figure X. Slump erosion rate timeseries and seasonal breakdown
slump_area_rate_combined_plot <- 
  slump_area_rate_timeseries_plot / 
  (slump_area_rate_by_yday_plot + 
     guides(fill = 'none') +
     theme(legend.position = c(0.85, 0.8),
           legend.background = element_blank())
  )+
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(slump_area_rate_combined_plot, filename = paste0(wd_figures, region_name, '_erosion_rate_combined_plot.png'),
       width = 6.5, height = 5.5) 
ggsave(slump_area_rate_combined_plot, filename = paste0(wd_figures, region_name, '_erosion_rate_combined_plot.pdf'),
       width = 6.5, height = 5.5, useDingbats = F) 

# SUPPLEMENTAL FIGURE -- Figure SX. Slump erosion rate timeseries vs. max area, including seasonal breakdown
slump_area_rate_vs_max_timeseries_plot <- ggplot(slump_area[
  # year == 2020
  N_dates > 15
], aes(x = month(Date)/12 + year, perc_area_vs_max,
       # group = period_season,
       # fill = period
)) +
  stat_summary(geom = 'errorbar', width = 0.1) +
  stat_summary(geom = 'line', fun = mean) +
  stat_summary(aes(fill = factor(month)), geom = 'point', pch = 21, fun = mean, size = 3.5, color = 'black') +
  # geom_text(data = slump_area_summary, aes(y = 1.5, label = N_samples_per_period), angle = 90) +
  # stat_summary(geom = 'point', fun = length) +
  theme_markdown +
  # facet_wrap(.~year) +
  # facet_wrap(.~period) +
  scale_fill_manual(values = c('#012E40','#038C8C','#F28705')) +
  # scale_fill_manual(values = c('2020' = 'black', 'post-2020' = 'grey40')) +
  labs(
    x = '',
    y = 'Slump erosion rate<br>(*% area/day*)',
    color = 'Season',
    fill = 'Month'
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_markdown(),
    legend.position = c(0.85, 0.75),
    legend.background = element_blank()
  )

ggsave(slump_area_rate_vs_max_timeseries_plot, filename = paste0(wd_figures, region_name, '_erosion_rate_vs_max_yday.png'),
       width = 6.5, height = 5)
ggsave(slump_area_rate_vs_max_timeseries_plot, filename = paste0(wd_figures, region_name, '_erosion_rate_vs_max_yday.pdf'),
       width = 6.5, height = 5, useDingbats = F)  

# SUPPLEMENTAL FIGURE -- Figure SX. Slump area timeseries vs. max area, including seasonal breakdown
slump_area_vs_max_timeseries_plot <- ggplot(slump_area[
  # year == 2020
  # N_dates > 12
], aes(x = month(Date)/12 + year, perc_area_vs_max,
       # group = period_season,
       # fill = period
)) +
  stat_summary(geom = 'errorbar', width = 0.1) +
  stat_summary(geom = 'line', fun = mean) +
  stat_summary(aes(fill = factor(month)), geom = 'point', pch = 21, fun = mean, size = 3.5, color = 'black') +
  # geom_text(data = slump_area_summary, aes(y = 1.5, label = N_samples_per_period), angle = 90) +
  # stat_summary(geom = 'point', fun = length) +
  theme_markdown +
  # facet_wrap(.~year) +
  # facet_wrap(.~period) +
  scale_fill_manual(values = c('#012E40','#038C8C','#F28705')) +
  # scale_fill_manual(values = c('2020' = 'black', 'post-2020' = 'grey40')) +
  labs(
    x = '',
    y = 'Slump erosion rate<br>(*% area/day*)',
    color = 'Season',
    fill = 'Month'
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_markdown(),
    legend.position = c(0.85, 0.7),
    legend.background = element_blank()
  )

ggsave(slump_area_vs_max_timeseries_plot, filename = paste0(wd_figures, region_name, '_slump_area_vs_max_timeseries_plot.png'),
       width = 4.5, height = 3)
ggsave(slump_area_vs_max_timeseries_plot, filename = paste0(wd_figures, region_name, '_slump_area_vs_max_timeseries_plot.pdf'),
       width = 4.5, height = 3, useDingbats = F)  


#### BERNHARD DATA ANALYSIS ####
bernhard_slump_data <- fread(paste0(wd_imports, 'berhnard_2021_thaw_slump_inventory.csv'))



slump_erosion_by_year <- slump_annual_sum[year >= 2018][,':='(erosion_by_year_km2 = c(0, diff(area_km2))),
                                                        by = .(label2024)]

largest_20_slumps <- slump_erosion_by_year[,.(area_m2_max = max(area_km2 * 1000^2, na.rm = T)),
                                           by = .(label2024, latitude, longitude)][
                                             order(-area_m2_max)
                                           ][1:20]
ggplot(bernhard_slump_data[Area == 'Peel'], aes(x = change_area_year)) +
  geom_histogram(data = slump_erosion_by_year[year == 2019 & area_km2 > 0], aes(x = erosion_by_year_km2*(1000^2)), fill = 'blue') +
  geom_histogram(alpha = 0.5) +
  theme_markdown 

alpha_scaling_exponent <- 1
ggplot(bernhard_slump_data[Area == 'Peel'], aes(x = change_vol_year)) +
  geom_histogram(data = slump_erosion_by_year[year == 2019 & area_km2 > 0], aes(x = 0.765*(erosion_by_year_km2*1000^2)^alpha_scaling_exponent), fill = 'violet',alpha = 0.5) +
  geom_histogram(data = slump_erosion_by_year[year == 2021 & area_km2 > 0], aes(x = 0.765*(erosion_by_year_km2*1000^2)^alpha_scaling_exponent), fill = 'blue',alpha = 0.5) +
  geom_histogram(data = slump_erosion_by_year[year == 2022 & area_km2 > 0], aes(x = 0.765*(erosion_by_year_km2*1000^2)^alpha_scaling_exponent), fill = 'red',alpha = 0.5) +
  geom_histogram(data = slump_erosion_by_year[year == 2023 & area_km2 > 0], aes(x = 0.765*(erosion_by_year_km2*1000^2)^alpha_scaling_exponent), fill = 'orange',alpha = 0.5) +
  geom_histogram(alpha = 0.5) +
  scale_x_log10(labels = fancy_scientific_modified) +
  theme_markdown +
  labs(
    x = '***Volumetric change*** (m<sup>3</sup>/yr)<br>(estimated from Bernhard, 2022 scaling)'
  )

# slump_volume_m3_annual <- ggplot(slump_annual_sum[year >= 2018 & area_km2 > 0], 
#                                  aes(x = year, y = 0.765*(area_km2*1000^2)^1.22/1e6)) +
slump_volume_m3_annual <- ggplot(slump_erosion_by_year, 
                                 aes(x = year, y = 0.765*(erosion_by_year_km2*1000^2)^alpha_scaling_exponent/1e6)) +
  # stat_summary(geom = 'ribbon', alpha = 0.25) +
  stat_summary(geom = 'point', alpha = 0.5, fun = sum, size = 3) +
  stat_summary(geom = 'line', fun = sum) +
  theme_markdown +
  # scale_x_continuous(limits = c((min(unique_years)-0.5),max(unique_years)+0.45), breaks = c((min(unique_years)):(max(unique_years)))) +
  scale_x_continuous(limits = c((2018-0.5),max(unique_years)+0.45), breaks = c((min(unique_years)):(max(unique_years)))) +
  # scale_y_log10(labels = fancy_scientific_modified) +
  # facet_grid(-(latitude - latitude%%0.25)~(longitude - longitude%%0.25)) +
  labs(
    x = '***Year***',
    y = '***Total slump volume*** (million m<sup>3</sup>)'
  ) +
  theme(
    axis.title.y = element_markdown()
  )

