#### i. LIBRARY IMPORTS ####
## Tables
library(data.table)
library(lubridate)

## Plots
library(ggplot2)
library(maps)
library(scales)
library(ggtext)
library(patchwork)

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
# Custom themes
theme_hydro <- theme_bw() + theme(
  strip.background = element_blank(),
  strip.text = element_text(hjust = 0, margin = margin(0,0,0,0, unit = 'pt'))
)

theme_markdown <- theme_hydro + theme(
  strip.text = element_markdown(hjust = 0, margin = margin(0,0,0,0, unit = 'pt')),
  axis.title.x = element_markdown(),
  axis.title.y = element_markdown(),
  legend.title = element_markdown()
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



#### ------ 1. IMPORT AND PREPARE DATA ------ ####
#### 1A. IMPORT DATA AND DEFINE COLUMNS ####
# Set region name for exports
region_name <- 'taymyr_peninsula_thaw_slump'

# Import climate data (ERA5)
climate_taymyr <- fread(paste0(wd_imports, 'taymyr_peninsula_climate_ERA5_Land.csv'))[
  ,':='(date = ymd(substr(`system:index`, 1,8)),
        .geo = NULL)
]

# Climate metadata
site_topo_metadata <- fread(paste0(wd_imports,'taymyr_peninsula_river_ls_training_station_topo.csv'))

# Add Russian name (and English phonetic) to data
river_russian_name <- data.table(river = c('A','B','C','D','Estuary 1', 'Estuary 2', 'E','F','G','Control'),
                                 river_nm = c('Malinovskogo','Fomina',"Proval'naya", 'Bujnaja','Leningradskaya','Nizhnyaya Taymyra', 'E','F','G','Control'),
                                 river_nm_russian = c('Малиновского','Фомина','Провальная','Буйная','Ленинградская','Нижняя Таймыра', 'E','F','G','Control')
)

# Combine metadata about stations (drainage area, etc. with Russian name)
site_topo_metadata <- merge(
  site_topo_metadata,
  river_russian_name,
  by = c('river'))

# Import slump area data, summarized by watershed
slumps_by_watershed <- merge(
  fread(file = paste0(wd_imports,'taymyr_slumps_by_wshd_20250820.csv'))[
    ,':='(.geo = NULL)],
  site_topo_metadata[,.(site_no, river, river_nm, station_nm)],
  by = 'site_no'
)

# Fill slump data with 2017 data, setting 2017 equal to average of 2016 and 2018
slumps_by_watershed_2017 <- slumps_by_watershed[
  year %in% c(2016,2018)][,.(
    area_km2 = mean(area_km2, na.rm = T)),
    by = .(river, watershed_area_km2, station_nm)][
      ,':='(year = 2017)
    ]

# Add inferred 2017 slump area data to full slump area dataset
slumps_by_watershed_filled <- rbind(
  slumps_by_watershed[,.(river, station_nm, year, watershed_area_km2, area_km2)],
  slumps_by_watershed_2017,
  use.names = T
)

# Import Landsat river profile data for each batch of thaw slump sites
# Combine Landsat sample data into one data.table
river_import <- rbindlist(
  lapply(paste0(wd_imports, 
                c('taymyr_peninsula_river_training_ls5789_rawBands_b7lt500.csv')
  ),
  fread
  ), fill = T, use.names = T)[
    ,':='(.geo = NULL)][
      !grepl('f', site_no)
    ]

# Merge landsat data with metadata file to standardize river names
river_import <- merge(river_import,
                      site_topo_metadata[,.(site_no, river, river_nm, station_nm)],
                      by = 'site_no')


#### 1B. PREPARE DATA COLUMNS, MAKE LANDSAT MATCHUP, CALCULATE SSC ESTIMATE ####
# Standardize Landsat data
# Landsat data do have station information
# They also have latitude and longitude
river_import <- na.omit(river_import[,
                                     ':='(
                                       site_no = site_no,
                                       # site_no = name,
                                       station_nm = station_nm,
                                       # Rename columns for simplicity
                                       B1 = B1_median,
                                       B2 = B2_median,
                                       B3 = B3_median,
                                       B4 = B4_median,
                                       B5 = B5_median,
                                       B6 = B6_median,
                                       B7 = B7_median,
                                       num_pix = B2_count,
                                       sample_dt = ymd(date),
                                       landsat_dt = ymd(date)
                                     )]
                        , cols = c('B1','B2','B3','B4','B5','B7'))[
                          B1 > 0 & B2 > 0 & B3 > 0 & B4 > 0 & B5 > 0 & B7 > 0 &
                            B1 < 5000 & B2 < 5000 & B3 < 5000 & B4 < 5000 & B6 < 4000][
                              ,':='( 
                                year = year(sample_dt),
                                # add new columns with band ratios
                                B1.2 = B1^2,
                                B2.2 = B2^2,
                                B3.2 = B3^2,
                                B4.2 = B4^2,
                                B5.2 = B5^2,
                                B7.2 = B7^2,
                                B2.B1 = B2/B1,
                                B3.B1 = B3/B1,
                                B4.B1 = B4/B1,
                                B5.B1 = B5/B1,
                                B7.B1 = B7/B1,
                                B3.B2 = B3/B2,
                                B4.B2 = B4/B2,
                                B5.B2 = B5/B2,
                                B7.B2 = B7/B2,
                                B4.B3 = B4/B3,
                                B5.B3 = B5/B3,
                                B7.B3 = B7/B3,
                                B5.B4 = B5/B4,
                                B7.B4 = B7/B4,
                                B7.B5 = B7/B5,
                                Latitude = lat,
                                Longitude = lon
                                # station_nm = paste0(0,station_no),
                                # site_no = paste0(0,site_no)
                              )][ 
                                # select only columns of interest
                                ,.(site_no, station_nm, year,
                                   river, river_nm,
                                   # distance_km,
                                   # width, drainage_area_km2,
                                   Latitude,Longitude,sample_dt, num_pix, 
                                   snow_ice_qa_count,
                                   cloud_cover, cloud_qa_count,
                                   landsat_dt,
                                   B1,B2,B3,B4,B5,B6,B7,B2.B1,B3.B1,B4.B1,B5.B1,B7.B1,B3.B2,B4.B2,B5.B2,
                                   B7.B2,B4.B3,B5.B3,B7.B3,B5.B4,B7.B4,B7.B5, B1.2,B2.2,B3.2,B4.2,B5.2,B7.2
                                )][site_no != "0"][
                                  !((B6 < 2800 & B1 > 900 & B2 > 900 & B3 > 900 & B5 > 300 & B7 > 200 & B1 > B3 & B1 < B4) | # Elimate snowy & cold images
                                      (B1 > 700 & B1/B2 > 1.2 & B5 > 200)|
                                      ((B1 + B2 + B3 + B4) > 3200 & B3 < B1 & B3/B1 < 1.5 & B6 < 2750 & B5 > 300) |
                                      (B4 > 1500 & B4/B3 > 1.5 & B6 < 2800)| # This eliminates many cloudy/snowy images at high latitudes
                                      # ((B1 + B2 + B3 + B4) > 4000 & B6 < 2750 & B5 > 300 & abs(Latitude) > 40)
                                      ((B1 + B2 + B3 + B4) > 4000 & B6 < 2750 & B5 > 500 & abs(Latitude) > 40) # *changed B5 min to 500*
                                    # (B1 > 700 &
                                    # snow_ice_qa_count > (num_pix * 10) & 
                                    # snow_ice_qa_count > 500 &
                                    # B3/B1 < 1.5)
                                  )
                                ][
                                  ,':='(month = month(sample_dt),
                                        decade = ifelse(year < 1990, 1990,
                                                        ifelse(year > 2019, 2020,
                                                               year - year%%5)))]

# Remove pixels with snow, ice, clouds
river_import <- river_import[cloud_cover < 70 & !(num_pix < 2 & cloud_qa_count > 100)][
  yday(sample_dt) > 120 & yday(sample_dt) < 290 # Winter months
]

#### 2. ------ CALCULATE LANDSAT SSC ------ ####
# Apply SSC calibration models to make predictions based on new surface reflectance inputs (cluster needed)
# First, import function file
ssc_cluster_funs <- readRDS(paste0(wd_imports, 'SSC_cluster_function.rds'))

# And import cluster centers
clusters_calculated_list <- readRDS(paste0(wd_imports,'cluster_centers.rds'))
# Set number of cluster centers (6)
cluster_n_best <- 6
clustering_vars <- colnames(clusters_calculated_list[[cluster_n_best]]$centers)

# Scaling for cluster calculation
site_band_scaling <- readRDS(paste0(wd_imports,'site_band_scaling_all.rds'))
# Regressors
regressors_all <- c('B1', 'B2', 'B3', 'B4', 'B5', 'B7', # raw bands
                    'B1.2', 'B2.2', 'B3.2', 'B4.2', 'B5.2', 'B7.2', # squared bands
                    'site_no', # no clear way to add categorical variables
                    'B2.B1', 'B3.B1', 'B4.B1', 'B5.B1', 'B7.B1', # band ratios
                    'B3.B2', 'B4.B2', 'B5.B2', 'B7.B2',
                    'B4.B3', 'B5.B3', 'B7.B3',
                    'B5.B4', 'B7.B4', 'B7.B5')

# For base, cluster, and site predictions
getSSC_pred <- function(lm_data, regressors, cluster_funs){ # Version that includes site specification
  lm_data$pred_st <- NA
  lm_data[,ssc_subset:=cluster_sel] # clusters
  subsets <- unique(lm_data$ssc_subset)
  for(i in subsets){ # for individual cluster models
    # print(i)
    regressors_sel <- regressors[-which(regressors == 'site_no')]
    lm_data_lm <- lm_data[ssc_subset == i] # only chooses sites within cluster
    
    ssc_lm <- cluster_funs[[i]]
    glm_pred <- predict(ssc_lm, newx = as.matrix(lm_data_lm[,..regressors_sel]), s = "lambda.1se")
    lm_data[ssc_subset == i, pred_cl:= glm_pred]
    lm_data_lm <- NA
    # lm_data$res_cl[which(lm_data$ssc_subset == i)] <- resid(ssc_lm)
  }
  return(lm_data)
}

# Calculate cluster based on cluster function including scaling

# Cluster reflectance values by site
# (Could also cluster by year)
getCluster_monthly_decadal <- function(df,clustering_vars,n_centers, kmeans_object){
  # Compute band median at each site for clustering variables
  site_band_quantiles_all <- df[
    # n_insitu_samples_bySite][N_insitu_samples > 12
    ,.(N_samples = .N,
       B1 = median(B1),
       B2 = median(B2),
       B3 = median(B3),
       B4 = median(B4),
       # B5 = median(B5),
       # B7 = median(B7),
       B2.B1 = median(B2.B1),
       B3.B1 = median(B3.B1),
       B4.B1 = median(B4.B1),
       B3.B2 = median(B3.B2),
       B4.B2 = median(B4.B2),
       B4.B3 = median(B4.B3),
       B4.B3.B1 = median(B4.B3/B1)), 
    # by = .(station_nm,site_no, month, decade)]
    by = .(station_nm,site_no)]
  
  site_band_quantile_scaled <- scale(site_band_quantiles_all[,..clustering_vars], 
                                     center = attributes(site_band_scaling)$`scaled:center`[clustering_vars], 
                                     scale = attributes(site_band_scaling)$`scaled:scale`[clustering_vars])
  
  closest.cluster <- function(x) {
    cluster.dist <- apply(kmeans_object$centers, 1, function(y) sqrt(sum((x-y)^2)))
    return(which.min(cluster.dist)[1])
  }
  site_band_quantiles_all$cluster <- apply(site_band_quantile_scaled, 1, closest.cluster)
  
  df_cluster <- merge(df,
                      # site_band_quantiles_all[,c('site_no','station_nm','cluster', 'month','decade')], 
                      # by = c('site_no', 'station_nm','month','decade'))
                      site_band_quantiles_all[,c('site_no','station_nm','cluster')], 
                      by = c('site_no', 'station_nm'))
  df_cluster$cluster_sel <- df_cluster$cluster
  return(df_cluster)
  
}
# Get cluster for each site based on typical spectral profile
# This takes a long time to run
river_landsat_cl <- getCluster_monthly_decadal(river_import, 
                                               clustering_vars,cluster_n_best, 
                                               clusters_calculated_list[[cluster_n_best]])

# Run SSC prediction algorithm to get clustered prediction for SSC
river_landsat_pred <- getSSC_pred(na.omit(river_landsat_cl, cols = c(regressors_all, 'cluster_sel')), 
                                  regressors_all, ssc_cluster_funs)[,':='(
                                    SSC_mgL = ifelse(pred_cl > 5.5, NA, 10^pred_cl),
                                    month = month(sample_dt),
                                    decade = ifelse(year(sample_dt) < 1990, 1990,
                                                    ifelse(year(sample_dt) > 2024, 2020, 
                                                           year(sample_dt) - year(sample_dt)%%5)))]


#### 3. CLEAN DATA AND WRITE TO DRIVE ####
# Select just simple columns for export
river_landsat_pred_clean <- river_landsat_pred[
  ,.(site_no, station_nm, river, river_nm, month, year, decade, Latitude, Longitude,sample_dt,
     num_pix, B1 = round(B1), B2 = round(B2), B3 = round(B3), B4 = round(B4), B6 = round(B6),
     cluster, SSC_mgL
  )
]

# Write full table to drive
# fwrite(river_landsat_pred_clean, paste0(wd_imports,'taymyr_thaw_slump_river_landsat_pred.csv'))

# Remove winter months from dataset
river_landsat_pred_clean_2 <- river_landsat_pred_clean[
  # yday(sample_dt) > 80 & yday(sample_dt) < 260 # Winter months
]

# Add an additional filter for high SSC (mostly due to errors/artifacts)
river_landsat_pred_clean_3 <- river_landsat_pred_clean_2[SSC_mgL > 0.5 & SSC_mgL < 15000 &
                                                           !(SSC_mgL > 1000 & (B1 + B2 + B3) < 700)][
                                                             num_pix > 2
                                                           ]
river_landsat_pred_clean_3[,.(N_images = .N), by = .(site_no, station_nm, river, cluster)]
# Write clean data to drive
# fwrite(river_landsat_pred_clean_3, file = paste0(wd_imports, 'tamyr_thaw_slump_river_ssc_warm_months.csv'))

#### 4. ------ LANDSAT SSC TIMESERIES AND SPATIAL ANALYSIS ------ ####
#### 4A. CALCULATE AND PLOT LANDSAT SSC TIMESERIES ####
# Add yday columns
# Add `reference` column for whether slump has occurred
river_landsat_pred_clean_3 <- river_landsat_pred_clean_3[
  ,':='(ten_day = yday(sample_dt)-yday(sample_dt)%%10,
        reference = factor(ifelse(grepl('control',site_no), 'Reference',
                                  ifelse(year > 2019, 'Affected, Post-slump', 'Affected, Pre-slump')),
                           levels = c('Reference', 'Affected, Pre-slump', 'Affected, Post-slump')),
        reference_day = factor(ifelse(grepl('control',site_no), 'Reference',
                                      ifelse(sample_dt > ymd('2020-08-04'), 'Affected, Post-slump', 'Affected, Pre-slump')),
                               levels = c('Reference', 'Affected, Pre-slump', 'Affected, Post-slump')))]

# Add river name and remove rivers E G (too small for reliable detection)
river_landsat_pred_clean_3 <- river_landsat_pred_clean_3[,':='(
  river = factor(river, 
                 levels = c('A','B','C','D','E','F','G','Estuary 1','Estuary 2', 'Control'), 
                 ordered = T),
  river_nm = factor(river_nm, 
                    levels = c('Malinovskogo','Fomina',"Proval'naya",'Bujnaja','E','F','G','Leningradskaya','Nizhnyaya Taymyra', 'Control'), 
                    ordered = T)
)]

river_landsat_pred_clean_3 <- river_landsat_pred_clean_3[!(river %in% c('E','G'))]

# Add Estuary, Control, Affected designation 
river_landsat_pred_clean_3 <- river_landsat_pred_clean_3[
  ,':='(reference_simple = ifelse(grepl('control', casefold(river)), 'Control rivers', 
                                  ifelse(grepl('Estuary', river),'Estuaries', 'Affected rivers')))
]

## FIGURE: Figure 2. SSC timeseries for affected rivers, pre- and post-slump colored.
SSC_timeseries_by_site_plot <- ggplot(river_landsat_pred_clean_3[
  !grepl('control', casefold(site_no))
][
  # SSC_mgL < 1400
], 
aes(x = year + yday(sample_dt)/365, y = SSC_mgL,
    color = reference_day
)) + 
  geom_point(alpha = 0.2, size = 1.5) +
  stat_summary(aes(x = year + 0.5), geom = 'line', fun = 'mean') + 
  stat_summary(aes(x = year + 0.5), geom = 'errorbar', width = 0.2, color = 'grey20', linewidth = 0.25) +
  stat_summary(aes(x = year + 0.5, fill = reference_day), geom = 'point', fun = 'mean', pch = 21, stroke = 0.25, color = 'black') + 
  geom_vline(xintercept = 2020 + 216/365, linetype = 'dashed') +
  facet_wrap(.~river_nm,
             scales = 'free',
             ncol = 2) +
  scale_color_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  scale_fill_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  theme_markdown +
  scale_x_continuous(limits = c(1984, 2025)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.22,0.95),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_markdown()) + 
  labs(
    x = '***Year***',
    y = '***SSC*** (mg/L)',
    color = '',
    fill = ''
  )

ggsave(SSC_timeseries_by_site_plot, filename = paste0(wd_figures, region_name, '_SSC_timeseries_by_site_plot.png'),
       width = 6.5, height = 7.5)
ggsave(SSC_timeseries_by_site_plot, filename = paste0(wd_figures, region_name, '_SSC_timeseries_by_site_plot.pdf'),
       width = 6.5, height = 7.5, useDingbats = F)


chalov_2022_ssc_comparison <- ggplot(river_landsat_pred_clean_3) +
  stat_summary(data = river_landsat_pred_clean_3[
    # !grepl('control', casefold(site_no))
    ], aes(x = year(sample_dt) + 0.5, y = SSC_mgL,
    fill = reference_day), pch = 21, stroke = 0.25, color = 'black'
) + 
  stat_summary(aes(x = year + 0.5, y = 1.42*exp(86.3*B4/10000), fill = 'Chalov et al., 2022'), pch = 21, stroke = 0.25, color = 'black') +
  scale_y_log10(limits = c(0.1, 100000), labels = fancy_scientific_modified) +
  facet_wrap(.~river_nm, scales = 'free', ncol = 2) +
  scale_color_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod',
                                'Chalov et al., 2022' = 'black')) +
  scale_fill_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod',
                    'Chalov et al., 2022' = 'black')) +
  theme_markdown +
  scale_x_continuous(limits = c(1984, 2025)) +
  theme(legend.position = "top",
        legend.position.inside = c(0.22,0.95),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_markdown()) + 
  labs(
    x = '***Year***',
    y = '***SSC*** (mg/L)',
    color = '',
    fill = ''
  )

ggsave(chalov_2022_ssc_comparison, filename = paste0(wd_figures, region_name, '_Chalov_2022_comparison_SSC_timeseries_by_site_plot.png'),
       width = 6.5, height = 7.5)
ggsave(chalov_2022_ssc_comparison, filename = paste0(wd_figures, region_name, '_Chalov_2022_comparison_SSC_timeseries_by_site_plot.pdf'),
       width = 6.5, height = 7.5, useDingbats = F)

#### 4B. LANDSAT DISTRIBUTIONS, PRE- AND POST-FAILURE ####
# FIGURE: Figure SX. SSC timeseries for site s002, pre- and post-slump colored.
# (NOT USED)
SSC_yday_by_site_plot <- ggplot(river_landsat_pred_clean_3[
  !grepl('control', casefold(site_no))
][
  # SSC_mgL < 1400
], 
# aes(x = factor(yday(sample_dt) - yday(sample_dt)%%10 + 5), y = SSC_mgL,
aes(x = yday(sample_dt) - yday(sample_dt)%%10 + 5, y = SSC_mgL,
    fill = reference
)) + 
  geom_point(aes(color = reference), alpha = 0.15) +
  stat_summary(aes(group = reference), geom = 'line', fun = 'mean') +
  stat_summary(aes(group = reference), geom = 'errorbar', width = 0.2, color = 'grey20', linewidth = 0.25) +
  stat_summary(aes(fill = reference), geom = 'point', fun = 'mean', pch = 21, size = 2.5, stroke = 0.25, color = 'black') +
  # geom_boxplot(color = 'black', linewidth = 0.5, outlier.colour = NA) +
  facet_wrap(.~river_nm,
             scales = 'free',
             ncol = 2) +
  scale_color_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  scale_fill_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  theme_markdown +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.8,0.95),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) + 
  labs(
    x = 'Day of year',
    y = 'SSC (mg/L)',
    color = '',
    fill = ''
  )

ggsave(SSC_yday_by_site_plot, filename = paste0(wd_figures, region_name, '_SSC_timeseries_by_site_plot.png'),
       width = 6, height = 6)
ggsave(SSC_yday_by_site_plot, filename = paste0(wd_figures, region_name, '_SSC_timeseries_by_site_plot.pdf'),
       width = 6, height = 6, useDingbats = F)

# FIGURE: Figure SX. SSC pre- and post-failure histograms
# (NOT USED)
SSC_by_site_histogram <- ggplot(river_landsat_pred_clean_3[
  !grepl('control', casefold(site_no))
], 
aes(y = SSC_mgL)) + 
  geom_histogram(data = river_landsat_pred_clean_3[!grepl('control', casefold(site_no)) & grepl('Post', reference)], 
                 aes(after_stat(density), fill = factor(gsub('Affected, ', '', reference))), 
                 position = 'identity', lwd = 0.25, bins = 20, color = 'black') +
  geom_histogram(data = river_landsat_pred_clean_3[!grepl('control', casefold(site_no)) & !grepl('Post', reference)], 
                 aes(after_stat(density), fill = factor(gsub('Affected, ', '', reference))), 
                 position = 'identity', linewidth = 0.25, bins = 20, color = NA, alpha = 0.7) +
  geom_histogram(data = river_landsat_pred_clean_3[!grepl('control', casefold(site_no)) & !grepl('Post', reference)], 
                 aes(after_stat(density)), 
                 position = 'identity', linewidth = 0.25, bins = 20, fill = NA, color = 'steelblue') +
  facet_wrap(.~river_nm,
             # scales = 'free',
             ncol = 2) +
  scale_fill_manual(values = c('Reference' = '#41A8BF', 'Pre-slump' = 'steelblue', 'Post-slump' = 'goldenrod')) +
  scale_color_manual(values = c('Reference' = '#41A8BF', 'Pre-slump' = 'steelblue', 'Post-slump' = 'goldenrod')) +
  # scale_fill_manual(values = c('Post-slump' = 'goldenrod', 'Pre-slump' = NA)) +
  # scale_color_manual(values = c('Pre-slump' = 'steelblue')) +
  theme_markdown +
  scale_x_continuous(expand = expansion(mult = c(0,0.1))) +
  scale_y_log10() +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.22,0.95),
        legend.background = element_blank(),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(),
        axis.line.x = element_line(linewidth = 0.5)) + 
  labs(
    x = 'Density',
    y = 'SSC (mg/L)',
    color = '',
    fill = ''
  )

ggsave(SSC_by_site_histogram, filename = paste0(wd_figures, region_name, '_SSC_by_site_histogram.png'),
       width = 5, height = 6)
ggsave(SSC_by_site_histogram, filename = paste0(wd_figures, region_name, '_SSC_by_site_histogram.pdf'),
       width =5, height = 6, useDingbats = F)

#### 4C. ANNUAL AVERAGE SSC AND SSC VS. SLUMP AREA ANALYSIS ####
# Calculate by-river annual average SSC using monthly averages 
river_landsat_pred_clean_annual_summary <- river_landsat_pred_clean_3[
  ,.(SSC_mgL = mean(SSC_mgL, na.rm = T)),
  by = .(river, river_nm, Latitude, Longitude, station_nm, month, year, reference, reference_simple)
][
  ,.(SSC_mgL = mean(SSC_mgL, na.rm = T),
     SSC_mgL_sd = sd(SSC_mgL, na.rm = T)),
  by = .(river, river_nm, station_nm, Latitude, Longitude, year, reference, reference_simple)
]

# Calculate by-river, pre-2020 average SSC (filtering to pre-2020 data only)
river_landsat_pred_clean_pre_slump_summary <- river_landsat_pred_clean_annual_summary[
  year < 2020][
    ,.(SSC_mgL_mean_pre = mean(SSC_mgL, na.rm = T),
       SSC_mgL_sd_pre = sd(SSC_mgL, na.rm = T)),
    by = .(river, river_nm, station_nm)
  ]

river_landsat_pred_clean_post_slump_summary <- river_landsat_pred_clean_annual_summary[
  year > 2020][
    ,.(SSC_mgL_mean_post = mean(SSC_mgL, na.rm = T),
       SSC_mgL_sd_post = sd(SSC_mgL, na.rm = T)),
    by = .(river, river_nm, station_nm)
  ]

river_landsat_pred_clean_pre_post_slump_summary <- merge(
  river_landsat_pred_clean_pre_slump_summary,
  river_landsat_pred_clean_post_slump_summary,
  by = c('river','river_nm','station_nm')
)[
  ,':='(percent_change = ifelse(SSC_mgL_mean_post > SSC_mgL_mean_pre, 
                                (SSC_mgL_mean_post/SSC_mgL_mean_pre)/SSC_mgL_mean_pre*100,
                                -(SSC_mgL_mean_pre/SSC_mgL_mean_post)/SSC_mgL_mean_pre*100),
        relative_change = ifelse(SSC_mgL_mean_post > SSC_mgL_mean_pre, 
                                (SSC_mgL_mean_post/SSC_mgL_mean_pre),
                                -(SSC_mgL_mean_pre/SSC_mgL_mean_post)
        )
  )
][
  ,':='(reference_simple = factor(ifelse(grepl('control', casefold(river)) | station_nm %chin% c('Estuary 1 1', 'Estuary 2 1'), 'Control rivers', 
                                  ifelse(grepl('Estuary', river),'Estuaries', 'Affected rivers')),
                                  levels = c('Control rivers','Affected rivers','Estuaries'), ordered = T)
  )
]



# Summarize pre- and post-2020 SSC relative change, by river category
river_landsat_pre_post_slump_by_category_summary <- river_landsat_pred_clean_pre_post_slump_summary[
  ,.(relative_change = mean(relative_change, na.rm = T),
     relative_change_sd = sd(relative_change, na.rm = T),
     relative_change_range = paste0(round(min(relative_change, na.rm = T),2),'-', round(max(relative_change, na.rm = T),2)),
     percent_change = mean(percent_change, na.rm = T),
     percent_change_sd = sd(percent_change, na.rm = T),
     percent_change_range = paste0(round(min(percent_change, na.rm = T),2),'-',round(max(percent_change, na.rm = T),2)),
     N_rivers = .N
  ),
  by = .(reference_simple)
]


## Figure SX. Pre- vs. Post-2020 SSC relative change
relative_change_by_river_type_boxplot <- ggplot(river_landsat_pred_clean_pre_post_slump_summary, 
       aes(x = reference_simple, y = relative_change, fill = reference_simple)) +
  geom_hline(yintercept = 0, lty = 'dashed') +
  geom_boxplot(outlier.color = NA) +
  # geom_point(pch = 21, size = 4, position = position_jitter(width = 0.3)) +
  geom_beeswarm(pch = 21, size = 4, cex = 2.5) +
  geom_richtext(data = river_landsat_pre_post_slump_by_category_summary, 
  aes(x = reference_simple, y = Inf, 
  label = paste0(
    "**Avg.** = ", round(relative_change, 2),
    "<br>±", round(relative_change_sd, 2), " SD",
    "<br>**N** = ", N_rivers, " stations"
  )),
            vjust = 1.3,
  label.color = NA,    # no border around the text box
  fill = NA,           # transparent background
  inherit.aes = FALSE) +
  scale_fill_manual(values = c('Control rivers' = 'grey','Estuaries' = '#e9c979','Affected rivers' = 'goldenrod')) +
  theme_markdown +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.2))) +
  labs(
    x = '***River category***',
    y = '***Relative change*** (pre-/post-2020 SSC OR -post-/pre-2020)'
  ) +
  theme(
    axis.title = element_markdown()
  )

ggsave(relative_change_by_river_type_boxplot, filename = paste0(wd_figures, 'relative_change_by_river_type_boxplot.png'),
       width = 4.5, height = 6)
ggsave(relative_change_by_river_type_boxplot, filename = paste0(wd_figures, 'relative_change_by_river_type_boxplot.pdf'),
       width = 4.5, height = 6, useDingbats = F)
# Add pre-2020 average SSC to complete annual-average SSC dataset
river_landsat_pred_clean_annual_summary <- merge(
  river_landsat_pred_clean_annual_summary,
  river_landsat_pred_clean_pre_slump_summary,
  by = c('river', 'river_nm','station_nm')
)[
  ,':='(SSC_mgL_stdize = (SSC_mgL - SSC_mgL_mean_pre)/SSC_mgL_sd_pre)
]

# Add slump area data to annual SSC data
river_landsat_pred_clean_annual_summary <- merge(
  river_landsat_pred_clean_annual_summary,
  slumps_by_watershed_filled[,.(watershed_area_km2=max(watershed_area_km2, na.rm = T), 
                         area_km2 = max(area_km2, na.rm = T)),
                         by = .(river, station_nm, year=year-1)],
  by = c('river', 'station_nm', 'year'),
  all.x = T
)

# Get non-control rivers
non_control_rivers <- river_landsat_pred_clean_annual_summary[!grepl('Control', river)][
  ,.(n_stations = uniqueN(gsub(paste0(river, ' '), '', station_nm))),
  by = .(river, river_nm)
]

sample_station_watershed_summary <- slumps_by_watershed_filled[,.(
  slump_area_km2 = max(area_km2)
), by = .(river, station_nm, watershed_area_km2)]

# Find SSC sampling stations with both SSC and slump data, since 2014
sites_with_SSC_and_slump_data <- river_landsat_pred_clean_annual_summary[
  year > 2014
  & watershed_area_km2 > 0
][,.(N_samples = .N),
  by = .(river, river_nm, station_nm, watershed_area_km2)]

# Get rivers that have both SSC and slump data
rivers_with_SSC_and_slump_data <- river_landsat_pred_clean_annual_summary[
  year > 2014
  & watershed_area_km2 > 0 & area_km2 > 0
][,.(N_samples = .N),
  by = .(river, river_nm)]

# Make a model relating SSC to slump area, BY STATION
by_station_ssc_vs_slump_model <- vector(mode = 'list', length = nrow(rivers_with_SSC_and_slump_data))

for(i in 1:nrow(sites_with_SSC_and_slump_data)){
  # Get river name, russian name, station name, watershed area
  river_sel <- sites_with_SSC_and_slump_data[i, river]
  river_nm_sel <- sites_with_SSC_and_slump_data[i, river_nm]
  station_nm_sel <- sites_with_SSC_and_slump_data[i, station_nm]
  watershed_area_km2_sel <- sites_with_SSC_and_slump_data[i, watershed_area_km2]
  # Subset to just data from the selected station
  dt <- river_landsat_pred_clean_annual_summary[
    year > 2014
    & station_nm == station_nm_sel
  ][
    ,':='(area_km2 = ifelse(area_km2 == 0, NA, area_km2))
  ]
  
  # Replace 0s with a very low value to include 0s in log scaling
  # Calculate SSC anomaly, percent slump area 
  dt <- dt[
    ,':='(area_km2 = ifelse(is.na(area_km2), 
                            ifelse(!is.infinite(min(area_km2, na.rm = T)), 
                                   min(area_km2, na.rm = T)/10, watershed_area_km2/100000), 
                            area_km2)),
    by = .(river, river_nm, station_nm)
  ][
    ,':='(SSC_mgL_dev = SSC_mgL-SSC_mgL_mean_pre,
          percent_slump_area = area_km2/watershed_area_km2*100,
          log10_percent_slump_area = log10(area_km2/watershed_area_km2*100)
    )
  ]
  
  # Run model for SSC vs. slump area
  model = lm(SSC_mgL ~ log10_percent_slump_area, data = dt)
  # model = lm(SSC_mgL_stdize ~ area_km2, data = dt)
  model_summary <- data.table(glance(model))[,':='(
    river = river_sel,
    river_nm = river_nm_sel,
    station_nm = station_nm_sel
  )]
  
  # Build a data.table for plotting the model later, populate with modeled SSC
  min_x_range = min(dt$log10_percent_slump_area,na.rm = T)
  max_x_range = max(dt$log10_percent_slump_area,na.rm = T)
  x_step = (max_x_range-min_x_range)/50
  
  synthetic_dt <- data.table(
    log10_percent_slump_area = seq(
      min_x_range-x_step, 
      max_x_range+x_step,
      x_step),
    river = river_sel,
    river_nm = river_nm_sel,
    station_nm = station_nm_sel,
    watershed_area_km2 = watershed_area_km2_sel
  )
  
  synthetic_dt$SSC_mgL_dev_pred = predict(model,newdata =synthetic_dt)
  
  # Save model, synthetic data for model plots, and summary tables
  by_station_ssc_vs_slump_model[[i]] <- model
  
  if(i == 1){
    ssc_vs_slump_summary <- model_summary
    ssc_vs_slump_by_station_dt <- dt
    synthetic_ssc_vs_slump_by_station_dt <- synthetic_dt
  }
  else{
    ssc_vs_slump_summary <- rbind(ssc_vs_slump_summary, model_summary, use.names = T)
    ssc_vs_slump_by_station_dt <-rbind(ssc_vs_slump_by_station_dt, dt, use.names = T)
    synthetic_ssc_vs_slump_by_station_dt <-rbind(synthetic_ssc_vs_slump_by_station_dt, synthetic_dt, use.names = T)
  }

}

# Make a model relating SSC* to slump area %, BY RIVER (combining stations)
# SSC* = (SSC annual - pre-2020 SSC average)
# slump area % = annual slump area/watershed area
by_river_ssc_vs_slump_model <- vector(mode = 'list', length = nrow(rivers_with_SSC_and_slump_data))
for(i in 1:nrow(rivers_with_SSC_and_slump_data)){
  # Get river name, russian name
  river_sel <- rivers_with_SSC_and_slump_data[i, river]
  river_nm_sel <- rivers_with_SSC_and_slump_data[i, river_nm]
  # Subset to just data from the selected river
  dt <- river_landsat_pred_clean_annual_summary[
    year > 2014
    & river == river_sel
    & watershed_area_km2 > 0
    # & watershed_area_km2 > 0 & area_km2 >= 0
  ][
    ,':='(area_km2 = ifelse(area_km2 == 0, NA, area_km2))
  ]
  
  # Replace 0s with a very low value to include 0s in log scaling
  # Calculate SSC anomaly, percent slump area
  dt <- dt[
    ,':='(area_km2 = ifelse(is.na(area_km2), 
                            ifelse(!is.infinite(min(area_km2, na.rm = T)), 
                                   min(area_km2, na.rm = T)/10, watershed_area_km2/100000), 
                            area_km2)),
    by = .(river, river_nm, station_nm)
  ][
    ,':='(SSC_mgL_dev = SSC_mgL-SSC_mgL_mean_pre,
    percent_slump_area = area_km2/watershed_area_km2*100,
    log10_percent_slump_area = log10(area_km2/watershed_area_km2*100)
    )
  ]
  
  # Run model for SSC vs. slump area
  model = lm(SSC_mgL_dev ~ log10_percent_slump_area, data = dt)
  # model = lm(SSC_mgL_dev ~ percent_slump_area, data = dt)
  model_summary <- data.table(glance(model))[,':='(
    river = river_sel,
    river_nm = river_nm_sel
  )][,':='(watershed_area_km2 = watershed_area_km2_sel)]
  
  # Build a data.table for plotting the model later, populate with modeled SSC
  min_x_range = min(dt$log10_percent_slump_area,na.rm = T)
  max_x_range = max(dt$log10_percent_slump_area,na.rm = T)
  x_step = (max_x_range-min_x_range)/50
  
  synthetic_dt <- data.table(
    log10_percent_slump_area = seq(
      min_x_range-x_step, 
      max_x_range+x_step,
      x_step),
    river = river_sel,
    river_nm = river_nm_sel
  )
  
  synthetic_dt$SSC_mgL_dev_pred = predict(model,newdata =synthetic_dt)
  
  # Save model, synthetic data for model plots, and summary tables
  by_river_ssc_vs_slump_model[[i]] <- model
  if(i == 1){
    ssc_vs_slump_by_river_summary <- model_summary
    ssc_vs_slump_by_river_dt <- dt
    synthetic_ssc_vs_slump_by_river_dt <- synthetic_dt
  }
  else{
    ssc_vs_slump_by_river_summary <-rbind(ssc_vs_slump_by_river_summary, model_summary, use.names = T)
    ssc_vs_slump_by_river_dt <-rbind(ssc_vs_slump_by_river_dt, dt, use.names = T)
    synthetic_ssc_vs_slump_by_river_dt <-rbind(synthetic_ssc_vs_slump_by_river_dt, synthetic_dt, use.names = T)
  }
  
}


# Add regression outputs to slump summary data for plotting
ssc_vs_slump_summary <- merge(
  ssc_vs_slump_summary,
  sample_station_watershed_summary,
  by = c('river','station_nm')
)
 
ssc_vs_slump_summary <- ssc_vs_slump_summary[,':='(slump_percent_watershed_area = slump_area_km2/watershed_area_km2 * 100)][order(p.value)]
SSC_slump_vs_slump_area_lm <- lm(log10(slump_percent_watershed_area) ~ r.squared, data = ssc_vs_slump_summary[slump_percent_watershed_area > 0])
glance(SSC_slump_vs_slump_area_lm)
SSC_slump_vs_slump_area_sig_only_lm <- lm(log10(slump_percent_watershed_area) ~ r.squared, data = ssc_vs_slump_summary[slump_percent_watershed_area > 0 & p.value < 0.05])
glance(SSC_slump_vs_slump_area_sig_only_lm)

variance_in_SSC_slump_vs_slump_area <- ggplot(ssc_vs_slump_summary[
  # !grepl('Estuary', river)
  ], 
  aes(x = slump_percent_watershed_area, y = r.squared)) +
  geom_smooth(method = 'lm', lty = 'dashed', color = 'black', lwd = 0.75) +
  geom_point(size = 3, pch = 21, stroke = 0.5, aes(fill = ifelse(p.value < 0.05, 'sign.', 'not sign.'))) +
  scale_fill_manual(values = c('sig.' = 'black', 'not sign.' = 'grey90')) +
  theme_markdown +
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_continuous(limits = c(0,1), expand = expansion(mult = c(0.1, 0.1))) +
  labs(
    x = '***Slump area*** (% watershed area)',
    y = '***R***<sup>2</sup>'
  ) +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

variance_in_SSC_slump_vs_slump_area_box <- ggplot(ssc_vs_slump_summary[
  # !grepl('Estuary', river)
], 
aes(x = ifelse(slump_percent_watershed_area > 0.3, '> 0.3%', '<= 0.3%'), y = r.squared)) +
  geom_boxplot(aes(fill = ifelse(slump_percent_watershed_area > 0.3, 'sign.', 'not sign.')), color = 'black', lwd = 0.5) +
  scale_fill_manual(values = c('sig.' = 'black', 'not sign.' = 'grey90')) +
  scale_y_continuous(limits = c(0,1), expand = expansion(mult = c(0.1, 0.1))) +
  theme_markdown +
  labs(
    x = '***% Watershed area***',
    y = '***R***<sup>2</sup>'
  ) +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

variance_in_SSC_slump_vs_slump_area_combined <- 
  variance_in_SSC_slump_vs_slump_area + 
  (variance_in_SSC_slump_vs_slump_area_box +
     theme(
       axis.title.y = element_blank()
     )) +
  plot_layout(widths = c(0.75, 0.4)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(variance_in_SSC_slump_vs_slump_area_combined, filename = paste0(wd_figures, 'variance_in_SSC_slump_vs_slump_area.pdf'),
       useDingbats = F, width = 6.5, height = 4.75)
ggsave(variance_in_SSC_slump_vs_slump_area_combined, filename = paste0(wd_figures, 'variance_in_SSC_slump_vs_slump_area.png'),
       width = 6.5, height = 4.75)


#### 4D. SSC VS. SLUMP AREA PLOTS ####
# Plot pre-slump SSC vs. post-slump SSC
pre_vs_post_slump_SSC_plot <- ggplot(river_landsat_pred_clean_pre_post_slump_summary, aes(x = SSC_mgL_mean_pre, y = SSC_mgL_mean_post)) +
  geom_point(pch = 21, size = 2.5, aes(fill = reference_simple), color = 'black') +
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(method = 'lm', se = F, linewidth = 0.5, color = 'black', lty = 'dashed')+
  theme_markdown +
  scale_fill_manual(values = c('Control rivers' = 'grey','Estuaries' = '#e9c979','Affected rivers' = 'goldenrod')) +
  labs(
    x = '***SSC*** (mg/L), pre-2020',
    y = '***SSC*** (mg/L), post-2020',
    fill = ''
  )

pre_vs_post_slump_SSC_lm <- lm(SSC_mgL_mean_post~SSC_mgL_mean_pre, data = river_landsat_pred_clean_pre_post_slump_summary)
glance(pre_vs_post_slump_SSC_lm)

slump_area_predicted_by_preslump_SSC_dt <- river_landsat_pred_clean_annual_summary[year == 2023]
slump_area_predicted_by_preslump_SSC <- lm(log10(area_km2/watershed_area_km2*100)~SSC_mgL_mean_pre, data = slump_area_predicted_by_preslump_SSC_dt)
glance(slump_area_predicted_by_preslump_SSC)
slump_area_predicted_by_preslump_SSC_dt$slump_area_predicted_by_preslump_SSC <- predict(slump_area_predicted_by_preslump_SSC)

river_landsat_pred_clean_annual_summary <- river_landsat_pred_clean_annual_summary[,':='(percent_watershed_area = area_km2/watershed_area_km2*100)]

pre_vs_post_2020_slump_area <- dcast.data.table(river + river_nm + station_nm + reference_simple ~ year, value.var = c('percent_watershed_area'),
                 data = river_landsat_pred_clean_annual_summary[year == 2019 | year == 2023])

pre_vs_post_2020_slump_area_lm <- lm(log10(`2023`)~log10(`2019`), data = pre_vs_post_2020_slump_area[`2019` > 0 & `2023` > 0])
glance(pre_vs_post_2020_slump_area_lm)

pre_vs_post_slump_area_plot <- ggplot(pre_vs_post_2020_slump_area[`2019` > 0], aes(x = `2019`, y = `2023`)) +
  geom_point(pch = 21, size = 2.5, aes(fill = reference_simple), color = 'black') +
  geom_smooth(method = 'lm', se = F, linewidth = 0.5, color = 'black', lty = 'dashed')+
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_log10(labels = fancy_scientific_modified) +
  theme_markdown +
  scale_fill_manual(values = c('Control rivers' = 'grey','Estuaries' = '#e9c979','Affected rivers' = 'goldenrod')) +
  labs(
    x = '***Slump area***, 2019<br>(% of watershed)',
    y = '***Slump area***, 2023<br>(% of watershed)',
    fill = ''
  )

# Plot pre-slump SSC vs. slump % watershed area
slump_area_predicted_by_pre2020_SSC_plot <- ggplot(slump_area_predicted_by_preslump_SSC_dt, aes(x = SSC_mgL_mean_pre, y = area_km2/watershed_area_km2*100)) +
  geom_point(pch = 21, size = 2.5, aes(fill = reference_simple), color = 'black') +
  theme_markdown +
  geom_line(aes(y = 10^slump_area_predicted_by_preslump_SSC), color = 'black', lty = 'dashed')+
  # scale_y_log10() +
  scale_fill_manual(values = c('Control rivers' = 'grey','Estuaries' = '#e9c979','Affected rivers' = 'goldenrod')) +
  labs(
    x = '***SSC*** (mg/L), pre-2020',
    y = '***Slump area***, 2023<br>(% of watershed)',
    fill = ''
  ) 


slump_area_predictors_plot <- (pre_vs_post_slump_SSC_plot / slump_area_predicted_by_pre2020_SSC_plot / pre_vs_post_slump_area_plot) +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'top')

ggsave(slump_area_predictors_plot, filename = paste0(wd_figures, 'slump_area_predictors_plot.pdf'),
       width = 5, height = 10, useDingbats = F)
ggsave(slump_area_predictors_plot, filename = paste0(wd_figures, 'slump_area_predictors_plot.png'),
       width = 5, height = 10)

# FIGURE: Figure A5 a-f. SSC vs. slump area, data + model fit for each sample station
# Each panel is a sampling station
# Add regression output statistics to each panel (R2, p-value)
all_ssc_vs_slump_plots <- vector(mode = 'list', length = nrow(non_control_rivers))
for(i in 1:nrow(non_control_rivers)){
  river_sel = non_control_rivers[i][,river]
  river_nm_sel = non_control_rivers[i][,river_nm]
  ssc_vs_slump_by_river_plot <- ggplot(river_landsat_pred_clean_annual_summary[
    # SSC_mgL_stdize > 0.08
    river == river_sel
    & !is.na(watershed_area_km2)
  ],
  aes(x = area_km2/watershed_area_km2*100, y = SSC_mgL))+
    geom_point(aes(fill = reference), pch = 21, color = 'black', size = 3, stroke = 0.3) +
    # geom_smooth(method = 'lm', se = F, color = 'black', lty = 'dashed') + # for linear fit
    geom_line(data = synthetic_ssc_vs_slump_by_station_dt[river %chin% river_sel], aes(x = 10^log10_percent_slump_area, y = SSC_mgL_dev_pred), color = 'black', lty = 'dashed')+ 
    geom_richtext(data = ssc_vs_slump_summary[river == river_sel], 
                  aes(x = -Inf, y = Inf, 
                      label = ifelse(p.value < 0.05, 
                                     ifelse(p.value < 0.001, 
                                            paste0('**R<sup>2</sup>: ', round(r.squared, 3), '; *p* < 0.001**'),
                                            paste0('**R<sup>2</sup>: ', round(r.squared, 3), '; *p*: ', round(p.value, 3), '**')),
                                     paste0('R<sup>2</sup>: ', round(r.squared, 3), '; *p*: ', round(p.value, 3))
                      )),
                  hjust = 0, vjust = 1, label.colour = NA, fill = NA, size = 3) + 
    scale_fill_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
    # scale_x_log10(labels = fancy_scientific_modified) +
    # scale_y_log10() +
    # facet_wrap(.~station_nm, scales = 'free', nrow = 1) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.25))) +
    facet_wrap(.~reorder(paste0(river, ': ', formatC(watershed_area_km2,format="f", big.mark=",", digits=0), ' km<sup>2</sup>'), -watershed_area_km2), nrow = 1) +
    theme_markdown +
    theme(strip.text = element_markdown()) +
    labs(
      x = 'Slump area (% of watershed)',
      y = 'SSC (mg/L)'
    ) +
    force_panelsizes(
      cols = unit(3, "cm"),
      rows = unit(3, "cm")
    )
  
  ggsave(ssc_vs_slump_by_river_plot, filename = paste0(wd_figures, 'taymyr_ssc_vs_slump_', gsub(' ', '_', river_sel), '.pdf'),
         width = non_control_rivers[i, n_stations]*1.5+1.5, height = 2, useDingbats=F)
  all_ssc_vs_slump_plots[[i]] <- ssc_vs_slump_by_river_plot
}

# FIGURE: Figure 3 a-d. SSC vs. slump area, by river
# Plot SSC vs. slump area, data + model fit for each river
# Each panel is a sampling station
# Add regression output statistics to each panel (R2, p-value)
ssc_vs_slump_area_plotlist <- vector(mode = "list", length = 3)
for(i in c(1,2,3)){
  # dt <- river_landsat_pred_clean_annual_summary[
  dt <- ssc_vs_slump_by_river_dt[
    # river != 'Control'
    grepl(c('Affected rivers', 'Estuaries','Control')[i], reference_simple)
  ]
  
  print(str(dt))
  rivers_sel <- unique(dt$river)
  ssc_vs_slump_area_plot <- ggplot(dt,
       aes(x = area_km2/watershed_area_km2*100, y = SSC_mgL_dev)) +
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=0), color = NA, fill = 'grey85', alpha = 0.25) +
  geom_point(aes(fill = reference), pch = 21, color = 'black', size = 3, stroke = 0.3) +
  # geom_smooth(method = 'lm', se = F, color = 'black', lty = 'dashed') +
  geom_line(data = synthetic_ssc_vs_slump_by_river_dt[river %chin% rivers_sel], aes(x = 10^log10_percent_slump_area, y = SSC_mgL_dev_pred), color = 'black', lty = 'dashed')+ 
  geom_richtext(data = ssc_vs_slump_by_river_summary[river %chin% rivers_sel], 
                aes(x = -Inf, y = Inf, 
                    label = ifelse(p.value < 0.05, 
                                   ifelse(p.value < 0.001, 
                                          paste0('**R<sup>2</sup>: ', round(r.squared, 3), '; *p* < 0.001**'),
                                          paste0('**R<sup>2</sup>: ', round(r.squared, 3), '; *p*: ', round(p.value, 3), '**')),
                                   paste0('R<sup>2</sup>: ', round(r.squared, 3), '; *p*: ', round(p.value, 3))
                                   
                    )),
                hjust = 0, vjust = 1, label.colour = NA, fill = NA, size = 3) + 
  scale_fill_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  # scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.25))) +
  facet_wrap(.~river_nm, scales = 'free', nrow = 2) +
  # facet_col(ifelse(grepl('Estuary', river), 'Estuary','')~., scales = 'free', space = 'free') +
  theme_markdown +
  labs(
    x = 'Slump area (% of watershed)',
    y = 'SSC anomaly (mg/L)'
  )
  
  ssc_vs_slump_area_plotlist[[i]] <- ssc_vs_slump_area_plot
}

# Combine SSC vs. slump area, by river plots for non-control rivers
ssc_vs_slump_area_combined <- wrap_plots(plotlist = ssc_vs_slump_area_plotlist[c(1,2)]) +
  plot_layout(ncol = 2, widths = c(1,0.41)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(ssc_vs_slump_area_combined, filename = paste0(wd_figures, 'ssc_vs_slump_area_by_river.pdf'),
       width = 6, height = 4, useDingbats = F)
ggsave(ssc_vs_slump_area_combined, filename = paste0(wd_figures, 'ssc_vs_slump_area_by_river.png'),
       width = 6, height = 4)

# Write annual summary to drive
# fwrite(river_landsat_pred_clean_annual_summary, file = paste0(wd_imports, region_name, '_ssc_warm_months_summary.csv'))

river_landsat_pred_clean_annual_summary <- river_landsat_pred_clean_annual_summary[
  # SSC_mgL_stdize > 0.08
  # !grepl('Estuary 2|Control', river)
  !is.na(watershed_area_km2)
][
  ,':='(
    SSC_anomaly_mgL = SSC_mgL - SSC_mgL_mean_pre,
    percent_slump_area = area_km2/watershed_area_km2*100,
    log10_percent_slump_area = log10(area_km2/watershed_area_km2*100)
  )
]


# Testing universality of slump area vs. SSC relationships
universal_ssc_vs_slump_model <- lm(SSC_anomaly_mgL~log10_percent_slump_area, 
   data = river_landsat_pred_clean_annual_summary[
     percent_slump_area>0
     # & !grepl('Estuary 2|Control', river)
     # & !grepl('Control', river)
     ]
)

glance(universal_ssc_vs_slump_model)
tidy(universal_ssc_vs_slump_model)

# Testing universality of slump area vs. SSC relationships, just watersheds with > 0.3% slumps
universal_ssc_vs_slump_area_gt0.01_model <- lm(SSC_anomaly_mgL~log10_percent_slump_area, 
                                              data = river_landsat_pred_clean_annual_summary[
                                                percent_slump_area > 0
                                                & percent_slump_area > 0.01
                                                # & !grepl('Estuary 2|Control', river)
                                                # & !grepl('Control', river)
                                              ]
)

glance(universal_ssc_vs_slump_area_gt0.01_model)
tidy(universal_ssc_vs_slump_area_gt0.01_model)
   
universal_ssc_vs_slump_area_gt0.01_model_summary <- data.table(glance(universal_ssc_vs_slump_area_gt0.01_model))
universal_ssc_vs_slump_area_gt0.01_model_std_error <- data.table(tidy(universal_ssc_vs_slump_area_gt0.01_model))[
  term == 'log10_percent_slump_area', std.error]

# Build a data.table for plotting the model later, populate with modeled SSC
min_x_range = min(river_landsat_pred_clean_annual_summary[
  percent_slump_area > 0
  & percent_slump_area > 0.01]$log10_percent_slump_area,na.rm = T)
max_x_range = max(river_landsat_pred_clean_annual_summary[
  percent_slump_area > 0
  & percent_slump_area > 0.01]$log10_percent_slump_area,na.rm = T)
x_step = (max_x_range-min_x_range)/50

synthetic_dt <- data.table(
  log10_percent_slump_area = seq(
    min_x_range-x_step, 
    max_x_range+x_step,
    x_step)
)

synthetic_dt$SSC_mgL_dev_pred = predict(universal_ssc_vs_slump_area_gt0.01_model,newdata =synthetic_dt)
synthetic_dt$SSC_mgL_dev_pred_lower = synthetic_dt$SSC_mgL_dev_pred - universal_ssc_vs_slump_area_gt0.01_model_std_error
synthetic_dt$SSC_mgL_dev_pred_upper = synthetic_dt$SSC_mgL_dev_pred + universal_ssc_vs_slump_area_gt0.01_model_std_error

# FIGURE: Figure A7a. SSC anomaly vs. slump area, with universal fit
universal_ssc_vs_slump_area_plot <- ggplot(river_landsat_pred_clean_annual_summary[
  # SSC_mgL_stdize > 0.08
  # !grepl('Control', river)
  # !grepl('Estuary 2|Control', river)
  !is.na(watershed_area_km2)
  & (area_km2/watershed_area_km2*100)>0.01
],
# aes(x = area_km2/watershed_area_km2*100, y = SSC_mgL))+
aes(x = area_km2/watershed_area_km2*100, y = SSC_mgL-SSC_mgL_mean_pre))+
  geom_rect(aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=0), color = NA, fill = 'grey85', alpha = 0.25) +
  geom_point(aes(fill = river_nm), pch = 21, color = 'black', size = 3, stroke = 0.3) +
  # geom_ribbon(data = synthetic_dt, 
  #             aes(x = 10^log10_percent_slump_area, ymin = SSC_mgL_dev_pred_lower, ymax = SSC_mgL_dev_pred_upper), 
  #             fill = 'grey', alpha = 0.4, inherit.aes = F)+
  geom_line(data = synthetic_dt, aes(x = 10^log10_percent_slump_area, y = SSC_mgL_dev_pred), lty = 'dashed')+
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.25))) +
  # scale_y_log10(labels = fancy_scientific_modified, expand = expansion(mult = c(0.1, 0.25))) +
  # geom_smooth(method = 'lm', lwd = 0.5, color = 'black',fill = 'black') +
  theme_markdown +
  theme(strip.text = element_markdown(),
        legend.position = 'top') +
  labs(
    x = 'Slump area (% of watershed)',
    y = 'SSC anomaly (mg/L)',
    fill = 'River'
  )

# FIGURE: Figure A7. SSC anomaly  vs. slump area, with universal fit & by-river with universal fit
universal_ssc_vs_slump_area_plot_combined <- (universal_ssc_vs_slump_area_plot +
    geom_richtext(data = universal_ssc_vs_slump_area_gt0.01_model_summary,
                  aes(x = -Inf, y = Inf, 
                      label = ifelse(p.value < 0.05, 
                                     ifelse(p.value < 0.001, 
                                            paste0('**R<sup>2</sup>: ', round(r.squared, 3), '; *p* < 0.001**'),
                                            paste0('**R<sup>2</sup>: ', round(r.squared, 3), '; *p*: ', round(p.value, 3), '**')),
                                     paste0('R<sup>2</sup>: ', round(r.squared, 3), '; *p*: ', round(p.value, 3))
                                     
                      )),
                  hjust = 0, vjust = 1, label.colour = NA, fill = NA, size = 3) + 
    theme(legend.position = 'none')) + 
  (universal_ssc_vs_slump_area_plot + # FIGURE: Figure A7b. SSC anomaly vs. slump area, by-river with universal fit
     facet_wrap(.~river_nm, ncol = 1) +
     theme(legend.position = 'none',
           axis.title.y = element_blank())) +
  # plot_layout(widths = c(1,0.75)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold')) 

ggsave(universal_ssc_vs_slump_area_plot_combined, filename = paste0(wd_figures, 'universal_ssc_vs_slump_area_plot_combined.png'),
       width = 5, height = 4.75)
ggsave(universal_ssc_vs_slump_area_plot_combined, filename = paste0(wd_figures, 'universal_ssc_vs_slump_area_plot_combined.pdf'),
       width = 5, height = 4.75, useDingbats = F)

# FIGURE: Figure 3c. SSC timeseries by river category (Control, Estuary, Affected)
# Make a rectangle
rectangle_dt <- data.table(x = NA, y = NA, xmin=-Inf, xmax=2019.5, ymin=-Inf, ymax = Inf)
SSC_timeseries_stdized_by_river_category_plot <- ggplot(river_landsat_pred_clean_annual_summary[
  # grepl(site_no_sel,site_no)
  SSC_mgL_stdize < 18
][
  # SSC_mgL < 600
], 
aes(x = year + yday(sample_dt)/365, y = SSC_mgL_stdize,
    color = reference_simple, fill = reference_simple
)) + 
  geom_rect(data = rectangle_dt, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax = ymax), fill = 'grey95', color = NA, inherit.aes = F) +
  geom_vline(xintercept = 2019.5, linetype = 'dashed') +
  stat_summary(aes(x = year), geom = 'line', fun = 'mean') + 
  stat_summary(geom = 'errorbar', aes(x = year), color = 'black', lwd = 0.3, width = 0.2) +
  stat_summary(geom = 'point', fun = 'mean', aes(x = year), pch = 21, color = 'black', size = 3, stroke = 0.3) +
  # geom_smooth(method = 'loess', span = 1, se = F) +
  scale_color_manual(values = c('Control rivers' = 'steelblue', 'Affected rivers' = 'goldenrod', 'Estuaries' = '#e9c979')) +
  scale_fill_manual(values = c('Control rivers' = 'steelblue', 'Affected rivers' = 'goldenrod', 'Estuaries' = '#e9c979')) +
  theme_markdown +
  facet_wrap(.~factor(reference_simple, levels = c('Control rivers', 'Affected rivers', 'Estuaries'))) +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.12, 0.77),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.title.x = element_blank()
  ) + 
  labs(
    x = '',
    y = 'SSC*',
    color = '',
    fill = ''
  )

ggsave(SSC_timeseries_stdized_by_river_category_plot, filename = paste0(wd_figures, region_name, '_SSC_timeseries_stdized_by_river_category_plot.png'),
       width = 6.5, height = 3)
ggsave(SSC_timeseries_stdized_by_river_category_plot, filename = paste0(wd_figures, region_name, '_SSC_timeseries_stdized_by_river_category_plot.pdf'),
       width = 6.5, height = 3, useDingbats = F)

# FIGURE: Figure 3d. SSC boxplot, pre- vs. post-event by river category (Control, Estuary, Affected)
SSC_boxplot_by_site_with_ref_plot <- ggplot(river_landsat_pred_clean_3[
  # grepl(site_no_sel,site_no)
][
  # SSC_mgL < 600
], 
aes(x = factor(reference_simple, 
               levels = c('Control rivers', 'Affected rivers','Estuaries'), 
               labels = c('Ctrl','Aff','Est')), 
    y = SSC_mgL,
    fill = factor(ifelse(year > 2019, 'post', 'pre'), levels = c('pre','post'))
)) + 
  geom_boxplot(linewidth = 0.3, outlier.colour = NA) +
  scale_y_continuous(limits = c(0,1300)) +
  scale_fill_manual(values = c('pre' = 'steelblue', 'post' = 'goldenrod')) +
  theme_markdown +
  theme(legend.position = c(0.38, 0.88),
        legend.title = element_blank(),
        legend.background = element_blank()) + 
  labs(
    x = '',
    y = 'SSC (mg/L)',
    color = ''
  )


# FIGURE: Figure X. SSC timeseries and boxplot by river category (Control, Estuary, Affected)
SSC_timeseries_stdized_by_river_category_plot_comb <- 
  (SSC_timeseries_stdized_by_river_category_plot  +
     scale_x_continuous(labels = abbrev_year)) +
  SSC_boxplot_by_site_with_ref_plot +
  plot_layout(widths = c(1,0.18)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))


ggsave(SSC_timeseries_stdized_by_river_category_plot_comb, filename = paste0(wd_figures, region_name, '_SSC_timeseries_stdized_by_river_category_comb_plot.png'),
       width = 7, height = 3.5)
ggsave(SSC_timeseries_stdized_by_river_category_plot_comb, filename = paste0(wd_figures, region_name, '_SSC_timeseries_stdized_by_river_category_comb_plot.pdf'),
       width = 7, height = 3.5, useDingbats = F)

# SUPPLEMENTAL FIGURE - Figure SX. Annual avg. SSC timeseries, by sampling site
# Plot annual timeseries for each river, site
annual_SSC_by_site_plot <- ggplot(river_landsat_pred_clean_3[
  # site_no %in% site_nos
  !grepl('control', site_no)
],
aes(x = year, y = SSC_mgL, group = paste0(site_no, ', ', reference), 
    color = reference)) + 
  geom_vline(xintercept = 2019.5, linetype = 'dashed') +
  stat_summary(geom = 'line', fun = mean) + 
  # stat_summary() + 
  scale_color_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +  
  facet_wrap(.~river_nm,
             scales = 'free_y',
             ncol = 2) +
  scale_x_continuous(labels = abbrev_year) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_markdown +
  labs(
    x = 'Year',
    y = 'SSC (mg/L)',
    color = 'Prediction model'
  )

ggsave(annual_SSC_by_site_plot, filename = paste0(wd_figures, region_name, '_SSC_by_site_annual_timeseries.png'),
       width = 6, height = 6)
ggsave(annual_SSC_by_site_plot, filename = paste0(wd_figures, region_name, '_SSC_by_site_annual_timeseries.pdf'),
       width = 6, height = 6, useDingbats = F)


#### 4E. IMPORT SLUMP BY WATERSHED (INDIVIDUAL) ####
slump_by_watershed_indiv <- rbind(
  fread(paste0(wd_imports, 'taymyr_estuary1_slumps_w_wshd.csv')),
  fread(paste0(wd_imports, 'taymyr_estuary2_slumps_w_wshd.csv')),
  use.names = T, fill = T
)
  
# Import individual slump area by year
slumps_all_year <- fread(paste0(wd_imports,'taymyr_slumps_validated_20250620.csv'))

slump_annual_sum <- slumps_all_year[,.(
  area_km2 = sum(area_km2, na.rm = T),
  N_segments = .N
), by = .(label2024, year)]
# ), by = .(label, year)]


# Join cartesian to make table with all slumps assigned to every watershed
slump_annual_sum <- slump_annual_sum[slump_by_watershed_indiv[,.(label2024, site_no)], on = .(label2024), allow.cartesian=TRUE][
  site_topo_metadata[,.(site_no, downstream_sequence, station_nm, river_nm)], 
  on = .(site_no)]

slump_annual_sum <- slump_annual_sum[,':='(erosion_by_year_km2 = c(0, diff(area_km2))),
                 by = .(label2024, site_no, station_nm, river_nm, downstream_sequence)]

slump_annual_sum<- slump_annual_sum[erosion_by_year_km2 < 0,':='(erosion_by_year_km2 = 0)]
slump_annual_sum <- slump_annual_sum[,':='(volume_loss_m3 = 0.765*erosion_by_year_km2*(1000^2)^1.05)]

slump_annual_sum <- slump_annual_sum[,':='(mass_loss_tons = volume_loss_m3 * 1.8)]

# Plot slump area by watershed
ggplot(slump_by_watershed_indiv[grepl('b0',site_no)], aes(y = area_km2)) +
  geom_histogram() +
  scale_y_log10() +
  facet_wrap(.~site_no, nrow = 1)

ggplot(slump_annual_sum[grepl('b0',site_no)], aes(y = volume_loss_m3)) +
  geom_histogram() +
  scale_y_log10(labels = fancy_scientific_modified) +
  facet_wrap(.~site_no, nrow = 1)

ggplot(slump_annual_sum[grepl('b0',site_no)], aes(y = mass_loss_tons)) +
  geom_histogram() +
  scale_y_log10(labels = fancy_scientific_modified) +
  facet_wrap(.~site_no, nrow = 1)

summary(slump_annual_sum)
# Conversion from tons/yr to mg/s
tons_yr_mg_s_conversion <- 72.338
km2_times_m_yr_m3_s_conversion <- 0.072
average_precip <- mean(climate_taymyr_annual[year > 2019,precip_mm_yr])
# Summarize slump erosion (tons) per year for each watershed
watershed_erosion_by_year <- merge(
  slump_annual_sum[,.(mass_loss_tons = sum(mass_loss_tons, na.rm = T)),
                                              by = .(site_no, station_nm, river_nm, downstream_sequence, year)],
  slumps_by_watershed[,.(watershed_area_km2 = mean(watershed_area_km2, na.rm = T)), by = .(site_no)],
  by = 'site_no'
)

ggplot(watershed_erosion_by_year, aes(x = year, y = mass_loss_tons/1e6, color = river_nm)) +
  geom_line(aes(group = downstream_sequence)) +
  facet_wrap(.~river_nm, scales = 'free_y')

watershed_erosion_by_year <- watershed_erosion_by_year[,':='(mass_loss_mg_s = mass_loss_tons*tons_yr_mg_s_conversion)]
watershed_erosion_by_year <- watershed_erosion_by_year[,':='(discharge_m3s = watershed_area_km2*average_precip*km2_times_m_yr_m3_s_conversion)]
watershed_erosion_by_year <- watershed_erosion_by_year[,':='(discharge_L_s = discharge_m3s*1000)]
watershed_erosion_by_year <- watershed_erosion_by_year[,':='(SSC_mgL = mass_loss_mg_s/discharge_L_s)]

ggplot(watershed_erosion_by_year[
  # downstream_sequence == 1
  ], aes(x = year, y = SSC_mgL , color = downstream_sequence)) +
  geom_line(aes(group = downstream_sequence)) +
  facet_wrap(.~river_nm, scales = 'free_y')


#### 4F. LANDSAT COLOR TIMESERIES ####
landsat_annual_color <- river_landsat_pred_clean_3[
  # SSC_mgL < 2500 & !grepl('est2', site_no)
][
  ,.(
    red = median(B3, na.rm = T),
    green = median(B2, na.rm = T),
    blue = median(B1, na.rm = T),
    nir = median(B4, na.rm = T)
  ),
  # by = .(site_no=ifelse(grepl('est', site_no), substr(site_no, 1, 7), substr(site_no, 1, 4)), year)
  by = .(river, river_nm, year)
]


# FIGURE: Figure Xa. RGB changes in river color, pre- vs. post-slump event
# (Not used)
SSC_color_timeseries_by_site_rgb_plot <- ggplot(landsat_annual_color[
  # !(year %in% c(2016:2019))
  !(as.character(river) %chin% c('E','G'))
], 
aes(x = year, y = river_nm,
    fill = rgb(red/2200,green/2200,blue/2200)
)) + 
  geom_tile(width = 1, color = 'black', linewidth = 0.25) +
  scale_fill_identity() +
  facet_col(.~ifelse(grepl('control',casefold(river)), 'Control', 'Affected'), scales = 'free_y', space = 'free') +
  # scale_color_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  theme_markdown +
  scale_y_discrete(limits = rev, expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  theme(legend.position = 'top') + 
  labs(
    x = 'Year',
    y = 'River reach',
    color = 'True color'
  )

# FIGURE: Figure X. NIR changes in river color, pre- vs. post-slump event
# (Not used)
SSC_color_timeseries_by_site_nir_plot <- ggplot(landsat_annual_color[
  !(as.character(river) %chin% c('E','G'))
], 
aes(x = year, y = river_nm,
    fill = rgb(nir/2200, red/2200,green/2200)
)) + 
  geom_tile(width = 1, color = 'black', linewidth = 0.25) +
  scale_fill_identity() +
  facet_col(.~ifelse(grepl('control',casefold(river)), 'Control', 'Affected'), scales = 'free_y', space = 'free') +
  # scale_color_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  theme_markdown +
  theme(legend.position = 'top') + 
  scale_y_discrete(limits = rev, expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  labs(
    x = 'Year',
    y = 'River reach',
    color = 'True color'
  )

# SUPPLEMENTAL FIGURE: Figure SX. Combined RGB and NIR changes in river color, pre- vs. post-slump event
# (Not used)
SSC_color_timeseries_by_site_plot <- SSC_color_timeseries_by_site_rgb_plot / SSC_color_timeseries_by_site_nir_plot +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(SSC_color_timeseries_by_site_plot, filename = paste0(wd_figures, region_name, '_SSC_color_timeseries_by_site_plot.png'),
       width = 6, height = 5.5)
ggsave(SSC_color_timeseries_by_site_plot, filename = paste0(wd_figures, region_name, '_SSC_color_timeseries_by_site_plot.pdf'),
       width = 6, height = 4.5, useDingbats = F)





#### 5. ------ CLIMATE ANALYSIS FOR TAYMYR PENINSULA ------ ####
#### 5A. CLIMATE DAY-OF-YEAR ANALYSIS ####
# Add date columns to climate data
climate_taymyr <- climate_taymyr[
  ,':='(yday = yday(date),
        year = year(date))
]

# Calculate 10-day running avg. maximum temperature
climate_taymyr <- climate_taymyr[
  ,':='(
    yday10_roll_temp = frollmean(temperature_2m_max-273.15, 10),
    yday10_roll_precip = frollsum(total_precipitation_sum, 10),
    snowmelt_runoff_m = c(NA, diff(snow_depth))
  )
]


top_temperature_days <- climate_taymyr[order(-temperature_2m_max)][1:50][,':='(temp_rank = rank(-temperature_2m_max))][year == 2020]

precip_early_august_2020 <- sum(climate_taymyr[year == 2020 & yday %in% c(215:217)][,total_precipitation_sum])*1000

# Add 3-day precipitation column
climate_taymyr <- climate_taymyr[,':='(yday3_roll_precip = frollsum(total_precipitation_sum, 3))]
climate_taymyr[year == 2020 & month(date) %in% c(6,7,8)]

# Calculate 1-day precipitation maximums
precip_1day_since_2000 <- round(climate_taymyr[year >= 2000 & month(date) %in% c(6,7,8)][order(-total_precipitation_sum),total_precipitation_sum]*1000,2)[]
# Number of 1-day periods with precip > 14 mm since 2000
length(which(precip_1day_since_2000 > 14))
length(which(precip_1day_since_2000 > 14))/(2024-2000)

# Calculate 3-day precipitation maximums
precip_3day_since_2000 <- round(climate_taymyr[year >= 2000 & month(date) %in% c(6,7,8)][order(-yday3_roll_precip),yday3_roll_precip]*1000,2)[]
# Number of 3-day periods with precip > 14 mm since 2000
length(which(precip_3day_since_2000 > 14))

length(which(precip_3day_since_2000 > 14))/(2024-2000)


precipitation_3day_distribution_plot <- ggplot(climate_taymyr[year >= 2000 & month(date) %in% c(6,7,8)], aes(y = yday3_roll_precip*1000)) +
  geom_histogram(aes(fill = '2000-2024')) +
  geom_histogram(data = climate_taymyr[year == 2020 & month(date) %in% c(6,7,8)], aes(fill = '2020')) +
  geom_hline(yintercept = 13.6) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_log10(labels = fancy_scientific_modified) +
  scale_fill_manual(values = c('2000-2024' = 'grey40', '2020' = 'red')) +
  theme_markdown +
  labs(
    x = '***Number of 3-day periods***',
    y = '***Precipitation*** (3-day total, mm)',
    fill = 'Period'
  ) +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.position = 'inside',
    legend.position.inside = c(0.6, 0.3)
  )

ggsave(precipitation_3day_distribution_plot, filename = paste0(wd_figures, 'precipitation_3day_distribution_plot.pdf'),
       width = 4, height = 5, useDingbats = F)
ggsave(precipitation_3day_distribution_plot, filename = paste0(wd_figures, 'precipitation_3day_distribution_plot.png'),
       width = 4, height = 5)

# FIGURE -- Figure 3a. Maximum temperature vs. day of year, showing the anamalous 2020 year in red
day_of_year_taymyr_plot <- ggplot(climate_taymyr, aes(x = yday, y = temperature_2m_max-273.15)) +
  geom_hline(yintercept = 0, color = 'grey90') +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_boot, fill = 'grey80') +
  stat_summary(geom = 'line', fun = 'mean') +
  geom_line(data = climate_taymyr[year == 2020], color = 'red', alpha = 0.1) +
  geom_line(data = climate_taymyr[year == 2020], 
            aes(y = yday10_roll_temp), color = 'red') +
  # geom_line(data = climate_taymyr[year == 2011], color = 'blue', alpha = 0.1) +
  # geom_line(data = climate_taymyr[year == 2011], 
  #              aes(y = yday10_roll_temp), color = 'blue') +
  theme_markdown +
  labs(
    x = '***Day of year***',
    y = '***Daily maximum temperature***(°C)'
  ) +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

#### 5B. 2020 HEATWAVE RECURRENCE INTERVAL ####
# Function for getting probability distribution function (PDF)
getPDF <- function(x, mean, sd){
  prob <- dnorm(x, 
                mean = mean, 
                sd = sd
  )
  return(prob)
}

# Annual climate averages
climate_taymyr_annual <- climate_taymyr[year < 2025][
  ,.(
    temperature_C = mean(temperature_2m_max-273.15, na.rm = T),
    precip_mm_yr = sum(total_precipitation_sum)
  ),
  by = .(site_no, year)
]

# STAT: Number of precipitation days of different categories
nrow(climate_taymyr[total_precipitation_sum > 0.01])/uniqueN(climate_taymyr[,year])
nrow(climate_taymyr[temperature_2m_max-273.15 > 0][total_precipitation_sum > 0.01])/uniqueN(climate_taymyr[,year])
nrow(climate_taymyr[month(date) %in% c(6,7,8,9)][total_precipitation_sum > 0.01])/uniqueN(climate_taymyr[,year])

precip_10cm <- climate_taymyr[total_precipitation_sum > 0.01]


precip_10cm_by_year <- precip_10cm[,.(N_precip_gt10 = .N), by = year]

precip_10cm_by_year_warm <- precip_10cm[temperature_2m_max-273.15 > 0][,.(N_precip_gt10 = .N), by = year]

ggplot(precip_10cm_by_year, aes(x = year, y = N_precip_gt10)) +
  geom_bar(stat = 'identity', aes(fill = 'Temp <= 0°C'), fill = 'grey50') +
  geom_bar(data = precip_10cm_by_year_warm, stat = 'identity', fill = 'steelblue', aes(fill = 'Temp > 0°C')) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = '',
    y = '***N precip events > 10 cm***'
  ) +
  theme_markdown


# Recurrence interval calculation for different scenarios
# 2020 annual average temperature (used to compare)
avg_2020_temp <- max(climate_taymyr_annual[,temperature_C])

# A) Long-term
climate_taymyr_annual_summary <- climate_taymyr_annual[
  ,.(temperature_C_avg = mean(temperature_C, na.rm = T),
     temperature_C_sd = sd(temperature_C, na.rm = T))
]

# A1) Statistic -- probability that annual temp in 2020 would be so high
probability_of_2020_temp_2020_included <- getPDF(avg_2020_temp,
                                                 climate_taymyr_annual_summary[,temperature_C_avg],
                                                 climate_taymyr_annual_summary[,temperature_C_sd])

recurrence_of_2020_temp_2020_included <- 1/probability_of_2020_temp_2020_included

# B) Long-term, no 2020
climate_taymyr_annual_summary_no2020 <- climate_taymyr_annual[
  year!=2020
][
  ,.(temperature_C_avg = mean(temperature_C, na.rm = T),
     temperature_C_sd = sd(temperature_C, na.rm = T))
]

# B1) Statistic -- probability that annual temp in 2020 would be so high, excluding 2020
probability_of_2020_temp <- getPDF(avg_2020_temp,
                                   climate_taymyr_annual_summary_no2020[,temperature_C_avg],
                                   climate_taymyr_annual_summary_no2020[,temperature_C_sd])
recurrence_of_2020_temp <- 1/probability_of_2020_temp

# C) Recent (post-1999), no 2020
climate_taymyr_annual_summary_2000s <- climate_taymyr_annual[
  year > 1999 & year != 2020
][
  ,.(temperature_C_avg = mean(temperature_C, na.rm = T),
     temperature_C_sd = sd(temperature_C, na.rm = T))
]

# C1) Statistic -- probability that annual temp in 2020 would be so high, only including the 2000s
probability_of_2020_temp_2000s <- getPDF(avg_2020_temp,
                                         climate_taymyr_annual_summary_2000s[,temperature_C_avg],
                                         climate_taymyr_annual_summary_2000s[,temperature_C_sd])
recurrence_of_2020_temp_2000s <- 1/probability_of_2020_temp_2000s

# D) Recent (post-2004), no 2020
climate_taymyr_annual_summary_post2004 <- climate_taymyr_annual[
  year > 2004 & year != 2020
][
  ,.(temperature_C_avg = mean(temperature_C, na.rm = T),
     temperature_C_sd = sd(temperature_C, na.rm = T))
]

# D1) Statistic -- probability that annual temp in 2020 would be so high, only including the 2000s
probability_of_2020_temp_post2004 <- getPDF(avg_2020_temp,
                                            climate_taymyr_annual_summary_post2004[,temperature_C_avg],
                                            climate_taymyr_annual_summary_post2004[,temperature_C_sd])
recurrence_of_2020_temp_post2004 <- 1/probability_of_2020_temp_post2004

heatwave_2020_probability_dt <- data.table(
  scenario_letter = c('A','B','C','D'),
  scenario = c('Long-term','Long-term, no 2020', 'Short-term (2000s)', 'Short-term (post-2004)'),
  probability = c(probability_of_2020_temp_2020_included,probability_of_2020_temp,
                  probability_of_2020_temp_2000s,probability_of_2020_temp_post2004),
  recurrence_yrs = c(recurrence_of_2020_temp_2020_included,recurrence_of_2020_temp,
                     recurrence_of_2020_temp_2000s, recurrence_of_2020_temp_post2004)
)

# Calculate probability and recurrence for every span of years
all_recurrence_years <- seq(1950,2020,1)
for(i in 1:length(all_recurrence_years)){
  year_post <- all_recurrence_years[i]
  climate_taymyr_annual_summary_ith <- climate_taymyr_annual[
    year >= year_post & year != 2020
  ][
    ,.(temperature_C_avg = mean(temperature_C, na.rm = T),
       temperature_C_sd = sd(temperature_C, na.rm = T))
  ]
  
  climate_taymyr_annual_summary_ith_w2020 <- climate_taymyr_annual[
    year >= year_post
  ][
    ,.(temperature_C_avg = mean(temperature_C, na.rm = T),
       temperature_C_sd = sd(temperature_C, na.rm = T))
  ]
  
  climate_taymyr_annual_summary_ith_w2020_20yr_span <- climate_taymyr_annual[
    year >= year_post & year < year_post + 20
  ][
    ,.(temperature_C_avg = mean(temperature_C, na.rm = T),
       temperature_C_sd = sd(temperature_C, na.rm = T))
  ]
  
  probability_of_2020_temp_ith <- getPDF(avg_2020_temp,
                                         climate_taymyr_annual_summary_ith[,temperature_C_avg],
                                         climate_taymyr_annual_summary_ith[,temperature_C_sd])
  recurrence_of_2020_temp_ith <- 1/probability_of_2020_temp_ith
  
  probability_of_2020_temp_ith_w2020 <- getPDF(avg_2020_temp,
                                               climate_taymyr_annual_summary_ith_w2020[,temperature_C_avg],
                                               climate_taymyr_annual_summary_ith_w2020[,temperature_C_sd])
  recurrence_of_2020_temp_ith_w2020 <- 1/probability_of_2020_temp_ith_w2020
  
  probability_of_2020_temp_ith_w2020_20yr_span <- getPDF(avg_2020_temp,
                                                         climate_taymyr_annual_summary_ith_w2020_20yr_span[,temperature_C_avg],
                                                         climate_taymyr_annual_summary_ith_w2020_20yr_span[,temperature_C_sd])
  recurrence_of_2020_temp_ith_w2020_20yr_span <- 1/probability_of_2020_temp_ith_w2020_20yr_span
  
  recurrence_scenarios_sel <- data.table(year = year_post,
                                         scenario = paste0('post-',year_post),
                                         probability = probability_of_2020_temp_ith,
                                         recurrence_yrs = recurrence_of_2020_temp_ith,
                                         probability_w2020 = probability_of_2020_temp_ith_w2020,
                                         recurrence_yrs_w2020 = recurrence_of_2020_temp_ith_w2020,
                                         probability_w2020_20yr_span = probability_of_2020_temp_ith_w2020_20yr_span,
                                         recurrence_yrs_w2020_20yr_span = recurrence_of_2020_temp_ith_w2020_20yr_span)
  
  if(i == 1){
    all_recurrence_scenarios <- recurrence_scenarios_sel
  }else{
    all_recurrence_scenarios <- rbind(all_recurrence_scenarios, recurrence_scenarios_sel, use.names = T)
  }
}

# SUPPLEMENTAL FIGURE -- Figure. SX. Recurrence interval calculation for different scenarios
# (Not used)
recurrence_interval_scenarios_plot <- ggplot(all_recurrence_scenarios, aes(x = year)) + 
  geom_line(aes(y = recurrence_yrs_w2020, color = 'incl. 2020')) +
  geom_line(aes(y = recurrence_yrs, color = 'excl. 2020')) +
  theme_markdown +
  scale_color_manual(values = c('incl. 2020' = 'orange', 'excl. 2020' = 'black', '20-yr span' = 'blue')) +
  scale_y_log10(labels = fancy_scientific_modified) +
  labs(
    x = '***Year of analysis start***', 
    y = '***Recurrence interval*** (yrs)',
    color = 'Scenario'
  ) +
  theme(
    legend.position = c(0.22, 0.8),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

ggsave(recurrence_interval_scenarios_plot, filename = paste0(wd_figures, region_name, '_heatwave_recurrence_interval_scenarios.png'),
       width = 4.5, height = 2.75)
ggsave(recurrence_interval_scenarios_plot, filename = paste0(wd_figures, region_name, '_heatwave_recurrence_interval_scenarios.pdf'),
       width = 4.5, height = 2.75, useDingbats = F)

recurrence_interval_scenarios_plot_w20yr_span <- recurrence_interval_scenarios_plot + 
  geom_line(aes(y = recurrence_yrs_w2020_20yr_span, color = '20-yr span')) +
  theme(
    legend.position = c(0.75, 0.73)
  )

ggsave(recurrence_interval_scenarios_plot_w20yr_span, filename = paste0(wd_figures, region_name, '_heatwave_recurrence_interval_scenarios_w20yr_span.png'),
       width = 4.5, height = 2.75)
ggsave(recurrence_interval_scenarios_plot_w20yr_span, filename = paste0(wd_figures, region_name, '_heatwave_recurrence_interval_scenarios_w20yr_span.pdf'),
       width = 4.5, height = 2.75, useDingbats = F)

#### 5C. HEATWAVE LIKELIHOOD PROBABILITY COMPARISON ####
# Create PDFs for different scenarios
temp_sequence <- seq(-40,20,0.1)
# A) Full record
climate_taymyr_annual_pdf <- data.table(
  temperature_C = temp_sequence,
  prob = sapply(temp_sequence, getPDF, 
                mean = climate_taymyr_annual_summary[,temperature_C_avg],
                sd = climate_taymyr_annual_summary[,temperature_C_sd])
)[prob > 1e-3]

# Full record, no 2020
climate_taymyr_annual_no2020_pdf <- data.table(
  temperature_C = temp_sequence,
  prob = sapply(temp_sequence, getPDF, 
                mean = climate_taymyr_annual_summary_no2020[,temperature_C_avg],
                sd = climate_taymyr_annual_summary_no2020[,temperature_C_sd])
)[prob > 1e-3]

# post-2000, no 2020
climate_taymyr_annual_2000s_pdf <- data.table(
  temperature_C = temp_sequence,
  prob = sapply(temp_sequence, getPDF, 
                mean = climate_taymyr_annual_summary_2000s[,temperature_C_avg],
                sd = climate_taymyr_annual_summary_2000s[,temperature_C_sd])
)[prob > 1e-3]

# Calculate the increase in likelihood post-2000 from pre-2000
likelihood_increase_2000s <- probability_of_2020_temp_2000s/probability_of_2020_temp

# FIGURE -- Figure 3b. Average annual temperature, calculated from maximum daily temperature
annual_temperature_timeseries_taymyr <- ggplot(climate_taymyr_annual, 
                                               aes(x = year, y = temperature_C)) +
  geom_line() + 
  geom_point() +
  theme_markdown +
  scale_y_continuous(limits = c(-18,-4)) +
  labs(
    x = '',
    # y = '***Daily maximum temperature*** (°C)',
    y = '***Max. temp.*** (°C)',
  ) +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

# FIGURE -- Figure 3c. Histogram of annual temperatures from 1950–2024, w/ scenario PDFs
taymyr_temp_distribution <- ggplot(climate_taymyr_annual[year != 2020], aes(x = temperature_C)) +
  geom_histogram(data = climate_taymyr_annual[year != 2020 & year > 1999], bins = 20, aes(y = after_stat(density)), alpha = 0.2, fill = 'red') +
  geom_histogram(bins = 20, aes(y = after_stat(density)), alpha = 0.2, fill = 'blue') +
  geom_line(data = climate_taymyr_annual_2000s_pdf, aes(y = prob), alpha = 0.7, color = 'red') +
  geom_line(data = climate_taymyr_annual_no2020_pdf, aes(y = prob), alpha = 0.7, color = 'blue') +
  # geom_area(data = climate_taymyr_annual_pdf, aes(y = prob), alpha =0.2) +
  geom_vline(xintercept = climate_taymyr_annual[year == 2020,temperature_C], linetype = 'dashed') +
  theme_markdown +
  scale_x_continuous(limits = c(-18,-4)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    # x = '***Daily maximum temperature*** (°C)',
    x = '***Avg. Tmax.*** (°C)',
    y = '***Density***'
  ) +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  ) + 
  coord_flip()
# rotate()


# Combine temperature and distribution PDFs from pre- and post-2000
taymyr_temperature_plot <- 
  (annual_temperature_timeseries_taymyr + labs(y = '***Avg. ann. Tmax.*** (°C)')) + 
  (taymyr_temp_distribution + labs(x = '***Avg. ann. Tmax.*** (°C)')) +
  plot_layout(widths = c(0.7, 0.2)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

# FIGURE -- Figure 3. Climate figure showing seasonal temp, long-term annual temp, and histograms
taymyr_temp_combined_plot <- 
  (day_of_year_taymyr_plot 
   + geom_vline(aes(xintercept = 217), lty = 'dashed')
   + annotate(geom = 'text', x = 215, y = -33, label = 'Modal SSC changepoint\nAug. 4th, 2020', hjust = 1)
   + labs(y = '***Tmax*** (°C)')) / 
  taymyr_temperature_plot +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(taymyr_temp_combined_plot, filename = paste0(wd_figures, region_name, '_temp_combined_plot.png'),
       width = 6, height = 5)
ggsave(taymyr_temp_combined_plot, filename = paste0(wd_figures, region_name, '_temp_combined_plot.pdf'),
       width = 6, height = 5, useDingbats = F)

# SUPPLEMENTAL FIGURE -- Figure A12. Day of estimated snow on and off
day_of_year_snow_taymyr_plot <- ggplot(climate_taymyr, aes(x = yday, y = snow_depth)) +
  stat_summary(geom = 'ribbon', fun.data = mean_cl_boot, fill = 'grey80') +
  stat_summary(geom = 'line', fun = 'mean') +
  geom_line(data = climate_taymyr[year == 2020], color = 'red', alpha = 0.5) +
  # geom_line(data = climate_taymyr[year == 2011], color = 'blue', alpha = 0.5) +
  theme_markdown +
  labs(
    x = '***Day of year***',
    y = '***Snow depth*** (m)'
  ) +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  )

ggsave(day_of_year_snow_taymyr_plot, filename = paste0(wd_figures, region_name, '_day_of_year_snow.png'),
       width = 4.5, height = 2.75)
ggsave(day_of_year_snow_taymyr_plot, filename = paste0(wd_figures, region_name, '_day_of_year_snow.pdf'),
       width = 4.5, height = 2.75, useDingbats = F)


#### 6. ------ SENTINEL-2 ANALYSIS------  ####
# Import Sentinel-2 data from slumps
river_s2_import <- rbindlist(
  lapply(paste0(wd_imports, 
                c('sentinel_2_taymyr_peninsula_rivers_2016_2026_20scale.csv')
  ),
  fread
  ), fill = T, use.names = T)[
    ,':='(.geo = NULL)]


river_s2_import <- merge(river_s2_import,
                         site_topo_metadata[,.(site_no, river, river_nm, station_nm)],
                         by = 'site_no')

river_s2_import <- na.omit(river_s2_import[,
                     ':='(
                       site_no = site_no,
                       # site_no = name,
                       station_nm = station_nm,
                       # Rename columns for simplicity
                       B1 = B1_median,
                       B2 = B2_median,
                       B3 = B3_median,
                       B4 = B4_median,
                       B5 = B5_median,
                       B6 = B6_median,
                       B7 = B7_median,
                       B8 = B8_median,
                       B8A = B8A_median,
                       B9 = B9_median,
                       B11 = B11_median,
                       B12 = B12_median,
                       num_pix = B2_count,
                       sample_dt = ymd(date)
                     )]
        , cols = c('B1','B2','B3','B4','B5','B7'))[
          B1 > 0 & B2 > 0 & B3 > 0 & B4 > 0 & B8 > 0 & B11 > 0 &
            B1 < 5000 & B2 < 5000 & B3 < 5000 & B4 < 5000 & B6 < 4000][
              ,':='( 
                year = year(sample_dt),
                Latitude = lat,
                Longitude = lon
                # station_nm = paste0(0,station_no),
                # site_no = paste0(0,site_no)
              )][ 
                # select only columns of interest
                ,.(site_no, station_nm, year,
                   river, river_nm,
                   Latitude,Longitude,sample_dt, num_pix, 
                   cloud_cover, product_id,
                   B1,B2,B3,B4,B5,B6,B7,B8,B8A,B9,B11,B12
                )][site_no != "0"][
                  ,':='(month = month(sample_dt),
                        decade = ifelse(year < 1990, 1990,
                                        ifelse(year > 2019, 2020,
                                               year - year%%5)))]




# Remove too-small rivers
river_s2_import <- river_s2_import[!(river %in% c('E','F','G'))]


#### 6A. SENTINEL-2 SSC ALGORITHM FROM LANDSAT TRAINING DATA ####
# Merge with Landsat data to train a model
river_s2_training_landsat <- merge(
  river_s2_import,
  river_landsat_pred_clean_3[,.(site_no, sample_dt, SSC_mgL)],
  by = c('site_no', 'sample_dt')
)

s2_landsat_model <- lm(log10(SSC_mgL) ~ 
                         B2 + B3 + B4 + B7 + B8 + B11,
                       data = river_s2_training_landsat)

summary(s2_landsat_model)

river_s2_training_landsat$SSC_mgL_landsat_pred <- 10^predict(s2_landsat_model, newdata = river_s2_training_landsat)


# Model from Landsat data
model <- s2_landsat_model
saveRDS(model,file=paste0(wd_imports, 's2_landsat_model_taymyr.rds'))
river_s2_import$SSC_mgL <- 10^predict(model, newdata = river_s2_import)


river_s2_import <- river_s2_import[
  SSC_mgL > 0.5 & SSC_mgL < 20000 &
    !(SSC_mgL > 1000 & (B2 + B3 + B4) < 700)
]

river_s2_import <- river_s2_import[,':='(
  yday7 = yday(sample_dt) - yday(sample_dt)%%7,
  ten_day = yday(sample_dt) - yday(sample_dt)%%10,
  period = ifelse(year(sample_dt) < 2020, 'pre-Slump', 'post-Slump'),
  reach = ifelse(grepl('control', site_no), 'Reference', 'Affected'),
  reference = factor(ifelse(grepl('control',site_no), 'Reference',
                                        ifelse(year > 2019, 'Affected, Post-slump', 'Affected, Pre-slump')),
                                 levels = c('Reference', 'Affected, Pre-slump', 'Affected, Post-slump')),
  reference_day = factor(ifelse(grepl('control',site_no), 'Reference',
                                        ifelse(sample_dt > ymd('2020-08-04'), 'Affected, Post-slump', 'Affected, Pre-slump')),
                                 levels = c('Reference', 'Affected, Pre-slump', 'Affected, Post-slump')),
  cluster = NA,
  reference_simple = ifelse(grepl('control', casefold(river)), 'Control rivers', 
                            ifelse(grepl('Estuary', river),'Estuaries', 'Affected rivers'))
)
]

#### 6B. LANDSAT AND SENTINEL-2 DATA COMPARISON ####
## FIGURE A2 a. Landsat vs. Sentinel-2 model estimates ##
s2_landsat_regression_model <- ggplot(river_s2_training_landsat[
  ,.(SSC_mgL = mean(SSC_mgL, na.rm = T),
     SSC_mgL_landsat_pred = mean(SSC_mgL_landsat_pred, na.rm = T)),
  by = .(station_nm, sample_dt)
], 
aes(x = SSC_mgL, y = SSC_mgL_landsat_pred)) +
  # geom_hex() +
  geom_point(aes(color = ifelse(sample_dt<ymd('2020-08-04'), 'Pre-slump event', 'Post-slump event')), size = 3,alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_log10(limits = c(3,3000), labels = fancy_scientific_modified, breaks = breaks, minor_breaks = NULL) +
  scale_y_log10(limits = c(3,3000), labels = fancy_scientific_modified, breaks = breaks, minor_breaks = NULL) +
  annotation_logticks(sides = 'trbl') +
  coord_cartesian(clip = 'off') +
  # scale_fill_gradientn(colors = rev(c('black','red','pink','white')), trans = 'log10') +
  scale_fill_gradientn(colors = rev(c('black','black','black','grey90',NA)), trans = 'log10') +
  scale_color_manual(values = c('Pre-slump event' = 'steelblue', 'Post-slump event' = 'goldenrod')) +
  theme_markdown +
  labs(
    x = '***Landsat-derived SSC*** (mg/L)',
    y = '***S2-derived SSC*** (mg/L), based on Landsat training data',
    color = ''
  ) +
  theme(
    axis.title.y = element_markdown(),
    axis.title.x = element_markdown(),
    legend.position.inside = 'inside',
    legend.position = c(0.75, 0.15)
  )

ggsave(s2_landsat_regression_model, filename = paste0(wd_figures, 's2_landsat_regression_model.pdf'),
       useDingbats = F, width = 4, height = 4)


## FIGURE A2 b. Landsat vs. Sentinel-2 timeseries examples, each river ##
s2_vs_landsat_timeseries_comparison_plot <- ggplot() + 
  geom_point(data = river_s2_import[
    station_nm %in% c('B 4', 'C 1', 'A 2', 'D 1') & 
      year == 2020 &
      ten_day < 260],
    aes(x = sample_dt, y = SSC_mgL, fill = 'Sentinel-2 estimated'), pch = 21, size = 3) +
  geom_line(data = river_s2_import[
    station_nm %in% c('B 4', 'C 1', 'A 2', 'D 1') & 
      year == 2020 &
      ten_day < 260],
    aes(x = sample_dt, y = SSC_mgL, group = station_nm), color = 'grey40', linetype = 'dashed') +
  geom_point(data = river_landsat_pred_clean_3[
    station_nm %in% c('B 4', 'C 1', 'A 2', 'D 1') & 
      year == 2020 &
      ten_day < 260],
    aes(x = sample_dt, y = SSC_mgL, fill = 'Landsat estimated'), pch = 21, size = 3) +
  scale_fill_manual(values = c('Landsat estimated' = 'grey10', 'Sentinel-2 estimated'='grey60')) +
  facet_wrap(.~river_nm) +
  theme_markdown +
  labs(
    x = '***Day of year, 2020***',
    y = '***SSC*** (mg/L)',
    fill = '',
    color = ''
  ) +
  theme(
    axis.title.y = element_markdown(),
    axis.title.x = element_markdown(),
    legend.position = 'bottom'
  )

## FIGURE A2. Landsat vs. Sentinel-2 validation ##
s2_vs_landsat_model_and_timeseries_combined_plot <- 
  s2_landsat_regression_model / s2_vs_landsat_timeseries_comparison_plot +
  plot_layout(heights = c(1,0.5)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(s2_vs_landsat_model_and_timeseries_combined_plot, filename = paste0(wd_figures, 's2_vs_landsat_model_and_timeseries_combined_plot.png'),
       width = 5, height = 8)
ggsave(s2_vs_landsat_model_and_timeseries_combined_plot, filename = paste0(wd_figures, 's2_vs_landsat_model_and_timeseries_combined_plot.pdf'),
       width = 5, height = 8, useDingbats = F)

#### 6C. SENTINEL-2 SLUMP TIMING: SSC CHANGEPOINT ANALYSIS ####
# Calculate SSC changepoint for 2020, BY STATION
for(i in 1:length(unique(river_s2_import[,station_nm]))){
  station_nm_sel <- unique(river_s2_import[,station_nm])[i] 
  print(station_nm_sel)
  dt <- river_s2_import[station_nm == station_nm_sel
                                   & year == 2020
                        & ten_day > 170 & ten_day < 260
                        ][
                                     order(sample_dt)]
  changepoint_sel <- cpt.mean(data = dt[,SSC_mgL], method = "AMOC")
  changepoint_dt <- dt[changepoint_sel@cpts[1]]
  # print(dt)
  # print(changepoint_dt)
  # plot(changepoint_sel)
  if(i == 1){
    changepoint_station_summary <- changepoint_dt
  }else{
    changepoint_station_summary <- rbind(changepoint_station_summary, changepoint_dt, use.names = T, fill = T)
  }
}



changepoint_station_summary_stats <- changepoint_station_summary[
  ,.(N_dates = .N),
  by = .(reach, sample_dt)]

total_n_s2_dates_2020 <- river_s2_import[year == 2020
                                         & ten_day > 170 & ten_day < 260,
                                         .(N_images = .N),
                                         by = .(yday = yday(sample_dt), sample_dt)
][order(sample_dt)]

s2_SSC_changepoint_yday_plot <- ggplot(river_s2_import[
  year == 2020
  & yday(sample_dt) > 170 & yday(sample_dt) < 260], aes(x = yday(sample_dt))) +
  # geom_histogram(aes(y = after_stat(density))) +
  geom_bar(data = changepoint_station_summary, aes(y = after_stat(count)), fill = 'red') +
  # geom_density(data = changepoint_station_summary, aes(y = after_stat(density)), color = 'red', fill = NA) +
  # geom_vline(data = changepoint_station_summary, aes(xintercept = sample_dt), color = 'red', alpha = 0.25)
  theme_markdown +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(axis.title.y = element_markdown(),
        axis.title.x = element_blank()) +
  labs(
    x = '',
    y = 'N stations with<br>SSC changepoint'
  ) 

ggsave(s2_SSC_changepoint_yday_plot, filename = paste0(wd_figures, 's2_SSC_changepoint_yday_plot.pdf'),
       width = 4, height = 1.5)

s2_SSC_changepoint_date_plot <- ggplot(river_s2_import[year == 2020], aes(x = sample_dt)) +
  # geom_histogram(aes(y = after_stat(density))) +
  geom_bar(data = changepoint_station_summary, aes(y = after_stat(count)), fill = 'red') +
  # geom_bar(data = changepoint_station_summary, aes(y = after_stat(count), fill = ifelse(reference_simple == 'Control rivers', 'Control','Affected'))) +
  geom_bar(data = changepoint_station_summary[order(-reference_simple)], aes(y = after_stat(count), 
                                                   fill = factor(reference_simple, levels = c('Control rivers','Estuaries','Affected rivers'))), 
                                                   color = 'black', lwd = 0.1) +
  # geom_density(data = changepoint_station_summary, aes(y = after_stat(density)), color = 'red', fill = NA) +
  # geom_vline(data = changepoint_station_summary, aes(xintercept = sample_dt), color = 'red', alpha = 0.25)
  theme_markdown +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_x_date(limits = c(ymd('2020-06-15', '2020-09-15'))) +
  scale_fill_manual(values = c('Control rivers' = 'grey','Estuaries' = '#e9c979','Affected rivers' = 'goldenrod')) +
  theme(axis.title.y = element_markdown(),
        axis.title.x = element_blank()) +
  labs(
    x = '',
    y = 'N stations with<br>SSC changepoint',
    fill = ''
  ) 


s2_2020_image_date_plot <- ggplot(river_s2_import[year == 2020][,.(N_stations = .N), by = .(sample_dt, station_nm)], aes(x = sample_dt)) +
  geom_bar(aes(y = after_stat(count))) +
  # geom_histogram(data = changepoint_station_summary, aes(y = after_stat(density)), fill = 'red')
  # geom_vline(data = changepoint_station_summary, aes(xintercept = sample_dt), color = 'red', alpha = 0.25)
  theme_markdown +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_x_date(limits = c(ymd('2020-06-15', '2020-09-15'))) +
  labs(
    x = '',
    y = 'N stations with<br>Sentinel-2 image',
    fill = ''
  ) +
  theme(axis.title.y = element_markdown(),
        axis.title.x = element_blank()) 

s2_SSC_changepoint_date_combined_plot <- s2_SSC_changepoint_date_plot + 
  theme(legend.position = 'top', 
        legend.key.height = unit(1,'mm')) +
  s2_2020_image_date_plot +
  plot_layout(heights = c(0.4, 0.4)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(s2_SSC_changepoint_date_combined_plot, filename = paste0(wd_figures, 's2_SSC_changepoint_date_combined_plot.pdf'),
       useDingbats = F, width = 4.5, height = 3)
ggsave(s2_SSC_changepoint_date_combined_plot, filename = paste0(wd_figures, 's2_SSC_changepoint_date_combined_plot.png'),
       width = 4.5, height = 3)
  
# Calculate SSC changepoint for 2020, BY RIVER
for(i in 1:length(unique(river_s2_import[,river]))){
  river_sel <- unique(river_s2_import[,river])[i] 
  print(river_sel)
  dt <- river_s2_import[river == river_sel
                                   & year == 2020
                        & ten_day > 170 & ten_day < 260
                        ][
                                     order(sample_dt)]
  changepoint_sel <- cpt.mean(dt[,SSC_mgL], method = "AMOC")
  changepoint_dt <- dt[changepoint_sel@cpts[1]]
  # print(dt)
  # print(changepoint_dt)
  plot(changepoint_sel)
  if(i == 1){
    changepoint_summary <- changepoint_dt
  }else{
    changepoint_summary <- rbind(changepoint_summary, changepoint_dt, use.names = T, fill = T)
  }
}

# FIGURE: Figure 2 insets. Sentinel-2 2020 SSC timeseries, pre- and post-changepoint colored
landsat_ssc_colnames <- colnames(river_landsat_pred_clean_3)
SSC_timeseries_2020_by_site_plot <- ggplot(
  rbind(river_landsat_pred_clean_3,
        river_s2_import[,..landsat_ssc_colnames],
        use.names = T, fill = T)
        [
  !grepl('control', casefold(site_no))
][
  year %in% c(2020)
  & !grepl('Estuary', river)
  & ten_day > 170 & ten_day < 260
], 
aes(x = yday(sample_dt), y = SSC_mgL,
    fill = reference_day
)) + 
  geom_rect(aes(xmin = 218, xmax = Inf, ymin = -Inf, ymax = Inf), fill = 'grey90', color = NA) +
  geom_vline(xintercept = 218, lty = 'dashed', lwd = 0.75) +
  stat_summary(geom = 'line', color = 'black', lty = 'dashed', alpha = 0.4) +
  stat_summary(geom = 'errorbar', width = 2, color = 'grey20', linewidth = 0.25) +
  stat_summary(geom = 'point', color = 'black', pch = 21, size = 3) +
  facet_wrap(.~reorder(river_nm, river),
             scales = 'free_y',
             ncol = 2) +
  scale_color_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  scale_fill_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  theme_markdown +
  scale_x_continuous() +
  labs(
    x = 'Day of year',
    y = 'SSC (mg/L)',
    color = '',
    fill = ''
  )

ggsave(SSC_timeseries_2020_by_site_plot, filename = paste0(wd_figures, region_name, '_SSC_2020_timeseries_by_site_plot.png'),
       width = 5, height = 3)
ggsave(SSC_timeseries_2020_by_site_plot, filename = paste0(wd_figures, region_name, '_SSC_2020_timeseries_by_site_plot.pdf'),
       width = 5, height = 3, useDingbats = F)

SSC_timeseries_2020_by_estuary_plot <- ggplot(
  rbind(river_landsat_pred_clean_3,
        river_s2_import[,..landsat_ssc_colnames],
        use.names = T, fill = T)
        [
  !grepl('control', casefold(site_no))
][
  year %in% c(2020)
  & grepl('Estuary', river)
  & ten_day > 170 & ten_day < 260
  & SSC_mgL < 1000
], 
aes(x = yday(sample_dt), y = SSC_mgL,
    fill = reference_day
)) + 
  geom_rect(aes(xmin = 218, xmax = Inf, ymin = -Inf, ymax = Inf), fill = 'grey90', color = NA) +
  geom_vline(xintercept = 218, lty = 'dashed', lwd = 0.25) +
  stat_summary(geom = 'line', color = 'black', lty = 'dashed', alpha = 0.4) +
  stat_summary(geom = 'errorbar', width = 2, color = 'grey20', linewidth = 0.25) +
  stat_summary(geom = 'point', color = 'black', pch = 21, size = 3) +
  facet_wrap(.~reorder(river_nm, river),
             scales = 'free_y',
             ncol = 2) +
  scale_color_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  scale_fill_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  theme_markdown +
  scale_x_continuous() +
  labs(
    x = 'Day of year',
    y = 'SSC (mg/L)',
    color = '',
    fill = ''
  )

ggsave(SSC_timeseries_2020_by_estuary_plot, filename = paste0(wd_figures, region_name, '_SSC_2020_timeseries_by_estuary_plot.png'),
       width = 5, height = 1.75)
ggsave(SSC_timeseries_2020_by_estuary_plot, filename = paste0(wd_figures, region_name, '_SSC_2020_timeseries_by_estuary_plot.pdf'),
       width = 5, height = 1.75, useDingbats = F)

#### 6D. SENTINEL-2 COLOR ANALYSIS ####
# FIGURE: Figure X. Changes in river color, pre- vs. post-slump event
# (NOT USED)
s2_annual_color <- river_s2_import[
  # SSC_mgL < 2500 & !grepl('est2', site_no)
][
  ,.(
    red = median(B4, na.rm = T),
    green = median(B3, na.rm = T),
    blue = median(B2, na.rm = T),
    nir = median(B8, na.rm = T)
  ),
  # by = .(site_no=ifelse(grepl('est', site_no), substr(site_no, 1, 7), substr(site_no, 1, 4)), year)
  by = .(river, river_nm, year)
]


# FIGURE: Figure X. Changes in river color, pre- vs. post-slump event
# (Not used)
SSC_S2_color_timeseries_by_site_rgb_plot <- ggplot(s2_annual_color[
  # !(year %in% c(2016:2019))
  !(as.character(river) %chin% c('E','F','G'))
], 
aes(x = year, y = river_nm,
    fill = rgb((red-400)/1200,(green-600)/1100,(blue-800)/1200)
)) + 
  geom_tile(width = 1, color = 'black', linewidth = 0.25) +
  scale_fill_identity() +
  facet_col(.~ifelse(grepl('control',casefold(river)), 'Control', 'Affected'), scales = 'free_y', space = 'free') +
  # scale_color_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  theme_markdown +
  scale_y_discrete(limits = rev, expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  theme(legend.position = 'top') + 
  labs(
    x = 'Year',
    y = 'River reach',
    color = 'True color'
  )

SSC_S2_color_timeseries_by_site_nir_plot <- ggplot(s2_annual_color[
  !(as.character(river) %chin% c('E','F','G'))
], 
aes(x = year, y = river_nm,
    fill = rgb((nir-200)/2200, (red-200)/2200,(green-200)/2200)
)) + 
  geom_tile(width = 1, color = 'black', linewidth = 0.25) +
  scale_fill_identity() +
  facet_col(.~ifelse(grepl('control',casefold(river)), 'Control', 'Affected'), scales = 'free_y', space = 'free') +
  # scale_color_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  theme_markdown +
  theme(legend.position = 'top') + 
  scale_y_discrete(limits = rev, expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  labs(
    x = 'Year',
    y = 'River reach',
    color = 'True color'
  )

SSC_S2_color_timeseries_by_site_plot <- SSC_S2_color_timeseries_by_site_rgb_plot / SSC_S2_color_timeseries_by_site_nir_plot +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(SSC_S2_color_timeseries_by_site_plot, filename = paste0(wd_figures, region_name, '_SSC_color_timeseries_by_site_plot.png'),
       width = 6, height = 5.5)
ggsave(SSC_S2_color_timeseries_by_site_plot, filename = paste0(wd_figures, region_name, '_SSC_color_timeseries_by_site_plot.pdf'),
       width = 6, height = 4.5, useDingbats = F)



# Add yday columns to Sentinel-2 data
# TO DO: STILL NEED THESE COLUMNS??
river_s2_import <- river_s2_import[
  ,':='(yday = yday(sample_dt),
        yday7 = yday(sample_dt) - yday(sample_dt)%%7 + 3.5,
        yday10 = yday(sample_dt) - yday(sample_dt)%%10 + 5,
        yday15 = yday(sample_dt) - yday(sample_dt)%%15 + 7.5
  )
]



#### 7. ------ FINAL STATION LANDSAT AND SENTINEL-2 METADATA SUMMARY ------ ####
## STATISTIC: NUMBER OF LANDSAT IMAGES
# Affected
n_landsat_affected_dates <- river_landsat_pred_clean_3[
  !grepl('_ref',site_no)
  & !grepl('control', site_no)
][,uniqueN(sample_dt)]

# Reference
n_landsat_ref_dates <- river_landsat_pred_clean_3[
  grepl('control',site_no)
  | grepl('ref', site_no)
][,uniqueN(sample_dt)]

## STATISTIC: NUMBER OF SENTINEL IMAGES BY STATION
landsat_images_by_station <- dcast.data.table(
  river_nm + station_nm ~ period,
  value.var = 'N_images',
  data = river_landsat_pred_clean_3[
    ten_day > 170 & ten_day < 260,
    .(N_images = .N),
    by = .(river_nm, station_nm, period = ifelse(grepl('Reference|Pre', reference), 'Landsat, Reference', 'Landsat, Post-slump'))
  ])

## STATISTIC: NUMBER OF SENTINEL IMAGES BY STATION
s2_images_by_station <- dcast.data.table(
  river_nm + station_nm ~ period,
  value.var = 'N_images',
  data = river_s2_import[
                ten_day > 170 & ten_day < 260,
                .(N_images = .N),
                by = .(river_nm, station_nm, period = ifelse(grepl('Pre', period), 'Sentinel-2, Reference', paste0('Sentinel-2, ', period)))
])

s2_images_by_month_year <- dcast.data.table(
  river_nm + station_nm ~ year + month,
  value.var = 'N_images',
  data = river_s2_import[
                ten_day > 170 & ten_day < 260,
                .(N_images = .N),
                by = .(river_nm, station_nm, month, year)
])

s2_images_by_month_year_long <- river_s2_import[
  ten_day > 170 & ten_day < 260,
  .(N_images = .N),
  by = .(river_nm, station_nm, month, year)]

ggplot(s2_images_by_month_year_long, aes(y = paste0(river_nm, ' ', station_nm), x = month)) +
         geom_tile(aes(fill = N_images)) +
  scale_fill_gradientn(limits = c(0,20), oob = squish, colors = c('black','steelblue', 'yellow')) +
  facet_wrap(.~year, ncol = 9) +
  theme_markdown

s2_images_by_river_by_month_year_long <- river_s2_import[
  ten_day > 170 & ten_day < 260,
  .(N_images = .N),
  by = .(river_nm, month, year)]

ggplot(s2_images_by_river_by_month_year_long, aes(y = river_nm, x = month)) +
         geom_tile(aes(fill = N_images)) +
  scale_fill_gradientn(limits = c(0,50), oob = squish, colors = c('black','steelblue', 'yellow')) +
  facet_wrap(.~year, ncol = 9) +
  theme_markdown
       
# Build table of Landsat and Sentinel-2 number-of-images metadata
s2_and_landsat_images_by_station = merge(
  s2_images_by_station,
  landsat_images_by_station,
  by = c('river_nm','station_nm')
)
## TABLE 1 -- Summary of station data:
# Name, watershed area, Landsat and Sentinel-2 dates (pre- and post-), changepoint date
station_table1_summary <- merge(
  merge(
    changepoint_station_summary[,.(
      station_nm, River = river_nm, Latitude, Longitude, `Changepoint date` = sample_dt)],
    sample_station_watershed_summary[,.(
      station_nm, `Watershed area (km2)` = round(watershed_area_km2,0), 
      `Slump area (km2)` = slump_area_km2, `% Slump area` = round(slump_area_km2/watershed_area_km2*100,3))],
    by = 'station_nm'
  ),
  s2_and_landsat_images_by_station[,.(station_nm, `Sentinel-2, post-Slump`, `Sentinel-2, pre-Slump`, `Landsat, Post-slump`, `Landsat, Reference`)],
  by = 'station_nm'
)[order(`Watershed area (km2)`)][order(River)]

# fwrite(station_table1_summary, file = paste0(wd_imports, 'Table_1_station_ssc_metadata_summary.csv'))

# Select stations for number-of-images analysis
example_image_count_stations <- c('A 2', 'B 1', 'C 1', 'D 1', 'Estuary 1 5', 'Estuary 2 4', 'Control 1')
# Set up blank lists container for plots
example_landsat_image_count_plots <- vector(mode = 'list', length = length(example_image_count_stations))
example_S2_image_count_plots <- vector(mode = 'list', length = length(example_image_count_stations))
for(i in 1:length(example_image_count_stations)){
  ## SUPPLEMENTAL FIGURE: Figure A1. Time distribution of Landsat images
  SSC_landsat_timeseries_time_distribution <- ggplot(river_landsat_pred_clean_3[
    station_nm %chin% example_image_count_stations[i]
  ], 
  aes(x = ten_day, fill = reference)
  ) + 
    geom_bar(stat = 'count', alpha = 0.7, color = 'black', lwd = 0.25) +
    # scale_fill_manual(values = c('Reference' = 'steelblue', 'Affected' = 'goldenrod')) +
    scale_fill_manual(values = c('Reference' = 'grey', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
    scale_x_continuous(limits = c(150,280)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
    facet_wrap(ifelse(grepl('Control', as.character(station_nm)), 'Control', as.character(river_nm))~
                 factor(ifelse(as.character(reference)=='Affected, Pre-slump', 'Reference', as.character(reference))), 
               ncol = 1) +
    geom_text(data = river_s2_import[
      station_nm %chin% example_image_count_stations[i]
    ][,.(N = .N), by = .(reference=ifelse(as.character(reference)=='Affected, Pre-slump', 'Reference', as.character(reference)), river_nm)][order(reference)][1], 
    aes(label = river_nm, x = 150, y = Inf), vjust = 1.5, hjust = 0, size = 3.5) +    theme_markdown +
    # theme(legend.position = 'top') + 
    labs(
      x = '***Day of Yr***',
      y = '***N images***',
      fill = ''
      ) +
    theme(axis.title = element_markdown(),
          strip.text = element_blank())
  
  # Modify x-axis labels to save space
  if(i %in% 1:4){
    SSC_landsat_timeseries_time_distribution <- SSC_landsat_timeseries_time_distribution + theme(axis.title.x = element_blank())
  }
  example_S2_image_count_plots[[i]] <- SSC_landsat_timeseries_time_distribution
  
  ## SUPPLEMENTAL FIGURE: Figure SXb. Time distribution of Sentinel-2 images
  SSC_S2_timeseries_time_distribution <- ggplot(river_s2_import[
    station_nm %chin% example_image_count_stations[i]
  ], 
  aes(x = ten_day, fill = reference)
  ) + 
    geom_bar(stat = 'count', alpha = 0.7, color = 'black', lwd = 0.25) +
    # scale_fill_manual(values = c('Reference' = 'steelblue', 'Affected' = 'goldenrod')) +
    scale_fill_manual(values = c('Reference' = 'grey', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
    facet_wrap(ifelse(grepl('Control', station_nm), 'Control', river_nm)~
                 factor(ifelse(as.character(reference)=='Affected, Pre-slump', 'Reference', as.character(reference))), 
               ncol = 1) +
    geom_text(data = river_s2_import[
      station_nm %chin% example_image_count_stations[i]
    ][,.(N = .N), by = .(reference=ifelse(as.character(reference)=='Affected, Pre-slump', 'Reference', as.character(reference)), river_nm)][order(reference)][1], 
    aes(label = river_nm, x = 150, y = Inf), vjust = 1.5, hjust = 0, size = 3.5) +
    theme_markdown +
    # theme(legend.position = 'top') + 
    labs(
      x = '***Day of Yr***',
      y = '***N images***',
      fill = ''
    ) +
    theme(axis.title = element_markdown(),
          strip.text = element_blank())
  
  # Modify x-axis labels to save space
  if(i %in% 1:4){
    SSC_S2_timeseries_time_distribution <- SSC_S2_timeseries_time_distribution + theme(axis.title.x = element_blank())
  }
  example_S2_image_count_plots[[i]] <- SSC_S2_timeseries_time_distribution

}


# Combine Landsat and Sentinel-2 image frequency plots
SSC_timeseries_time_distribution <- plot_grid(
  wrap_plots(example_S2_image_count_plots) , wrap_plots(example_S2_image_count_plots),
  ncol = 1,
  labels = c("a", "b"),
  label_fontface = "bold",
  label_x = 0.02,  # top-left placement
  label_y = 0.98,
  hjust = 0, vjust = 1
)


ggsave(SSC_timeseries_time_distribution, filename = paste0(wd_figures, region_name, '_SSC_timeseries_time_distribution.png'),
       width = 6, height = 9.5)
ggsave(SSC_timeseries_time_distribution, filename = paste0(wd_figures, region_name, '_SSC_timeseries_time_distribution.pdf'),
       width = 6, height = 9.5 , useDingbats = F)

#### 7A. PRE- AND POST-SLUMP SSC BY RIVER ####

s2_SSC_pre_post_by_station <- dcast.data.table(
  river_nm + station_nm ~ period,
  value.var = 'SSC_mgL',
  data = river_s2_import[
    ten_day > 170 & ten_day < 260,
    .(SSC_mgL = mean(SSC_mgL, na.rm = T)),
    by = .(river_nm, station_nm, period = ifelse(grepl('Pre', period), 'Sentinel-2, Reference', paste0('Sentinel-2, ', period)))
  ])

s2_SSC_pre_post_by_river <- dcast.data.table(
  river_nm ~ period,
  value.var = 'SSC_mgL',
  data = river_s2_import[
    ten_day > 170 & ten_day < 260,
    .(SSC_mgL = mean(SSC_mgL, na.rm = T)),
    by = .(river_nm, period = ifelse(grepl('Pre', period), 'Sentinel-2, Reference', paste0('Sentinel-2, ', period)))
  ])

s2_images_by_station <- dcast.data.table(
  river_nm + station_nm ~ period,
  value.var = 'N_images',
  data = river_s2_import[
    ten_day > 170 & ten_day < 260,
    .(N_images = .N),
    by = .(river_nm, station_nm, period = ifelse(grepl('Pre', period), 'Sentinel-2, Reference', paste0('Sentinel-2, ', period)))
  ])


SSC_s2_by_site_histogram <- ggplot(river_s2_import[
  !grepl('control', casefold(site_no))
], 
aes(y = SSC_mgL)) + 
  geom_histogram(data = river_s2_import[!grepl('control', casefold(site_no)) & grepl('Post', reference)], 
                 aes(after_stat(density), fill = factor(gsub('Affected, ', '', reference))), 
                 position = 'identity', lwd = 0.25, bins = 20, color = 'black') +
  geom_histogram(data = river_s2_import[!grepl('control', casefold(site_no)) & !grepl('Post', reference)], 
                 aes(after_stat(density), fill = factor(gsub('Affected, ', '', reference))), 
                 position = 'identity', linewidth = 0.25, bins = 20, color = NA, alpha = 0.7) +
  geom_histogram(data = river_s2_import[!grepl('control', casefold(site_no)) & !grepl('Post', reference)], 
                 aes(after_stat(density)), 
                 position = 'identity', linewidth = 0.25, bins = 20, fill = NA, color = 'steelblue') +
  facet_wrap(.~river_nm,
             # scales = 'free',
             ncol = 2) +
  scale_fill_manual(values = c('Reference' = '#41A8BF', 'Pre-slump' = 'steelblue', 'Post-slump' = 'goldenrod')) +
  scale_color_manual(values = c('Reference' = '#41A8BF', 'Pre-slump' = 'steelblue', 'Post-slump' = 'goldenrod')) +
  # scale_fill_manual(values = c('Post-slump' = 'goldenrod', 'Pre-slump' = NA)) +
  # scale_color_manual(values = c('Pre-slump' = 'steelblue')) +
  theme_markdown +
  scale_x_continuous(expand = expansion(mult = c(0,0.2))) +
  scale_y_log10() +
  theme(legend.position = 'top',
        legend.background = element_blank(),
        legend.title = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(),
        axis.line.x = element_line(linewidth = 0.5)) + 
  labs(
    x = 'Density',
    y = 'SSC (mg/L)',
    color = '',
    fill = ''
  )

ggsave(SSC_by_site_histogram, filename = paste0(wd_figures, region_name, '_SSC_by_site_histogram.png'),
       width = 5, height = 6)
ggsave(SSC_by_site_histogram, filename = paste0(wd_figures, region_name, '_SSC_by_site_histogram.pdf'),
       width =5, height = 6, useDingbats = F)


#### 8. ------ SENTINEL-2 ESTUARY SSC ANALYSIS ------ ####
#### 8A. CALCULATE AND PLOT SENTINEL-2 ESTUARY SSC TIMESERIES ####
# Import Sentinel-2 data from estuaries
# Import Sentinel-2 data from slumps
estuary_s2_import <- rbindlist(
  lapply(paste0(wd_imports, 
                c('sentinel_2_taymyr_peninsula_estuaries_2016_2026_20scale.csv')
  ),
  fread
  ), fill = T, use.names = T)[
    ,':='(.geo = NULL)]

# Standardize columns for Estuary data
estuary_s2_import <- na.omit(estuary_s2_import[,
                        ':='(
                          site_no = paste0('st', sub(".*_", "", `system:index`)),
                          # site_no = name,
                          station_nm = 106-as.integer(sub(".*_", "", `system:index`))*2,
                          # Rename columns for simplicity
                          B1 = B1_median,
                          B2 = B2_median,
                          B3 = B3_median,
                          B4 = B4_median,
                          B5 = B5_median,
                          B6 = B6_median,
                          B7 = B7_median,
                          B8 = B8_median,
                          B8A = B8A_median,
                          B9 = B9_median,
                          B11 = B11_median,
                          B12 = B12_median,
                          num_pix = B2_count,
                          sample_dt = ymd(date)
                        )]
        , cols = c('B1','B2','B3','B4','B5','B7'))

estuary_s2_import$SSC_mgL <- 10^predict(model, newdata = estuary_s2_import)

# Remove artifact samples (snow, ice, corrupted pixels)
estuary_s2_import <- estuary_s2_import[
  SSC_mgL > 0.5 & SSC_mgL < 20000 &
    !(SSC_mgL > 1000 & (B2_median + B3_median + B4_median) < 700)
]

# Add date columns, river distance, river names, period
estuary_s2_import <- estuary_s2_import[,':='(
  year = year(date),
  month = month(date),
  yday7 = yday(date) - yday(date)%%7,
  period = ifelse(year(date) < 2020, 'Pre-slump', 'Post-slump'),
  river = ifelse(lon > 100, 'Estuary 1', 'Estuary 2'),
  river_nm = ifelse(lon > 100, 'Leningradskaya', 'Nizhnyaya Taymyra'),
  distance_km = 106-as.integer(sub(".*_", "", `system:index`))*2,
  site_no = paste0('st', sub(".*_", "", `system:index`)))
]

# Make a table with the distance limits of each estuary
estuary_limits <- data.table(river = c('Estuary 1', 'Estuary 2'),
                             river_nm = c('Leningradskaya', 'Nizhnyaya Taymyra'),
                             xmin = c(0, 0),
                             xmax = c(Inf, Inf),
                             ymin = c(-Inf, -Inf),
                             ymax = c(Inf, Inf))

# Pre- vs. Post-slump average profiles, estuary 1 and 2
estuary_ssc_profile_average <- ggplot(estuary_s2_import[
  month %in% c(7,8,9)
  & SSC_mgL < 3000
  ], 
  aes(x = distance_km, y = SSC_mgL, color = period, fill = period)) +
  geom_rect(data = estuary_limits, aes(x = NULL, y = NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax = ymax), fill = 'grey95', color = NA) +
  geom_vline(data = estuary_limits, aes(xintercept = xmin), linetype = 'dashed', alpha = 0.7) + 
  # geom_point() +
  stat_summary(geom = 'ribbon', aes(group = period), alpha = 0.4, color = NA) +
  stat_summary(geom = 'line', aes(group = period)) +
  # stat_summary(geom = 'point', aes(group = period)) +
  scale_color_manual(values = c('Reference' = '#41A8BF', 'Pre-slump' = 'steelblue', 'Post-slump' = 'goldenrod')) +
  scale_fill_manual(values = c('Reference' = '#41A8BF', 'Pre-slump' = 'steelblue', 'Post-slump' = 'goldenrod')) +
  theme_markdown +
  facet_grid(.~river, scales = 'free_y')+
  labs(
    x = 'Distance to mouth (km)',
    y = 'SSC (mg/L)'
  )

# Combine plots
estuary_profile_average_monthly_panels <- estuary_ssc_profile_average / 
  (estuary_ssc_profile_average + facet_grid(month~river))
  
## STATISTIC: Peak SSC in the estuary and % attenuation at mouth
estuary_peak_vs_mouth_SSC <- estuary_s2_import[
  distance_km >= 0 & distance_km < 70 
  & SSC_mgL < 3000
  & month %in% c(7,8,9)][,.(
  SSC_mgL = mean(SSC_mgL, na.rm = T)),
  by = .(period, distance_km, river_nm)][, ':='(
    'distance_km_max' = {
      if (all(is.na(SSC_mgL))) NA_real_
      else distance_km[ which.max(SSC_mgL) ]
    }
  ), by = .(period, river_nm)][
    distance_km == 0|distance_km == distance_km_max][
    ,.(SSC_mgL_max = max(SSC_mgL, na.rm = T),
       SSC_mgL_min = min(SSC_mgL, na.rm = T)),
       by = .(period, river_nm)][
    ,':='(percent_SSC_trapped = (SSC_mgL_max - SSC_mgL_min)/(SSC_mgL_max)*100,
          percent_SSC_reduction = SSC_mgL_min/SSC_mgL_max*100)
  ][order(river_nm)]

# 
monthly_ssc_profile_estuary_1 <- ggplot(estuary_s2_import[
  month %in% c(7,8,9)
  & river == 'Estuary 1'
  & year > 2017
  & year < 2025
  & SSC_mgL < 3000
  # & period == 'Post-slump'
  ], 
       aes(x = distance_km, y = SSC_mgL, color = period)) +
  geom_rect(data = estuary_limits[river == 'Estuary 1'], 
            aes(x = NULL, y = NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax = ymax), fill = 'grey95', color = NA) +
  geom_vline(data = estuary_limits[river == 'Estuary 1'], aes(xintercept = xmin), linetype = 'dashed', alpha = 0.7) + 
  stat_summary(geom = 'line', aes(group = yday7), alpha = 0.5) +
  stat_summary(geom = 'line', aes(x = distance_km-distance_km%%6 + 3, group = month), alpha = 0.7, color = 'black') +
  
  scale_color_manual(values = c('Reference' = '#41A8BF', 'Pre-slump' = 'steelblue', 'Post-slump' = 'goldenrod')) +
  theme_markdown +
  facet_grid(month~year) +
  labs(
    x = 'Distance (km)',
    y = 'SSC (mg/L)'
  )

# ggsave(monthly_ssc_profile_estuary_1)
# Doesn't show much
monthly_ssc_profile_estuary_2 <- ggplot(estuary_s2_import[
  month %in% c(7,8,9)
  & river == 'Estuary 2'
  & year > 2017
  & SSC_mgL < 3000
  # & period == 'Post-slump'
  ], 
       aes(x = distance_km, y = SSC_mgL, color = period)) +
  stat_summary(geom = 'line', aes(group = yday7), alpha = 0.5) +
  stat_summary(geom = 'line', aes(x = distance_km-distance_km%%6 + 3, group = month), alpha = 0.7, color = 'black') +
  scale_color_manual(values = c('Reference' = '#41A8BF', 'Pre-slump' = 'steelblue', 'Post-slump' = 'goldenrod')) +
  theme_markdown +
  facet_grid(month~year) +
  labs(
    x = 'Distance (km)',
    y = 'SSC (mg/L)'
  )

#### 8B. S2 ESTUARY COLOR ####
s2_estuary_pre_post_color <- estuary_s2_import[
  # SSC_mgL < 2500 & !grepl('est2', site_no)
  B3_median < 2100
  & B2_median < 1800
  # & B6_median < 1000
][
  ,.(
    red = median(B4_median, na.rm = T),
    green = median(B3_median, na.rm = T),
    blue = median(B2_median, na.rm = T),
    nir = median(B8_median, na.rm = T)
  ),
  # by = .(site_no=ifelse(grepl('est', site_no), substr(site_no, 1, 7), substr(site_no, 1, 4)), year)
  by = .(river, river_nm, distance_km = distance_km-distance_km%%4 + 2, period)
  # by = .(river, distance_km = distance_km, year)
]

s2_estuary_annual_color <- estuary_s2_import[
  # SSC_mgL < 2500 & !grepl('est2', site_no)
  B3_median < 2100
][
  ,.(
    red = median(B4_median, na.rm = T),
    green = median(B3_median, na.rm = T),
    blue = median(B2_median, na.rm = T),
    nir = median(B8_median, na.rm = T)
  ),
  # by = .(site_no=ifelse(grepl('est', site_no), substr(site_no, 1, 7), substr(site_no, 1, 4)), year)
  by = .(river, river_nm, distance_km = distance_km-distance_km%%4 + 2, year)
  # by = .(river, distance_km = distance_km, year)
]

s2_estuary_monthly_color <- estuary_s2_import[
  # SSC_mgL < 2500 & !grepl('est2', site_no)
  B3_median < 2100
  & B2_median < 1800
  & B6_median < 1000
][
  ,.(
    red = median(B4_median, na.rm = T),
    green = median(B3_median, na.rm = T),
    blue = median(B2_median, na.rm = T),
    nir = median(B8_median, na.rm = T)
  ),
  # by = .(site_no=ifelse(grepl('est', site_no), substr(site_no, 1, 7), substr(site_no, 1, 4)), year)
  by = .(river, river_nm, distance_km = distance_km-distance_km%%4 + 2, month, period)
  # by = .(river, distance_km = distance_km, year)
]

s2_estuary_yday7_color <- estuary_s2_import[
  # SSC_mgL < 2500 & !grepl('est2', site_no)
  B3_median < 2100
  & B2_median < 2100
  & B6_median < 1000
][
  ,.(
    red = median(B4_median, na.rm = T),
    green = median(B3_median, na.rm = T),
    blue = median(B2_median, na.rm = T),
    nir = median(B8_median, na.rm = T)
  ),
  # by = .(site_no=ifelse(grepl('est', site_no), substr(site_no, 1, 7), substr(site_no, 1, 4)), year)
  by = .(river, river_nm, distance_km = distance_km-distance_km%%4 + 2, yday7, year, month, period)
  # by = .(river, distance_km = distance_km, year)
]

estuary_color_plot_list <- vector("list", 2)

for(i in 1:length(estuary_color_plot_list)){
  
  # Pre-post color
  SSC_S2_estuary_color_timeseries_by_reach_rgb_plot <- ggplot(s2_estuary_pre_post_color[
    # (year %in% c(2018:2024))
    # & year != 2021
    # & river == 'Estuary 1'
    river_nm == c('Leningradskaya', 'Nizhnyaya Taymyra')[i]
    # !(as.character(river) %chin% c('E','F','G'))
  ],
  aes(x = distance_km, y = factor(period, levels = c('Pre-slump', 'Post-slump'), ordered = T),
      # fill = rgb((red-400)/1300,(green-600)/1300,(blue-800)/1600)
      # fill = rgb(min((red-800)/1000,0),(green-600)/1000,(blue-600)/1300)
      fill = rgb(red/1700,green/1700,blue/1700)
      # fill = rgb(nir/1500,red/1500,green/1500)
  )) +
    geom_tile(width = 4, color = 'black', linewidth = 0.25) +
    # geom_tile(width = 2, color = 'black', linewidth = 0.25) +
    scale_fill_identity() +
    # facet_wrap(.~river, ncol = 1) +
    facet_col(.~factor(period, levels = c('Pre-slump', 'Post-slump'), ordered = T),
              scales = 'free_y', space = 'free') +
    # scale_color_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
    theme_markdown +
    scale_y_discrete(limits = rev, expand = expansion(mult = 0)) +
    scale_x_continuous(expand = expansion(mult = 0)) +
    theme(legend.position = 'top') +
    labs(
      x = 'Distance (km) (avg. every 4 km)',
      y = '',
      color = 'True color'
    )
  
  estuary_sel_ssc_profile_average <- ggplot(estuary_s2_import[
    month %in% c(7,8,9)
    & river_nm == c('Leningradskaya', 'Nizhnyaya Taymyra')[i]
    & SSC_mgL < 5000
  ], 
  aes(x = distance_km, y = SSC_mgL, color = period, fill = period)) +
    geom_rect(data = estuary_limits[river_nm == c('Leningradskaya', 'Nizhnyaya Taymyra')[i]], aes(x = NULL, y = NULL, xmin=xmin, xmax=xmax, ymin=ymin, ymax = ymax), fill = 'grey95', color = NA) +
    geom_vline(data = estuary_limits[river_nm == c('Leningradskaya', 'Nizhnyaya Taymyra')[i]], aes(xintercept = xmin), linetype = 'dashed', alpha = 0.7) + 
    geom_text(data = estuary_limits[river_nm == c('Leningradskaya', 'Nizhnyaya Taymyra')[i]], aes(x = xmin, label = 'Estuary mouth', y = Inf), hjust = -0.05, vjust = 3, inherit.aes = F) +
    stat_summary(geom = 'ribbon', aes(group = period), alpha = 0.4, color = NA) +
    stat_summary(geom = 'line', aes(group = period)) +
    # stat_summary(geom = 'point', aes(group = period)) +
    scale_color_manual(values = c('Reference' = '#41A8BF', 'Pre-slump' = 'steelblue', 'Post-slump' = 'goldenrod')) +
    scale_fill_manual(values = c('Reference' = '#41A8BF', 'Pre-slump' = 'steelblue', 'Post-slump' = 'goldenrod')) +
    scale_x_continuous(expand = expansion(mult = 0)) +
    theme_markdown +
    facet_grid(.~river_nm)+
    labs(
      x = 'Distance (km)',
      y = 'SSC (mg/L)'
    )
  
  estuary_sel_ssc_profile_average_w_color <- (SSC_S2_estuary_color_timeseries_by_reach_rgb_plot + 
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(0, 0, 0, 0.2), "cm")
            )
  ) / 
    (estuary_sel_ssc_profile_average + 
       theme(plot.margin = unit(c(0, 0, 0, 0.2), "cm"))) +
    plot_layout(heights = c(0.2, 0.5)) +
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(face = 'bold'))
  
  estuary_sel_ssc_profile_average_w_color_w_months <- (SSC_S2_estuary_color_timeseries_by_reach_rgb_plot + 
                                                         theme(axis.title.x = element_blank(),
                                                               axis.title.y = element_blank(),
                                                               axis.text.y = element_blank(),
                                                               axis.ticks.y = element_blank())
  ) / 
    (estuary_sel_ssc_profile_average + 
       theme(axis.title.x = element_blank())) /
    (estuary_sel_ssc_profile_average + facet_grid(month~river_nm)) +
    plot_layout(heights = c(0.15, 0.5, 0.5)) +
    plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(face = 'bold'))
  
  estuary_color_plot_list[[i]] <- estuary_sel_ssc_profile_average_w_color
}

estuary_SSC_and_color_combined <- wrap_plots(estuary_color_plot_list)

ggsave(estuary_SSC_and_color_combined, filename = paste0(wd_figures, 'estuary_SSC_and_color_combined.pdf'),
       width = 6.5, height = 3.5, useDingbats = F)

wrap_plots(estuary_color_plot_list) / 
  (estuary_profile_average_monthly_panels + scale_x_continuous(expand = expansion(mult = 0)))

SSC_S2_color_timeseries_by_site_nir_plot <- ggplot(s2_annual_color[
  !(as.character(river) %chin% c('E','F','G'))
], 
aes(x = year, y = river,
    fill = rgb(nir/1350, red/1350,green/1350)
)) + 
  geom_tile(width = 1, color = 'black', linewidth = 0.25) +
  scale_fill_identity() +
  facet_col(.~ifelse(grepl('control',casefold(river)), 'Control', 'Affected'), scales = 'free_y', space = 'free') +
  # scale_color_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
  theme_markdown +
  theme(legend.position = 'top') + 
  scale_y_discrete(limits = rev, expand = expansion(mult = 0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  labs(
    x = 'Year',
    y = 'River reach',
    color = 'True color'
  )



#### 9. LANDSAT & SENTINEL-2 COMPARISON WITH ARCTIC GRO DATA ####
#### 9A. IMPORT LANDSAT ARCTIC GRO DATA ####
# Import Landsat river profile data for each batch of thaw slump sites
# Combine Landsat sample data into one data.table
river_AG_import <- fread(paste0(wd_imports, 
                                c('arctic_gro_river_training_ls5789_rawBands_b7lt500.csv')
))[
  ,':='(.geo = NULL)]

# Merge landsat data with metadata file to standardize river names
site_topo_metadata_AG <- fread(paste0(wd_imports, 'ls_arctic_gro_training_station_topo.csv'))[
  ,':='(river_nm = site_no,
        river = site_no,
        station_nm = site_no)
]
# Add river name, etc.
river_AG_import <- merge(river_AG_import,
                         site_topo_metadata_AG[,.(site_no, river, river_nm, station_nm)],
                         by = 'site_no')

#### 9B. PREPARE ARCTIC GRO LANDSAT DATA COLUMNS, CALCULATE CLUSTER & SSC ####
# Standardize Landsat data
# Landsat data do have station information
# They also have latitude and longitude
river_AG_import <- na.omit(river_AG_import[,
                                           ':='(
                                             site_no = site_no,
                                             # site_no = name,
                                             station_nm = station_nm,
                                             # Rename columns for simplicity
                                             B1 = B1_median,
                                             B2 = B2_median,
                                             B3 = B3_median,
                                             B4 = B4_median,
                                             B5 = B5_median,
                                             B6 = B6_median,
                                             B7 = B7_median,
                                             num_pix = B2_count,
                                             sample_dt = ymd(date),
                                             landsat_dt = ymd(date)
                                           )]
                           , cols = c('B1','B2','B3','B4','B5','B7'))[
                             B1 > 0 & B2 > 0 & B3 > 0 & B4 > 0 & B5 > 0 & B7 > 0 &
                               B1 < 5000 & B2 < 5000 & B3 < 5000 & B4 < 5000 & B6 < 4000][
                                 ,':='( 
                                   year = year(sample_dt),
                                   # add new columns with band ratios
                                   B1.2 = B1^2,
                                   B2.2 = B2^2,
                                   B3.2 = B3^2,
                                   B4.2 = B4^2,
                                   B5.2 = B5^2,
                                   B7.2 = B7^2,
                                   B2.B1 = B2/B1,
                                   B3.B1 = B3/B1,
                                   B4.B1 = B4/B1,
                                   B5.B1 = B5/B1,
                                   B7.B1 = B7/B1,
                                   B3.B2 = B3/B2,
                                   B4.B2 = B4/B2,
                                   B5.B2 = B5/B2,
                                   B7.B2 = B7/B2,
                                   B4.B3 = B4/B3,
                                   B5.B3 = B5/B3,
                                   B7.B3 = B7/B3,
                                   B5.B4 = B5/B4,
                                   B7.B4 = B7/B4,
                                   B7.B5 = B7/B5,
                                   Latitude = lat,
                                   Longitude = lon
                                   # station_nm = paste0(0,station_no),
                                   # site_no = paste0(0,site_no)
                                 )][ 
                                   # select only columns of interest
                                   ,.(site_no, station_nm, year,
                                      river, river_nm,
                                      # distance_km,
                                      # width, drainage_area_km2,
                                      Latitude,Longitude,sample_dt, num_pix, 
                                      snow_ice_qa_count,
                                      cloud_cover, cloud_qa_count,
                                      landsat_dt,
                                      B1,B2,B3,B4,B5,B6,B7,B2.B1,B3.B1,B4.B1,B5.B1,B7.B1,B3.B2,B4.B2,B5.B2,
                                      B7.B2,B4.B3,B5.B3,B7.B3,B5.B4,B7.B4,B7.B5, B1.2,B2.2,B3.2,B4.2,B5.2,B7.2
                                   )][site_no != "0"][
                                     !((B6 < 2800 & B1 > 900 & B2 > 900 & B3 > 900 & B5 > 300 & B7 > 200 & B1 > B3 & B1 < B4) | # Elimate snowy & cold images
                                         (B1 > 700 & B1/B2 > 1.2 & B5 > 200)|
                                         ((B1 + B2 + B3 + B4) > 3200 & B3 < B1 & B3/B1 < 1.5 & B6 < 2750 & B5 > 300) |
                                         (B4 > 1500 & B4/B3 > 1.5 & B6 < 2800)| # This eliminates many cloudy/snowy images at high latitudes
                                         # ((B1 + B2 + B3 + B4) > 4000 & B6 < 2750 & B5 > 300 & abs(Latitude) > 40)
                                         ((B1 + B2 + B3 + B4) > 4000 & B6 < 2750 & B5 > 500 & abs(Latitude) > 40) # *changed B5 min to 500*
                                       # (B1 > 700 &
                                       # snow_ice_qa_count > (num_pix * 10) & 
                                       # snow_ice_qa_count > 500 &
                                       # B3/B1 < 1.5)
                                     )
                                   ][
                                     ,':='(month = month(sample_dt),
                                           decade = ifelse(year < 1990, 1990,
                                                           ifelse(year > 2019, 2020,
                                                                  year - year%%5)))]

# Remove pixels with snow, ice, clouds
river_AG_import <- river_AG_import[cloud_cover < 70 & !(num_pix < 2 & cloud_qa_count > 100)][
  yday(sample_dt) > 120 & yday(sample_dt) < 290 # Winter months
]

# Get cluster for each site based on typical spectral profile
# This takes a long time to run
river_AG_landsat_cl <- getCluster_monthly_decadal(river_AG_import, 
                                                  clustering_vars,cluster_n_best, 
                                                  clusters_calculated_list[[cluster_n_best]])

# Run SSC prediction algorithm to get clustered prediction for SSC
river_AG_landsat_pred <- getSSC_pred(na.omit(river_AG_landsat_cl, cols = c(regressors_all, 'cluster_sel')), 
                                     regressors_all, ssc_cluster_funs)[,':='(
                                       SSC_mgL = ifelse(pred_cl > 5.5, NA, 10^pred_cl),
                                       month = month(sample_dt),
                                       decade = ifelse(year(sample_dt) < 1990, 1990,
                                                       ifelse(year(sample_dt) > 2024, 2020, 
                                                              year(sample_dt) - year(sample_dt)%%5)))]


#### 9C. CLEAN LANDSAT ARCTIC GRO DATA AND WRITE TO DRIVE ####
# Select just simple columns for export
river_AG_landsat_pred_clean <- river_AG_landsat_pred[
  ,.(site_no, station_nm, river, river_nm, month, year, decade, Latitude, Longitude,sample_dt,
     num_pix, B1 = round(B1), B2 = round(B2), B3 = round(B3), B4 = round(B4), B6 = round(B6),
     cluster, SSC_mgL
  )
]


# Remove winter months from dataset
river_AG_landsat_pred_clean_2 <- river_AG_landsat_pred_clean[
  # yday(sample_dt) > 80 & yday(sample_dt) < 260 # Winter months
]

# Add an additional filter for high SSC (mostly due to errors/artifacts)
river_AG_landsat_pred_clean_3 <- river_AG_landsat_pred_clean_2[SSC_mgL > 0.5 & SSC_mgL < 15000 &
                                                                 !(SSC_mgL > 1000 & (B1 + B2 + B3) < 700)][
                                                                   num_pix > 2
                                                                 ]
river_AG_landsat_pred_clean_3[,.(N_images = .N), by = .(site_no, station_nm, river, cluster)]

# Write full table to drive
fwrite(river_AG_landsat_pred_clean_3, paste0(wd_imports,'arcticgro_river_landsat_pred.csv'))


#### 9D. IMPORT ARCTIC GRO SENTINEL-2 DATA ####
# Import 
arctic_gro <- fread(paste0(wd_imports, 'ArcticGRO-water-quality.knit.csv'))
arctic_gro_s2_comparison_import <- fread(paste0(wd_imports,'sentinel_2_arcticgro_comparison_rivers_2016_2026_20scale.csv'))[
  ,':='(.geo = NULL)]

# Set up columns for Sentinel-2 ArcticGRO samples
river_s2_comparison_import <- na.omit(arctic_gro_s2_comparison_import[,
                                                                      ':='(
                                                                        site_no = site_no,
                                                                        # site_no = name,
                                                                        station_nm = site_no,
                                                                        river = site_no,
                                                                        river_nm = site_no,
                                                                        # Rename columns for simplicity
                                                                        B1 = B1_median,
                                                                        B2 = B2_median,
                                                                        B3 = B3_median,
                                                                        B4 = B4_median,
                                                                        B5 = B5_median,
                                                                        B6 = B6_median,
                                                                        B7 = B7_median,
                                                                        B8 = B8_median,
                                                                        B8A = B8A_median,
                                                                        B9 = B9_median,
                                                                        B11 = B11_median,
                                                                        B12 = B12_median,
                                                                        num_pix = B2_count,
                                                                        sample_dt = ymd(date)
                                                                      )]
                                      , cols = c('B1','B2','B3','B4','B5','B7'))[
                                        B1 > 0 & B2 > 0 & B3 > 0 & B4 > 0 & B8 > 0 & B11 > 0 &
                                          B1 < 5000 & B2 < 5000 & B3 < 5000 & B4 < 5000 & B6 < 4000][
                                            ,':='( 
                                              year = year(sample_dt),
                                              Latitude = lat,
                                              Longitude = lon
                                              # station_nm = paste0(0,station_no),
                                              # site_no = paste0(0,site_no)
                                            )][ 
                                              # select only columns of interest
                                              ,.(site_no, station_nm, year,
                                                 river, river_nm,
                                                 Latitude,Longitude,sample_dt, num_pix, 
                                                 cloud_cover, product_id,
                                                 B1,B2,B3,B4,B5,B6,B7,B8,B8A,B9,B11,B12
                                              )][site_no != "0"][
                                                ,':='(month = month(sample_dt),
                                                      decade = ifelse(year < 1990, 1990,
                                                                      ifelse(year > 2019, 2020,
                                                                             year - year%%5)))]

# Use Landsat-based Sentinel-2 model to predict SSC for ArcticGRO Sentinel-2 data
river_s2_comparison_import$SSC_mgL <- 10^predict(model, newdata = river_s2_comparison_import)



river_s2_comparison_import <- river_s2_comparison_import[
  SSC_mgL > 0.5 & SSC_mgL < 3000 &
    !(SSC_mgL > 1000 & (B2 + B3 + B4) < 700)
]

arctic_gro[river == "Ob'",':='(river = 'Ob')]

#### 9E. COMPARE TAYMYR CLUSTERS TO ARCTIC GRO CLUSTERS ####
# Cluster and N image summary of Taymyr sites
river_landsat_cluster_summary <- river_landsat_pred_clean_3[,.(N_images = .N), by = .(site_no, station_nm, river, cluster)]
# Cluster and N image summary of ArcticGRO sites
river_AG_landsat_cluster_summary <- river_AG_landsat_pred_clean_3[,.(N_images = .N), by = .(site_no, station_nm, river, cluster)]

# ArcticGRO sites with Taymyr cluster
arcticGRO_cl2_cl3_sites <- river_AG_landsat_cluster_summary[cluster %in% c(2,3)]

#### 9F. IMPORT ARCTIC GRO TSS DATA, TEST AS OUT-OF-BAG SAMPLE ####
# Merge Arctic GRO and Dethier, 2019 Landsat estimates of SSC
# Arcit GRO data was *not* used in generating the regression, it is an "out of bag" test sample
arctic_gro_ls_merge <- arctic_gro[parameter == 'TSS'][,':='(sample_dt = date)][
  river_AG_landsat_pred_clean_3,
  on = .(river, sample_dt),
  roll = "nearest",
  nomatch = NULL
]

arctic_gro_ls_merge[,':='(lag = abs(as.IDate(sample_dt) - date))
]


# Test Arctic GRO vs. Dethier, 2020 algorithms
# Limit lag to less than 10 days
arctic_gro_validation <- lm(log10(value)~log10(SSC_mgL), data = arctic_gro_ls_merge[lag <= 9 & value > 0])
summary(arctic_gro_validation)
glance(arctic_gro_validation)

for(i in 1:30){
  arctic_gro_validation_i <- lm(log10(value)~log10(SSC_mgL), data = arctic_gro_ls_merge[lag <= i & value > 0])
  arctic_gro_validation_i_glance <- data.table(glance(arctic_gro_validation_i))[,':='(lead_lag = i)]
  if(i == 1){
    arctic_gro_regression_validation_summary <- arctic_gro_validation_i_glance
  }else{
    arctic_gro_regression_validation_summary <- rbind(
      arctic_gro_regression_validation_summary, 
      arctic_gro_validation_i_glance, 
      use.names = T, fill = T)
  }
}

## FIGURE AXXa.
# Plot Arctic GRO vs. Dethier, 2020 algorithms
arctic_gro_regression_validation <- ggplot(arctic_gro_ls_merge[lag <= 9][value > 0], 
                                           aes(x = value, y = SSC_mgL)) +
  geom_point(pch = 21, stroke = 0.5, fill = 'grey40', size = 2) +
  # geom_point(aes(color = river)) +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_log10(labels = fancy_scientific_modified, limits = c(0.1, 1000)) +
  scale_y_log10(labels = fancy_scientific_modified, limits = c(0.1, 1000)) +
  labs(
    x = '***SSC*** (mg/L)<br>[data: Arctic Great Rivers Observatory]',
    y = '***SSC*** (mg/L)<br>[data: Dethier, 2020 Landsat algorithms]'
  ) +
  theme_markdown

## Figure AXXb. Arctic GRO TSS data vs. Landsat-based estimates, day of year estimates
arctic_gro_validation_day_of_year_plot <- ggplot(arctic_gro[parameter == 'TSS'][year(date) > 2000], aes(x = yday(date)-yday(date)%%3, y = value)) +
  stat_summary(geom = 'line', aes(color = 'in situ data, Arctic Global River Observatory')) +
  # stat_summary(geom = 'line', data = river_s2_comparison_import[month %in% c(3:12)], 
  #           aes(x = yday(sample_dt)-yday(sample_dt)%%3, y = SSC_mgL, color = 'Taymyr-specific S2 algorithm')) +
  stat_summary(data = river_AG_landsat_pred_clean_3,
               # geom = 'line',
               color = 'black',
               pch = 21, stroke = 0.5,
               aes(x = yday(sample_dt)-yday(sample_dt)%%3, y = SSC_mgL,
                   # color = 'Landsat algorithm (Dethier et al., 2020)')) +
                   fill = 'Landsat algorithm (Dethier et al., 2020)')) +
  # scale_y_log10(labels = fancy_scientific_modified) +
  scale_color_manual(values = c('in situ data, Arctic Global River Observatory' = 'black', 
                                'Taymyr-specific S2 algorithm' = 'steelblue',
                                'Landsat algorithm (Dethier et al., 2020)' = 'grey')) +
  scale_fill_manual(values = c('in situ data, Arctic Global River Observatory' = 'black', 
                               'Taymyr-specific S2 algorithm' = 'steelblue',
                               'Landsat algorithm (Dethier et al., 2020)' = 'grey')) +
  facet_wrap(.~river, scales = 'free') +
  theme_markdown +
  theme(legend.position = 'top') +
  labs(
    x = 'Day of year',
    y = '***SSC*** (mg/L)',
    color = '',
    fill = ''
  )


## Figure AXXc. Sentinel-2 data for rivers in the Taymyr cluster, day of year estimates
arctic_gro_validation_day_of_year_taymyr_specific_plot <- ggplot(
  arctic_gro[parameter == 'TSS'][
    # river %chin% c('Kolyma','Mackenzie')
    river %chin% arcticGRO_cl2_cl3_sites[,river]
  ][year(date) > 2000], aes(x = yday(date)-yday(date)%%3, y = value)) +
  stat_summary(geom = 'line', aes(color = 'in situ data, Arctic Global River Observatory')) +
  stat_summary(data = river_s2_comparison_import[month %in% c(3:12)][
    # river %chin% c('Kolyma','Mackenzie')
    river %chin% arcticGRO_cl2_cl3_sites[,river]
  ],color = 'black',
  pch = 21, stroke = 0.5,
  # geom = 'line',
  aes(x = yday(sample_dt)-yday(sample_dt)%%3, y = SSC_mgL, fill = 'Taymyr-specific S2 algorithm')) +
  # stat_summary(data = river_AG_landsat_pred_clean_3[
  #   river %chin% c('Kolyma','Mackenzie')
  # ],
  # color = 'black',
  # pch = 21, stroke = 0.5,
  #              aes(x = yday(sample_dt)-yday(sample_dt)%%3, y = SSC_mgL, fill = 'Landsat algorithm (Dethier et al., 2020)')) +
  # scale_y_log10(labels = fancy_scientific_modified) +
  scale_color_manual(values = c('in situ data, Arctic Global River Observatory' = 'black', 
                                'Taymyr-specific S2 algorithm' = 'steelblue',
                                'Landsat algorithm (Dethier et al., 2020)' = 'grey')) +
  scale_fill_manual(values = c('in situ data, Arctic Global River Observatory' = 'black', 
                               'Taymyr-specific S2 algorithm' = 'steelblue',
                               'Landsat algorithm (Dethier et al., 2020)' = 'grey')) +
  facet_wrap(.~river, scales = 'free') +
  theme_markdown +
  theme(legend.position = 'top') +
  labs(
    x = 'Day of year',
    y = '***SSC*** (mg/L)',
    color = '',
    fill = ''
  )

arctic_gro_validation_combined <- (arctic_gro_regression_validation/arctic_gro_validation_day_of_year_plot/arctic_gro_validation_day_of_year_taymyr_specific_plot) +
  plot_layout(heights = c(0.7,0.5,0.5)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(arctic_gro_validation_combined, filename = paste0(wd_figures, region_name, '_arctic_gro_validation_combined.png'),
       height = 10, width = 5.5)
ggsave(arctic_gro_validation_combined, filename = paste0(wd_figures, region_name, '_arctic_gro_validation_combined.pdf'),
       height = 10, width = 5.5, useDingbats = F)


#### ------ END OF CODE USED IN ANALYSES ----- #####
#### vvvv DRAFT AND TEST DATA vvvv ####
#### 6E. SENTINEL-2 MISC. PLOTS ####
ggplot(river_s2_import[
  # !grepl('ref', river) & !(river %in% c('E','F'))
  # grepl('s008|s002', site_no)
  !grepl('control', casefold(site_no))
], aes(x = year , y = SSC_mgL,
       # ], aes(x = year + month/12, y = B8_median/B2_median,
       color = reach, group = site_no)) + 
  stat_summary(geom = 'errorbar', width = 0.05, color = 'grey30') +
  stat_summary(geom = 'line', fun = mean) + 
  stat_summary(geom = 'point', fun = mean) + 
  scale_color_manual(values = c('Affected' = 'orange', 'Reference' = 'navy')) +
  facet_wrap(.~river, 
             scales = 'free_y', ncol = 2) +
  scale_x_continuous(labels = abbrev_year) +
  theme_markdown +
  labs(
    x = 'Year',
    y = 'SSC (mg/L)',
    color = 'Reach'
  )


ggplot(river_s2_import[
  !grepl('ref', river) & !(river %in% c('E','F'))
  & !grepl('Estuary', river)
  & ten_day > 200 & ten_day < 280
], aes(x = num_pix , y = SSC_mgL,
       # ], aes(x = year + month/12, y = B8_median/B2_median,
       color = period, group = reach)) + 
  # stat_summary(geom = 'errorbar', width = 0.05, color = 'grey30') +
  # stat_summary(geom = 'line', fun = mean) + 
  stat_summary(geom = 'point', fun = mean) + 
  scale_x_log10() + 
  scale_y_log10() +
  # scale_color_manual(values = c('Affected' = 'orange', 'Reference' = 'navy')) +
  facet_wrap(reach~station_nm, 
             scales = 'free') +
  theme_markdown +
  labs(
    x = 'N pix',
    y = 'SSC (mg/L)',
    color = 'Reach'
  )


ggplot(river_s2_import[month > 5 & month < 10
                       & !grepl('Control', river)
                       # grepl('s008', site_no)
], aes(x = sample_dt, y = SSC_mgL, group = paste0(year, river_nm),
       # ], aes(x = year + month/12, y = B8_median/B2_median,
       color = period)) + 
  stat_summary(aes(x = ymd(paste0(year,'-08-01')))) +
  stat_summary(aes(x = ymd(paste0(year,'-08-01')), group = paste0(river, period)), geom = 'line', linetype = 'dashed') +
  stat_summary(geom='line') +
  scale_color_manual(values = c('post-Slump' = 'goldenrod', 'pre-Slump' = 'steelblue')) +
  facet_wrap(.~river, 
             scales = 'free_y', ncol = 2) +
  # scale_x_continuous(labels = abbrev_year) +
  theme_markdown +
  labs(
    x = 'Year',
    y = 'SSC (mg/L)',
    color = 'Reach'
  )

ggplot(river_s2_import[month >= 5 & month <= 10
                       & !(as.character(river) %chin% c('E','F','G'))
                       & !grepl('Control', river)
                       # grepl('s008', site_no)
], aes(x = yday15, y = SSC_mgL, group = period,
       # ], aes(x = year + month/12, y = B8_median/B2_median,
       color = period, fill = period)) + 
  stat_summary(geom='ribbon', alpha = 0.4, color = NA) +
  stat_summary(geom='line') +
  scale_color_manual(values = c('post-Slump' = 'goldenrod', 'pre-Slump' = 'steelblue')) +
  scale_fill_manual(values = c('post-Slump' = 'goldenrod', 'pre-Slump' = 'steelblue')) +
  facet_wrap(.~river, 
             scales = 'free_y', ncol = 2) +
  # scale_x_continuous(labels = abbrev_year) +
  theme_markdown +
  labs(
    x = 'Year',
    y = 'SSC (mg/L)',
    color = 'Period',
    fill = 'Period'
  )

ggplot(river_s2_import[month >= 5 & month <= 10
                       & !grepl('Control', river)
                       & river == 'C'
                       # grepl('s008', site_no)
], aes(x = yday, y = SSC_mgL, group = year,
       # ], aes(x = year + month/12, y = B8_median/B2_median,
       color = period, fill = period)) + 
  # stat_summary(geom='ribbon', alpha = 0.4, color = NA) +
  stat_summary(geom='line') +
  scale_color_manual(values = c('post-Slump' = 'goldenrod', 'pre-Slump' = 'steelblue')) +
  scale_fill_manual(values = c('post-Slump' = 'goldenrod', 'pre-Slump' = 'steelblue')) +
  facet_wrap(site_no~river, 
             scales = 'free_y', ncol = 2) +
  # scale_x_continuous(labels = abbrev_year) +
  theme_markdown +
  labs(
    x = 'Year',
    y = 'SSC (mg/L)',
    color = 'Period',
    fill = 'Period'
  )

plume_images <- c(
  '20200611T194911_20200611T194907_T10WEA_4'
)
snowy_images <- c(
  '20180603T200849_20180603T200849_T10WEA_4',
  '20180605T200001_20180605T200001_T10WEA_4'
)


# # Annual color
# SSC_S2_estuary_color_timeseries_by_reach_rgb_plot <- ggplot(s2_estuary_annual_color[
#   (year %in% c(2018:2024))
#   & year != 2021
#   # & river == 'Estuary 1'
#   & river == c('Leningradskaya', 'Nizhnyaya Taymyra')[i]
#   # !(as.character(river) %chin% c('E','F','G'))
# ], 
# aes(x = distance_km, y = factor(year),
#     # fill = rgb((red-400)/1300,(green-600)/1300,(blue-800)/1600)
#     fill = rgb((red-400)/1000,(green-600)/1000,(blue-800)/1300)
# )) + 
#   geom_tile(width = 4, color = 'black', linewidth = 0.25) +
#   # geom_tile(width = 2, color = 'black', linewidth = 0.25) +
#   scale_fill_identity() +
#   # facet_wrap(.~river, ncol = 1) +
#   facet_col(.~factor(ifelse(year < 2020, 'Pre-slump', 'Post-slump'), levels = c('Pre-slump', 'Post-slump'), ordered = T), 
#             scales = 'free_y', space = 'free') +
#   # scale_color_manual(values = c('Reference' = '#41A8BF', 'Affected, Pre-slump' = 'steelblue', 'Affected, Post-slump' = 'goldenrod')) +
#   theme_markdown +
#   scale_y_discrete(limits = rev, expand = expansion(mult = 0)) +
#   scale_x_continuous(expand = expansion(mult = 0)) +
#   theme(legend.position = 'top') + 
#   labs(
#     x = 'Distance (km) (avg. every 4 km)',
#     y = 'Year',
#     color = 'True color'
#   )

# Color by month
# SSC_S2_estuary_color_timeseries_by_reach_rgb_plot <- ggplot(s2_estuary_monthly_color[
#   river == c('Leningradskaya', 'Nizhnyaya Taymyra')[i]
#   & month %in% c(7,8,9)
# ], 
# aes(x = distance_km, y = factor(month),
#     # fill = rgb((red-400)/1300,(green-600)/1300,(blue-800)/1600)
#     # fill = rgb((red-400)/1000,(green-600)/1000,(blue-800)/1300)
#     fill = rgb((red-200)/1600,(green-500)/1600,(blue-800)/1800)
# )) + 
#   geom_tile(width = 4, color = 'black', linewidth = 0.25) +
#   # geom_tile(width = 2, color = 'black', linewidth = 0.25) +
#   scale_fill_identity() +
#   # facet_wrap(.~river, ncol = 1) +
#   facet_col(.~factor(period, levels = c('Pre-slump', 'Post-slump'), ordered = T), 
#             scales = 'free_y', space = 'free') +
#   theme_markdown +
#   scale_y_discrete(limits = rev, expand = expansion(mult = 0)) +
#   scale_x_continuous(expand = expansion(mult = 0)) +
#   theme(legend.position = 'top') + 
#   labs(
#     x = 'Distance (km) (avg. every 4 km)',
#     y = 'Year',
#     color = 'True color'
#   )
# Color by YDAY7
# SSC_S2_estuary_color_timeseries_by_reach_rgb_plot <- ggplot(s2_estuary_yday7_color[
#   river == c('Leningradskaya', 'Nizhnyaya Taymyra')[i]
#   & month %in% c(7,8,9)
#   # & month %in% c(9)
# ], 
# aes(x = distance_km, y = factor(year + yday7/365),
#     # fill = rgb((red-400)/1300,(green-600)/1300,(blue-800)/1600)
#     fill = rgb((red-200)/1600,(green-500)/1600,(blue-800)/1800)
# )) + 
#   geom_tile(width = 4, color = 'black', linewidth = 0.25) +
#   # geom_tile(width = 2, color = 'black', linewidth = 0.25) +
#   scale_fill_identity() +
#   # facet_wrap(.~river, ncol = 1) +
#   facet_col(.~factor(period, levels = c('Pre-slump', 'Post-slump'), ordered = T), 
#             scales = 'free_y', space = 'free') +
#   theme_markdown +
#   scale_y_discrete(limits = rev, expand = expansion(mult = 0)) +
#   scale_x_continuous(expand = expansion(mult = 0)) +
#   theme(legend.position = 'top') + 
#   labs(
#     x = 'Distance (km) (avg. every 4 km)',
#     y = 'Year',
#     color = 'True color'
#   )
#### APPROXIMATE RUNOFF ANALYSIS ####
ggplot(climate_taymyr[
  year > 2018
  # & month(date) %in% c(7,8,9)
], aes(x = yday, y = yday10_roll_precip)) +
  geom_line() +
  geom_bar(aes(y = snowmelt_runoff_m), stat = 'identity') +
  facet_wrap(.~year) +
  theme_markdown 

climate_taymyr <- climate_taymyr[,':='(
  discharge_rel = (total_precipitation_sum + 
                     ifelse(snowmelt_runoff_m < 0, -snowmelt_runoff_m, 0) + 
                     frollmean(ifelse(snowmelt_runoff_m < 0, -snowmelt_runoff_m, 0), 20) + 
                     frollmean(total_precipitation_sum, 365) + 
                     frollmean(total_precipitation_sum, 30) +
                     frollmean(total_precipitation_sum, 10)
  )/6
)]

ggplot(climate_taymyr[
  year > 2018
  # & month(date) %in% c(7,8,9)
], aes(x = yday, y = discharge_rel)) +
  geom_line(color = 'blue') +
  geom_bar(aes(y = total_precipitation_sum), stat = 'identity') +
  facet_wrap(.~year) +
  theme_markdown 

ggplot(climate_taymyr[
  year == 2020
  & month(date) %in% c(6,7,8,9,10)
], aes(x = yday, y = discharge_rel)) +
  # geom_line(color = 'blue') +
  geom_bar(aes(y = total_precipitation_sum), stat = 'identity') +
  geom_vline(xintercept = 218) +
  stat_summary(data = river_s2_import[
    month %in% c(6,7,8,9,10)
    & !grepl('Control', river)
    # & river == 'Estuary 1' & grepl('3|4|5', station_nm)
    & river == 'B'
    & year == 2020
  ], aes(x = yday, y = SSC_mgL/100000, group = year, fill = period, group = station_nm), geom='point', pch = 21) + 
  theme_markdown +
  labs(
    x = 'Day of year',
    y = 'SSC (mg/L)',
    color = 'Period',
    fill = 'Period'
  )

estuary_s2_wQ <- merge(
  estuary_s2_import,
  climate_taymyr[,.(date, discharge_rel)],
  by = c('date')
)

estuary_s2_wQ <- estuary_s2_wQ[,':='(
  Qss_rel = SSC_mgL * discharge_rel * 10000
)]

ggplot(estuary_s2_wQ[
  month(date) >= 5 & month(date) <= 10
  & distance_km > 50 & distance_km < 70
  & river == 'Estuary 1'
], aes(x = yday(date), y = SSC_mgL*discharge_rel*400, group = year,
       # ], aes(x = year + month/12, y = B8_median/B2_median,
       color = period, fill = period)) + 
  # stat_summary(geom='ribbon', alpha = 0.4, color = NA) +
  geom_bar(data = climate_taymyr[
    year > 2015
    & month(date) %in% c(6,7,8,9,10)
    # ], aes(x = yday, y = discharge_rel*30000), color = 'blue', stat = 'identity', inherit.aes = F) +
  ], aes(x = yday, y = total_precipitation_sum*30000), color = 'blue', stat = 'identity', inherit.aes = F) +
  stat_summary(geom='line') +
  scale_color_manual(values = c('Post-slump' = 'goldenrod', 'Pre-slump' = 'steelblue')) +
  scale_fill_manual(values = c('Post-slump' = 'goldenrod', 'Pre-slump' = 'steelblue')) +
  facet_wrap(year~river, 
             scales = 'free_y', ncol = 2) +
  # scale_x_continuous(labels = abbrev_year) +
  theme_markdown +
  labs(
    x = 'Year',
    y = 'SSC (mg/L)',
    color = 'Period',
    fill = 'Period'
  )



ggplot(estuary_s2_wQ[
  month(date) >= 7 & month(date) < 10
  & distance_km > 50 & distance_km < 70
], aes(x = discharge_rel, y = Qss_rel, color = period)) +
  # ], aes(x = discharge_rel, y = SSC_mgL, color = factor(year))) +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = year)) +
  scale_color_manual(values = c('Post-slump' = 'goldenrod', 'Pre-slump' = 'steelblue')) +
  # scale_fill_manual(values = c('Post-slump' = 'goldenrod', 'Pre-slump' = 'steelblue')) +
  # facet_wrap((distance_km - distance_km%%5)~river,
  facet_wrap(.~river) +
  # facet_wrap(ifelse(date < ymd('2020-07-15'), 'pre-Slump', 'Post-Slump')~river, 
  #            scales = 'free_y', ncol = 2) +
  scale_x_log10() +
  scale_y_log10() +
  theme_markdown +
  theme(legend.position = 'top') +
  labs(
    x = 'Year',
    y = 'SSC (mg/L)',
    color = 'Period',
    fill = 'Period'
  )