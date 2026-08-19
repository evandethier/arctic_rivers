// GENERATE BEST-PIXEL ANNUAL S2 IMAGES 
// FOR TAYMYR PENINSULA THAW SLUMP REGION
// Useful for ML segmentation
// EVAN DETHIER 2025

// SET DATE
// Set Date range 
// Start Date to End Date (advance N days, weeks, months from Start Date)
var startDate = '2020-01-01'; // Change this to select a date
var endDate = '2024-10-01'; //
var start_yday = 210
var end_yday = 240

// SET LOCATION, BUFFER, ZOOM
var zoom_sel = 14;
var cloud_thresh = 18
var scale_sel_ls = 30
var scale_sel_sen = 10
var im_vis_min = 500
var im_vis_max = 2100
var im_vis_bands = ['B8','B4','B3']
var bands = ['B2','B3','B4','B8','B8A','B11','B12','ndvi', 'ndwi'];
var s2_mission = "COPERNICUS/S2_HARMONIZED"
var cloud_prob_thresh = 50
var cloud_thresh = 50

// IMPORT EXTRA DATA
var rivers = ee.Image("MERIT/Hydro/v1_0_1").select('upa')
var rivers_buff = rivers.gt(2).selfMask().focalMax({kernel:ee.Kernel.circle(150, 'meters')})
var elevation = ee.Image("MERIT/DEM/v1_0_3")
var slope = ee.Terrain.slope(elevation)

// REGION 
// Grid nums in Taymyr study: 0475, 0517, 0518, 0685, 0686
var taymyr_grid_nums = ee.List(['0475', '0499','0517', '0518', '0685', '0686'])

// Slumps (lat/long) that have been validated as real
var valid_ids = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_peninsula_thaw_slump_metadata')

// Change ID property name for slump assets to align with validation dataset
var getID = function(feature){
    return feature.set('assetID', ee.String('ID').cat(feature.get('label')))
  }
  
// Slump polygons
// filter to only validated slumps
var slump_polys = ee.FeatureCollection([
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0475').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0499').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0517').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0518').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0685').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0686').map(getID)
  ]).flatten()
  .filter(ee.Filter.inList('assetID', valid_ids.aggregate_array('ID')))
  
// Get area of interest bounding the valid slumps
var aoi = valid_ids.geometry()

var grid_taymyr = aoi.bounds().coveringGrid({
  proj: 'EPSG:3995',
  scale:20000
})

var grid_taymyr_paint = ee.Image(0).byte().paint({
  featureCollection: grid_taymyr,
  color: 1
}).selfMask()

var aoi = grid_taymyr.filterBounds(aoi.centroid()).geometry()
// Minimize area for testing
// var aoi = aoi.centroid().buffer(20000)

// Add aoi to map and zoom to it
Map.addLayer(aoi, {}, 'AOI')
// Map.centerObject(ee.Geometry.Point(101.92564758716567,76.06970237495507), 13)

// // Cloud masking dataset
// // https://medium.com/google-earth/more-accurate-and-flexible-cloud-masking-for-sentinel-2-images-766897a9ba5f
var maskClouds = function(image){
  var image_id = image.get('system:index')
  var clouds = ee.ImageCollection('COPERNICUS/S2_CLOUD_PROBABILITY')
                        .filterBounds(image.geometry().centroid())
                        .filterDate(image.date().advance(-1,'day'),image.date().advance(1,'day'))
                        .filter(ee.Filter.eq('system:index', image_id))
                        .first()
    
  var not_clouds = clouds.lte(cloud_prob_thresh).rename('not_clouds')
                          .reproject({'crs': image.select(0).projection(), 'scale': 2000})

  return image.updateMask(not_clouds)
}
    
// GET ANNUAL S2 IMAGE MEDIAN
var getAnnualImage = function(year){
  var median = ee.ImageCollection(s2_mission)
          .filterBounds(aoi)
          .filter(ee.Filter.calendarRange(year,year,'year'))
          .filter(ee.Filter.calendarRange(start_yday, end_yday, 'day_of_year'))
          .filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than',cloud_thresh)
          .filterDate(startDate, endDate)
          .select(['B2','B3','B4','B5','B8','B8A','B11','B12'])
          .map(function(image){
            return image
                      .addBands(image.normalizedDifference(['B8','B4']).rename('ndvi'))
                      .addBands(image.normalizedDifference(['B8','B3']).rename('ndwi'))
          })
                    .map(maskClouds)
                    .median()
  
  return median.set('year', year)
}


var year_list = ee.List.sequence(2016,2024,1)

var annual_images = ee.ImageCollection(year_list.map(getAnnualImage))


// MAP IMAGES
var s2_vis = {min:im_vis_min, max: im_vis_max, bands:im_vis_bands}
Map.addLayer(annual_images.filter(ee.Filter.eq('year', 2016)).first(), s2_vis, '2016 image',0)
Map.addLayer(annual_images.filter(ee.Filter.eq('year', 2019)).first(), s2_vis, '2019 image',0)
Map.addLayer(annual_images.filter(ee.Filter.eq('year', 2020)).first(), s2_vis, '2020 image',0)
Map.addLayer(annual_images.filter(ee.Filter.eq('year', 2021)).first(), s2_vis, '2021 image')
Map.addLayer(annual_images.filter(ee.Filter.eq('year', 2024)).first(), s2_vis, '2024 image')