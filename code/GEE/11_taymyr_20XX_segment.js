// ML APPROACH FOR IDENTIFYING SLUMPS, 2024
// TAYMYR PENINSULA, RUSSIA
// EVAN DETHIER 2025
// Imports Sentinel-2 data
// Applies classifier to all images from a year
// Slumps are verified if they are id'ed in more than 60% of images
// Vectors of slump polygons are exported to asset

// SET DATE
// Start Date to End Date
var year_sel = 2023
var startDate = ee.Date.fromYMD(year_sel, 7, 1)
var endDate = ee.Date.fromYMD(year_sel, 10, 1)
var start_yday = 210
var end_yday = 240
// var end_yday = 218

// SET LOCATION, BUFFER, ZOOM
// Set buffer distance (include just one or multiple images)
var buffer_dist = 150000;
var export_buffer = 2400; // Change this to widen view for image export
var zoom_sel = 14;
var location_name = 'taymyr_peninsula' // Change this to change image name
var imgName = location_name + '_' + ee.Date(startDate).format('yyyyMMdd').getInfo()
var cloud_thresh = 30
var scale_sel_ls = 30
var scale_sel_sen = 10
var im_vis_min = 500
var im_vis_max = 2100
var im_vis_bands = ['B8','B4','B3']
var bands = ['B2','B3','B4','B8','B8A','B11','B12','ndvi', 'ndwi'];
var s2_mission = "COPERNICUS/S2_HARMONIZED"
var cloud_prob_thresh = 50
// IMPORT EXTRA DATA
var rivers = ee.Image("MERIT/Hydro/v1_0_1").select('upa')
var rivers_buff = rivers.gt(2).selfMask().focalMax({kernel:ee.Kernel.circle(150, 'meters')})
var elevation = ee.Image("MERIT/DEM/v1_0_3")
var slope = ee.Terrain.slope(elevation)

// REGION 
// Grid nums in Taymyr study: 0475, 0517, 0518, 0685, 0686
var taymyr_grid_nums = ee.List(['0475', '0499','0517', '0518', '0685', '0686',
                                '475_', '499_','517_', '518_', '685_', '686_',
])

// var taymyr_grid_nums = ee.List(['0686'])
// Slumps (lat/long) that have been validated as real
// var valid_ids = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_peninsula_thaw_slump_metadata')
var valid_ids = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/global_trendvalidated_slumps')
                    .filter(ee.Filter.inList('grid_num',taymyr_grid_nums))
                    .filter(ee.Filter.eq('trend', 'significant'))

print('first validated slump', valid_ids.limit(3))
// Change ID property name for slump assets to align with validation dataset
var getID = function(feature){
    return feature.set('assetID', ee.String('ID').cat(feature.get('label')))
  }
  
// filter to only validated slumps
var slump_polys = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2024_20250620_120px_60m_all')

  
// Get area of interest bounding the valid slumps
var aoi = valid_ids.geometry()


// // Covering grid to break up analysis
var grid_taymyr = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_cover_grid_20km')

// Make a painted version of grid
var grid_taymyr_paint = ee.Image(0).byte().paint({
  featureCollection: grid_taymyr,
  color: 1
}).selfMask()

Map.centerObject(ee.Geometry.Point(101.92564758716567,76.06970237495507), 12)

// --------- // IMPORT AND APPLY SAVED CLASSIFIER // --------- //
// IMPORT IMAGES
// First, import data to improve image quality
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

var s2_ic = ee.ImageCollection(s2_mission)
          .filterBounds(aoi)
          .filterDate(startDate,endDate)
          .filter(ee.Filter.calendarRange(start_yday, end_yday, 'day_of_year'))
          .filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than',cloud_thresh)
          .select(['B2','B3','B4','B5','B8','B8A','B11','B12'])
          .map(function(image){
            return image
                      .addBands(ee.Image([
                        image.normalizedDifference(['B8','B4']).rename('ndvi'),
                        image.normalizedDifference(['B8','B3']).rename('ndwi')
                        ]))
          })
                    .map(maskClouds)

// Where the RF classifier is stored
var classifierAssetId = 'projects/ee-edethier/assets/arctic_erosion/taymyr_slump_unscaled_rf';
// Once the classifier export finishes, we can load our saved classifier.
var classifier = ee.Classifier.load(classifierAssetId);

print('grid filtered to aoi', grid_taymyr.filterBounds(aoi))
// Classify by grid cell
var classification_by_grid = grid_taymyr
  .filterBounds(aoi)
  // .limit(3)
  .map(function(feature){
    // Get grid geometry
    var geometry = feature.geometry()
    
    // Limit pre-idenitied slumps to grid cell
    var slump_polys_mask = ee.Image(0).byte().paint({
    featureCollection: slump_polys.filterBounds(geometry),
    color: 1
  }).selfMask()
  
  // Filter sentinel data to grid cell
  // & Mask clouds
  var s2_ic = ee.ImageCollection(s2_mission)
          .filterBounds(geometry)
          .filterDate(startDate,endDate)
          // .filter(ee.Filter.calendarRange(year,year,'year'))
          .filter(ee.Filter.calendarRange(start_yday, end_yday, 'day_of_year'))
          .filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than',cloud_thresh)
          .select(['B2','B3','B4','B5','B8','B8A','B11','B12'])
          .map(function(image){
            return image
                      .addBands(ee.Image([
                        image.normalizedDifference(['B8','B4']).rename('ndvi'),
                        image.normalizedDifference(['B8','B3']).rename('ndwi')
                        ]))
          })
                    .map(maskClouds)
            
    // Display the inputs and the results.
    var getClassification = function(image){
    
      var classified = image
                          // .addBands(slope.rename('slope')).select(bands)
                          .updateMask(slump_polys_mask)
                          .classify(classifier);
      return classified
    }
    
    // Classify all pixels in the image collection as slump/non-slump
    var classified = s2_ic.map(getClassification)
    
    
    
        // Convert slump pixels to polygons
    // Based on getting classified as slump in X% of images
    // % of images each pixel is classified as slump
    var class_percent = classified.sum().divide(classified.count()).multiply(100).int8()
    // Limit to 30% of images or more to reduce noise
    // Remove river areas
    var class_percent = class_percent
                            .updateMask(class_percent.gt(60)) // mask for frequency of classification
                            .updateMask(rivers_buff.unmask(0).eq(0)) // mask to eliminate rivers
    // print('simple classified image', classified_2024)
    // Connected pixels to distinguish between features
    var classified_connected = class_percent.gt(60).connectedComponents(ee.Kernel.plus(1), 120)
    var classified_area = classified_connected.select('labels').connectedPixelCount(12, true)
    // Only large slumps
    var classified_connected = classified_connected.updateMask(classified_area.gt(10)) 
    
    // Convert to vectors
    var classified_slumps = classified_connected.select('labels').reduceToVectors({
      geometry: geometry,
      scale: 60,
      maxPixels: 1e12
    })
  
    // Simplify polygons by only selecting the outer geometry
    var classified_slumps = classified_slumps.map(function(feature){
          var geometry_new = ee.Geometry.Polygon(feature.geometry().coordinates().get(0))
          return feature
                    .setGeometry(geometry_new)
                    // .set('geometry_test', geometry_new)
    })
      
      
  
  return classified_slumps

}).flatten()

Export.table.toAsset({
  collection: classification_by_grid,
  description: 'taymyr_slumps_' + year_sel + '_20250620',
  assetId: 'arctic_erosion/taymyr_slumps_' + year_sel + '_20250620',
})
print('example polygon', classification_by_grid.first())

var getPaint = function(fc){
  return ee.Image(0).byte().paint({
    featureCollection: fc,
  color: 1,
  width: 1
}).selfMask()
}

var s2_ic = ee.ImageCollection(s2_mission)
          .filterBounds(aoi.centroid())
          .filterDate(startDate,endDate)
          // .filter(ee.Filter.calendarRange(start_yday, end_yday, 'day_of_year'))
          .filter(ee.Filter.calendarRange(ee.Number(end_yday).subtract(25), end_yday, 'day_of_year'))
          .filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than',cloud_thresh)

Map.addLayer(s2_ic.first(), {min: im_vis_min, max: im_vis_max, bands: im_vis_bands}, 's2 img')          
Map.addLayer(ee.Image(0).byte().paint({
  featureCollection: grid_taymyr,
  color: 1,
  width: 1
}).selfMask(), {palette: 'white'}, 'grid')
Map.addLayer(getPaint(slump_polys), {palette: 'white'}, 'slumps 2024')
Map.addLayer(getPaint(classification_by_grid), {palette: 'pink'}, 'slumps 2023')