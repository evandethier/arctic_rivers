// ML APPROACH FOR IDENTIFYING SLUMPS, 2024
// TAYMYR PENINSULA, RUSSIA
// EVAN DETHIER 2025
// Imports Sentinel-2 data, trains a ml classifier to id slumps
// Applies classifier to all images from a year
// Slumps are verified if they are id'ed in more than 30% of images
// Vectors of slump polygons are exported to asset

// SET DATE
// Start Date to End Date
var startDate = '2024-07-01'; // Change this to select a date
var endDate = '2024-10-01'; //
var start_yday = 210
var end_yday = 240
// var end_yday = 218

// TRAINING DATA PARAMETERS
var non_slump_training_n = 200 // for export 400
var slump_training_n = 75 // for export 100
var n_trees = 10
// var non_slump_training_n = 10 // for testing
// var slump_training_n = 4 // for testing
// var n_trees = 3
// SET LOCATION, BUFFER, ZOOM
// Set buffer distance (include just one or multiple images)
var buffer_dist = 150000;
var export_buffer = 2400; // Change this to widen view for image export
var zoom_sel = 14;
var location_name = 'taymyr_peninsula' // Change this to change image name
var imgName = location_name + '_' + ee.Date(startDate).format('yyyyMMdd').getInfo()
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
// var rivers_buff = rivers.gt(2).selfMask().reproject({'crs': rivers.projection(), 'scale': 200})
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
  
// Slump polygons
// filter to only validated slumps
var slump_polys = ee.FeatureCollection([
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0475'),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0499'),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0517'),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0518'),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0685'),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0686')
  ]).flatten()
  .filter(ee.Filter.inList('label', valid_ids.aggregate_array('label')))
  
print('example filtered slump polys', slump_polys.limit(3))
// Get area of interest bounding the valid slumps
var aoi = valid_ids.geometry()


// // Covering grid to break up analysis
// var grid_taymyr = aoi.bounds().coveringGrid({
//   proj: 'EPSG:3995',
//   scale:20000
// })

var grid_taymyr = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_cover_grid_20km')

// Make a painted version of grid
var grid_taymyr_paint = ee.Image(0).byte().paint({
  featureCollection: grid_taymyr,
  color: 1
}).selfMask()


var getPaint = function(fc){
  return ee.Image(0).byte().paint({
    featureCollection: fc,
  color: 1,
  width: 1
}).selfMask()
}

Map.addLayer(getPaint(grid_taymyr), {palette: 'white'}, 'grid')
Map.addLayer(getPaint(slump_polys), {palette: 'blue'}, 'slumps')
// Remake the aoi to center middle of region
// var aoi = grid_taymyr.filterBounds(aoi.centroid()).geometry()
// Minimize area for testing
// var aoi = aoi.centroid().buffer(20000)

// Map.centerObject(ee.Geometry.Point(101.92564758716567,76.06970237495507), 12)

// IMPORT TRAINING DATA FROM ASSET
var training_data = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_peninsula_slump_train_2024_20250605')
  .map(function(feature){
    var geometry = feature.geometry()
    var lat = geometry.coordinates().get(1)
    var long = geometry.coordinates().get(0)
    
    return feature.setGeometry(null).set('latitude', lat, 'longitude', long)
  })

// OPTIONAL FOR SCALING TO MIN-MAX
// var scaling_params = ee.FeatureCollection([
//   ee.Feature(null).set('feature', 'B2', 'min', 0, 'max', 7500),
//   ee.Feature(null).set('feature', 'B3', 'min', 0, 'max', 7500),
//   ee.Feature(null).set('feature', 'B4', 'min', 0, 'max', 7500),
//   ee.Feature(null).set('feature', 'B8', 'min', 0, 'max', 7500),
//   ee.Feature(null).set('feature', 'B8A', 'min', 0, 'max', 8000),
//   ee.Feature(null).set('feature', 'B11', 'min', 0, 'max', 6500),
//   ee.Feature(null).set('feature', 'B12', 'min', 0, 'max', 5500),
//   ee.Feature(null).set('feature', 'ndvi', 'min', -0.6, 'max', 1),
//   ee.Feature(null).set('feature', 'ndwi', 'min', -0.7, 'max', 0.7)
//   ])
  
// print(scaling_params.filter(ee.Filter.eq('feature', 'b')))


// var training_data = training_data.map(function(feature){
//   var input_dictionary = feature.toDictionary(bands)
//   var scaled_data = input_dictionary.map(function(key,value){
//     var min = scaling_params.filter(ee.Filter.eq('feature', ee.String(key))).first().get('min')
//     var max = scaling_params.filter(ee.Filter.eq('feature', ee.String(key))).first().get('max')
//     var scaled = ee.Number(value).unitScale(min, max)
    
//     return scaled
//   })
//   return feature.set(scaled_data)
// })

print(training_data.limit(3))

// // scaling_params: FeatureCollection with properties 'feature', 'min', 'max'
// // Example row: {feature: 'B2', min: 0.0, max: 7772.0}

// var scaleBands = function(image, scalingParams) {
//   var features = scalingParams.aggregate_array('feature');
//   var mins = scalingParams.aggregate_array('min');
//   var maxs = scalingParams.aggregate_array('max');
//   var scaledNames = features.map(function(name) {
//     return ee.String(name).cat('_scaled');
//   });

//   return image
//     .select(features)
//     .unitScale(mins, maxs)
//     .rename(scaledNames);
// };
  
print(training_data.limit(4))

Export.table.toDrive({
  collection: training_data,
  folder: 'arctic_erosion_imports',
  description: 'taymyr_slump_train_2024_20250605',
  fileNamePrefix: 'taymyr_slump_train_2024_20250605',
  fileFormat: 'CSV'
})

// TRAIN A ML CLASSIFIER
// Use these bands for classification
// var bands = ['B2','B3','B4','B8','B8A','B11','B12','slope'];
var bands = ['B2','B3','B4','B8','B8A','B11','B12','ndwi','ndvi'];
// This property stores the land cover labels as consecutive
// integers starting from zero.
var label = 'slump';


// Train a CART classifier with default parameters.
var classifier = ee.Classifier.smileRandomForest(n_trees).train(training_data, label, bands);



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

// Where the RF classifier is stored
var classifierAssetId = 'projects/ee-edethier/assets/arctic_erosion/taymyr_slump_unscaled_rf';
// Once the classifier export finishes, we can load our saved classifier.
var classifier = ee.Classifier.load(classifierAssetId);

// Make a mask for existing slumps:
// captures only new slump area near previously id'ed slumps
var slump_polys_mask = ee.Image(0).byte().paint({
  featureCollection: slump_polys.map(function(feature){
    return feature.setGeometry(feature.geometry().buffer(500))
  }),
  color: 1
}).selfMask()

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
    featureCollection: slump_polys.filterBounds(geometry).map(function(feature){
      return feature.setGeometry(feature.geometry().buffer(500))
    }),
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
                    .set('geometry_test', geometry_new)
    })
      
      
  
  return classified_slumps

}).flatten()


Export.table.toAsset({
  collection: classification_by_grid,
  description: 'taymyr_slumps_2024_20250620_120px_60m_all',
  assetId: 'arctic_erosion/taymyr_slumps_2024_20250620_120px_60m_all'
})
print('example polygon', classification_by_grid.first())

// // Apply classifier to all images in S2 collection
// // Display the inputs and the results.
// var getClassification = function(image){
  
//     var classified = image
//                         // .addBands(slope.rename('slope')).select(bands)
//                         .updateMask(slump_polys_mask)
//                         .classify(classifier);
//     return image.addBands(classified.rename('class'))
//   }
  
// var s2_ic_class = s2_ic
//             // .limit(3)
//             .map(getClassification).select('class')

// // Map.addLayer(s2_ic_class.count(), {min:0,max:50}, 'class count')
// // Map.addLayer(s2_ic.select('B2').count(), {min:0,max:50}, 's2 ic count')

// // % of images each pixel is classified as slump
// var classified_2024 = s2_ic_class.sum().divide(s2_ic_class.count()).multiply(100).int8()
// // Limit to 30% of images or more to reduce noise
// // Remove river areas
// var classified_2024 = classified_2024
//                           .updateMask(classified_2024.gt(60)) // mask for frequency of classification
//                           .updateMask(rivers_buff.unmask(0).eq(0)) // mask to eliminate rivers

// // print('simple classified image', classified_2024)
// // Connected pixels to distinguish between features
// var classified_connected = classified_2024.gt(60).connectedComponents(ee.Kernel.plus(1), 600)
// // var classified_connected = classified_2024.gt(60).connectedComponents(ee.Kernel.plus(1), 60)
// var classified_area = classified_connected.select('labels').connectedPixelCount(12, true)
// // Only large slumps
// var classified_connected = classified_connected.updateMask(classified_area.gt(10))

// // print('classified connected image', classified_connected)

// var s2_img = s2_ic.median()
// //// MAP IMAGES ////
// // Set Landsat visualization parameters
// var s2_vis = {min:im_vis_min,max:im_vis_max,bands:['B4','B3','B2']};
// var s2_vis = {min:im_vis_min,max:im_vis_max,bands:['B8','B4','B3']};

// Map.addLayer(ee.Image(s2_img), s2_vis, 'sentinel 2 example',0)
// // Map.addLayer(classified.selfMask(),
// //         {min: 0, max: 2, palette: ['orange']},
// //         'classification 2024');
         
// Map.addLayer(classified_2024,
//         {min: 0.3, max: 1, palette: ['black','purple']},
//         'classification 2024 sum',1);






