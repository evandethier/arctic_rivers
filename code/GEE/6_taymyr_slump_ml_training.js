// ML APPROACH FOR IDENTIFYING SLUMPS, 2024
// TAYMYR PENINSULA, RUSSIA
// EVAN DETHIER 2025
// Imports Sentinel-2 data, trains a ml classifier to id slumps
// Applies classifier to all images from a year
// Slumps are verified if they are id'ed in more than 30% of images
// Vectors of slump polygons are exported to asset

// SET DATE
// Start Date to End Date (advance N days, weeks, months from Start Date)
var startDate = '2024-07-05'; // Change this to select a date
var endDate = ee.Date(startDate).advance(45, 'days'); // change the number here to select more or fewer images
var s2_endDate = endDate
var startDate_pre = '2019-07-05';
var s2_endDate_pre = ee.Date(startDate_pre).advance(45, 'days')
// var endDate = ee.Date(startDate).advance(5, 'years'); // for date sliders

// TRAINING DATA PARAMETERS
var non_slump_training_n = 200 // for export 400
var slump_training_n = 75 // for export 100
// var non_slump_training_n = 10 // for testing
// var slump_training_n = 4 // for testing
// SET LOCATION, BUFFER, ZOOM
// Set buffer distance (include just one or multiple images)
var buffer_dist = 150000;
var export_buffer = 2400; // Change this to widen view for image export
var zoom_sel = 14;
var location_name = 'taymyr_peninsula' // Change this to change image name
var imgName = location_name + '_' + ee.Date(startDate).format('yyyyMMdd').getInfo()
var cloud_thresh = 5
var scale_sel_ls = 30
var scale_sel_sen = 10
var landsat_min = 500
var landsat_max = 2100
var cloud_prob_thresh = 50
var cloud_thresh = 50
// var s2_mission = "COPERNICUS/S2_SR_HARMONIZED"
var s2_mission = "COPERNICUS/S2_HARMONIZED"

    
// IMPORT EXTRA DATA
var rivers = ee.Image("MERIT/Hydro/v1_0_1").select('upa')
var rivers_buff = rivers.gt(2).selfMask().focalMax({kernel:ee.Kernel.circle(150, 'meters')})

// REGION 
// Grid nums in Taymyr study: 0475, 0517, 0518, 0685, 0686
var taymyr_grid_nums = ee.List(['0475', '0517', '0518', '0685', '0686'])

var valid_ids = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_peninsula_thaw_slump_metadata')

var getID = function(feature){
    return feature.set('assetID', ee.String('ID').cat(feature.get('label')))
  }
  
var slump_polys = ee.FeatureCollection([
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0475').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0499').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0517').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0518').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0685').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0686').map(getID)
  ]).flatten()
  // .filterBounds(ee.Geometry.Point(102.98123, 76.24531).buffer(1000))
  .filter(ee.Filter.inList('assetID', valid_ids.aggregate_array('ID')))
  
var aoi = valid_ids.geometry()

var addSlumpLabel = function(feature){
  return feature.set('slump',1)
}

var addNonSlumpLabel = function(feature){
  return feature.set('slump',0)
}

var valid_ids = valid_ids.map(addSlumpLabel)
// Make random points for sampling

var non_slump = ee.FeatureCollection.randomPoints({
  region:valid_ids.geometry().bounds(), 
  points: 3000, 
  seed: 0
})
.filter(ee.Filter(ee.Filter.bounds(slump_polys)).not())
.map(addNonSlumpLabel)

// Map.addLayer(non_slump, {color: 'purple'}, 'Non slump')
print(slump_polys.first())


// Map.centerObject(aoi,zoom_sel);
var elevation = ee.Image("UMN/PGC/ArcticDEM/V3/2m_mosaic")
var slope = ee.Terrain.slope(elevation)

var s2 = ee.ImageCollection(s2_mission)

// FILTER IMAGES BY DATE, CLOUD COVER, ETC.
var s2_ic_all = s2
          .filterBounds(aoi)
          .filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than',cloud_thresh)
          .map(function(image){
            return image
                      .addBands(image.normalizedDifference(['B8','B4']).rename('ndvi'))
                      .addBands(image.normalizedDifference(['B8','B3']).rename('ndwi'))
          })
    
          
var s2_ic = s2_ic_all
          .filterDate(startDate, s2_endDate)
          .sort('CLOUDY_PIXEL_PERCENTAGE'
          ,false
          )
          
var s2_ic_2021 = s2_ic_all
          .filterDate('2022-07-15', '2022-08-15')
          .sort('CLOUDY_PIXEL_PERCENTAGE'
          ,false
          )
          
var s2_ic_pre = s2_ic_all
          .filterDate(startDate_pre, s2_endDate_pre)
          .filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than',cloud_thresh)
          .sort('CLOUDY_PIXEL_PERCENTAGE'
          ,false
          )

// ID CLOUD PIXELS
// Determine the direction to project cloud shadow from clouds (assumes UTM projection).
var noClouds_s2 = function(image){
  var shadow_azimuth = ee.Number(90).subtract(ee.Number(image.get('MEAN_SOLAR_AZIMUTH_ANGLE')));
  // var sun_zenith_proj = ee.Number(image.get('MEAN_SOLAR_ZENITH_ANGLE')).divide(12).pow(3).int(); // accomodates cirrus
  var sun_zenith_proj = ee.Number(image.get('MEAN_SOLAR_ZENITH_ANGLE')).divide(3.5).int();
  
  var dark_pixels = image.select('B5').lt(1100).rename('dark_pixels')
  var cloud_prob = image.select('cloud_probability')
  var clouds = ee.Image(cloud_prob).rename('clouds')
                    // .focalMax({radius: 20,units:'meters'})
                    .focalMax()
  // Project shadows from clouds
  var cld_proj = clouds.gt(cloud_prob_thresh).directionalDistanceTransform(shadow_azimuth, sun_zenith_proj)
        .reproject({'crs': image.select(0).projection(), 'scale': 100})
        .select('distance')
        .mask()
        .rename('cloud_transform')
        
  // Identify the intersection of dark pixels with cloud shadow projection.
  var shadows = cld_proj.multiply(dark_pixels).rename('shadows')
    
  var not_clouds = clouds.lte(cloud_prob_thresh).rename('not_clouds')
  var not_shadows = shadows.neq(1).rename('not_shadows')
  
  // Add dark pixels, cloud projection, and identified shadows as image bands.
  return image.addBands(ee.Image([clouds.gt(cloud_prob_thresh), cloud_prob, cld_proj, not_shadows, not_clouds, dark_pixels, shadows]))
    
}

// Cloud masking dataset
// https://medium.com/google-earth/more-accurate-and-flexible-cloud-masking-for-sentinel-2-images-766897a9ba5f
var s2_clouds = ee.ImageCollection('COPERNICUS/S2_CLOUD_PROBABILITY')
          // .filterBounds(trainingSites.filter(ee.Filter.eq('site_no', '000000000000000016ad')).first().geometry())
          // .filterBounds(trainingSites.map(getCentroid))
          .filterBounds(aoi)
          .filterDate(startDate,endDate)
            
// Need to catch when there are no cloud masks
var joinCloudImage = function(image){
  var image_id = image.get('system:index')
  var s2_cloud_sel = s2_clouds
                        .filterBounds(image.geometry())
                        .filter(ee.Filter.eq('system:index', image_id))
                        .first()
  var s2_with_clouds = image.addBands(s2_cloud_sel.rename('cloud_probability'))
return(s2_with_clouds)
}

// // All Sentinel-2 cloud probability image ids
var cloud_prob_img_ids = s2_clouds.aggregate_array('system:index')

print('cloud ids', cloud_prob_img_ids)
// All Sentinel-2 images for training
var s2_ic = s2_ic
    .filter(ee.Filter.inList('system:index', cloud_prob_img_ids))
    .map(joinCloudImage)  // Add cloud image
    .map(noClouds_s2)     // Add cloud bands
print('training images', s2_ic.first())

Map.addLayer(s2_ic.select('B2').count(), {min:0,max:50}, 's2 ic count, with clouds')

// // Mask image for clouds, shadows
var s2_ic = s2_ic    
    .map(function(image){
      return image.updateMask(image.select('not_clouds')
                    .multiply(image.select('not_shadows')))
    })
    
    
// CALCULATE WATER ONLY FROM SENTINEL-2 
// Function to get water only from an image
// For Sentinel 2
var s2_thresh = 0
var waterOnly_s2 = function(image){
  var s2_water = image.normalizedDifference(['B3','B11'])
  // MNDWI index masking
  var return_image = image.addBands(
              s2_water.updateMask(s2_water.gt(s2_thresh))// threshold for MNDWI
                  .updateMask(image.select('B3').add(image.select('B4')).lt(5000)) // threshold for snow
                  .updateMask(image.select('B9').lt(1500)) // for SR
                  // .updateMask(image.select('B9').lt(500)) // for TOA
                  .updateMask(image.select('B11').lt(900)) // May need to remove
                  // .updateMask(slope.unmask(0).lt(slope_thresh))
                  .rename('water')) // rename band 'water'
  return(return_image);        
};

var s2_img = s2_ic.median()
var s2_img_2021 = s2_ic_2021.median()
var s2_img_pre = s2_ic_pre.median()

var s2_water = waterOnly_s2(s2_img).select('water')

var ndsi = s2_img_pre.normalizedDifference(['B8','B3'])


// TRAIN A ML CLASSIFIER
// Use these bands for prediction.
// var bands = ['B2','B3','B4','B8','B8A','B11','B12','slope'];
var bands = ['B2','B3','B4','B8','B8A','B11','B12','ndwi','ndvi'];


var points = ee.FeatureCollection([
  valid_ids.limit(non_slump_training_n),
  non_slump.limit(slump_training_n) 
  ]).flatten()
// This property stores the land cover labels as consecutive
// integers starting from zero.
var label = 'slump';

var test_img = s2_img.addBands(slope.rename('slope'))
var test_img_2021 = s2_img_2021.addBands(slope.rename('slope'))
var test_img_pre = s2_img_pre.addBands(slope.rename('slope'))
// Overlay the points on the imagery to get training.
// Function for training

var getTraining = function(image){
  var training = image
        // .addBands(slope.rename('slope'))
        .select(bands)
        .sampleRegions({
          collection: points,
          properties: [label],
          scale: 30,
          geometries:true
        });

return training
}

// Single-image approach to training
// var training = test_img.select(bands).sampleRegions({
//   collection: points,
//   properties: [label],
//   scale: 30
// });

var training = s2_ic.filter(ee.Filter.calendarRange(210,250,'day_of_year')).map(getTraining).flatten()


Export.table.toAsset({
  collection: training,
  description: location_name + '_slump' + slump_training_n + '_non_slump' + non_slump_training_n,
  assetId: 'arctic_erosion/' + location_name + '_slump' + slump_training_n + '_non_slump' + non_slump_training_n,
})

var training = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_peninsula_slump75_non_slump200')

// Train a CART classifier with default parameters.
var trained = ee.Classifier.smileRandomForest(10).train(training, label, bands);

// Classify the image with the same bands used for training.
var classified = test_img.select(bands).classify(trained);
var classified_2021 = test_img_2021.select(bands).classify(trained);
var classified_pre = test_img_pre.select(bands).classify(trained);

var test_aoi = ee.Geometry.Point(102.3167, 76.0134)
var slump_polys_mask = ee.Image(0).byte().paint({
  featureCollection: slump_polys.map(function(feature){
    return feature.setGeometry(feature.geometry().buffer(500))
  }),
  color: 1
}).selfMask()


// Display the inputs and the results.
var getClassification = function(image){
  
  // var image = noClouds_s2(image)
  var classified = image
                      // .addBands(slope.rename('slope')).select(bands)
                      .updateMask(slump_polys_mask)
                      .classify(trained);
  
  // var classified = classified.updateMask(image.select('not_clouds')
  //                   .multiply(image.select('not_shadows')))
                    
  return image.addBands(classified.rename('class'))
}

// Apply classifier to all images in S2 collection
var s2_ic_class = s2_ic.map(getClassification).select('class')

// Map.addLayer(s2_ic_class.count(), {min:0,max:50}, 'class count')
// Map.addLayer(s2_ic.select('B2').count(), {min:0,max:50}, 's2 ic count')

// % of images each pixel is classified as slump
var classified_2024 = s2_ic_class.sum().divide(s2_ic_class.count())
// Limit to 30% of images or more to reduce noise
// Remove river areas
var classified_2024 = classified_2024
                          .updateMask(classified_2024.gt(0.6)) // mask for frequency of classification
                          .updateMask(rivers_buff.unmask(0).eq(0)) // mask to eliminate rivers

print('simple classified image', classified_2024)
// Connected pixels to distinguish between features
var classified_connected = classified_2024.gt(0.6).connectedComponents(ee.Kernel.plus(1), 600)
var classified_area = classified_connected.select('labels').connectedPixelCount(12, true)
// Only large slumps
var classified_connected = classified_connected.updateMask(classified_area.gt(10))

print('classified connected image', classified_connected)
Map.addLayer(aoi.bounds(), {}, 'aoi bounds')
print('aoi km2', aoi.bounds().area(1000))

Export.image.toAsset({
  image: classified_connected.select('labels'),
  region: aoi.bounds(),
  description: 'taymyr_connected_2024_20250601',
  assetId: 'arctic_erosion/taymyr_slumps_imgs/' + 'taymyr_connected_2024_20250601',
  scale: 10,
  maxPixels: 1e13
})
// Convert to vectors
var classified_slumps = classified_connected.select('labels').reduceToVectors({
  geometry: aoi.bounds(),
  scale: 30,
  maxPixels: 1e12
})

// Simplify polygons by only selecting the outer geometry
var classified_slumps = classified_slumps.map(function(feature){
      var geometry_new = ee.Geometry.Polygon(feature.geometry().coordinates().get(0))
      return feature
                .setGeometry(geometry_new)
                .set('geometry_test', geometry_new)
})


Export.table.toAsset({
  collection: classified_slumps,
  description: 'taymyr_slumps_2024_20250530',
  assetId: 'arctic_erosion/taymyr_slumps_2024_20250530'
})


// PRINT RESULTS 
// Print dates of images within date range
print('s2 dates', s2_ic.aggregate_array('GRANULE_ID'))
print('sentinel 2 example', s2_img)
//// MAP IMAGES ////
// Set Landsat visualization parameters
var s2_vis = {min:landsat_min,max:landsat_max,bands:['B4','B3','B2']};
var s2_vis = {min:landsat_min,max:landsat_max,bands:['B8','B4','B3']};

// center AOI
Map.addLayer(aoi.centroid().buffer(200),{color:'blue'},'aoi',0);
Map.addLayer(ee.Image(s2_img), s2_vis, 'sentinel 2 example')
Map.addLayer(ee.Image(s2_img_2021), s2_vis, 'sentinel 2 2021',0)
Map.addLayer(ee.Image(s2_img_pre), s2_vis, 'sentinel 2 pre',0)
// Map.addLayer(classified.selfMask(),
//         {min: 0, max: 2, palette: ['orange']},
//         'classification 2024');
         
Map.addLayer(classified_2024,
         {min: 0.3, max: 1, palette: ['black','orange']},
         'classification 2024 sum',0);
// Map.addLayer(classified_2021.selfMask(),
//         {min: 0, max: 2, palette: ['green']},
//         'classification 2021')
// Map.addLayer(classified_pre.selfMask(),
//         {min: 0, max: 2, palette: ['blue']},
//         'classification 2019');

// Map.addLayer(classified_connected.randomVisualizer(), {}, '2024 components')
// Map.addLayer(classified_area, {min:0,max:7,palette:['purple','white']}, '2024 count',1)
Map.addLayer(classified_connected, {}, '2024 components',0)
Map.addLayer(classified_slumps, {color:'blue'}, '2024 vectors')

Map.addLayer(slump_polys, {color: 'yellow'}, 'slumps', 0)
// Map.addLayer(valid_ids, {color: 'red'}, 'slumps valid')
// Map.addLayer(cincia_sites, {color: 'red'}, 'Cincia Ponds')
// Map.addLayer(ndsi, {min: -1, max: 1}, 'ndsi')
// Map.addLayer(ndsi.lt(0.2).multiply(s2_img_pre.select('B4').gt(2000)).selfMask(), {min: 0, max: 1, palette: 'blue'}, 'snow')

// S2 image export
Export.image.toDrive({
  image: s2_img.visualize(s2_vis), 
  folder: 'satellite-visualization/s2_turbid_arctic_lakes',
  description: imgName + '_s2_composite',
  fileNamePrefix: imgName + '_s2_composite',
  region: aoi.centroid().buffer(export_buffer).bounds(), 
  crs: 'EPSG:3857',
  scale: scale_sel_sen, 
  maxPixels: 1e13
  })
      