// USE EXISTING ML-GENERATED SLUMP POLYGONS TO CREATE SLUMP EROSION TIMESERIES
// EVAN DETHIER 2025

// SET DATE
// Set Date range 
// Start Date to End Date (advance N days, weeks, months from Start Date)
var startDate = '2024-07-15'; // Change this to select a date
var endDate = ee.Date(startDate).advance(30, 'days'); // change the number here to select more or fewer images
var s2_endDate = endDate.advance(30, 'days')
var startDate_pre = '2019-07-15';
var s2_endDate_pre = ee.Date(startDate_pre).advance(30, 'days')

// SET LOCATION, BUFFER, ZOOM
// Set buffer distance (include just one or multiple images)
var buffer_dist = 150000;
var export_buffer = 2400; // Change this to widen view for image export
var zoom_sel = 14;
var location_name = 'taymyr_peninsula' // Change this to change image name
var imgName = location_name + '_' + ee.Date(startDate).format('yyyyMMdd').getInfo()
var cloud_thresh = 10
var scale_sel_ls = 30
var scale_sel_sen = 10
var landsat_min = 500
var landsat_max = 2100
var s2_mission = "COPERNICUS/S2_HARMONIZED"

// IMPORT EXTRA DATA
var rivers = ee.Image("MERIT/Hydro/v1_0_1").select('upa')
var rivers_buff = rivers.gt(2).selfMask().focalMax({kernel:ee.Kernel.circle(150, 'meters')})
var elevation = ee.Image("UMN/PGC/ArcticDEM/V3/2m_mosaic")
var elevation = ee.Image("MERIT/DEM/v1_0_3")
var slope = ee.Terrain.slope(elevation)

// REGION 
// Grid nums in Taymyr study: 0475, 0517, 0518, 0685, 0686
var taymyr_grid_nums = ee.List(['0475', '0517', '0518', '0685', '0686'])

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

// ADD LABELS
// Labels for slumps
var addSlumpLabel = function(feature){
  return feature.set('slump',1)
}

// Label valid slumps
var valid_ids = valid_ids.map(addSlumpLabel)

// Labels for non-slumps
var addNonSlumpLabel = function(feature){
  return feature.set('slump',0)
}

// Import slump polygons for 2024 at Taymyr
var slumps_2024 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2024_20250529')
var slumps_2023 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2023_20250529')
var slumps_2022 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2022_20250529')
var slumps_2021 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2021_20250529')
var slumps_2020 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2020_20250529')
var slumps_2016 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2016_20250529')
var slumps_2017 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2017_20250529')
var slumps_2018 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2018_20250529')
var slumps_2019 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2019_20250529')


// MODEL TRAINING LOCATIONS
// Make random points for sampling
// And label as non-slump
// (exclude random points that intersect slumps)
var non_slump = ee.FeatureCollection.randomPoints({
  region:valid_ids.geometry().bounds(), 
  points: 2000, 
  seed: 0
})
.filter(ee.Filter(ee.Filter.bounds(slump_polys)).not()) 
.map(addNonSlumpLabel)

// Map.addLayer(non_slump, {color: 'purple'}, 'Non slump')
print(slump_polys.first())

// Map.centerObject(aoi,zoom_sel);

// IMPORT IMAGERY
var s2 = ee.ImageCollection("COPERNICUS/S2")
var s1 = ee.ImageCollection("COPERNICUS/S1_GRD")
var s2 = ee.ImageCollection(s2_mission)

// Import Sentinel-2 data
// filter to location, cloud percentage
// Add NDWI and NDVI for slump ID
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
          .filterDate('2021-07-15', '2021-08-15')
          .sort('CLOUDY_PIXEL_PERCENTAGE'
          ,false
          )
          
var s2_ic_pre = s2_ic_all
          .filterDate(startDate_pre, s2_endDate_pre)
          .filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than',cloud_thresh)
          .sort('CLOUDY_PIXEL_PERCENTAGE'
          ,false
          )


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

// Select first image in Sentinel collections
var s2_img = s2_ic.median()
var s2_img_2021 = s2_ic_2021.median()
var s2_img_pre = s2_ic_pre.median()

var s2_water = waterOnly_s2(s2_img).select('water')

// TRAIN A MODEL FOR SLUMP ID
// Use these bands for prediction.
// var bands = ['B2','B3','B4','B8','B8A','B11','B12','slope','ndvi'];
var bands = ['B2','B3','B4','B8','B8A','B11','B12','ndvi', 'ndwi'];

// Create training data from existing slump points
var points = ee.FeatureCollection([
  // valid_ids.limit(50),
  // non_slump.limit(200) // for live viewing
  valid_ids.limit(100),
  non_slump.limit(400) // for export
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
        .addBands(slope.rename('slope'))
        .select(bands)
        .sampleRegions({
          collection: points,
          properties: [label],
          scale: 30});

return training
}


var training = s2_ic.filter(ee.Filter.calendarRange(205,220,'day_of_year')).map(getTraining).flatten()

// var training = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_peninsula_slump75_non_slump200')

// Train a CART classifier with default parameters.
var trained = ee.Classifier.smileRandomForest(10).train(training, label, bands);

// Classify the image with the same bands used for training.
var classified = test_img.select(bands).classify(trained);
var classified_2021 = test_img_2021.select(bands).classify(trained);
var classified_pre = test_img_pre.select(bands).classify(trained);

var test_aoi = ee.Geometry.Point(102.3167, 76.0134)
var slump_polys_mask = ee.Image(0).byte().paint({
  featureCollection: slumps_2024
    // .map(function(feature){
    //   return feature.setGeometry(feature.geometry().buffer(500))
    // })
    // .filterBounds(test_aoi.buffer(500))
  ,
  color: 1
}).selfMask()


var getPaint = function(fc){
  return ee.Image(0).byte().paint({
    featureCollection: fc,
  color: 1,
  width: 1
}).selfMask()
}

// Display the inputs and the results.
var getClassification = function(image){
  
  var classified = image.addBands(slope.rename('slope')).select(bands)
                        .updateMask(slump_polys_mask)
                        // .clipToCollection(slumps_2024)
                        .classify(trained);
  
  return image.addBands(classified.rename('class'))
}


var year_list = ee.List.sequence(2016,2024,1)

var classified_by_yr = ee.ImageCollection(
  year_list.map(function(year){
    var images_sel = s2_ic_all
                        .filter(ee.Filter.calendarRange(year, year, 'year'))
                        .filter(ee.Filter.calendarRange(215,250,'day_of_year'))
    
    // Classify every image, then filter to only frequently classified pixels
    // var classification = images_sel.map(getClassification).select('class')
    // var classified_sel = classification.sum()
    //                           .divide(classification.count())
                                   
    // var classified_sel = classified_sel.updateMask(classified_sel.gt(0.2))
    
    // Classify based on median annual image
    var classified_sel = getClassification(images_sel.median()).select('class')
    
    return ee.Image(classified_sel.set('year', ee.Number(year)))
  })
  )

// print('first classed img', classified_by_yr.filter(ee.Filter.eq('year', 2024)).first().gte(0.3))
var getConnected = function(year){
  var year_num = ee.Number(year)
  var image = classified_by_yr.filter(ee.Filter.eq('year', year_num)).first()
                              .gte(0.3)
                              .selfMask()
                              
  var classified_connected = image.gt(0.3).connectedComponents(ee.Kernel.plus(1), 6000)
  var classified_area = classified_connected.select('labels').connectedPixelCount(12, true)

  var classified_connected = classified_connected.updateMask(classified_area.gt(10))

  // return classified_connected.set('year', year_num)
  return image.set('year', year)
}

var connected_by_yr = ee.ImageCollection(year_list.map(getConnected))

print('first connected img', connected_by_yr.first())

var getAnnualSlumpVectors = function(year){
  var year_num = ee.Number(year)
  var image = ee.Image(connected_by_yr.filter(ee.Filter.eq('year', year_num)).first())
                              
  var classified_slumps = image.select('class').reduceToVectors({
    geometry: aoi.bounds(),
    scale: 30,
    maxPixels: 1e12
  }).map(function(feature){
    return feature.set('year', year_num)
  }).map(function(feature){
      var geometry_new = ee.Geometry.Polygon(feature.geometry().coordinates().get(0))
      
      return feature
                .setGeometry(geometry_new)
                .set('geometry_test', geometry_new)
    })
  
  return classified_slumps
}

var year_sel = 2016
var classified_slumps_sel = ee.FeatureCollection(getAnnualSlumpVectors(year_sel))
Export.table.toAsset({
  collection: classified_slumps_sel,
  description: 'taymyr_slumps_' + year_sel + '_20250529',
  assetId: 'arctic_erosion/taymyr_slumps_' + year_sel + '_20250529'
})

var videoArgs_zoom = {
  region: aoi.centroid().buffer(10000),
  min: -20, 
  max:1, 
  gamma: 1.77,
  bands:['VV'],
  // bands:['HV'],
  scale: 90,
  framesPerSecond: 7,
};

// print('zoomed in gif', s1_video_lc.getVideoThumbURL(videoArgs_zoom));
// print('video dates', s1_video_lc
//                         .aggregate_array('system:time_start')
//                             .map(function(date){
//                                 return(ee.Date(date).format('yyyy-MM-dd'))
//                           }))
// PRINT RESULTS 

    // Print dates of images within date range
    // print('ls5 dates', ls5_ic.aggregate_array('SENSING_TIME'));
    
    // print('ls7 dates', ls7_ic.map(function(image){
    //   return(image.set('date',image.date().format('YYYY-MM-dd')))
    // }).aggregate_array('date'));
    
    // print('ls8 dates', ls8_ic.map(function(image){
    //   return(image.set('date',image.date().format('YYYY-MM-dd')))
    // }).aggregate_array('date'));
    
    // print('ls9 dates', ls9_ic);
    // print('s1 images', s1_ic)
    // print('s2 dates', s2_ic.aggregate_array('GRANULE_ID'))
    // print('sentinel 2 example', s2_img)
//// MAP IMAGES ////
// Set Landsat visualization parameters
var landsat_vis = {min:landsat_min,max:landsat_max,bands:['B3','B2','B1']};
var s2_vis = {min:landsat_min,max:landsat_max,bands:['B4','B3','B2']};
var s2_vis = {min:landsat_min,max:landsat_max,bands:['B8','B4','B3']};
// center AOI
Map.addLayer(aoi.centroid().buffer(200),{color:'blue'},'aoi',0);
Map.addLayer(ee.Image(s2_img), s2_vis, 'sentinel 2 example')
Map.addLayer(ee.Image(s2_img).normalizedDifference(['B8','B4']), {min:0,max:0.8}, 'sentinel 2 ndvi')
Map.addLayer(ee.Image(s2_img).normalizedDifference(['B8','B3']), {min:0.6,max:0}, 'sentinel 2 ndwi')
Map.addLayer(ee.Image(s2_img_2021), s2_vis, 'sentinel 2 2021',0)
Map.addLayer(ee.Image(s2_img_pre), s2_vis, 'sentinel 2 pre',0)

         
Map.addLayer(connected_by_yr.filter(ee.Filter.eq('year', 2024)).first(),
         {min: 0.3, max: 1, palette: ['#01665e']},
         'classification 2024 sum',0);

Map.addLayer(connected_by_yr.filter(ee.Filter.eq('year', 2023)).first(),
         {min: 0.3, max: 1, palette: ['#5ab4ac']},
         'classification 2023 sum',0);
         
Map.addLayer(connected_by_yr.filter(ee.Filter.eq('year', 2022)).first(),
         {min: 0.3, max: 1, palette: ['#c7eae5']},
         'classification 2022 sum',0);

Map.addLayer(connected_by_yr.filter(ee.Filter.eq('year', 2021)).first(),
         {min: 0.3, max: 1, palette: ['#f6e8c3']},
         'classification 2021 sum',0);
         
Map.addLayer(connected_by_yr.filter(ee.Filter.eq('year', 2020)).first(),
         {min: 0.3, max: 1, palette: ['#d8b365']},
         'classification 2020 sum',0);
         
Map.addLayer(connected_by_yr.filter(ee.Filter.eq('year', 2019)).first(),
         {min: 0.3, max: 1, palette: ['#8c510a']},
         'classification 2019 sum',0);

         
Map.addLayer(connected_by_yr.filter(ee.Filter.eq('year', 2016)).first(),
         {min: 0.3, max: 1, palette: ['black']},
         'classification 2016 sum',0);

// Map.addLayer(rivers_buff.unmask(0).eq(0), {min:0, max: 1}, 'upa')

    
// Map.addLayer(classified_2021.selfMask(),
//         {min: 0, max: 2, palette: ['green']},
//         'classification 2021')
// Map.addLayer(classified_pre.selfMask(),
//         {min: 0, max: 2, palette: ['blue']},
//         'classification 2019');

// Map.addLayer(classified_connected.randomVisualizer(), {}, '2024 components')
// Map.addLayer(classified_area, {min:0,max:7,palette:['purple','white']}, '2024 count',1)
// Map.addLayer(classified_connected, {}, '2024 components',0)
// Map.addLayer(classified_slumps, {color:'blue'}, '2024 vectors')
Map.addLayer(slumps_2024, {color:'orange'}, '2024 vectors pre', 0)
Map.addLayer(getPaint(slumps_2023), {palette:'#5ab4ac'}, '2023 slump img')
Map.addLayer(getPaint(slumps_2022), {palette:'#c7eae5'}, '2022 slump img')
Map.addLayer(getPaint(slumps_2021), {palette:'#f6e8c3'}, '2021 slump img')
Map.addLayer(getPaint(slumps_2020), {palette:'#d8b365'}, '2020 slump img')
Map.addLayer(getPaint(slumps_2019), {palette:'#8c510a'}, '2019 slump img')
Map.addLayer(getPaint(slumps_2018), {palette:'navy'}, '2018 slump img')
Map.addLayer(getPaint(slumps_2017), {palette:'black'}, '2017 slump img')
Map.addLayer(getPaint(slumps_2016), {palette:'black'}, '2016 slump img')
// var slumps_2024 = slumps_2024.map(function(feature){
//   var geometry_new = ee.Geometry.Polygon(feature.geometry().coordinates().get(0))
  
//   return feature
//             .setGeometry(geometry_new)
//             .set('geometry_test', geometry_new)
// })

print('slump geom test', slumps_2024.limit(3))
var slumps_2024_img = ee.Image(0).byte().paint({
  featureCollection:slumps_2024,
  color:1,
  width:1
})
.selfMask()

var slumps_2024_img_slp = ee.Image(0).byte().paint({
  featureCollection:slumps_2024,
  color:1
})
.selfMask()
.updateMask(slope.gt(2))

// Map.centerObject(slumps_2024.first(), 12)
print(slumps_2024_img)
Map.addLayer(slumps_2024_img, {palette:'#01665e'}, '2024 vectors img')
// Map.addLayer(slumps_2024_img_slp, {palette:'pink'}, '2024 vectors img masked slp')
// Map.addLayer(slumps_2024, {color:'blue'}, '2024 vectors', 0)

Map.addLayer(slump_polys, {color: 'yellow'}, 'slumps', 0)
// Map.addLayer(valid_ids, {color: 'red'}, 'slumps valid')
