// TRAINING DATA COLLECTION FOR SLUMPS, 2024
// TAYMYR PENINSULA, RUSSIA
// EVAN DETHIER 2025
// Imports Sentinel-2 data and slump polygons
// Makes random points, with each point assigned to a even grid
// Maps a sampling function over the grid
// Labels each sample as slump=1 or nonslump=0

// SET DATE
// Start Date to End Date
var startDate = '2024-01-01'; // Change this to select a date
var endDate = '2024-10-01'; //
var start_yday = 210
var end_yday = 240
// var end_yday = 218

// TRAINING DATA PARAMETERS
// var non_slump_training_n = 200 // for export 400
// var slump_training_n = 75 // for export 100
var n_trees = 10
var non_slump_training_n = 10 // for testing
var slump_training_n = 4 // for testing
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
var taymyr_grid_nums = ee.List(['0475', '0499','0517', '0518', '0685', '0686'])

// Slumps (lat/long) that have been validated as real
var valid_ids = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_peninsula_thaw_slump_metadata')

// Change ID property name for slump assets to align with validation dataset
var getID = function(feature){
    return feature.set('assetID', ee.String('ID').cat(feature.get('label')))
  }
  
// Slump polygons
// filter to only validated slumps
var slump_polys_all = ee.FeatureCollection([
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0475').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0499').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0517').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0518').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0685').map(getID),
  ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/slumps_20250131/slumps_250kgrid_30m_g0686').map(getID)
  ]).flatten()
  
var slump_polys = slump_polys_all  
  .filter(ee.Filter.inList('assetID', valid_ids.aggregate_array('ID')))
  
// Get area of interest bounding the valid slumps
var aoi = valid_ids.geometry()


// // Covering grid to break up analysis
// var grid_taymyr = aoi.bounds().coveringGrid({
//   proj: 'EPSG:3995',
//   scale:20000
// })

var grid_taymyr = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_cover_grid_20km')
          .filterBounds(slump_polys_all)

// Make a painted version of grid
var grid_taymyr_paint = ee.Image(0).byte().paint({
  featureCollection: grid_taymyr,
  color: 1,
  width: 2
}).selfMask()

Map.addLayer(grid_taymyr_paint, {palette: 'white'}, 'grid')
// Remake the aoi to center middle of region
var aoi = grid_taymyr.filterBounds(aoi.centroid()).geometry()
// Minimize area for testing
// var aoi = aoi.centroid().buffer(20000)

// Map.centerObject(ee.Geometry.Point(101.92564758716567,76.06970237495507), 13)

// LABEL POLYGONS
var addSlumpLabel = function(feature){
  return feature.set('slump',1)
}

var addNonSlumpLabel = function(feature){
  return feature.set('slump',0)
}

var valid_ids = valid_ids.map(addSlumpLabel)
// Make random points for sampling

var non_slump = ee.FeatureCollection.randomPoints({
  // region:aoi.geometry().bounds(), 
  region:grid_taymyr, 
  points: 6000, 
  seed: 0
})
.filter(ee.Filter(ee.Filter.bounds(slump_polys)).not())
.map(addNonSlumpLabel)

Map.addLayer(valid_ids, {color:'red'}, 'slump sites')
Map.addLayer(non_slump, {color:'blue'}, 'non slump sites')

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

// COLLECT TRAINING DATA
// Use these bands for classification
// var bands = ['B2','B3','B4','B8','B8A','B11','B12','slope'];
var bands = ['B2','B3','B4','B8','B8A','B11','B12','ndwi','ndvi'];
  
// This property stores the land cover labels as consecutive
// integers starting from zero.
var label = 'slump';


// Apply function to get training data for each grid
var training_all = grid_taymyr.map(function(feature){
  var geometry = feature.geometry()
  // Filter sentinel data to grid
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
                    
  // Overlay the points on the imagery to get training.
// Function for training
var getTraining = function(image){
  var seed = image.date().get('year')
  var points = ee.FeatureCollection([
        valid_ids
          .filterBounds(geometry)
          .filterBounds(image.geometry())
          .randomColumn('random', seed)
            .limit(non_slump_training_n, 'random'),
        non_slump
            .filterBounds(geometry)
            .filterBounds(image.geometry())
            .randomColumn('random', seed)
            .limit(slump_training_n,'random') 
      ]).flatten()
      
  var training = image
        // .clip(aoi)
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

  var training = s2_ic.map(getTraining).flatten()
  
  return training
}).flatten()
  .filter(ee.Filter.gt('B3',0))


print(training_all.limit(20))


Export.table.toAsset({
  collection: training_all,
  description: location_name + '_slump_train_2024_20250604',
  assetId: 'arctic_erosion/' + location_name + '_slump_train_2024_20250604',
})

