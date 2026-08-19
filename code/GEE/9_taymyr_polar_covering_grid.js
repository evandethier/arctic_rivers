// CREATE COVERING GRID FOR TAYMYR SLUMPS
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

var grid_taymyr = aoi.bounds().coveringGrid({
  proj: 'EPSG:3995',
  scale:20000
})

Map.addLayer(grid_taymyr.limit(10))
var grid_taymyr_paint = ee.Image(0).byte().paint({
  featureCollection: grid_taymyr,
  color: 1,
  width: 2
}).selfMask()

Export.table.toAsset({
  collection:grid_taymyr,
  description: 'taymyr_cover_grid',
  assetId: 'arctic_erosion/taymyr_cover_grid_20km'
})
Map.addLayer(grid_taymyr_paint, {palette: 'white'}, 'grid')


Map.addLayer(slump_polys_all.map(function(feature){return feature.geometry().centroid()}), {}, 'all slumps')
Map.addLayer(valid_ids, {color: 'yellow'}, 'valid')

