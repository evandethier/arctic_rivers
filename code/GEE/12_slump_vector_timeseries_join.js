// UNIFY TIMESERIES OF INDIVIDUAL SLUMPS PRESENT IN 2024
// TAYMYR PENINSULA, RUSSIA
// EVAN DETHIER 2025
// Joins slump polygons from pre-2024 to the 2024 polygon
// Exports to drive


// SET LOCATION, BUFFER, ZOOM
var location_name = 'taymyr_peninsula' // Change this to change image name

 
// Import slump polygons for 2024 at Taymyr
var slumps_2024 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2024_20250620_120px_60m_all')
var slumps_2023 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2023_20250620')
var slumps_2022 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2022_20250620')
var slumps_2021 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2021_20250620')
var slumps_2020 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2020_20250620')
var slumps_2019 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2019_20250620')
var slumps_2018 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2018_20250620')
// var slumps_2017 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2017_20250620')
var slumps_2016 = ee.FeatureCollection('projects/ee-edethier/assets/arctic_erosion/taymyr_slumps_2016_20250620')


// Get area from feature
var getArea = function(feature){
  return feature.set('area_km2', ee.Number(feature.geometry().area()).divide(1e6))
}

// Print total area over time
print('2016 area', slumps_2016.map(getArea).aggregate_sum('area_km2'))
print('2018 area', slumps_2018.map(getArea).aggregate_sum('area_km2'))
print('2019 area', slumps_2019.map(getArea).aggregate_sum('area_km2'))
print('2020 area', slumps_2020.map(getArea).aggregate_sum('area_km2'))
print('2021 area', slumps_2021.map(getArea).aggregate_sum('area_km2'))
print('2022 area', slumps_2022.map(getArea).aggregate_sum('area_km2'))
print('2023 area', slumps_2023.map(getArea).aggregate_sum('area_km2'))
print('2024 area', slumps_2024.map(getArea).aggregate_sum('area_km2'))

// Combine slump data into feature collection
var all_slumps_fc = ee.FeatureCollection([
  slumps_2016.map(function(feature){return feature.set('year', 2016)}),
  slumps_2018.map(function(feature){return feature.set('year', 2018)}),
  slumps_2019.map(function(feature){return feature.set('year', 2019)}),
  slumps_2020.map(function(feature){return feature.set('year', 2020)}),
  slumps_2021.map(function(feature){return feature.set('year', 2021)}),
  slumps_2022.map(function(feature){return feature.set('year', 2022)}),
  slumps_2023.map(function(feature){return feature.set('year', 2023)}),
  slumps_2024.map(function(feature){return feature.set('year', 2024)})
  ]).flatten().map(getArea)
  
// Join slumps to 2024 extent and add common label
var slumps_progression_fc = all_slumps_fc
        .filter(ee.Filter.eq('year', 2024))
        .map(function(feature){
          var label2024 = feature.id()
          var slumps_contained = all_slumps_fc
                                  .filterBounds(feature.geometry(1))
                                  .map(function(feature1){
                                    var latitude = feature1.centroid().geometry().coordinates().get(1)
                                    var longitude = feature1.centroid().geometry().coordinates().get(0)
                                    return ee.Feature(feature1
                                              .setGeometry(null)
                                              .set('label2024', label2024,'latitude', latitude, 'longitude', longitude)
                                              )
                                              .select(['label','year','count', 'label2024', 'area_km2', 'latitude', 'longitude'])
                                  })
          return slumps_contained
})
.flatten()

print('slump progression example', slumps_progression_fc.filter(ee.Filter.neq('year', 2024)).limit(10))

// Export data to CSV
Export.table.toDrive({
  collection: slumps_progression_fc,
  folder: 'arctic_erosion_imports',
  description: 'taymyr_slumps_20250620',
  fileNamePrefix: 'taymyr_slumps_validated_20250620',
  fileFormat: 'CSV'
})


var covering_grid_5km = all_slumps_fc.geometry().coveringGrid({
  proj: 'EPSG:3995',
  scale: 5000
})

print('test fc filter', all_slumps_fc
                            .filterBounds(covering_grid_5km.first().geometry())
                            .filter(ee.Filter.eq('year', 2024))
                            .aggregate_sum('area_km2')
                            )
                            
var year_list = ee.List([2019,2020,2021,2022,2023,2024])
var slump_density_by_yr = covering_grid_5km.map(function(feature){
  var slumps_in_grid = ee.FeatureCollection(year_list.map(function(year){
    var year_sel = ee.Number(year)
    var slump_area_km2 = ee.Number(
                          all_slumps_fc
                            .filterBounds(feature.geometry())
                            .filter(ee.Filter.eq('year', year_sel)
                          ).aggregate_sum('area_km2'))
    
    return feature.set('year', year_sel, 'area_km2', slump_area_km2)
    
  }))
  return slumps_in_grid
}).flatten()

print('first grid area', slump_density_by_yr.filter(ee.Filter.gt('area_km2', 0)))
Map.addLayer(ee.Image(0).paint(slump_density_by_yr.filter(ee.Filter.eq('year',2024)),'area_km2').selfMask(), 
          {min: 0, max: 5, palette: ['black','blue','red']}, '2024 slump_density')
Map.addLayer(ee.Image(0).paint(slump_density_by_yr.filter(ee.Filter.eq('year',2022)),'area_km2').selfMask(), 
          {min: 0, max: 5, palette: ['black','blue','red']}, '2022 slump_density')
Map.addLayer(ee.Image(0).paint(slump_density_by_yr.filter(ee.Filter.eq('year',2020)),'area_km2').selfMask(), 
          {min: 0, max: 5, palette: ['black','blue','red']}, '2020 slump_density')
Map.addLayer(ee.Image(0).byte().paint(covering_grid_5km,1,1).selfMask(), {palette: 'white'}, 'grid 5km')

