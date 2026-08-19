# Watershed delineation script
def delineate_watershed(watershed_name):
    # Fill watershed sinks
    processing.run("grass7:r.fill.dir", 
        {
        'input':f'/Users/dethier/arctic_rivers/imports/taymyr_elevation_200m_merit_copernicus.tif',
        'format':0,
        '-f':False,
        'output':f'/Users/dethier/arctic_rivers/imports/{watershed_name}_fil.tif',
        'direction':'TEMPORARY_OUTPUT',
        'areas':'TEMPORARY_OUTPUT',
        'GRASS_REGION_PARAMETER':None,
        'GRASS_REGION_CELLSIZE_PARAMETER':0,
        'GRASS_RASTER_FORMAT_OPT':'',
        'GRASS_RASTER_FORMAT_META':''
        })
        

processing.run("grass7:r.watershed", 
    {
    'elevation':f'/Users/dethier/arctic_rivers/imports/{watershed_name}_fil.tif',
    'depression':None,
    'flow':None,
    'disturbed_land':None,
    'blocking':None,
    'threshold':500,
    'max_slope_length':None,
    'convergence':5,
    'memory':300,
    '-s':True,'-m':False,'-4':False,'-a':False,'-b':False,
    'accumulation':f'/Users/dethier/arctic_rivers/imports/{watershed_name}_acc.tif',
    'drainage':f'/Users/dethier/arctic_rivers/imports/{watershed_name}_dir.tif',
    'basin':'TEMPORARY_OUTPUT',
    'stream':'TEMPORARY_OUTPUT',
    'half_basin':'TEMPORARY_OUTPUT',
    'length_slope':'TEMPORARY_OUTPUT',
    'slope_steepness':'TEMPORARY_OUTPUT',
    'tci':'TEMPORARY_OUTPUT',
    'spi':'TEMPORARY_OUTPUT',
    'GRASS_REGION_PARAMETER':None,
    'GRASS_REGION_CELLSIZE_PARAMETER':0,
    'GRASS_RASTER_FORMAT_OPT':'',
    'GRASS_RASTER_FORMAT_META':''})
    
watershed_name = 'taymyr_estuary1'