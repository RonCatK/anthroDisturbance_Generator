# tests/testthat/helper-disturbance-fixtures.R

library(terra)
library(sf)
library(data.table)

#— shared raster just for CRS and example rasters ---------------
r <- rast(nrows=10, ncols=10,
          xmin=0, xmax=10, ymin=0, ymax=10,
          vals=1)
crs(r) <- "EPSG:3005"

#— 1) create the *baseline* disturbanceList, including a dummy sectorX -------
createDisturbanceList <- function() {
  empty_vect <- vect()
  crs(empty_vect) <- crs(r)
  
  list(
    settlements = list(
      settlements = vect(
        st_sfc(st_polygon(list(rbind(
          c(1,1), c(1,3), c(3,3), c(3,1), c(1,1)
        ))), crs = crs(r))
      ),
      potentialSettlements = vect(
        st_buffer(st_sfc(st_polygon(list(rbind(
          c(1,1), c(1,3), c(3,3), c(3,1), c(1,1)
        ))), crs = crs(r)), dist = 1)
      )
    ),
    
    wind = list(
      windTurbines = vect(),  # no existing turbines
      potentialWindTurbines = vect(
        st_sfc(st_multipoint(rbind(c(7,7), c(8,2))), crs = crs(r))
      )
    ),
    
    pipelines = list(
      pipelines = empty_vect,
      potentialPipelines = vect(
        st_sfc(st_point(c(2,8)), st_point(c(4,9)), crs = crs(r))
      ),
      roads = vect(
        st_sfc(st_linestring(rbind(c(0,5), c(10,5))), crs = crs(r))
      )
    ),
    
    mining = list(
      mining = vect(
        st_sfc(st_polygon(list(rbind(
          c(5,1), c(5,2), c(6,2), c(6,1), c(5,1)
        ))), crs = crs(r))
      ),
      potentialMining = vect(
        st_buffer(st_sfc(st_polygon(list(rbind(
          c(5,1), c(5,2), c(6,2), c(6,1), c(5,1)
        ))), crs = crs(r)), dist = 0.5)
      )
    ),
    
    forestry = list(
      cutBlocks = vect(
        st_sfc(st_polygon(list(rbind(
          c(8,4), c(8,6), c(9,6), c(9,4), c(8,4)
        ))), crs = crs(r))
      ),
      potentialCutBlocks = vect(
        st_sfc(st_polygon(list(rbind(
          c(8,4), c(8,6), c(9,6), c(9,4), c(8,4)
        ))), crs = crs(r))
      )
    ),
    
    oilGas = list(
      seismicLines = vect(
        st_sfc(st_linestring(rbind(c(3,5), c(3,10))), crs = crs(r))
      ),
      potentialSeismicLines = vect(
        st_sfc(st_point(c(3,7)), st_point(c(2,9)), crs = crs(r))
      )
    ),
    
    # dummy sector with no pastDist entries
    sectorX = list()
  )
}

#— 2) create a matching disturbanceParameters table ---------------
createDisturbanceParameters <- function(distList) {
  rows <- list()
  for (sector in names(distList)) {
    for (lay in names(distList[[sector]])) {
      rows[[length(rows)+1]] <- data.table(
        dataName            = sector,
        dataClass           = lay,
        disturbanceType     = "Generating",
        disturbanceRate     = NA_real_,
        disturbanceSize     = NA_real_,
        disturbanceOrigin   = lay,
        disturbanceEnd      = "",  
        disturbanceInterval = 1L,
        resolutionVector    = list(NA)
      )
    }
  }
  dt <- rbindlist(rows)
  
  # override the “Enlarging” types for settlements & wind:
  dt[dataName=="settlements" & disturbanceOrigin=="settlements", disturbanceType := "Enlarging"]
  dt[dataName=="wind"        & disturbanceOrigin=="windTurbines", disturbanceType := "Enlarging"]
  
  return(dt[])
}

#— 3) create an updatedLayersAll that actually hits *every* branch ---
createUpdatedLayersAll <- function() {
  # 3a) Enlarging vectors
  new_settlement_vect <- vect(
    st_sfc(st_polygon(list(rbind(
      c(1.5, 1.5), c(1.5, 4), c(4, 4), c(4, 1.5), c(1.5, 1.5)
    ))))
  )
  crs(new_settlement_vect) <- crs(r)
  
  new_wind_vect <- vect(
    st_sfc(st_point(c(8.5, 8.5)))
  )
  crs(new_wind_vect) <- crs(r)
  
  new_dummy_vect <- vect(
    st_sfc(st_point(c(0, 0)))
  )
  crs(new_dummy_vect) <- crs(r)
  
  # 3b) A Raster for mining to force raster→vector conversion
  new_mining_raster <- rast(nrows = 5, ncols = 5,
                            xmin = 5, xmax = 6, ymin = 1, ymax = 2,
                            vals = 1)
  crs(new_mining_raster) <- crs(r)
  
  # 3c) A different-geometry SpatVector for forestry (linestring)
  new_forestry_line <- vect(
    st_sfc(st_linestring(rbind(c(8, 5), c(9, 5))))
  )
  crs(new_forestry_line) <- crs(r)
  
  # 3d) Seismic lines + first year
  new_seis_vect <- vect(
    st_sfc(st_linestring(rbind(c(3, 6), c(3, 9))))
  )
  crs(new_seis_vect) <- crs(r)
  
  first_year_seis <- vect(
    st_sfc(st_linestring(rbind(c(2, 5), c(2, 8))))
  )
  crs(first_year_seis) <- crs(r)
  
  list(
    individuaLayers  = list(
      settlements = list(settlements = new_settlement_vect),
      wind = list(windTurbines = new_wind_vect),
      pipelines = list(), # no updates → tests “no updates” path
      mining = list(mining = new_mining_raster),
      forestry = list(cutBlocks = new_forestry_line),
      oilGas = list(seismicLines = new_seis_vect),
      sectorX = list(layerX = new_dummy_vect)
    ),
    seismicLinesFirstYear = first_year_seis
  )
}
