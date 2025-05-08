# tests/testthat/helper-disturbance-fixtures.R
library(terra)
library(sf)
library(data.table)

#--- Build raster (once) ----------------
r <- rast(nrows=10, ncols=10, xmin=0, xmax=10, ymin=0, ymax=10, vals=1)
crs(r) <- "EPSG:3005"

#--- Build disturbanceList -------------
# Create an empty SpatVector for pipelines
empty_vect <- vect()
suppressWarnings(crs(empty_vect) <- crs(r))

suppressWarnings({
  disturbanceList <- list(
    settlements = list(
      settlements          = vect(st_sfc(st_polygon(list(
        rbind(c(1,1),c(1,3),c(3,3),c(3,1),c(1,1))
      )), crs=crs(r))),
      potentialSettlements = vect(st_buffer(
        st_sfc(st_polygon(list(
          rbind(c(1,1),c(1,3),c(3,3),c(3,1),c(1,1))
        )), crs=crs(r)),
        dist = 1))
    ),
    wind = list(
      windTurbines          = vect(st_sfc(crs=crs(r))),
      potentialWindTurbines = vect(
        st_sfc(st_multipoint(rbind(c(7,7),c(8,2))), crs=crs(r)))
    ),
    pipelines = list(
      pipelines          = empty_vect,
      potentialPipelines = vect(st_sfc(st_point(c(2,8)),
                                       st_point(c(4,9)), crs=crs(r))),
      roads              = vect(st_sfc(st_linestring(rbind(c(0,5),c(10,5))),
                                       crs=crs(r)))
    ),
    mining = list(
      mining          = vect(st_sfc(st_polygon(list(
        rbind(c(5,1),c(5,2),c(6,2),c(6,1),c(5,1))
      )), crs=crs(r))),
      potentialMining = vect(st_buffer(
        st_sfc(st_polygon(list(
          rbind(c(5,1),c(5,2),c(6,2),c(6,1),c(5,1))
        )), crs=crs(r)),
        dist = 0.5))
    ),
    forestry = list(
      cutBlocks          = vect(st_sfc(st_polygon(list(
        rbind(c(8,4),c(8,6),c(9,6),c(9,4),c(8,4))
      )), crs=crs(r))),
      potentialCutBlocks = vect(st_geometry(
        st_geometry(st_sfc(st_polygon(list(
          rbind(c(8,4),c(8,6),c(9,6),c(9,4),c(8,4))
        )), crs=crs(r)),
        dx=1, dy=0) + c(1000, 0)))
    ),
    seismicLines = list(
      seismicLines          = vect(
        st_sfc(st_linestring(rbind(c(3,5),c(3,10))),
               crs=crs(r))),
      potentialSeismicLines = vect(
        st_sfc(st_point(c(3,7)),st_point(c(2,9)),
               crs=crs(r)))
    )
  )
})

#--- Build disturbanceParameters -------
disturbanceParameters <- data.table(
  dataName            = names(disturbanceList),
  dataClass           = c("potentialSettlements",
                          "potentialWindTurbines",
                          "potentialPipelines",
                          "potentialMining",
                          "potentialCutBlocks",
                          "potentialSeismicLines"),
  disturbanceType     = c("Enlarging","Generating","Connecting",
                          "Enlarging","Enlarging","Connecting"),
  disturbanceRate     = c(0.05,0.10,NA,0.20,0.30,NA),
  disturbanceSize     = c(NA,2500,NA,NA,NA,NA),
  disturbanceOrigin   = c("settlements","windTurbines","pipelines",
                          "mining","cutBlocks","seismicLines"),
  disturbanceEnd      = c("","", "roads","","","seismicLines"),
  disturbanceInterval = rep(1,6),
  resolutionVector    = rep(list(NA),6)
)
