saveDisturbances <- function(disturbanceList,
                             currentTime,
                             overwrite,
                             runName) {
  # Helper to write one spatial object
  write_one <- function(obj, sector, layer = NULL) {
    # decide extension and write func
    if (is.null(obj)) {
      warning(sprintf("The layer for %s%s is NULL. Not saving.", sector,
                      if (!is.null(layer)) paste0(" -- ", layer) else ""),
              immediate. = TRUE)
      return()
    }
    # Coerce and write vector
    if (any(class(obj) %in% c("SpatVector", "sf")) || is(obj, "Spatial")) {
      if (!inherits(obj, "SpatVector")) obj <- terra::vect(obj)
      fname <- paste0("disturbances_", sector,
                      if (!is.null(layer)) paste0("_", layer) else "",
                      "_", currentTime, "_", runName, ".shp")
      terra::writeVector(obj, file.path(Paths$outputPath, fname),
                         filetype = "ESRI Shapefile", overwrite = overwrite)
    }
    # Raster branch
    else if (any(class(obj) %in% c("RasterLayer", "SpatRaster"))) {
      if (inherits(obj, "RasterLayer")) obj <- terra::rast(obj)
      fname <- paste0("disturbances_", sector,
                      if (!is.null(layer)) paste0("_", layer) else "",
                      "_", currentTime, "_", runName, ".tif")
      terra::writeRaster(obj, file.path(Paths$outputPath, fname),
                         filetype = "GTiff", overwrite = overwrite)
    } else {
      stop(sprintf("Objects of class %s can't be used. Please use raster, sp, sf, or terra formats.",
                   paste(class(obj), collapse=",")))
    }
  }
  
  for (sector in names(disturbanceList)) {
    obj <- disturbanceList[[sector]]
    # flat list: single object
    if (any(class(obj) %in% c("SpatVector", "sf", "RasterLayer", "SpatRaster")) ||
        is(obj, "Spatial")) {
      message(sprintf("Layer: %s was likely not generated. Saving current disturbance.", sector))
      write_one(obj, sector)
    } else if (is.list(obj)) {
      # multi-layer
      layers <- setdiff(names(obj), grep("potential", names(obj), value=TRUE))
      for (lay in layers) {
        message(sprintf("Saving layer: %s -- %s", sector, lay))
        layer_obj <- obj[[lay]]
        # preserve or assign Class
        if (any(class(layer_obj) %in% c("SpatVector", "sf")) || is(layer_obj, "Spatial")) {
          if (!"Class" %in% names(layer_obj) || all(is.na(layer_obj$Class)))
            layer_obj$Class <- lay
        }
        write_one(layer_obj, sector, lay)
      }
    }
  }
  message(crayon::green(sprintf("All disturbances saved for %s", currentTime)))
}
