#’ Buffer & combine multiple disturbance layers (vector + raster) into one output
#’
#’ @param disturbanceList   A named list of disturbance‐type sub‐lists. Each sub‐list
#’                          has elements that are either SpatVector/SpatRaster or RasterLayer.
#’                          Any element whose name contains “potential” will be excluded.
#’ @param bufferSize        Numeric: buffer distance (in the same CRS units as `studyArea`).
#’ @param rasterToMatch     A terra::SpatRaster (template) defining extent/resolution/CRS.
#’ @param studyArea         A terra::SpatVector polygon of the entire study area (for area %).
#’ @param currentTime       (NOT USED) Placeholder to match module signature.
#’ @param convertToRaster   Logical: if TRUE, return a raster; if FALSE, return a combined polygon.
#’
#’ @return  If convertToRaster=TRUE: a SpatRaster with 1 = buffered disturbance, 0 = no disturbance, NA outside extent.
#’          If convertToRaster=FALSE: a SpatVector polygon of all buffered disturbances (dissolved).
#’ @export
createBufferedDisturbances <- function(
    disturbanceList,
    bufferSize,
    rasterToMatch,
    studyArea,
    currentTime = NULL,
    convertToRaster = TRUE
) {
  # 1) Coerce rasterToMatch to SpatRaster (if needed)
  if (!inherits(rasterToMatch, "SpatRaster")) {
    # If user passed a RasterLayer, convert to SpatRaster
    rasterToMatch <- terra::rast(rasterToMatch)
  }
  
  # 2) Compute total studyArea (in km2) once, if we plan to use it in messages
  if (!inherits(studyArea, "SpatVector")) {
    stop("`studyArea` must be a terra::SpatVector polygon.")
  }
  studyAreaTotalKm2 <- terra::expanse(studyArea, transform = FALSE, unit = "km")
  
  # 3) Flatten disturbanceList safely (so names stay attached)
  allLayers <- do.call(c, disturbanceList)  
  layerNames <- names(allLayers)
  if (is.null(layerNames)) {
    stop("`disturbanceList` must be a named list of named sub‐lists.")
  }
  
  # 4) Exclude any layer whose name contains “potential” (case‐sensitive)
  nonPotIdx <- which(!grepl("^potential", layerNames))  # only exclude if name starts with “potential”
  if (length(nonPotIdx) == 0) {
    # If no “current” layers at all, behave like “all empty”:
    if (convertToRaster) {
      # Return a zero‐filled raster matching rasterToMatch
      outR <- rasterToMatch
      outR[] <- terra::ifel(!is.na(rasterToMatch[]), 0, NA)
      # (optional message)
      message("No non‐potential layers found; returning all‐zero raster.")
      return(outR)
    } else {
      stop("No non‐potential disturbance layers present; cannot return vector.")
    }
  }
  
  # 5) For each “current” layer, buffer + (if needed) rasterize → store in a list
  bufferedObjects <- vector("list", length(nonPotIdx))
  names(bufferedObjects) <- layerNames[nonPotIdx]
  
  for (i in seq_along(nonPotIdx)) {
    nm <- layerNames[nonPotIdx[i]]
    layerObj <- allLayers[[nm]]
    
    # 5a) Convert RasterLayer → SpatRaster if needed
    if (inherits(layerObj, "RasterLayer")) {
      layerObj <- terra::rast(layerObj)
    }
    
    # 5b) If it's a SpatRaster, polygonize only cells == 1
    if (inherits(layerObj, "SpatRaster")) {
      # If no cells equal 1, skip
      vals      <- terra::values(layerObj)
      if (all(is.na(vals) | vals != 1)) {
        message(paste0("Layer '", nm, "' has no cells == 1; skipping."))
        next
      }
      # Mask all values ≠ 1 to NA:
      layerObj[layerObj[] != 1] <- NA
      # Polygonize:
      polyObj <- terra::as.polygons(layerObj,
                                    values = TRUE,
                                    na.rm   = TRUE,
                                    dissolve = FALSE)
      # If for some reason there’s a “value” column other than 1, drop it:
      if (ncol(polyObj) > 1) {
        polyObj <- polyObj[polyObj[[1]] == 1, ]
      }
      layerObj <- polyObj
    }
    
    # 5c) Now layerObj should be a SpatVector (points, lines, or polygons)
    if (!inherits(layerObj, "SpatVector")) {
      warning(paste0("Layer '", nm, "' is neither raster nor vector—skipping."))
      next
    }
    
    # 5d) If this vector has zero rows, skip
    if (nrow(layerObj) == 0) {
      message(paste0("Layer '", nm, "' is empty; skipping."))
      next
    }
    
    # 5e) Buffer + dissolve
    b <- terra::buffer(layerObj, width = bufferSize)
    b <- terra::aggregate(b, dissolve = TRUE)
    if (nrow(b) == 0) {
      message(paste0("After buffering, layer '", nm, "' has zero features; skipping."))
      next
    }
    
    # 5f) If user wants a raster, rasterize now; otherwise keep as vector
    if (convertToRaster) {
      # Rasterize into same grid as rasterToMatch
      # We'll create a new blank layer matching rasterToMatch, then set cells inside b = 1
      r_blank <- rasterToMatch
      r_blank[] <- 0L
      # Note: terra::rasterize expects a field to burn in; we can add a dummy column “burn = 1”
      b$burn <- 1L
      rast_i <- terra::rasterize(b, r_blank, field = "burn", background = 0L) 
      names(rast_i) <- nm
      bufferedObjects[[nm]] <- rast_i
    } else {
      bufferedObjects[[nm]] <- b
    }
  }
  
  # At this point, bufferedObjects is a list of either SpatRaster layers (if convertToRaster) or SpatVector polygons
  # Some elements may be NULL (the ones we skipped above)—filter them out:
  bufferedObjects <- Filter(Negate(is.null), bufferedObjects)
  if (length(bufferedObjects) == 0) {
    # This can only happen if all “current” layers were empty or skipped
    if (convertToRaster) {
      outR <- rasterToMatch
      outR[] <- terra::ifel(!is.na(rasterToMatch[]), 0, NA)
      message("All non‐potential layers were empty after processing; returning all‐zero raster.")
      return(outR)
    } else {
      stop("All non‐potential layers were empty or invalid; nothing to return.")
    }
  }
  
  # 6) Combine everything into one final object
  if (convertToRaster) {
    # 6a) Stack all buffered rasters and take pixel‐wise “any==1”
    rasters_to_stack <- rast(bufferedObjects)   # a multi‐layer SpatRaster
    final_rast <- terra::app(rasters_to_stack,
                             fun = function(x) as.integer(any(x == 1)),
                             cores = 1)
    # 6b) Write a message about total disturbed area
    cell_area_km2 <- prod(terra::res(final_rast) / 1000)   # e.g., (0.25 km * 0.25 km) = 0.0625
    n1 <- sum(terra::values(final_rast) == 1, na.rm = TRUE)
    totalDistKm <- n1 * cell_area_km2
    perc <- round(100 * (n1 / sum(!is.na(terra::values(final_rast)))), 2)
    message(paste0(
      "Buffered disturbances (raster): ",
      round(totalDistKm, 2), " km² (", perc, "% of study area)."
    ))
    return(final_rast)
    
  } else {
    # 6c) Combine (rbind) all buffered polygons & dissolve
    all_polys <- do.call(c, bufferedObjects) 
    combined <- terra::aggregate(all_polys, dissolve = TRUE)
    
    totalDistKm <- terra::expanse(combined, transform = FALSE, unit = "km")
    perc <- round(100 * (totalDistKm / studyAreaTotalKm2), 2)
    message(paste0(
      "Buffered disturbances (vector): ",
      round(totalDistKm, 2), " km² (", perc, "% of study area)."
    ))
    return(combined)
  }
}

