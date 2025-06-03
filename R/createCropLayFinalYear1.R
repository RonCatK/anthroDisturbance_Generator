createCropLayFinalYear1 <- function(Lay, potLayTopValid, runClusteringInParallel, 
                                    clusterDistance, studyAreaHash){
  # 1. Crop and mask the seismicLines layer to the actual most probable area (potLayTopValid)
  cropLay <- Cache(postProcessTo, Lay, potLayTopValid)
  # 2. Convert all lines to polygons to exclude them from selecting the random point
  cropLayBuf <- Cache(buffer, cropLay, width = 50)
  cropLayAg <- Cache(aggregate, cropLayBuf, dissolve = TRUE)
  finalPotLay <- Cache(erase, potLayTopValid, cropLayAg)
  # 4. Calculate the total length of lines in the "chosen area", their mean and sd 
  message("Getting the line's length, average and sd...")
  # Assign unique ID
  cropLay$individualID <- 1:NROW(cropLay)
  tic("Time elapsed for intersecting potential and seismic lines layer: ")
  cropLay <- reproducible::Cache(terra::intersect(potLayTopValid, cropLay))
  toc()
  
  # Remove overlapping
  tic("Time elapsed to identify overlapping features: ")
  overlap_matrix <- terra::relate(x = cropLay, relation = "T********", pairs = TRUE)
  overlapMatrix <- as.data.table(overlap_matrix)
  overlapMatrix[, self := fifelse(id.1 == id.2, TRUE, FALSE)]
  overlapMatrix <- overlapMatrix[self == FALSE,]
  overlapMatrix[, self := NULL]
  overlapMatrix <- overlapMatrix[, .(id.1 = pmin(id.1, id.2), id.2 = pmax(id.1, id.2))]
  overlapMatrix <- unique(overlapMatrix)
  
  to_remove <- integer()
  for (i in seq_len(nrow(overlapMatrix))) {
    angle1 <- calculateLineAngle(cropLay[overlapMatrix[i, "id.1"]])
    angle2 <- calculateLineAngle(cropLay[overlapMatrix[i, "id.2"]])
    # Skip if angle is NA
    if (is.na(angle1) || is.na(angle2)) next
    if (round(angle1, 4) != round(angle2, 4)){
      next
    } else {
      coords1 <- terra::crds(cropLay[overlapMatrix[i, "id.1"]])
      coords2 <- terra::crds(cropLay[overlapMatrix[i, "id.2"]])
      len1 <- sqrt((coords1[2,1] - coords1[1,1])^2 + (coords1[2,2] - coords1[1,2])^2)
      len2 <- sqrt((coords2[2,1] - coords2[1,1])^2 + (coords2[2,2] - coords2[1,2])^2)
      smallest <- if (len1 > len2) overlapMatrix[i, id.2] else overlapMatrix[i, id.1]
      to_remove <- c(to_remove, smallest)
    }
  }
  toc()
  if (length(to_remove) > 0) {
    cropLay <- cropLay[-unique(to_remove), ]
  }
  # End overlapping removal
  
  # Loop through the potentials, and make clusters
  cropLayFinal <- do.call(rbind, lapply(unique(cropLay$Potential), function(Pot){
    cropLayInt <- Cache(clusterLines, cropLay[cropLay$Potential == Pot,], 
                        distThreshold = clusterDistance, 
                        currPotential = Pot,
                        runInParallel = runClusteringInParallel,
                        totPotential = length(unique(cropLay$Potential)), 
                        userTags = studyAreaHash)
    return(cropLayInt)
  }))
  return(cropLayFinal)
}
