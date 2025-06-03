# First, per polygon (I have the area divided into polygons of oil potential), 
# I cluster the Lines to identify “individual grids” (or as close to it as possible). 
# Once I know the clusters from data, I can identify the average 
# 1) number of Lines, as well 
# 2) line distance, 
# 3) length, 
# 4) angles --> how many parallel and how many perpendicular Lines 
# respective SDs per cluster. 
# Then I can just randomly choose which type of cluster the generated next will be and draw the grid based on the data.
 
clusterLines <- function(Lines, distThreshold = 5000, 
                         currPotential, totPotential,
                         plotClusterDiagnostic = FALSE,
                         runInParallel){
  # If fewer than 2 lines, skip clustering and assign default cluster
  if (nrow(Lines) < 2) {
    if (nrow(Lines) == 0) return(Lines)
    Lines$cluster <- 1L
    Lines$Pot_Clus <- paste0(unique(Lines$Potential), "_", 1)
    Lines$calculatedLength <- perim(Lines)
    # For a single line, compute angle if possible, else NA
    coords <- terra::crds(Lines)
    if (!is.null(coords) && nrow(coords) >= 2) {
      Lines$angles <- calculateLineAngle(Lines)
    } else {
      Lines$angles <- NA_real_
    }
    return(Lines)
  }
  
  # Calculate the centroids of each line
  centroids <- centroids(Lines)
  xy <- geom(centroids)
  D <- dist(data.frame(x = xy[, "x"], y = xy[, "y"]))
  chc <- hclust(D, method = "complete")
  # Distance threshold clustering
  chc.d <- cutree(chc, h = distThreshold)
  if (plotClusterDiagnostic) {
    tb <- as.data.table(table(chc.d))
    plot(tb$chc.d, tb$N, xlab = "Number of Clusters", ylab = "Number of Lines")
  }
  Lines$cluster <- chc.d
  totalLineClusters <- length(unique(Lines$cluster))
  
  if (runInParallel) {
    totalLineClusters <- length(unique(Lines$cluster))
    n_cores <- parallel::detectCores()
    cluster <- parallel::makeCluster(max(1, min(n_cores - 1, totalLineClusters)), type = "PSOCK")
    parallel::clusterEvalQ(cluster, library(terra))
    parallel::clusterExport(cluster,
                            varlist = c("Lines", "calculateLineAngle", "totalLineClusters", "currPotential", "totPotential"),
                            envir = globalenv())
    doParallel::registerDoParallel(cluster)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    
    chunks <- foreach(i = unique(Lines$cluster), .packages = "terra") %dopar% {
      sub <- Lines[Lines$cluster == i, ]
      sub$Pot_Clus <- paste0(unique(sub$Potential), "_", i)
      sub$calculatedLength <- perim(sub)
      sub$angles <- vapply(seq_len(nrow(sub)), function(j) calculateLineAngle(sub[j, ]), numeric(1))
      sub
    }
    Lines <- do.call(rbind, chunks)
  } else {
    for (i in unique(Lines$cluster)) {
      currPerc <- round(100 * (i / totalLineClusters), 2)
      currPercOfAll <- round(100 * (currPotential / totPotential), 2)
      if (currPerc %% 10 == 0) {
        message(paste0("Information for Potential layer ", currPotential,
                       " (", currPercOfAll, "%) extracted: ", currPerc, "% done"))
      }
      idx <- which(Lines$cluster == i)
      Lines[idx, "Pot_Clus"] <- paste0(unique(Lines[idx, ]$Potential), "_", i)
      Lines[idx, "calculatedLength"] <- perim(Lines[idx, ])
      angles <- numeric(length(idx))
      for (ii in seq_along(idx)) {
        angles[ii] <- calculateLineAngle(Lines[idx[ii], ])
      }
      Lines[idx, "angles"] <- angles
    }
  }
  return(Lines)
}
