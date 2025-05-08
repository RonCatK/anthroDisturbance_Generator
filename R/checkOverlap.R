# checkOverlap <- function(line1, line2) {
#  # # Calculate the intersection
#  # buff1 <- terra::buffer(line1, 10)
#  # buff2 <- terra::buffer(line2, 10)
#  # doIntersec <- terra::intersect(buff1, buff2)
#  # if (length(doIntersec) == 0) return(FALSE) 
#  # return(TRUE)
#  overlap_matrix <- terra::relate(x = line1, y = line2, relation = "T********")
#  overlap_matrix[1,2]
#  
#  } # overlap_matrix only returns TRUE when the second feature of a 
     # multi-feature line overlaps the first line

checkOverlap <- function(line1, line2, buffer = NULL) {
  # — Input validation —
  if (!inherits(line1, "SpatVector") || !inherits(line2, "SpatVector")) {
    stop("Both inputs must be terra::SpatVector objects.")
  }
  if (nrow(line1) != 1 || nrow(line2) != 1) {
    stop("Each SpatVector must contain exactly one feature.")
  }
  if (!identical(terra::geomtype(line1), "lines") ||
      !identical(terra::geomtype(line2), "lines")) {
    stop("Each SpatVector must be of type 'lines'.")
  }
  
  # — “Within buffer meters” —
  if (!is.null(buffer)) {
    if (!is.numeric(buffer) || length(buffer) != 1 || buffer < 0) {
      stop("`buffer` must be a single non‐negative number (in map units).")
    }
    buff1 <- terra::buffer(line1, buffer)
    buff2 <- terra::buffer(line2, buffer)
    inter <- suppressWarnings(terra::intersect(buff1, buff2))
    return(nrow(inter) > 0)
  }
  
  # — Fast DE‐9IM interior‐interior overlap —
  mat <- terra::relate(line1, line2, relation = "T********")
  return(as.logical(mat[1,1]))
}




