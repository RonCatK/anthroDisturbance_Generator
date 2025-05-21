whichDisturbancesToGenerate <- function(startTime,
                                        endTime,
                                        currentTime,
                                        disturbanceParameters) {
  intervals <- disturbanceParameters$disturbanceInterval
  
  whichOnes <- vapply(
    seq_along(intervals),               
    function(i) {                       
      interval <- intervals[i]
      if (is.na(interval) || interval <= 0) return(NA_integer_)
      fullSeq <- seq(startTime, endTime, by = interval)
      if (currentTime %in% fullSeq) i else NA_integer_
    },
    integer(1)                          
  )
  
  return(as.integer(na.omit(whichOnes)))
}

