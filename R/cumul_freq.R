## cumalative frequency histogramic data library
## orignally devloped to process data from tables ACS DP04/Census DP4 for owner-occupied home values
## (c)2017 Michael Penhallegon, All Right Reserved
## contact me: crimson@crimsonfae.net

## libaries
library(dplyr)

## histo is the representation of the histogram buckets and values
## the default is for the home values of census dp4/acs table dp04
## this must be modified to fit the cut-off for each bin
histo <- data.frame(c("0-50,000",
                      "50,000-100,000",
                      "100,000-150,0000",
                      "150,000-200,000",
                      "200,000-300,000",
                      "300,000-500,000",
                      "500,000-1,000,000",
                      "1,000,000-2,000,000"))
histo["upper"] <- c(50000, 100000,
                    150000, 200000,
                    300000, 500000,
                    1000000, 2000000)
histo["lower"] <- c(0,50000, 100000,
                    150000, 200000,
                    300000, 500000,
                    1000000)

#' cumul_buckets converts a discrete histogram data set to cumalative frequencies
#' ex:
#' frequencies: [1, 4, 5, 3, 1] becomes the cumalative frequencies: [1, 5, 10, 13, 14]
#' @param x input histogram vector
#' @return cumulative frequencies vector
#' @export
cumul_buckets <- function(x){
  sums = c()
  while(length(x) >= length(sums)){
    sums = append(sums, sum(x[0:length(sums)+1]))
  }
  return(sums[1:length(x)])
}


#' findbin takes a percentile takes a value of % n
#' the target histogramic data buckets and returns
#' which buckets would perc exist in that histogram
#' @param perc percentiles vector
#' @param buckets histogram vector
#' @return integer index of the bin on histo
#' @example
#' wanting the bin of histo where 20% of histogram's n:
#' histogram_freq = [10, 20, 40, 10, 0, 0, 1]
#' perc_histogram = sum(histogram_freq) * 0.20 <- returns 16.2
#' findbin(perc_histogram, histogram_freq)
#' [1] 2
perc_findbin <- function(perc, buckets){
  perc_buck = perc * sum(buckets)
  total = 0
  index = 1
  while(index < length(buckets)){
    if(total >= perc_buck){
      break
    }
    total = buckets[index] + total
    index = index + 1
  }
  if(index >= length(buckets)){
    index = length(buckets)
  }
  return(index)
}

#' line returns a function that is the line equation in relation to Ns of histogram (not value)
#' has three inner functions:
#' findslope, finds the slope of the line segments:
#' - p1 & p2 the points on the line segment
#' - outputs the slope aka: (y1-y2)/(x1-x2)
#' findb, finds the y-intercept of the line segments: using point slope eqution:
#' y2 - m * x2
#' findline return the line function in terms of x
#' @param idx index of bin integer
#' @param buckets histogram vector
#' @return function of line to calculate
n_line <- function(idx, buckets){
  if(idx == length(buckets)){
    idx = idx-1
  }
  p1 = c(histo$upper[idx], cumul_buckets(buckets)[idx])
  p2 = c(histo$lower[idx], cumul_buckets(buckets)[idx-1])
  findslope <- function(){
    return((p1[2]-p2[2])/(p1[1]-p2[1]))
  }
  m = findslope()

  findb <- function(){
    return(p2[2]-m*p2[1])
  }
  b = findb()

  findline <- function(y){
    return((y - b)/m)
  }
  
  return(findline)
}

#' histo_findbin takes a histogram and return the percentiles values
#' @param buckets histogram buckets
#' @param percentiles vector defualts to 20%(0.2) and 80%(0.8
#' @return percentile values as double
#' @export
histo_findbin <- function(buckets, percentiles = c(0.2, 0.8)){
    perc_n <- function(x){x*sum(buckets)}
    ls = percentiles %>% lapply(perc_findbin, buckets) %>% lapply(n_line, buckets)
    cs = percentiles %>% perc_n
    result = c()
    for(n in 1:length(cs)){
        result <- append(result, ls[[n]](cs[n]))
    }
    
    return(result)
}

#' findctbin takes a value and find the bin tat value would exist based on histo
#' val = value
#' @param val double value bound of percentile
#' @return integer bin index
findctbin <- function(val){
  index = 1
  while(index < length(histo$upper)){
    if((histo$upper[index] > val) & (histo$lower[index] <= val)){
      break
    }
    index = index + 1
  }
  return(index)
}

#' ctLine takes the index of a bin and returns a line function mapping
#' a Value to the cumalative number of tracts below that value
#' @param idx interger of bin
#' @param buckets histogram data as vector
#' @return # home cumalatively
ctLine <- function(idx, buckets){
    if(idx == length(buckets)){
      idx = idx-1
    }
    p1 = c(histo$upper[idx], cumul_buckets(buckets)[idx])
    p2 = c(histo$lower[idx], cumul_buckets(buckets)[idx-1])
    findslope <- function(){
      return((p1[2]-p2[2])/(p1[1]-p2[1]))
    }
    m = findslope()
    findb <- function(){
      return(-(-p1[2]+m*p1[1]))
    }
    b = findb()
    findline <- function(x){
      return(x*m+b)
    }
    return(findline)
  }
#' findICE connects everything together for the census tract data functions
#' @param buckets histogram data vector
#' @return data.frame count of houses > 0.2 and < 0.8 
findICE <- function(buckets, percs=c(0.2, 0.8)){
} 
#' perc_MSA_2000 is a helper function to compenstate for census 2000 not publishing  home value occupied
#' by MSA
#' @param ct_data data.frame of census tract data
#' @param percentiles vector of percentiles
#' @Return percentiles values vector
#' @export
percs_MSA_2000 <- function(ct_data, percentiles=c(0.2, 0.8)){
    buckets = ct_data %>% select(HC01_VC63:HC01_VC72) %>% colSums %>% t %>% as.data.frame.ts
    return(histo_histbin(buckets, percentiles=percentiles))
