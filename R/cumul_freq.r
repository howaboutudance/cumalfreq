## cumalative frequency histogramic data library
## orignally devloped to process data from tables ACS DP04/Census DP4 for owner-occupied home values
## (c)2017 Michael Penhallegon, All Right Reserved
## contact me: crimson@crimsonfae.net

## libaries
library(dplyr)

## histo is the representation of the histogram buckets and values
##
##  the default is for the home values of census dp4/acs table dp04
##
histo <- data.frame(label = c("0-50,000",
                      "50,000-100,000",
                      "100,000-150,0000",
                      "150,000-200,000",
                      "200,000-300,000",
                      "300,000-500,000",
                      "500,000-1,000,000",
                      "1,000,000 or more"))
histo["upper"] <- c(50000, 100000,
                    150000, 200000,
                    300000, 500000,
                    1000000, 2000000)
histo["lower"] <- c(0,50000,
                    100000, 150000, 
                    200000, 300000,
                    500000, 1000000)

#' return_bucket return the text label of the bucket
#' 
#' this is used for testing to make sure that value falls inside
#' 
#' @param idx the index of the bucket to be returned
#' @return the text label of the bucket at index idx
#' @export
return_bucket <- function(idx){
  return(histo %>% slice(idx) %>% select(label))
}

#' cumul_buckets generate cumalative frequencies
#' 
#' converts a histogram data set to cumalative frequencies
#'
#' @param x input histogram vector
#' @return cumulative frequencies vector
#' 
#' @examples
#' freq = [1, 4, 5, 3, 1]
#' cumul_buckets(freq)
#' 
#' @export
cumul_buckets <- function(x){
  sums = rep(0, length(x))
  for(n in 1:length(x)){
         sums[n] = sum(x[1:n])
     }
  return(sums)
}


#' findbin takes a percentile and returnsa value of % n
#' 
#' the target histogramic data buckets and returns which buckets would perc exist in that histogram
#' 
#' @param perc percentiles vector
#' @param buckets histogram vector
#' 
#' @return integer index of the bin on histo
#' 
#' @examples
#' histogram_freq = [10, 20, 40, 10, 0, 0, 1]
#' 
#' percentile_20th_idx = perc_findbin(0.5, histogram_freq)
#' 
#' median_idx = perc_bin(0.5, histogram_freq)
#' median_idx == median(histogram_freq)
#' @export
perc_findbin <- function(perc, buckets){
  perc_buck = perc * sum(buckets)
  cum_bucks = cumul_buckets(buckets)
  
  lenb = length(buckets)
  lower = 0
  index = 1
  upper = cum_bucks[index]
  
  while(index <= lenb){
    if((lower <= perc_buck) & (perc_buck < upper)){
      result = index
      break
    }
    lower = cum_bucks[index]
    index = index + 1
    upper = cum_bucks[index]
  }
  
  return(result)
}

#' line returns a function that is the line equation 
#' 
#' returns a line equation in relation to ns of histogram. This will provide a closure that will return the value of the houses.
#'
#' @param idx index of bin integer
#' @param buckets histogram vector
#' @return function of line to calculate
#' @export
n_line <- function(idx, buckets){
  p1 = c(histo$upper[idx], cumul_buckets(buckets)[idx])
  p2 = c(histo$lower[idx], cumul_buckets(buckets)[idx-1])
  if(idx == 1){p2 = c(0,0)}
  findslope <- function(){
    return((p1[2]-p2[2])/(p1[1]-p2[1]))
  }
  m = findslope()

  findb <- function(){
    return(p1[2]-(m*p1[1]))
  }
  b = findb()

  findline <- function(y){
    return((y - b)/m)
  }
  
  return(findline)
}

#' histo_findbin takes a histogram and return the percentiles values
#' 
#' takes a histogram and returns the percentile values. aggregrates findbin and line together
#' 
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
#'
#' @param val double value bound of percentile
#' @return index of bin on census tract histogram
#' @export
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
#' 
#' a Value to the cumalative number of tracts below that value
#' 
#' @param idx interger of bin
#' @param buckets histogram data from cesnsus tract as vector
#' @return closure function to calculate number of homes at percentile
#' @export
ctLine <- function(idx, buckets){
  p1 = c(histo$upper[idx], cumul_buckets(buckets)[idx])
  p2 = c(histo$lower[idx], cumul_buckets(buckets)[idx-1])
  if(idx == 1){p2 = c(0,0)}
  findslope <- function(){
    return((p1[2]-p2[2])/(p1[1]-p2[1]))
  }
  m = findslope()
  findb <- function(){
    return(p1[2]-(m*p1[1]))
  }
  b = findb()
  findline <- function(x){
    return(x*m+b)
  }
  return(findline)
}

#'perc_MSA_2000 is a helper function to compenstate for census 2000
#'
#' Census 2000 did not include MSA home value data but can generate those values from census tract data
#' 
#' @param ct_data data.frame of census tract data
#' @param percentiles vector of percentiles
#' @return percentiles values vector
#' @export
percs_MSA_2000 <- function(ct_data, percentiles=c(0.2, 0.8)){
    buckets = ct_data %>% select(HC01_VC63:HC01_VC72) %>% colSums %>% t %>% as.data.frame.ts
    return(histo_histbin(buckets, percentiles=percentiles))
}
