#' Plots a climate chart
#' 
#' Plots Walter-Lieth climate chart. Uses a base function from climatol package and
#' adds some more details. Work in progress.
#' 
#' @param adata a data frame of daily weather data. First columns include locality data
#' @return nothing
#' @export
chartWL <- function(adata){
  
  latd = unique(adata$LATD)
  lond = unique(adata$LOND)
  elev = unique(adata$ELEV)
  locl = unique(adata$ADMIN4)
  cntr = unique(adata$CNTRY)
  locl = paste(locl,", ", cntr,sep="")
  
  adata = adata[,c(6:ncol(adata))]
  mnth = str_sub(adata$Date,6,7)
  year = str_sub(adata$Date,1,4)
  perd = range(as.integer(year))
  perd = paste(perd[1],perd[2],sep="-")
  adata = cbind(mnth, adata)
  adata = adata[,-c(2)]
  adata[,1] = as.character(adata[,1])
  
  dsum = aggregate(.~mnth, data = adata,FUN = "mean")
  if(nrow(dsum)==12){
    
    
    wl = t(dsum[,2:ncol(dsum)])
    wl = wl[c(4,3,2),]
    
    dsum = aggregate(TMIN~mnth, data = adata,FUN = "min")
    wl = rbind(wl, t(dsum[,2]))
    
    slat = 1
    if(!is.na(latd)) slat = sign(latd)
    
    #diagwl(wl, est = locl, alt = elev, mlab="en", per = perd, shem = slat)
    diagwl(wl, est = locl, alt = elev, mlab="en", per = perd)
    if(!is.na(latd)){
      latlon = paste("LAT",latd, "/ LON", lond)
      mtext(latlon,3,-1, cex = 0.8) 
    }
  } else {
    subt = locl
    plot(x=1:12, y=rep(NA,12), ylim=c(1,12), main = "Not enough data for climate chart",
         sub = subt, xlab = "month", ylab="")
  }
}


# data = read.csv('climate/climate_data.csv', stringsAsFactors = FALSE, sep="\t")
# data = filterLocality(data, locality = "Gisozi")

#chartWL(data)