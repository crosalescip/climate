# Ciro test
# # This is the server logic for a Shiny web application.
# # You can find out more about building applications with Shiny here:
# #
# # http://shiny.rstudio.com
# #
# 
# library(shiny)
# 
# shinyServer(function(input, output) {
# 
#   output$distPlot <- renderPlot({
# 
#     # generate bins based on input$bins from ui.R
#     x    <- faithful[, 2]
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#     # draw the histogram with the specified number of bins
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')
# 
#   })
# 
# })

#setwd("D:\\apps")
library(stringr)
library(climatol)
library(ClimClass)
#source("E:/Documents and Settings/user/My Documents/test/wl/wl.R")

months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

data = read.csv(file.path(getwd(),'climate_data_d.csv'), stringsAsFactors = FALSE, sep="\t")
data_M = read.csv(file.path(getwd(),'climate_data_m.csv'), stringsAsFactors = FALSE, sep="\t")

trange = c(min(data$TMIN, na.rm=T), max(data$TMAX, na.rm=T))
trange_M = c(min(data_M$TMIN, na.rm=T), max(data_M$TMAX, na.rm=T))

#Funcion filtrar localidad:
filterLocality <- function (data, country = NA, locality = NA) {
  if(!is.na(country)) data = data[country == data$CNTRY,]
  if(length(locality) > 0){
    if(!is.na(locality))  data = data[locality == data$ADMIN4,]
  }
  data
}

#Funcion crea lista Julian Day:
makeRefDayList <- function(month=c("Jan","Dec")){
  dmonths = c(1,31, 2,28, 3,31, 4,30, 5,31, 6,30, 7,31, 8,31, 9,30, 10,31, 11,30, 12,31)
  dmonths = matrix(dmonths, nrow=12, byrow=T)
  out=NULL
  ii = which(months %in% month)
  if(length(ii)==1) ii=c(ii,ii)
  for(i in ii[1]:ii[2]) out=c(out, paste(str_pad(i,2,pad="0"),"-",str_pad(1:dmonths[i,2],2,pad="0"),sep=""))
  out = as.data.frame(out, stringsAsFactors = FALSE)
  names(out)[1] = "JulDay"
  out
}

#Funcion extraer marcas:
getTicks <- function (yearRg) {
  s = yearRg$JulDay
  days = str_sub(s, 4, 5)  
  which(days == "01")
}

#Funcion agregar grilla:
addGrid <- function (data, varName, ticks) {
  varName = varName[varName %in% names(data)]
  if(!all(is.na(data[,varName]))){
    trange = c(min(data[,varName], na.rm=T), max(data[,varName], na.rm=T))
    x = round(trange[1],0) : round(trange[2],0)
    x = x[x %% 5==0]
    abline(h=x, col="grey90")
    abline(v=ticks, col="grey90")
  }
}

#Funcion genera matriz general Julian Day:
getY <- function(data, yearRg = NA){
  yer = str_sub(data$Date, 1, 4)
  JulDay = str_sub(data$Date, 6, 10)
  
  dat = cbind(yer,JulDay, data)
  dat[,1] = as.character(dat[,1])
  dat[,2] = as.character(dat[,2])
  yrs = unique(yer)
  vrs = 7:ncol(data)
  nms = names(data)[vrs]
  
  m = length(vrs)
  res = list()
  rdl = yearRg
  
  n = length(yrs)
  for(j in 1:m){
    tmp = rdl
    for(i in 1:n){
      sdt = dat[dat$yer == yrs[i],]
      sdt = sdt[,c(2,9:ncol(sdt))]
      dps = duplicated(sdt$JulDay)
      sdt = sdt[!dps,]
      
      tmp = merge(tmp, sdt[,c("JulDay",nms[j])], by="JulDay", all.x = TRUE)
      if(length(yrs[i]) > 0) names(tmp)[i+1] = yrs[i]
    }
    res[[names(data)[vrs[j]]]] = tmp    
  }
  res
}

#Funcion genera lineas
varLine <- function (Y, var, color, year) {
  n = length(year)
  if(n>0){
    for(i in 1:n){
      yy = Y[[var]]
      if(year[i] %in% names(yy)) lines(yy[,year[i]], col=color)  
    }
  }
}

#Funcion grafico temperatura Julian Day:
plotJulianDay <- function (data, main, xlab, ylab, varName, varState, 
                           varColor = c("red","blue","darkgreen"),
                           country=NA, locality = NA, year=NA, month=NA) {
  try({
    data = filterLocality(data, country, locality)
    vn = varName %in% names(data)
    
    varName = varName[vn]
    varState = varState[vn]
    varColor = varColor[vn]
    
    yearRg = makeRefDayList(month)
    
    x = 1:nrow(yearRg)
    y = rep(NA,length(x))
    
    Y = getY(data, yearRg)
    
    if(!all(is.na(data[,varName]))){
      trange = c(round(min(data[,varName],na.rm=T),0), round(max(data[,varName], na.rm=T),0))
      ticks = getTicks(yearRg)
      
      if(!is.infinite(trange[1])) plot(x, y, ylim=trange, main=main, xlab=xlab, ylab=ylab, xaxt="n")
      addGrid(data, varName, ticks)
      
      wm = which(months %in% month)
      if(length(wm)==1) wm = c(wm, wm)
      axis(1, labels=months[wm[1]:wm[2]], at=ticks)
      
      m = length(varName)
      for(i in 1:m){
        if(varState[i]) varLine(Y,varName[i], varColor[i], year)
      }
    }
  })
}

#Funcion grafico temperatura Time Series:
plotTimeSeries <-function(data, ylab, dateRange, varName, country = NA, locality = NA, main, trange) {
  data = filterLocality(data, country, locality)
  data=data[data$Date >= dateRange[1] & data$Date <= dateRange[2],]
  x = 1:nrow(data)
  
  trange3 = c(min(data$RHMIN, na.rm=T), max(data$RHMAX, na.rm=T))
  
  if(length(x) == nrow(data)){
    if(!is.infinite(trange[1]))  plot(x, data[,varName], type="l", col="red", ylim=trange, xlab="Date", ylab=ylab, xaxt="n", main=main)
    
    for(i in seq(-20, 200, by = 5)){
      abline(h=i, col="grey90")
    }
    
    if(varName == "RAIN"){
      for(i in 0:200){
        abline(h=i, col="grey90")
      }
    }
    
    if(varName == "TMEAN"){
      lines(data[,"TMEAN"],col="red")
      lines(data[,"TMIN"],col="blue")
      lines(data[,"TMAX"],col="darkgreen")  
    }
    
    if(varName == "RAIN") lines(data[,"RAIN"],col="blue")
    
    if(varName == "RHMEAN"){
      lines(data[,"RHMEAN"],col="red")
      lines(data[,"RHMIN"],col="blue")
      lines(data[,"RHMAX"],col="darkgreen")  
    }
    
    tickN = 5
    ticks = integer(tickN)
    ticks[1] = 1
    ticks[5] = nrow(data)
    ticks[3] = ticks[1] + round(((ticks[5]-ticks[1])/2),0)
    ticks[2] = ticks[1] + round(((ticks[3]-ticks[1])/2),0)
    ticks[4] = ticks[3] + round(((ticks[5]-ticks[3])/2),0)
    axis(1, labels=data$Date[ticks], at=ticks)
  }
}

shinyServer(function(input, output) {
  
  #Mostrar localidades Julian Day:
  output$uiLocality_JD <- renderUI({
    data = data[input$country == data$CNTRY, "ADMIN4"]
    selLocality = sort(unique(data))
    selectInput("locality", "Locality:", selLocality)
  })
  
  #Mostrar localidades Time Series:
  output$uiLocality_TS <- renderUI({
    data = data[input$country_TS == data$CNTRY, "ADMIN4"]
    selLocality = sort(unique(data))
    selectInput("locality_TS", "Locality:", selLocality)
  })
  
  #Mostrar localidades Time Series Mensual:
  output$uiLocality_TS_M <- renderUI({
    data = data_M[input$country_TS_M == data_M$CNTRY, "ADMIN4"]
    selLocality = sort(unique(data))
    selectInput("locality_TS_M", "Locality:", selLocality)
  })
  
  #Mostrar limite meses:
  output$uitillMonth <- renderUI({
    mm = input$fromMonth
    wm = which(months %in% mm)
    selectInput("tillMonth", "Till Month", months[wm:12], "Dec")
  })
  
  #Mostrar años:
  output$uiyear <- renderUI({
    data = filterLocality(data, input$country, input$locality)
    selYears = sort(unique(str_sub(data$Date, 1, 4)))
    if(length(selYears>0)) checkboxGroupInput("year","Year:",selYears, selYears)
  })
  
  #Mostrar grafico wl - Walter Lieth:
  output$plot_wl <- renderPlot({
    data = filterLocality(data, input$country, input$locality)
    if(nrow(data)>1) chartWL(data)
  })
  
  #Mostrar grafico temperatura:
  output$plot_temp <- renderPlot({
    plotJulianDay(data, "Temperature", "Julian Day", "Degree Celsius (C)", 
                  varName = c("TMEAN","TMIN","TMAX" ),
                  varState = c(input$tMean, input$tMin, input$tMax),
                  country = input$country,
                  locality = input$locality,
                  year = input$year,
                  month = c(input$fromMonth, input$tillMonth)
    )
  })
  
  #Mostrar grafico precipitacion:
  output$plot_prec <- renderPlot({
    plotJulianDay(data, "Precipitation", "Julian Day", "mm", 
                  varName = c("RAIN"),
                  varState = c(input$prec),
                  varColor = c("blue"),
                  country = input$country,
                  locality = input$locality,
                  year = input$year,
                  month = c(input$fromMonth, input$tillMonth)
    )
  })
  
  #Mostrar grafico humedad relativa:
  output$plot_rh <- renderPlot({
    plotJulianDay(data, "Relative Humidity", "Julian Day", "%", 
                  varName = c("RHMEAN", "RHMIN", "RHMAX"),
                  varState = c(input$rhMean, input$rhMin, input$rhMax),
                  country = input$country,
                  locality = input$locality,
                  year = input$year,
                  month = c(input$fromMonth, input$tillMonth)
    )
  })
  
  #Mostrar rango de dias:
  output$uiDateRange <- renderUI({
    data = filterLocality(data, input$country_TS, input$locality_TS)
    sdate = sort(data$Date)
    minDate = sdate[1]
    maxDate = sdate[length(sdate)]
    dateRangeInput("dateRange","Date range:",min = minDate, max = maxDate, start=minDate, end=maxDate)
  })
  
  #Mostrar rango de dias Mensual:
  output$uiDateRange_M <- renderUI({
    data = filterLocality(data_M, input$country_TS_M, input$locality_TS_M)
    sdate = sort(data$Date)
    minDate = sdate[1]
    maxDate = sdate[length(sdate)]
    dateRangeInput("dateRange_M","Date range:",min = minDate, max = maxDate, start=minDate, end=maxDate, startview = "year")
  })
  
  #Mostrar grafico temperatura - Time Series:
  output$plot_temp_TS <- renderPlot({
    data = filterLocality(data, input$country_TS, input$locality_TS)
    sdate = sort(data$Date)
    minDate = sdate[1]
    maxDate = sdate[length(sdate)]
    dateRange = paste(minDate,"::", maxDate,sep="")
    plotTimeSeries(data,
                   ylab = "Degree Celsius (C)",
                   input$dateRange,
                   varName = "TMEAN",
                   country = input$country_TS, 
                   locality = input$locality_TS,
                   main = "Temperature",
                   trange = trange
    )
  })
  
  #Mostrar grafico precipitacion - Time Series:
  output$plot_prec_TS <- renderPlot({
    data = filterLocality(data, input$country_TS, input$locality_TS)
    sdate = sort(data$Date)
    minDate = sdate[1]
    maxDate = sdate[length(sdate)]
    dateRange = paste(minDate,"::", maxDate,sep="")
    trangeP = c(min(data$RAIN, na.rm=T), max(data$RAIN, na.rm=T))
    plotTimeSeries(data,
                   ylab = "mm",
                   input$dateRange,
                   varName = "RAIN",
                   country = input$country_TS, 
                   locality = input$locality_TS,
                   main = "Precipitation",
                   trange = trangeP
    )
  })
  
  #Mostrar grafico humedad relativa - Time Series:
  output$plot_rh_TS <- renderPlot({
    data = filterLocality(data, input$country_TS, input$locality_TS)
    sdate = sort(data$Date)
    minDate = sdate[1]
    maxDate = sdate[length(sdate)]
    dateRange = paste(minDate,"::", maxDate,sep="")
    trangeRH = c(min(data$RHMIN, na.rm=T), max(data$RHMAX, na.rm=T))
    plotTimeSeries(data,
                   ylab = "%",
                   input$dateRange,
                   varName = "RHMEAN",
                   country = input$country_TS, 
                   locality = input$locality_TS,
                   main = "Relative Humidity",
                   trange = trangeRH
    )
  })
  
  #Mostrar grafico temperatura - Time Series (Mensual):
  output$plot_temp_TS_M <- renderPlot({
    data = filterLocality(data_M, input$country_TS_M, input$locality_TS_M)
    sdate = sort(data$Date)
    minDate = sdate[1]
    maxDate = sdate[length(sdate)]
    dateRange = paste(minDate,"::", maxDate,sep="")
    plotTimeSeries(data,
                   ylab = "Degree Celsius (C)",
                   input$dateRange_M,
                   varName = "TMEAN",
                   country = input$country_TS_M,
                   locality = input$locality_TS_M,
                   main = "Temperature",
                   trange = trange_M
    )
  })
  
  #Mostrar grafico precipitacion - Time Series (Mensual):
  output$plot_prec_TS_M <- renderPlot({
    data = filterLocality(data_M, input$country_TS_M, input$locality_TS_M)
    sdate = sort(data$Date)
    minDate = sdate[1]
    maxDate = sdate[length(sdate)]
    dateRange = paste(minDate,"::", maxDate,sep="")
    trangeP = c(min(data$RAIN, na.rm=T), max(data$RAIN, na.rm=T))
    plotTimeSeries(data,
                   ylab = "mm",
                   input$dateRange_M,
                   varName = "RAIN",
                   country = input$country_TS_M,
                   locality = input$locality_TS_M,
                   main = "Precipitation",
                   trange = trangeP
    )
  })
  
  #Mostrar grafico humedad relativa - Time Series (Mensual):
  output$plot_rh_TS_M <- renderPlot({
    data = filterLocality(data_M, input$country_TS_M, input$locality_TS_M)
    sdate = sort(data$Date)
    minDate = sdate[1]
    maxDate = sdate[length(sdate)]
    dateRange = paste(minDate,"::", maxDate,sep="")
    trangeRH = c(min(data$RHMIN, na.rm=T), max(data$RHMAX, na.rm=T))
    plotTimeSeries(data,
                   ylab = "%",
                   input$dateRange_M,
                   varName = "RHMEAN",
                   country = input$country_TS_M,
                   locality = input$locality_TS_M,
                   main = "Relative Humidity",
                   trange = trangeRH
    )
  })
  
  #Mostrar tabla - Time Series (Mensual):
  output$plot_table_TS_M <- renderTable({
    data = filterLocality(data_M, input$country_TS_M, input$locality_TS_M)
    data = subset(data, Date>=input$dateRange_M[1] & Date<=input$dateRange_M[2])
    data
    #print(head(data2))
  })
  
  #Download summary Table
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("summary", '.csv', sep='')
    },
    
    content = function(file) {
      data = filterLocality(data_M, input$country_TS_M, input$locality_TS_M)
      data2 = subset(data, Date>=input$dateRange_M[1] & Date<=input$dateRange_M[2])
      data2
      write.csv(data2, file)
    }
  )
})