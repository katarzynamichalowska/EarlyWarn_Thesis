# K. Michalowska

#### Reading data ####

#Libraries
#To install libraries use install.packages(c("package1", "package2"))
sapply(c("RCurl", "jsonlite","rlist", "numbers"), require, character.only = TRUE)


#' @name weatherInfo
#' @description Read data about weather stations (sources), types of measurements (elements),
#' locations of weather stations (locations) and return it as a table
#' @param client_id Frost Met client ID
#' @param par ['sources', 'elements', 'locations'] Type of information to be read
#' @return data.frame
weatherInfo <- function(client_id="66cb548e-f40e-4d44-b9dc-8ab2d9e66026:3158f32d-470b-4655-b884-e251a6f04970",
                     par="sources"){
  
  url <- getURL(paste0("https://frost.met.no/", par, "/v0.jsonld"), 
                userpwd=client_id, httpauth = 1L, .encoding="UTF-8") 
  outcome <- fromJSON(url)
  
  if(par=="locations"){
    table <- data.frame(outcome$data[1:2],
                        geom.type = outcome$data$geometry[1],
                        cor1 = unlist(lapply(outcome$data$geometry$coordinates, "[[",1)),
                        cor2 = unlist(lapply(outcome$data$geometry$coordinates, "[[",2))) 
  }
  
  else if(par == "sources"){
    outcome$data[[7]]$coordinates[outcome$data[[7]]$coordinates=="NULL"] <- NA
    outcome$data[outcome$data$shipCodes=="NULL",]$shipCodes <- NA
    outcome$data[outcome$data$icaoCodes=="NULL",]$icaoCodes <- NA
    outcome$data[outcome$data$externalIds=="NULL",]$externalIds <- NA
    outcome$data[outcome$data$stationHolders=="NULL",]$stationHolders <- NA
    
    table <- cbind(outcome$data[1:6],
                   geom.type = outcome$data[[7]][[1]],
                   cor1 = unlist(lapply(outcome$data[[7]]$coordinates, "[[", 1)),
                   cor2 = unlist(lapply(outcome$data[[7]]$coordinates, function(l) if (is.na(l[[1]])) NA else l[[2]])),
                   outcome$data[8:10],
                   validFrom = outcome$data[11],
                   outcome$data[12:13],
                   outcome$data[15:16])
    table$validFrom <- as.POSIXct(table$validFrom)
    #table$cor1 <- as.numeric(as.character(table$cor1))
    #table$cor2 <- as.numeric(as.character(table$cor2))
    
  }
  else if (par == "elements"){
    table <- cbind(outcome$data[1:5],
                   outcome$data[[6]],
                   outcome$data[7],
                   outcome$data[[8]],
                   outcome$data[[9]],
                   outcome$data[[10]],
                   outcome$data[[11]],
                   codeTable = outcome$data[[12]])
  }
  return(table)
}


#' @name availableObservations
#' @description Returns metadata about informations available at a given source
#' @param client_id Frost Met client ID
#' @param source_list String of source IDs
#' @return data.frame
availableObservations <- function(client_id="66cb548e-f40e-4d44-b9dc-8ab2d9e66026:3158f32d-470b-4655-b884-e251a6f04970",
                                  source_list){
  url <- getURL(paste0("https://frost.met.no/observations/availableTimeSeries/v0.jsonld?",
                       "sources=", paste(source_list, collapse = ",")), 
                userpwd=client_id, httpauth = 1L) 
  
  outcome <- fromJSON(url)
  obs <- outcome$data
  obs$validFrom <- as.POSIXct(paste0(as.Date(obs$validFrom)," ",substr(obs$validFrom,12,19)))
  return(obs)
}

#' @name closestSource
#' @description calculates the distances between given city and all the sources
#' and returns the ID of n sources that are the closest
#' @param cityname (character) Name of the city in UTF-8
#' @param n (integer) number of sources that should be returned
#' @return (character vector) ID of the source which is the closest to the city
closestSource <- function(cityname="Sn\u00e5sa", n=1){
  
  dist <- function(x1,y1,x2,y2){
    return(sqrt((x1-x2)^2+(y1-y2)^2))
  }
  
  sources$dist <- dist(sources$cor1, sources$cor2, 
                       locations[locations$name==cityname,]$cor1, locations[locations$name==cityname,]$cor2)
  
  id <- sources[order(sources$dist),][1:n,]$id
  return(id)
}

#' @name readObservations
#' @param source (character vector) - list of sources (stations) where the observations are recorded
#' @param element (character vector) - list of elements
#' @param start_date (POSIXct date) - date from which the observations should be collected (YYYY-mm-ddThh:mm)
#' @param end_date (POSIXct date) - date until which the observations should be colllected (YYYY-mm-ddThh:mm)
#' @param df (boolean) - parameter whether the observations should be returned in form of a data.frame (TRUE) or a JSON list (FALSE)
#' @return data.frame (or a list) - observations of given element recorded at given sources between the dates start_date and end_date
#' @description reads observations from start_date to end_date from all sources listed as source, for a given time resolution
readObservations <- function(client_id="66cb548e-f40e-4d44-b9dc-8ab2d9e66026:3158f32d-470b-4655-b884-e251a6f04970",
                             source_list = c("SN4780"), 
                             start_date = "2015-01-01T00:00",
                             end_date = "2015-01-03T00:00",
                             df = TRUE,
                             timeresolution="PT1H"){
  obsToDf <- function(outcome){
    obs <- outcome$data$observations
    reftimes <- do.call(c,lapply(mapply(rep, outcome$data$referenceTime, 
                                        unlist(lapply(lapply(obs, '[[', 1), length))), function(x){as.POSIXct(paste0(as.Date(x)," ",unlist(lapply(x, substr, 12, 19))))
                                        }))
    source  <- unlist(mapply(rep, outcome$data$sourceId, 
                             unlist(lapply(lapply(obs, '[[', 1), length))))
    
    data <- data.frame(sourceId = source,
                       referenceTime = as.POSIXct(paste0(as.Date(reftimes)," ",substr(reftimes,12,19))))
    data <- cbind(data, list.stack(lapply(obs, list.flatten, use.names=TRUE), fill=TRUE))
    data <- data[data$timeResolution==timeresolution,]
    data$referenceTime <- as.POSIXct(format(data$referenceTime, "%Y-%m-%d"))
    
    return(data)
  }
  
  av <- availableObservations(source_list=source_list)
  av <- av[!((av$validTo>start_date)==FALSE) | is.na(av$validTo),]
  element <- unique(av[av$timeResolution==timeresolution,]$elementId)
  
  start_date <- paste0(substr(start_date, 1,10), "T", substr(start_date, 12, 16))
  end_date <- paste0(substr(end_date, 1,10), "T", substr(end_date, 12, 16))
  element <- unique(element)
  
  url <- getURL(paste("https://frost.met.no/observations/v0.jsonld?",
                      "sources=", paste(source_list, collapse = ","),
                      "&referencetime=", start_date, "/", end_date,
                      "&elements=", gsub(" ", "%20", paste(element, collapse = ",")),
                      sep = "", collapse = "", 
                      paste0("&timeresolutions=", timeresolution)),
                userpwd=client_id, httpauth = 1L) 
  
  outcome <- try(fromJSON(url, simplifyDataFrame = TRUE), silent=T)
  if(class(outcome)=="try-error"){
    print(outcome[[1]])
  }
  else{
    if(df){
      outcome <- obsToDf(outcome)
    }
    return(outcome)
  }
}

#' @name singleFcast
#' @param day integer
#' @param month integer
#' @param year integer
#' @param lat numeric latitude of the forecast
#' @param lon numeric longitude of the forecast
#' @return data.frame of forecast for a given day and given coordinates
#' @description a function returning a single-day forecast for given coordinates
#' Warning 05.11.2018: the server has been temporarily unavailable

singleFcast <- function(day = 24, month = 12, year = 2016, lat=64.24557, lon=12.381806){
  if(class(day)!="character" & day<10){day <- paste0(0,as.character(day))}
  if(class(month)!="character" & month<10){month <- paste0(0,as.character(month))}
  url <- paste0("http://thredds.met.no/thredds/ncss/meps25epsarchive/", 
                paste(year, month, day, sep="/"), 
                "/meps_mbr0_pp_2_5km_",
                paste0(year, month, day),
                "T00Z.nc?var=land_area_fraction&var=latitude&var=longitude&var=cloud_area_fraction&var=helicopter_triggered_index&var=high_type_cloud_area_fraction&var=low_type_cloud_area_fraction&var=medium_type_cloud_area_fraction&var=precipitation_amount&var=precipitation_amount_high_estimate&var=precipitation_amount_low_estimate&var=precipitation_amount_middle_estimate&var=precipitation_amount_prob_low&var=thunderstorm_index_combined&var=wind_speed_of_gust&var=x_wind_10m&var=y_wind_10m&var=fog_area_fraction&var=precipitation_amount_acc&",
                #"var=surface_air_pressure&",
                "var=air_temperature_2m&var=relative_humidity_2m&var=air_pressure_at_sea_level&", "latitude=", 
                lat, 
                "&longitude=", 
                lon,
                "&time=", 
                paste(year, month, day, sep="-"), 
                "T00%3A00%3A00Z&vertCoord=0.0&accept=csv_file")
  a <- getURL(url)
  lista <- strsplit(a, split="\n")[[1]]
  lista <- lista[!lista==""]
  lista <- lapply(lista, function(x) unlist(strsplit(x, split=",")))
  k <- 1
  obs <- list()
  for(i in length(lista):1){
    if(mod(i,2)!=1){
      obs[[k]] <- as.data.frame(t(as.data.frame(lista[[i]])))
      colnames(obs[[k]]) <- lista[[i-1]]
      rownames(obs[[k]]) <- NULL
      k <- k+1
    }
  }
  obs <- list.stack(obs, fill=TRUE)
  
  obs <- unique(as.data.frame(lapply(apply(obs,2,unique), function(x) x[!is.na(x)])))
  obs <- obs[obs$vertCoord.unit..m..=="0.0",]
  if(length(obs)==nrow(variable_units)){
    names(obs) <- unlist(sapply(lapply(as.list(names(obs1)), function(x) as.data.frame(t(unlist(strsplit(x, split=".unit.."), recursive = TRUE)))), "[[", 1))
    names(obs)[1] <- "referenceTime"
  }
  if(nrow(obs)>0){
    obs[,1] <- as.POSIXct(obs[,1])
    obs[,t(obs)=="NaN"] <- NA
    obs[sapply(obs, class)=="factor"] <- 
      sapply(obs[sapply(obs, class)=="factor"], function(x) as.numeric(as.character(x)))
  }
  return(obs)
}

#' @name readFcast
#' @param dayfrom integer
#' @param monthfrom integer
#' @param yearfrom integer
#' @param dayto integer
#' @param monthto integer
#' @param yearto integer
#' @param lat numeric latitude of the forecast
#' @param lon numeric longitude of the forecast
#' @return data.frame of forecasts
#' @description returns a dataframe of weather forecast for specified coordinates from and to a given date
readFcast <- function(dayfrom = 24, monthfrom = 11, yearfrom = 2016,
                    dayto = 26, monthto = 11, yearto = 2016,
                    lat=64.24557, lon=12.381806){
  
  dates <- as.Date(as.Date(paste(yearfrom, monthfrom, dayfrom, sep="-"), format = "%Y-%m-%d"):as.Date(paste(yearto, monthto, dayto, sep="-"), format = "%Y-%m-%d"))
  fcast <- list()
  for(i in 1:length(dates)){
    fcast[[i]] <- singleFcast(day = format(dates[[i]], "%d"), month = format(dates[[i]], "%m"), year = format(dates[[i]], "%Y"),
                              lat = lat, lon=lon)
    print(paste0("Forecast date: ", dates[[i]]))
  }
  fcast <- list.rbind(fcast)
  fcast <- rmEmptyCol(fcast)
  
  return(fcast)
}

#' @name readEventLogs
#' @param filename string the name of the xlsx file in the folder
#' @param sheet integer sheet number of the xlsx file
#' @param skiplines integer the number of lines to be skipped at the beginning of the xlsx file
#' @param merge.repeating boolean should lines with less than 10 minutes time distance be treated as one sample?
#' @return data.frame of timestamps of faults
#' @description reads xlsx files with fault event timestamps
readEventLogs <- function(filename="title.xlsx", sheet=1, skiplines=4, merge.repeating = TRUE){
  df <- read_excel(paste0("C:/KASIA/SINTEF/Data/", filename), sheet = sheet, skip=skiplines, col_names = FALSE)
  df <- as.data.frame(do.call(rbind, strsplit(df$X__1, " ")))
  df <- df[-c(2,3,7)]
  df <- data.frame(event = df[,1],
                   referenceTime = as.POSIXct(paste0(paste0(as.Date(df[,2], format="%d-%m-%Y"), " ", df[,3]), ".", df[,4])),
                   endTime = as.POSIXct(paste0(paste0(as.Date(df[,5], format="%d-%m-%Y"), " ", df[,6]), ".", df[,7])))
  
  if(merge.repeating){
    df$timediff <- c(0, diff(df$referenceTime))
    df <- df[df$timediff>600,] #only obs with more than 10 minutes difference
    df <- df[,-length(df)]
  }
  
  df <- df[order(df$referenceTime),]
  return(df)
}
