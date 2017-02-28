#' \code{tidyGeography}
#' @description Uses Google's geocoding API to return geocoded data from a vector of locations.
#' @param x A vector of locations.
#' @param api.key Google geocoding API key.
#' @details This fuction iterates through a character vector of locations and returns
#' a data.frame of geocoded variables including country, city, latitude, longitude, etc.
#' @examples
#' x <- c('98406', '40601', '32801', '79835', '61701') #vector of US postal codes
#' x <- c('Yorba Linda, CA', 'Boise, ID', 'Tulsa, OK', 'Pittsburgh, PA', 'Birmingham, AL') #vector of US cities and states
#' x <- c('Okinawa, Japan', 'Lisbon, Portugal', 'Toronto, Canada', 'Melbourne, Australia', 'Cairo, Egypt') #vector of cities around the world
#' @importFrom data.table rbindlist
#' @export
tidyGeography <- function(x, api.key) {

    source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
    source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")

    geocode_apply <- function(x){
        geocode(x, source = "google", output = "all", api_key=api.key)
    }

    .extractCountry <- function(x)
    {
        country <- ""
        for (comp in x$results[[1]]$address_components)
            if ("country" %in% comp$types)
            {
                country <- comp$long_name
                break
            }
        country
    }

    .extractState <- function(x)
    {
        state <- ""
        for (comp in x$results[[1]]$address_components)
            if ("administrative_area_level_1" %in% comp$types)
            {
                state <- comp$short_name
                break
            }
        state
    }

    .extractCity <- function(x)
    {
        city <- ""
        for (comp in x$results[[1]]$address_components)
            if ("locality" %in% comp$types)
            {
                city <- comp$short_name
                break
            }
        city
    }

    .extractCounty <- function(x)
    {
        county <- ""
        for (comp in x$results[[1]]$address_components)
            if ("administrative_area_level_2" %in% comp$types)
            {
                county <- comp$short_name
                break
            }
        county
    }


    #Check for valid API key
    testloc <- '1600 Amphitheatre Parkway, Mountain+View, CA'
    geocode_results <- sapply(testloc, geocode_apply, simplify = F)
    condition_a <- sapply(geocode_results, function(x) x["status"]=="OK")
    geocode_results <- geocode_results[condition_a]

    if(length(geocode_results) == 0) {
        stop("Invalid API key")
    }

    #Check for character vector
    if(!is.vector(x) || is.list(x) || is.data.frame(x)) {
        print("Error: Input is not a vector.")
    }
    else
    {

        x <- gsub("%", " ", x)

        geocode_results <- sapply(x, geocode_apply, simplify = F)

        #if (x["status"]=="OK"){
            condition_b <- lapply(geocode_results, lapply, length)
            condition_b2 <- sapply(condition_b, function(x) x["results"]=="1")
            #print(geocode_results)
            geocode_results <- geocode_results[condition_b2]
            #geocode_results[!condition_b2] <- rep(NaN,ncol(geocode_results))
            geocode_results

            for(i in 1:length(geocode_results)){
                dynamic_j <- length(geocode_results[[i]]$results[[1]]$address_components)
                for(j in 1:dynamic_j){
                    if(length(geocode_results[[i]]$results[[1]]$address_components[[j]]$types)>2){
                        geocode_results[[i]]$results[[1]]$address_components[[j]]$types<-geocode_results[[i]]$results[[1]]$address_components[[j]]$types[(length(geocode_results[[i]]$results[[1]]$address_components[[j]]$types)-1):length(geocode_results[[i]]$results[[1]]$address_components[[j]]$types)]
                    }
                }
                if(length(geocode_results[[i]]$results[[1]]$types)>2){
                    geocode_results[[i]]$results[[1]]$types<-geocode_results[[i]]$results[[1]]$types[(length(geocode_results[[i]]$results[[1]]$types)-1):length(geocode_results[[i]]$results[[1]]$types)]
                }
                if(length(geocode_results[[i]]$results[[1]]$types)<1){
                    geocode_results[[i]]$results[[1]]$types<-"Unknown"
                }
                dynamic_k <- length(geocode_results[[i]]$results[[1]]$address_components)
                for(k in 1:dynamic_k){
                    if(length(geocode_results[[i]]$results[[1]]$address_components[[k]]$types)<1){
                        geocode_results[[i]]$results[[1]]$address_components[[k]]$types<-"Unknown"
                    }
                }
                if(length(geocode_results[[i]]$results[[1]]$postcode_localities)>2){
                    geocode_results[[i]]$results[[1]]$postcode_localities<-geocode_results[[i]]$results[[1]]$postcode_localities[(length(geocode_results[[i]]$results[[1]]$postcode_localities)-1):length(geocode_results[[i]]$results[[1]]$postcode_localities)]
                }
            }

            results_b <- lapply(geocode_results, as.data.frame)
            results_c <- lapply(results_b,function(x) subset(x, select=c("results.formatted_address", "results.geometry.location")))
            results_d <- lapply(results_c,function(x) data.frame(Location=x[1,"results.formatted_address"],
                                                                lat=x[1,"results.geometry.location"],
                                                                lng=x[2,"results.geometry.location"]))

            results_e <- rbindlist(results_d)

            results_f <- results_e[,Original_value:=names(results_d)]
        #}

        results_f$city <- unname(sapply(geocode_results, .extractCity))
        results_f$county <- unname(sapply(geocode_results, .extractCounty))
        results_f$state <- unname(sapply(geocode_results, .extractState))
        results_f$country <- unname(sapply(geocode_results, .extractCountry))
        results_f

    }
}
