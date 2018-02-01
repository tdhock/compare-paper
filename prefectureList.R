source("packages.R")

## Download locations of prefectures from google.

load("sushi.RData")

prefLines <- readLines("prefectures.txt")
pattern <- paste("(?<code>[0-9]+)",
                 ":",
                 "(?<prefecture>.*)",
                 sep="")
matched <- namedCapture::str_match_named(prefLines, pattern)
prefectures <- matched[,"prefecture"]
names(prefectures) <- matched[,"code"]

## check if there are some equal valued scores, OK!
head(sushi$scores)
apply(sushi$sc, 1, table)

## copied from
## http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps

construct.geocode.url <- function
(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address) {
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    c(lat, lng)
  } else {
    c(NA,NA)
  }
}

prefLocations <-
  matrix(NA, length(prefectures), 2,
         dimnames=list(prefecture=prefectures,
           coordinate=c("latitude", "longitude")))
x <- gGeoCode("Tokushima")
prefLocations["Tokushima",] <- x

while(any(is.na(prefLocations))){
  for(pref.i in seq_along(prefectures)){
    pref <- prefectures[[pref.i]]
    pref.str <- if(pref=="foreign countries"){
      "Berkeley, CA"## Assumption: all foreigners come from Berkeley!
    }else{
      paste0(pref, ", Japan")
    }
    if(is.na(prefLocations[pref,1])){
      g.vec <- gGeoCode(pref.str)
      cat(sprintf("%4d / %4d %s -> %s %s\n", pref.i, length(prefectures), pref.str, g.vec[1], g.vec[2]))
      prefLocations[pref,] <- g.vec
      Sys.sleep(1)
    }
  }
}

locs <- data.frame(prefLocations, prefectures)
ggplot(locs)+
  geom_text(aes(longitude, latitude, label=prefectures))

prefectureList <- list(location=locs, code=prefectures)

save(prefectureList, file="prefectureList.RData")
