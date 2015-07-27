
### Function: create color vector, given values ###
getColor <- function(variable, data, year, region){

     values <- data[[variable]]

     if(any(variable %in% c("LifeExpectancy", "UnderFiveMortality", "ChildrenPerWoman", "HDI"))) {
          lower <- min(regionData[regionData$Year == year, variable],
                       na.rm = TRUE)
          upper <- max(regionData[regionData$Year == year, variable],
                       na.rm = TRUE)
          if(lower == Inf) return("#808080")

          pal <<- colorNumeric("YlGnBu", c(lower, upper))
          variableScale <<- values
     }

     else if (any(variable %in% c("TotalPopulation", "GDPPerCapita"))) {
          lower <- min(log(regionData[regionData$Year == year, variable]+0.1),
                       na.rm = TRUE)
          upper <- max(log(regionData[regionData$Year == year, variable]+0.1),
                       na.rm = TRUE)
          if(lower == Inf) return("#808080")

          pal <<- colorNumeric("YlGnBu", c(lower, upper))
          variableScale <<- log(values+0.1)

     }

     else if (any(variable %in% c("NumIncidents","NumWounded","NumFatalities"))){
          lower <- min(log(regionData[regionData$Year == year, variable]+1),
                       na.rm = TRUE)
          upper <- max(log(regionData[regionData$Year == year, variable]+1),
                       na.rm = TRUE)
          if(lower == Inf) return("#808080")

          pal <<- colorNumeric(rev(heat.colors(99)), c(lower,upper))
          variableScale <<- log(values+1)

     }

     else if ( variable == "Religion") {
          pal <<- colorFactor(
               c("gold", "#CFB53B", "#F3E5AB", "#009900", "forestgreen", "#4CBB17", "deepskyblue", "maroon", "darkorange", "lightgrey"),
               c("Christian (Catholic)", "Christian (Orthodox)", "Christian (Other)", "Muslim (Other)", "Muslim (Sunni)", "Muslim (Shiʿa)", "Jewish", "Hindu", "Buddhist", "None/Other"),
               ordered=TRUE
          )
          variableScale <<- values

     }

     else if (any(variable %in% c("MilXpnd"))) {
          lower <- min(log(regionData[regionData$region == region, variable]+0.01),
                       na.rm = TRUE)
          upper <- max(log(regionData[regionData$region == region, variable]+0.01),
                       na.rm = TRUE)
          if(lower == Inf) return("#808080")

          pal <<- colorNumeric("YlGnBu", c(lower,upper))
          variableScale <<- log(values+0.01)

     }

     pal(variableScale)

}
