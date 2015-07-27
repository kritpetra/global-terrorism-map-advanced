
#

progress <- shiny::Progress$new()

##

progress$set(value = 0.1, message = "Loading packages",
             detail = "leaflet")
if ( !require(leaflet) ) install.packages('leaflet')
library('leaflet')
###

progress$set(value = 0.15, message = "Loading packages",
             detail = "maptools")
if ( !require(maptools) ) install.packages('maptools')
library(maptools)

####

progress$set(value = 0.2, message = "Loading packages",
             detail = "shinydashboard")
if ( !require(shinydashboard) ) install.packages('shinydashboard')
library(shinydashboard)

#####

progress$set(value = 0.25, message = "Loading packages",
             detail = "magrittr")
if ( !require(magrittr) ) install.packages('magrittr')
library(magrittr)

######

progress$set(value = 0.3, message = "Loading datasets",
             detail = "ESRI shapefile")
worldshapes <- readShapeSpatial('worldshapes/worldshapes')

########

progress$set(value = 0.4, message = "Loading datasets",
             detail = "Country data")
require(readr)
regionData <- read_csv('regionData.csv')

############

progress$set(message = "Loading datasets", value = 0.6,
             detail = "Terrorism database")
terrorismData <- read_csv('terrorismData.csv')

##################

progress$set(value = 0.9, message = "Loading datasets",
             detail = "Region information")
regionInfo <- {data.frame('Name' = c('MidEast' = 'Middle East & North Africa',
                                     'NorthAm' = 'North America',
                                     'SouthAs' = 'South Asia',
                                     'SubSahr' = 'Sub-Saharan Africa',
                                     'Eurasia' = 'Europe & Central Asia',
                                     'LatinAm' = 'Latin America & Caribbean',
                                     'AsiaPac' = 'East Asia & Pacific'),
                          'Map' = c('MidEast' = 'MidEastMap',
                                    'NorthAm' = 'NorthAmMap',
                                    'SouthAs' = 'SouthAsMap',
                                    'SubSahr' = 'SubSahrMap',
                                    'Eurasia' = 'EurasiaMap',
                                    'LatinAm' = 'LatinAmMap',
                                    'AsiaPac' = 'AsiaPacMap'),
                          'X' = c('MidEast' = 23,
                                  'NorthAm' = -95,
                                  'SouthAs' = 78,
                                  'SubSahr' = 17,
                                  'Eurasia' = 35,
                                  'LatinAm' = -85,
                                  'AsiaPac' = 130),
                          'Y' = c('MidEast' = 28,
                                  'NorthAm' = 38,
                                  'SouthAs' = 24,
                                  'SubSahr' = -5,
                                  'Eurasia' = 50,
                                  'LatinAm' = -10,
                                  'AsiaPac' = 5),
                          'Z' = c('MidEast' = 4,
                                  'NorthAm' = 4,
                                  'SouthAs' = 5,
                                  'SubSahr' = 4,
                                  'Eurasia' = 3,
                                  'LatinAm' = 3,
                                  'AsiaPac' = 3))}

####################

progress$close()
