if ( !require(shiny) ) install.packages('shiny')
library('shiny')

source("global.R")

shinyServer(function(input, output) {

     #initialize Data -- skip when testing
     if(!(exists('regionData') & exists('regionInfo') & exists('terrorismData'))) {
          source("initialize.R")}

     ###############################################################################
     ############################ Creating the Maps ################################
     ###############################################################################

     #    Main functions:
     updateShapes <- function(){

          leafletProxy('Map') %>%
               removeControl('legend') %>%
               clearShapes()

          if ( input$showCountries) {
               regionName <- regionInfo[input$region,]$Name

               regionEvents <- terrorismData[terrorismData$iyear == input$year &
                                                  terrorismData$region2 == regionName,]
               mapdata <- reactive({
                    regionData[regionData$Year == input$year &
                                    (regionData$Country %in% worldshapes$admin[worldshapes$region_wb == regionName]),]
               })

               mapshapes <- worldshapes[worldshapes$region_wb == regionName,]





               #         Renders country shapes
               leafletProxy('Map') %>%
                    addPolygons(
                         data = mapshapes, layerId = ~admin,
                         weight = 2, fillColor = getColor(input$countryVar, mapdata(), input$year, input$region),
                         color = "black", fillOpacity = 0.7) %>%
                    addLegend("bottomleft", pal = pal, values = variableScale,
                              opacity = 0.7,
                              labFormat = labelFormat( transform =
                                                            ifelse(
                                                                 any( input$countryVar %in% c("TotalPopulation", "GDPPerCapita", "MilXpnd", "NumIncidents", "NumFatalities", "NumWounded") ) ,
                                                                 exp,
                                                                 identity),
                                                       digits = 3),
                              title = input$countryVar, layerId = 'legend')

          }
     }
     updateMarkers <- function(){

          if(!input$freezeMarkers) leafletProxy('Map') %>% clearMarkers()

          regionName <- regionInfo[input$region,]$Name

          regionEvents <- terrorismData[terrorismData$iyear == input$year &
                                             terrorismData$region2 == regionName,]

          mapdata <- reactive({
               regionData[regionData$Year == input$year &
                               (regionData$Country %in% worldshapes$admin[worldshapes$region_wb == regionName]),]
          })

          mapshapes <- worldshapes[worldshapes$region_wb == regionName,]

          if(input$showIncidents) {

               #         Checks if there are any events for that year and breaks if there aren't any
               if(nrow(regionEvents) == 0) return()

               #         Renders markers if there are
               leafletProxy('Map') %>% addCircleMarkers(
                    lng=regionEvents$longitude, lat=regionEvents$latitude,
                    color = "red", opacity = .2, weight = 7,
                    fillColor = "yellow", fillOpacity = .7,
                    radius = regionEvents$severity,
                    popup=regionEvents$info
               )

          }
     }
     updateRegion <- function(){

          region <- regionInfo[input$region,]

          leafletProxy('Map') %>%
               clearControls() %>%
               clearMarkers() %>%
               clearShapes() %>%
               setView(region$X, region$Y, zoom = region$Z) %>%
               addControl({
                    "<center><h4>Mouseover a country for more information.</h4></center>"},
                    position = 'topright',
                    layerId = 'CountryInfo'
               )

     }

     #    Create blank map
     output$Map <- renderLeaflet({
          leaflet()  %>%
               addProviderTiles("CartoDB.Positron")
     })

     #    Updates polygons and markers whenever year changes
     observeEvent({input$year}, {
          if(!input$freezeShapes) updateShapes()
          updateMarkers()
     })

     #    Updates polygons whenever options change
     observeEvent({input$countryVar; input$freezeShapes}, {
          if(!input$freezeShapes) updateShapes()
     })

     #    Reloads everything when new region selected
     observeEvent({input$region}, {
          updateRegion()
          updateShapes()
          updateMarkers()
     })

     #    Toggles between showing and hiding shapes
     observeEvent(input$showCountries, {
          if (!input$showCountries) leafletProxy('Map') %>% clearShapes() %>% removeControl('legend')
          else updateShapes()
     })

     #    Toggles between showing and hiding markers
     observeEvent(input$showIncidents, {
          if (!input$showIncidents) leafletProxy('Map') %>% clearMarkers()
          else updateMarkers()
     })

     ###############################################################################
     ##################### Creating the Information Box ############################
     ###############################################################################

     # Main function
     createInfoBox <- function(overId){

          if(overId %>% is.null()) return(
               "<center><h4>Mouseover a country for more information.</h4></center>"
          )

          dataset <- regionData[regionData$Country == overId &
                                     regionData$Year == input$year,]

          paste0(
               paste0("<center><h3>", overId, ", ", input$year, "</h3></center><br/>"),
               "<table style='width: 50%; display:inline-block;' align='right'>
               <tr><td colspan='2'><center><h4>Country Information</h4></center></td>",
               createTextRow(dataset, "Population", "TotalPopulation", "in millions"),
               createTextRow(dataset, "GDP Per Capita", "GDPPerCapita", "fixed PPP$"),
               createTextRow(dataset, "Life Expectancy", "LifeExpectancy"),
               createTextRow(dataset, "Child mortality", "UnderFiveMortality", "deaths per 1000 births"),
               createTextRow(dataset, "Fertility Rate", "ChildrenPerWoman", "children per woman"),
               createTextRow(dataset, 'HDI', "HDI", 'last available data'),
               createTextRow(dataset, 'Military Expenditure', "MilXpnd", 'percent of GDP'),
               "</table>",

               "<table style='width: 50%; display:inline-block' align='left'>
               <tr><td colspan='2'><center><h4>Terrorism Details</h4></center></td>",
               createTextRow(dataset, "Number of attacks", "NumIncidents"),
               createTextRow(dataset, "Fatalities", "NumFatalities"),
               createTextRow(dataset, "Injuries", "NumWounded"),
               createTextRow(dataset, "Percent successful", "PctSuccess"),
               createTextRow(dataset, "Deaths per incident", "FatalPerInc"),
               "</table>",



               "<br/>",
               "<table style='width: 90%;' align='left'>",
               createTextRow(dataset, "Dominant religious identity", "ReligionDetail"),
               "</table>"
          )

     }

     #    Smaller helper function for creating individual rows
     createTextRow <- function(dataset, varname, var, explanation = ""){
          vardata <- dataset[[var]]
          paste0("<tr><td style='white-space:nowrap'><p class='cell'><b>",
                 varname, '<br/><small>', explanation, '</small></p>',
                 "</b></td> <td style='white-space:normal'><p class='numbercell'>",
                 ifelse ( vardata %>% is.na, "-", format(vardata, big.mark = ",", digits = 5, nsmall = 0)),
                 ifelse ( (var == "PctSuccess") & (vardata %>% is.na %>% `n'est pas`), "%", ""),
                 "</p></td></tr>" )
     }

     #    Renders information box when mouse is over a polygon
     observeEvent({input$Map_shape_mouseover}, {
          CountryOver <- input$Map_shape_mouseover


          leafletProxy('Map')  %>%
               removeControl('CountryInfo') %>%
               addControl(
                    createInfoBox(CountryOver$id),
                    position = 'topright',
                    layerId = 'CountryInfo'
               )

     })

     observeEvent(input$Map_shape_mouseout, {

          if( input$Map_shape_mouseout$id %>% is.null %>% not) {
               leafletProxy('Map') %>%
                    removeControl('CountryInfo')

          }
          })


     # Text to be displayed in the side bar
     output$sidebarText <- renderText({"
          <div style='padding:1em'>
               Click on an incident for more details, or
               search the <a href='http://www.start.umd.edu/gtd/search/BrowseBy.aspx'>
               Global Terrorism database</a>.
          </div>

          <div style='padding:1em'>
               <b>Note:</b> The locations of some incidents had to be estimated
               with the <a href='http://www.geonames.org/'>GeoNames database</a>.
               As a result, a few markers may appear in weird places!
          </div>

          <div style='padding:1em'>
               More resources for instructors: <a href=''>Stats2Labs</a>.
</div>
          "})

})

