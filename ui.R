if(!require(shinydashboard)) install.packages('shinydashboard')
if(!require(leaflet)) install.packages('leaflet')
library(shinydashboard)
library(leaflet)

shinyUI(dashboardPage(
     skin="blue",

     dashboardHeader(title="Exploring Terrorism", disable = F),

     dashboardSidebar(
          selectInput('region', "Region", choices = c(
               'Middle East & North Africa' = "MidEast",
               "North America" = "NorthAm",
               "South Asia" = "SouthAs",
               "Sub-Saharan Africa" = "SubSahr",
               "Europe & Central Asia" = "Eurasia",
               "Latin America & Caribbean" = "LatinAm",
               "East Asia & Pacific" = "AsiaPac"
          )),
          sliderInput("year",
                      "Year",
                      min = 1970,
                      max = 2013,
                      value = 1970,
                      step = 1,
                      sep = "",
                      animate = animationOptions(interval = 1000)),
          selectInput("countryVar", "Color countries by:",
                      choices = c(
                           "Total Population (log)" = "TotalPopulation",
                           "GDP Per Capita (log)" = "GDPPerCapita",
                           "Life Expectancy" = "LifeExpectancy",
                           "Child mortality" = "UnderFiveMortality",
                           "Children per woman" = "ChildrenPerWoman",
                           "Human Development Index" = "HDI",
                           "Military Expenditure (log)" = "MilXpnd",
                           "Dominant religion" = "Religion",
                           "Number of incidents (log)" = "NumIncidents",
                           "Total fatalities (log)" = "NumFatalities",
                           "Total wounded (log)" = "NumWounded")),

          div(style='padding: 0.5em 1em 0.5em',
              actionButton('advOptions', "Advanced Options")),

          conditionalPanel(condition = 'input.advOptions % 2',
                           checkboxInput("showIncidents",
                                         "Show incident markers", TRUE),
                           checkboxInput("showCountries",
                                         "Show country polygons", TRUE),
                           checkboxInput('freezeShapes',
                                         "Freeze country polygons", FALSE),
                           checkboxInput('freezeMarkers',
                                         "Don't remove markers (uses more memory!)", FALSE)),

          htmlOutput('sidebarText')
     ),

     dashboardBody(
          tags$head(tags$style(HTML( # Additional style parameters
                              ' 
                              td {
                                   padding: 0px 10px 0px 10px;
                                   vertical-align: middle;
                              }
                              small {
                                   font-size: 12px;
                                   color: #444;
                                   font-weight: normal;
                                   font-style: italic;
                              }
                              p.cell {
                                   line-height: 80%;
                              }
                              p.numbercell{
                                   left: 0px;
                              }
                              .shiny-input-container {
                                padding: 0px 15px 0px 12px;
                              }
                              .legend {
                                   white-space: nowrap;
                              }
                              .main-header > .navbar { 
                                   display: none; 
                              }
                              .content-wrapper, .main-footer, .right-side {
                                   margin-left: inherit;
                              }
.skin-blue .main-header .logo {
font-family: "Arial";
}
                             '))),
          leafletOutput("Map", width='100%', height='50em')


     )

))
