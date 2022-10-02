# ==========================================================================
# Create U.S. map with url links for each state
# ==========================================================================
library(tidyverse)
library(tigris)
library(leaflet)
library(htmlwidgets)
library(leaflet.extras)

jsCode <- paste0('
 function(el, x, data) {
  var marker = document.getElementsByClassName("leaflet-interactive");
  for(var i=0; i < marker.length; i++){
    (function(){
      var v = data.win_url[i];
      marker[i].addEventListener("click", function() { window.open(v);}, false);
  }()); 
  }
 }
')

state <-
    states(
        cb = TRUE
    ) %>% 
    filter(STUSPS %in% c('CA', 'WA', 'MD', 'OH')) %>%
    mutate(win_url = 
        case_when(
            STUSPS == 'CA' ~ 'https://shiny.demog.berkeley.edu/alexramiller/kqed-evictions/', 
            STUSPS == 'WA' ~ 'https://evictionresearch.net/washington/maps/summary.html',
            STUSPS == 'MD' ~ 'https://evictionresearch.net/maryland/report/baltimore.html',
            STUSPS == 'OH' ~ 'https://evictionresearch.net/ohio/dayton.html'
        )
        )

map <- 
leaflet(
  state, 
  options = leafletOptions(zoomControl = FALSE, minZoom = 3.5, maxZoom = 3.5, dragging = FALSE)) %>% 
    # suspendScroll() %>% 
    setView(lat = 39.82, lng = -98.57, zoom = 3.5) %>% 
    addMapPane(name = "polygons", zIndex = 410) %>% 
    addMapPane(name = "maplabels", zIndex = 420) %>% 
     addProviderTiles("CartoDB.PositronNoLabels") %>%
     addProviderTiles("CartoDB.PositronOnlyLabels", 
                    options = leafletOptions(pane = "maplabels"),
                    group = "map labels") %>%
  addPolygons(
        fillOpacity = .5, 
        color = 'Red', 
        stroke = TRUE, 
        label = ~NAME, 
        weight = 1, 
        opacity = .5, 
        highlightOptions = highlightOptions(
                    color = "#ff4a4a", 
                    weight = 5,
                    bringToFront = TRUE
                    )  
        )  %>%
  htmlwidgets::onRender(jsCode, data=state) 

map
saveWidget(map, '~/git/evictionresearch/evictions-study.github.io/maps/us_map.html')
