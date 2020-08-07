#Author: Zaporozhtsev I.F.
#Created: May, 2019

library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(DBI)
library(shinyalert)

towns <- read_sf("./towns/towns.shp",options = "ENCODING=UTF-8",stringsAsFactors = FALSE);
Encoding(towns$name) <- "UTF-8";

conn <- dbConnect(RPostgres::Postgres(),
                    dbname = '###',                   
                    host = '###',
                    port = 5432,
                    user = '###',
                    password = '###');

shinyServer(function(input, output,session) {
  
  town_labels <- sprintf(
    "<strong>%s</strong><br/>%.3f км<sup>2</sup>",
    towns$name, towns$area
  ) %>% lapply(htmltools::HTML);
  
  long_space <- "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
  firefighters_label <- paste0("<strong>",long_space,"ГУ МЧС России по Мурманской области",long_space,"</strong>") %>% lapply(htmltools::HTML);
  
  firefighters_icon <- makeIcon(iconUrl = "images/station.png",iconWidth = 40, iconHeight = 50);
  fire_icon <- makeIcon(iconUrl = "images/fire.png",iconWidth = 40, iconHeight = 50);
  
  firefighters_point_index <- 12716;
  firefighters_point_lat <- 68.8924;
  firefighters_point_lon <- 33.1059;  
  
  output$murmap <- renderLeaflet({
    top<-69.1;
    bottom<-68.8;
    right<-33.55;
    left<-32.85;
    
    osm_route_source <- "[(32.93, 68.86), 
                          (33.5, 68.86), 
                          (33.5, 69.05),
                          (32.93, 69.05),
                          (32.93, 68.86)]";
    
    fire_area <- "[(33.09, 68.89), 
                  (33.19, 68.89), 
                  (33.19, 69.04),
                  (33.09, 69.04),
                  (33.09, 68.89)]";
    c1 <- chartr('()[]','    ', osm_route_source);
    c2 <- chartr('()[]','    ', fire_area);
    m1 <- matrix(as.numeric(strsplit(c1,",")[[1]]),ncol=2,byrow=TRUE);
    m2 <- matrix(as.numeric(strsplit(c2,",")[[1]]),ncol=2,byrow=TRUE);
    
    leaflet(options = leafletOptions(minZoom = 10,maxZoom = 16))%>%setMaxBounds(right,bottom,left,top)%>%
      addTiles(group = "OpenStreetMap.Default") %>%
      addPolygons(data = m1, color = "green", weight = 2,  dashArray = "3",fillOpacity = 0.0)  %>%
      addPolygons(data = m2, color = "red", weight = 2,  dashArray = "3",layerId = "userinput") %>%
      addPolygons(data=towns,
                  fillColor = "yellow",
                  fillOpacity = 0.7,
                  weight = 4,
                  color = "black",
                  opacity = 0.2,
                  dashArray = "5",
                  highlight = highlightOptions(weight = 5,
                                               color = "red",
                                               dashArray = "",
                                               fillOpacity = 0.9,
                                               bringToFront = TRUE),
                  label = town_labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                              padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"));
  })  
  
  observe({
    click <- input$murmap_shape_click;
    
    if(is.null(click)) return();
    if(length(input$murmap_shape_click$id) == 0) return();
    if(input$murmap_shape_click$id != "userinput") return();
    
    cllat <- input$murmap_shape_click$lat;
    cllon <- input$murmap_shape_click$lng;
   
    fire_text <- paste0("<strong>",long_space,"Очаг пожара: <br/>",long_space,"широта= ", 
                        format(round(cllat, 4), nsmall = 4), "°с.ш.,  ",
                        "<br/>",long_space,"долгота=", format(round(cllon, 4), nsmall = 4),
                        "°в.д.",long_space,"</strong>") %>% lapply(htmltools::HTML);
    
    sql = paste("WITH vertices AS (select * from ways_vertices_pgr where id in (",
                "SELECT source FROM vehicle_net UNION SELECT target FROM vehicle_net)) ",
                "SELECT id FROM vertices ",
                "ORDER BY the_geom <-> ST_SetSRID(ST_Point(",cllon,",",cllat,"), 4326) LIMIT 1;",sep="");
    dttemp <- dbGetQuery(conn, sql);
    nearest_point_index <-dttemp$id[1];

    sql = paste0("WITH dijkstra AS (SELECT * FROM pgr_dijkstra(
                          'SELECT gid AS id, * FROM vehicle_net',",firefighters_point_index,",",nearest_point_index,")),
                       get_geom AS (SELECT dijkstra.*, ways.name, CASE WHEN dijkstra.node = ways.source THEN the_geom
                           ELSE ST_Reverse(the_geom) END AS route_geom FROM dijkstra JOIN ways ON (edge = gid) ORDER BY seq)
                  SELECT seq, ST_AsText(route_geom) FROM get_geom ORDER BY seq;");
    dttemp <- dbGetQuery(conn, sql);
    proxy_map <- leafletProxy("murmap") %>%
      clearMarkers() %>%
      removeShape(layerId="route") %>%
      addMarkers(lat = cllat,
                  lng = cllon,
                  icon = fire_icon,
                  label = fire_text,
                  labelOptions = labelOptions(noHide = T,  textOnly = TRUE,
                                    style = list(
                                      "color" = "red", 
                                      "font-size" = "22px",
                                      "font-family" = "serif", 
                                      "font-style" = "bold"))) %>%
      addMarkers(lat = firefighters_point_lat,
                 lng = firefighters_point_lon,
                 icon = firefighters_icon,
        label = firefighters_label,
        labelOptions = labelOptions(noHide = T, textOnly = TRUE,
                                    style = list(
                                      "color" = "blue", 
                                      "font-size" = "22px",
                                      "font-family" = "serif", 
                                      "font-style" = "bold")));
    
    lon_lats <- chartr(' LINESTRING()[]',',              ', dttemp$st_astext[1]);
    split_result <- strsplit(lon_lats,",");
    if(length(split_result) > 0)
      m <- matrix(as.numeric(strsplit(lon_lats,",")[[1]]),ncol=2,byrow=TRUE)
    else shinyalert("Ошибка построения маршрута","Маршрут не построен!",type = "error");
    
    for (i in 2:nrow(dttemp)){
      lon_lats <- chartr(' LINESTRING()[]',',              ',dttemp$st_astext[i]);
      split_result <- strsplit(lon_lats,",");
      if(length(split_result) > 0)
        m <- rbind(m,matrix(as.numeric(split_result[[1]]),ncol=2,byrow=TRUE));
    }
    proxy_map <- proxy_map %>% addPolylines(data = m,layerId="route");
  })
})