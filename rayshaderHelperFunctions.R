############### RAYSHADER HELPER FUNCTIONS ###############
# Credits: https://wcmbishop.github.io/rayshader-demo/
# Credits: https://gist.github.com/slopp/8fc582d50c8eaa2a0fb365dab7da4559
# Helper functions----
## Create box----
boundingBox <- function(df){
  # define bounding box with longitude/latitude coordinates
  bbox <- list(
    p1 = list(long = min(df$lng), lat = min(df$lat)),
    p2 = list(long = max(df$lng), lat = max(df$lat))
  )
  
  # leaflet() %>%
  #   addTiles() %>% 
  #   addRectangles(
  #     lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
  #     lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
  #     fillColor = "transparent"
  #   ) %>%
  #   fitBounds(
  #     lng1 = bbox$p1$long, lat1 = bbox$p1$lat,
  #     lng2 = bbox$p2$long, lat2 = bbox$p2$lat,
  #   )
  return(bbox)
}
## Define image size----
define_image_size <- function(bbox, major_dim = 600) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
  # define dimensions
  img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
  img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height, width = img_width, size = size_str)
}

## Get elevation data----
get_usgs_elevation_data <- function(bbox, size = "600,600", file = NULL,
                                    sr_bbox = 4326, sr_image = 4326) {
  library(httr)
  
  # TODO - validate inputs
  
  url <- parse_url("https://elevation.nationalmap.gov/arcgis/rest/services/3DEPElevation/ImageServer/exportImage")
  res <- GET(
    url, 
    query = list(
      bbox = paste(bbox$p1$long, bbox$p1$lat, bbox$p2$long, bbox$p2$lat,
                   sep = ","),
      bboxSR = sr_bbox,
      imageSR = sr_image,
      size = size,
      format = "tiff",
      pixelType = "F32",
      noDataInterpretation = "esriNoDataMatchAny",
      interpolation = "+RSP_BilinearInterpolation",
      f = "json"
    )
  )
  
  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    # TODO - check that bbox values are correct
    # message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    
    img_res <- GET(body$href)
    img_bin <- content(img_res, "raw")
    if (is.null(file)) 
      file <- tempfile("elev_matrix", fileext = ".tif")
    writeBin(img_bin, file)
    # message(paste("image saved to file:", file))
  } else {
    warning(res)
  }
  invisible(file)
}

# Update messages while running the function----
ui <- function(phrase) {
  cat(paste(crayon::red(clisymbols::symbol$circle)), phrase, sep = " ")
  cat("\n")
} 

ui_check <- function(...) {
  phrase <- do.call(paste, list(...))
  cat(paste(crayon::green(clisymbols::symbol$tick), phrase, sep = " "))
  cat("\n")
}

## Get Arc GIS map image----
get_arcgis_map_image <- function(bbox, map_type = "World_Street_Map", file = NULL, 
                                 width = 400, height = 400, sr_bbox = 4326) {
  require(httr)
  require(glue) 
  require(jsonlite)
  
  url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")
  
  # define JSON query parameter
  web_map_param <- list(
    baseMap = list(
      baseMapLayers = list(
        list(url = jsonlite::unbox(glue("https://services.arcgisonline.com/ArcGIS/rest/services/{map_type}/MapServer",
                                        map_type = map_type)))
      )
    ),
    exportOptions = list(
      outputSize = c(width, height)
    ),
    mapOptions = list(
      extent = list(
        spatialReference = list(wkid = jsonlite::unbox(sr_bbox)),
        xmax = jsonlite::unbox(max(bbox$p1$long, bbox$p2$long)),
        xmin = jsonlite::unbox(min(bbox$p1$long, bbox$p2$long)),
        ymax = jsonlite::unbox(max(bbox$p1$lat, bbox$p2$lat)),
        ymin = jsonlite::unbox(min(bbox$p1$lat, bbox$p2$lat))
      )
    )
  )
  
  res <- GET(
    url, 
    query = list(
      f = "json",
      Format = "PNG32",
      Layout_Template = "MAP_ONLY",
      Web_Map_as_JSON = jsonlite::toJSON(web_map_param))
  )
  
  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    # message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    if (is.null(file)) 
      file <- tempfile("overlay_img", fileext = ".png")
    
    img_res <- GET(body$results[[1]]$value$url)
    img_bin <- content(img_res, "raw")
    writeBin(img_bin, file)
    # message(paste("image saved to file:", file))
  } else {
    message(res)
  }
  invisible(file)
}

# Find image coordinates----
find_image_coordinates <- function(long, lat, bbox, image_width, image_height) {
  x_img <- round(image_width * (long - min(bbox$p1$long, bbox$p2$long)) / abs(bbox$p1$long - bbox$p2$long))
  y_img <- round(image_height * (1-(lat - min(bbox$p1$lat, bbox$p2$lat)) / abs(bbox$p1$lat - bbox$p2$lat)))
  list(x = x_img, y = y_img)
}

# Render the route as labels----
render_route <- function(route,
                         sample = 10, routeColor = 'black', nudgeMultiplier = .005){
  
  # render the route as labels
  
  # calculate nudge amount
  znudge <- nudgeMultiplier*(max(route$elev_matrix) - min(route$elev_matrix))
  
  pb <- progress::progress_bar$new(total = nrow(route$df)/sample)
  for (i in seq(1, nrow(route$df), by = sample)) {
    # translate lat/lon into x,y within this box
    # TODO: This should be more efficient!
    pb$tick()
    pos <- find_image_coordinates(
      long = route$df$lng[i],
      lat = route$df$lat[i],
      bbox = route$bbox,
      image_width = route$image_size$width,
      image_height = route$image_size$height
    )
    
    
    # add a dot tracking progress
    render_label(
      route$elev_matrix,
      x = pos$x,
      y = pos$y,
      z = znudge,
      zscale = route$zscale,
      relativez = TRUE,
      offset = 0,
      alpha = 0,
      textsize = 2,
      text = "."
      , textcolor = routeColor
    )
  }
  
}

# Combine the helper functions in a single function----
plot_3d_route_area <- function(df, zscale = 15, mapType = c('World_Street_Map', 'World_Topo_Map', 'World_Imagery')[3]){
  
  # create bounding box for the ride
  ui("1 of 5: Locating Route...")
  bbox <- boundingBox(df)
  
  # get elevation data for the box
  ui("2 of 5: Getting Route Elevation Data...")
  image_size <- define_image_size(bbox, major_dim = 600)
  file <- get_usgs_elevation_data(bbox, size = image_size$size)
  
  elev_img <- raster::raster(file)
  elev_matrix <- matrix(
    raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
    nrow = ncol(elev_img), ncol = nrow(elev_img)
  )
  
  # calculate rayshader layers
  ui("3 of 5: Calculating Rayshader Layers...")
  ambmat <- ambient_shade(elev_matrix, zscale = zscale)
  raymat <- ray_shade(elev_matrix, zscale = zscale, lambert = TRUE)
  watermap <- detect_water(elev_matrix)
  
  # get the overlay map
  ui("4 of 5: Overlaying Street Data...")
  file <- get_arcgis_map_image(bbox, 
                               map_type = mapType,
                               width = image_size$width, 
                               height = image_size$height)
  
  overlay_img <- png::readPNG(file)
  
  # create the plot
  ui("5 of 5: Opening RGL Device...")
  rgl::clear3d()
  elev_matrix %>% 
    sphere_shade(texture = "imhof4") %>% 
    add_water(watermap, color = "imhof4") %>%
    add_overlay(overlay_img, alphalayer = 0.5) %>%
    add_shadow(raymat, max_darken = 0.5) %>%
    add_shadow(ambmat, max_darken = 0.5) %>%
    plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
            water = TRUE, wateralpha = 0,
            theta = 25, phi = 30, zoom = 0.65, fov = 60)
  
  ui("You should now see a map. Call render_route with the results of this function to view your ride!")
  list(
    elev_matrix = elev_matrix,
    df = df,
    bbox = bbox,
    image_size = image_size,
    zscale = zscale
  )
}

