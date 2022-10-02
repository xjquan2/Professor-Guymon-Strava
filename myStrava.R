# Strava API
# Libabies----
library(tidyverse)
library(magrittr)
library(lubridate)
library(cetcolor) # color palette
library(leaflet) # For details of map
library(leaflet.extras) # For heatmap
library(rayshader) # For 3d map
library(here)
source('rayshaderHelperFunctions.R')
# Read in data after it has already been compiled and converted to an rds file----
allData <- readRDS('activityData.rds')

# Plot a single activity without a map----
oneActivity <- allData %>% 
  # dplyr::filter(activity_id == 6017255489) %>%
  # dplyr::filter(activity_name == 'Run with my sons') %>%
  dplyr::filter(activity_name == 'Plummet from the Summit') %>%
  # dplyr::filter(activity_name == "Zion's Observation Point") %>%
  rename(lng = long)
ggplot(oneActivity, aes(x = lng, y = lat, color = altitude_f)) + 
  geom_path() +
  theme_void() +
  scale_color_gradientn('Altitude (ft)', colours = cet_pal(5, name="inferno")) +
  labs(title = oneActivity$activityTitle[1])

# 3d scatterplot using rayshader----
p1 <- ggplot(oneActivity, aes(x = lng, y = lat, color = altitude_f)) + 
  geom_point() +
  scale_color_gradientn('Altitude (ft)', colours = cet_pal(5, name="inferno")) +
  labs(title = oneActivity$activityTitle[1])
p1
rgl::clear3d()
p2 <- p1 %>% plot_gg( height = 3
                , width = 3.5
                , multicore = T
                , pointcontract = .7
                , soliddepth = -200)
render_camera(zoom = .5, theta = -30, phi = 30)
render_snapshot(clear = T)

# 3d scatterplot using rayshader and a map----
oneRide <- plot_3d_route_area(oneActivity
                              , zscale = 15
                              , mapType = 'World_Street_Map')
render_route(oneRide, sample = 2, routeColor = 'red', nudgeMultiplier = .0005)

# 3d scatterplot GIF using rayshader and a map----
bbox <- boundingBox(oneActivity)
image_size <- define_image_size(bbox, major_dim = 600)
elev_file <- get_usgs_elevation_data(bbox, size = image_size$size
                        # , file = elev_file
                        , sr_bbox = 4326, sr_image = 4326)
elev_img <- raster::raster(elev_file)
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

# calculate rayshader layers
ambmat <- ambient_shade(elev_matrix, zscale = 30)
raymat <- ray_shade(elev_matrix, zscale = 30, lambert = TRUE)
watermap <- detect_water(elev_matrix)

overlay_file <- get_arcgis_map_image(bbox, map_type = "World_Topo_Map"
                                     # , file = overlay_file
                                     , width = image_size$width, height = image_size$height
                                     , sr_bbox = 4326)
overlay_img <- png::readPNG(overlay_file)


# gif transition variables
n_frames <- 180
theta <- transition_values(from = 0, to = 360, steps = n_frames, 
                           one_way = TRUE, type = "lin")
phi <- transition_values(from = 10, to = 70, steps = n_frames, 
                         one_way = FALSE, type = "cos")
zoom <- transition_values(from = 0.4, to = 0.8, steps = n_frames, 
                          one_way = FALSE, type = "cos")

# GIF it!
zscale <- 10
elev_matrix %>% 
  sphere_shade(texture = "imhof4") %>% 
  add_water(watermap, color = "imhof4") %>%
  add_overlay(overlay_img, alphalayer = 0.5) %>%
  add_shadow(raymat, 0.4) %>%
  add_shadow(ambmat, 0.4) %>%
  save_3d_gif(elev_matrix
              , file = "plummetFromTheSummit.gif"
              , duration = 6,
              zscale = zscale, windowsize = c(1200, 1000), wateralpha = 0,
              water = TRUE, soliddepth = -max(elev_matrix)/zscale, 
              theta = theta, phi = phi, zoom = zoom, fov = 60)


# 3d scatterplot using plotly----
plotly::plot_ly(oneActivity, x = ~long, y = ~lat, z = ~altitude_f
                , color = ~heart_rate_bpm, size = 1
)

# Heatmap of all activities on one map----
allData %>%
  # filter(activityNumber >= 500) %>%
  filter(!is.na(long)) %>%
  leaflet() %>%
  addTiles() %>%
  addHeatmap(lng = ~long, lat = ~lat, blur = 20, radius = 10)
# Facetplot of all data by type----
plotData <- allData %>% dplyr::filter(type == 'Ride' & !is.na(lat))
ggplot(plotData
       , aes(x = long, y = lat, color = speed_m_h)) + geom_path() + theme_void() +
  scale_color_gradientn('Speed (mi/hr)', colours = cet_pal(5, name="inferno")) +
  facet_wrap(vars(activityTitle), ncol = 5, scales = 'free')



# Plot a single activity with a map----
leaflet(oneActivity) %>%
  addTiles() %>%
  addCircles(lng = ~long, lat = ~lat)

# Overlay two routes (from different times or people) to evaluate differences----

# Try a rayshader plot----
# Download a raster file and unzip it----
# loadzip = tempfile()
# download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
# localtif = raster::raster(unzip(loadzip, "dem_01.tif"))
# unlink(loadzip)

# If the raster file has already been downloaded and unzipped
localtif = raster::raster("dem_01.tif")

#And convert it to a matrix:
elmat = raster_to_matrix(localtif)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()
#sphere_shade can shift the sun direction:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  plot_map()
#detect_water and add_water adds a water layer to the map:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  plot_map()

#And we can add a raytraced layer from that sun direction as well:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat), 0.5) %>%
  plot_map()


# 3d map
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot()

# All rides----
plotData <- allData %>%
  dplyr::filter(type == 'Ride' & !is.na(lat))
allRides <- ggplot(plotData
                  , aes(x = long, y = lat, color = altitude_m)) + geom_path() + theme_void() +
  scale_color_gradientn('Altitude', colours = cet_pal(5, name="inferno")) +
  facet_wrap(vars(date), nrow = 20, scales = 'free')
allRides
test <- allData %>% dplyr::filter(activityNumber == 1)
testPlot <- ggplot(test, aes(x = long, y = lat, color = altitude_m)) + geom_path() + theme_void() +
  scale_color_gradientn('Altitude', colours = cet_pal(5, name="inferno"))
# Plot on a map----

  








# devtools::install_github('fawda123/rStrava')
library(tidyverse)
library(magrittr)
library(lubridate)
library(rStrava)
# Authenticate----
app_name <- 'RonsData' # chosen by user
app_client_id  <- '33837' # an integer, assigned by Strava
app_secret <- '7b3aff9f576964b2659dcd67080cd4009af7a014' # an alphanumeric secret, assigned by Strava

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret
                                            , cache = T # This saves the file for later use
                                            ))
# stoken <- httr::config(token = readRDS('.httr-oauth')[[1]]) # Useful if you don't want to authenticate again.

# Get elevation data from Google----
# https://developers.google.com/maps/documentation/elevation/start#api_key
# # save the key, do only once
# cat("google_key=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n",
#     file=file.path(normalizePath("~/"), ".Renviron"),
#     append=TRUE)
# 
# # retrieve the key, restart R if not found
# mykey <- Sys.getenv("google_key")


# Get my information----
myinfo <- get_athlete(stoken)
head(myinfo)



