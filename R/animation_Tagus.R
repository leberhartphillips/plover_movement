# Processing Pathtrack nanoFix-mini and Lotek PinPoint-10 tag data

# install libraries
library(mapview)
library(sp)
library(RColorBrewer)
library(stringr)
library(RSQLite)
library(dplyr)
library(moveVis)
library(move)
library(parallel)
library(doParallel)
library(tidyverse)
library(rgdal)
library(plotKML)

# function to read tagging data
import_plover_tag_spatial <- 
  function(data_loc, tag_ID, projection,
           time_zone = "America/Mazatlan",
           tag_model, bird_ID, bird_sex, bird_code){
    
    if(tag_model == "nanoFix-mini")
    {
      # read data file and extract data
      Tag <- 
        read.table(sep = ",", skip = 5, file = data_loc) %>% 
        
        # assign column names
        `colnames<-` (c("day", "month", "year", "hour", "minute", "second", "unknown_col", 
                        "satellites", "latitude", "longitude", "elevation", 
                        "clock_offset", "accuracy_indicator", "battery")) %>% 
        
        mutate(
          # make a fix number column
          fix_number = str_pad(row.names(.), 2, pad = "0"),
          
          # make a timestamp column
          timestamp = paste(paste(year, month, 
                                  day, sep = "-"), 
                            paste(hour, minute, 
                                  second, sep = ":"), sep = " ")) %>% 
        mutate(
          # convert to a useable POSIX time/date string
          timestamp = as.POSIXct(strptime(as.character(timestamp), 
                                          format = "%y-%m-%d %H:%M:%S"), 
                                 tz = time_zone),
          
          # assign name of tag
          tag_ID = tag_ID,
          
          # make a simplified version of the time sting
          timestamp_simple = as.Date(str_sub(as.character(timestamp), 
                                             start = 1, end = 10), 
                                     format = "%Y-%m-%d")) %>% 
        
        # remove observations without a reliable location
        dplyr::filter(latitude != 0 & !is.na(latitude)) %>%
        
        # remove observations without a reliable location
        dplyr::filter(timestamp != 0 & !is.na(timestamp)) %>%
        
        # names() %>% 
        # as.data.frame()
        
        dplyr::select(tag_ID, timestamp_simple, fix_number, timestamp, 
                      satellites, latitude, longitude, elevation, battery) %>% 
        
        mutate(# assign bird ring
          ring = bird_ID,
          
          # assign sex of bird
          sex = bird_sex, 
          
          # assign bird color combo
          code = bird_code) %>% 
        
        # make the dataframe a SpatialPointsDataFrame and define coordinates
        `coordinates<-` (c("longitude", "latitude")) %>% 
        
        # define projection
        `proj4string<-` (projection)
      
      # output result
      Tag
    }
    else if(tag_model == "PinPoint-10"){
      # read data file and extract data
      Tag <- 
        read.table(sep = "", na.strings = "", file = data_loc, fill = TRUE,
                   stringsAsFactors = FALSE, header = TRUE) %>% 
        
        mutate(
          # extract the temporal information from the string
          year = paste0("20", str_sub(string = RTC.date, start = 1, end = 2)),
          month = paste0(str_sub(string = RTC.date, start = 4, end = 5)),
          day = paste0(str_sub(string = RTC.date, start = 7, end = 8)),
          hour = paste0(str_sub(string = RTC.time, start = 1, end = 2)),
          minute = paste0(str_sub(string = RTC.time, start = 4, end = 5)),
          second = paste0(str_sub(string = RTC.time, start = 7, end = 8))
        ) %>% 
        
        mutate(
          # Merge the time and date columns together to formulate the time stamp for a given row
          timestamp = ISOdate(year = year, month = month, day = day, 
                              hour = hour, min = minute, sec = second, 
                              tz = time_zone),
          
          # Tag$datetime[nrow(Tag)] = Tag$datetime[nrow(Tag)] + 8 * 60 * 60,
          # 
          # Tag$hour[nrow(Tag)] = as.character(as.numeric(Tag$hour[nrow(Tag)]) + 8),
          # 
          # timestamp = paste0(year, "-", month, "-", day, " ", hour, ":", minute, ":", second)
          
          timestamp_simple = as.Date(str_sub(as.character(timestamp), 
                                             start = 1, end = 10), 
                                     format = "%Y-%m-%d"),          
          # make a fix number column
          fix_number = str_pad(row.names(.), 2, pad = "0"),
          
          # assign name of tag
          tag_ID = tag_ID
        ) %>% 
        
        # remove observations without a reliable location
        filter(latitude != 0) %>%
        
        dplyr::select(tag_ID, timestamp_simple, fix_number, timestamp, 
                      Sats, Latitude, Longitude, Altitude.m.,
                      Voltage.V.) %>% 
        
        `colnames<-` (tolower(names(.))) %>% 
        
        rename(elevation = altitude.m.,
               battery = voltage.v.,
               satellites = sats,
               tag_ID = tag_id) %>% 
        
        mutate(# assign bird ring
          ring = bird_ID,
          
          # assign sex of bird
          sex = bird_sex, 
          
          # assign bird color combo
          code = bird_code) %>% 
        
        # remove observations without a reliable location
        filter(latitude != 0 | !is.na(latitude)) %>% 
        
        # make the dataframe a SpatialPointsDataFrame and define coordinates
        `coordinates<-` (c("longitude", "latitude")) %>% 
        
        # define projection
        `proj4string<-` (projection)
      
      # output result
      Tag
    }
    
    else{
      print("Unknown tag_model. Options are 'PinPoint-10' or 'nanoFix-mini'.")
    }
  }

NFTag55584 <- 
  import_plover_tag_spatial(data_loc = "data/raw/Tag55584/Obs040621_133158_Tag55584.pos",
                            tag_ID = "NF55584", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "Europe/Lisbon", tag_model = "nanoFix-mini",
                            bird_ID = "D35644", bird_code = "NA", bird_sex = "M")

NFTag55719 <- 
  import_plover_tag_spatial(data_loc = "data/raw/Tag55719/Obs050721_232954_Tag55719.pos",
                            tag_ID = "NF55719", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "Europe/Lisbon", tag_model = "nanoFix-mini",
                            bird_ID = "P01903", bird_code = "NA", bird_sex = "M")

NFTag21200 <- 
  import_plover_tag_spatial(data_loc = "data/raw/Tag21200/Obs060721_152942_Tag21200.pos",
                            tag_ID = "NF21200", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "Europe/Lisbon", tag_model = "nanoFix-mini",
                            bird_ID = "D59933", bird_code = "NA", bird_sex = "F")

NFTag55808 <- 
  import_plover_tag_spatial(data_loc = "data/Tagus/raw/Tag55808/Obs300522_210831_Tag55808.pos",
                            tag_ID = "NF55808", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "Europe/Lisbon", tag_model = "nanoFix-mini",
                            bird_ID = "D59182", bird_code = "NA", bird_sex = "F")

NFTag55831 <- 
  import_plover_tag_spatial(data_loc = "data/Tagus/raw/Tag55831/Obs300522_213604_Tag55831.pos",
                            tag_ID = "NF55808", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "Europe/Lisbon", tag_model = "nanoFix-mini",
                            bird_ID = "D59932", bird_code = "NA", bird_sex = "F")

nrow(as.data.frame(NFTag55808))
head(as.data.frame(NFTag55719))
tail(as.data.frame(NFTag55719))

nrow(as.data.frame(NFTag21200))
head(as.data.frame(NFTag21200))
tail(as.data.frame(NFTag21200))


max(as.data.frame(NFTag55719)$timestamp) - min(as.data.frame(NFTag55719)$timestamp)

All_tags <- 
  bind(NFTag55719, NFTag21200)

UTM13n <- CRS("+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# specify global map options
mapviewOptions(basemaps = c("Esri.WorldImagery","OpenStreetMap.BlackAndWhite","Thunderforest.OpenCycleMap","Esri.WorldShadedRelief"),
               raster.palette = colorRampPalette(brewer.pal(9, "Greys")),
               vector.palette = colorRampPalette(brewer.pal(9, "BrBG")[9:1]),
               na.color = "magenta",
               layers.control.pos = "topright")

# map tag data
mapview(NFTag55831, zcol = "sex", 
        col.regions = brewer.pal(8, "Dark2")[c(2, 1)])#colorRampPalette(brewer.pal(8, "Dark2")))

mapview(NFTag55808, zcol = "sex", 
        col.regions = brewer.pal(8, "Dark2")[c(2, 1)])

# make move object for animation
NFTag55831_move <- df2move(df = arrange(as.data.frame(NFTag55831), timestamp_simple), track_id = "ring", 
                         proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                         x = "longitude", y = "latitude", time = "timestamp")

NFTag55808_move <- df2move(df = arrange(as.data.frame(NFTag55808), timestamp_simple), track_id = "ring", 
                           proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                           x = "longitude", y = "latitude", time = "timestamp")

# check the amount of time between fixes. The tracks in move_data have 
# irregular timestamps and sampling rates. print unique timestamps and timeLag 
# (might vary due to different tagging schedules and models)
unique(timestamps(NFTag55831_move))
unique(timestamps(NFTag55808_move))

timeLag(NFTag55831_move, unit = "hours")
timeLag(NFTag55808_move, unit = "hours")

# use align_move to correct move_data to a uniform time scale and lag using interpolation.
# resolution of 12 hours per timestamp:
NFTag55831_move_align <- align_move(NFTag55831_move, res = 2, unit = "hours")
NFTag55808_move_align <- align_move(NFTag55808_move, res = 2, unit = "hours")

unique(unlist(timeLag(All_tags_move_align, units = "hours")))

ext <- extent(-9.125, -8.955, 38.595, 38.735)

ext[1] - ((ext[1] - ext[2]) * 0.2)
ext[3] - ((ext[3] - ext[4]) * 0.9)

# cities in Sinaloa

All_tags_frames <- 
  {
  frames_spatial(All_tags_move_align, 
                 path_alpha = 0.8, path_size = 3,
                 trace_show = TRUE, tail_size = 0.75, 
                 path_legend = TRUE, path_fade = TRUE,
                 path_colours = brewer.pal(7, "Dark2")[c(1, 2)],
                 map_service = "mapbox", map_type = "satellite",
                 map_token = "pk.eyJ1IjoibHVrZWViZXJoYXJ0IiwiYSI6ImNqeHA5bnUzaTBmZjUzbXF1ZHhwbjlzNXgifQ.zvOb_q-tUGOEtRKoyyki-Q",
                 # map_dir = "data/mapbox/",
                 alpha = 0.9,
                 margin_factor = 2,
                 equidistant = FALSE, 
                 ext = ext) %>%
  # map_service = "osm", map_type = "hydda", alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow(colour = "white",label_size = 4, height = 0.14, size = 0.8,
                 x = ext[1] - ((ext[1] - ext[2]) * 0.32), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.825)) %>%
  add_scalebar(colour = "white", distance = 2, 
               x = ext[1] - ((ext[1] - ext[2]) * 0.065), 
               y = ext[3] - ((ext[3] - ext[4]) * 0.8), 
               height = 0.03) %>%
  add_timestamps(type = "label", 
                 x = ext[1] - ((ext[1] - ext[2]) * 0.15), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.93), 
                 size = 4) %>%
  add_progress()
  }

NFTag55808_move_align_frames <-
  {
    frames_spatial(NFTag55808_move_align, 
                   path_alpha = 0.8, path_size = 3,
                   trace_show = TRUE, tail_size = 0.75, 
                   path_legend = FALSE, path_fade = TRUE,
                   path_colours = brewer.pal(7, "Dark2")[c(2)],
                   map_service = "mapbox", map_type = "satellite",
                   map_token = "pk.eyJ1IjoibHVrZWViZXJoYXJ0IiwiYSI6ImNqeHA5bnUzaTBmZjUzbXF1ZHhwbjlzNXgifQ.zvOb_q-tUGOEtRKoyyki-Q",
                   # map_dir = "data/mapbox/",
                   alpha = 0.9,
                   margin_factor = 2,
                   equidistant = FALSE, 
                   ext = ext) %>%
      # map_service = "osm", map_type = "hydda", alpha = 0.5) %>% 
      add_labels(title = "Kentish Plover, Tagus Estuary, Portugal", 
                 subtitle = "Female D59182",
                 x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
      add_northarrow(colour = "white",label_size = 4, height = 0.1, size = 0.8,
                     x = ext[1] - ((ext[1] - ext[2]) * 0.05), 
                     y = ext[3] - ((ext[3] - ext[4]) * 0.65)) %>%
      add_scalebar(colour = "white", distance = 3, 
                   x = ext[1] - ((ext[1] - ext[2]) * 0.15), 
                   y = ext[3] - ((ext[3] - ext[4]) * 0.9), 
                   height = 0.03) %>%
      add_timestamps(type = "label", 
                     x = ext[1] - ((ext[1] - ext[2]) * 0.75), 
                     y = ext[3] - ((ext[3] - ext[4]) * 0.9), 
                     size = 4)
  }
NFTag55831_move_align_frames <- 
  {
    frames_spatial(NFTag55831_move_align, 
                   path_alpha = 0.8, path_size = 3,
                   trace_show = TRUE, tail_size = 0.75, 
                   path_legend = FALSE, path_fade = TRUE,
                   path_colours = brewer.pal(7, "Dark2")[c(1)],
                   map_service = "mapbox", map_type = "satellite",
                   map_token = "pk.eyJ1IjoibHVrZWViZXJoYXJ0IiwiYSI6ImNqeHA5bnUzaTBmZjUzbXF1ZHhwbjlzNXgifQ.zvOb_q-tUGOEtRKoyyki-Q",
                   # map_dir = "data/mapbox/",
                   alpha = 0.9,
                   margin_factor = 2,
                   equidistant = FALSE, 
                   ext = ext) %>%
      # map_service = "osm", map_type = "hydda", alpha = 0.5) %>% 
      add_labels(title = "Kentish Plover, Tagus Estuary, Portugal", 
                 subtitle = "Female D59932",
                 x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
      add_northarrow(colour = "white",label_size = 4, height = 0.1, size = 0.8,
                     x = ext[1] - ((ext[1] - ext[2]) * 0.05), 
                     y = ext[3] - ((ext[3] - ext[4]) * 0.65)) %>%
      add_scalebar(colour = "white", distance = 3, 
                   x = ext[1] - ((ext[1] - ext[2]) * 0.15), 
                   y = ext[3] - ((ext[3] - ext[4]) * 0.9), 
                   height = 0.03) %>%
      add_timestamps(type = "label", 
                     x = ext[1] - ((ext[1] - ext[2]) * 0.75), 
                     y = ext[3] - ((ext[3] - ext[4]) * 0.9), 
                     size = 4)
  }

All_tags_frames[[80]]
NFTag55808_move_align_frames[[1065]]
NFTag55831_move_align_frames[[4041]]

recovery_location <- 
  as.data.frame(NFTag55584)[c(23),]

sinaloa_cities <- 
  data.frame(y = c(24.8091, 23.2494, 23.9019),
             x = c(-107.3940, -106.4111, -106.9286)) %>% 
  mutate(laby = y - 0.08,
         labx = x - 0.1)

All_tags_frames2 <- 
  add_gg(All_tags_frames, 
         gg = expr(
           geom_point(aes(x = longitude, y = latitude), 
                      data = recovery_location, 
                      color = "white", fill = "#7570B3", 
                      shape = 21, size = 3)
         ), 
         data = recovery_location)

All_tags_frames2[[80]]

All_tags_frames2 <- add_text(All_tags_frames2, "Found dead on 06-01\nTag recovered from carcass  06-04", 
                             y = recovery_location[1, "latitude"], 
                             x = recovery_location[1, "longitude"] - 0.012,
                             colour = "black", size = 2, type = "label")

All_tags_frames2 <- add_text(All_tags_frames2, "Notes: no data collected between 05-22 1300 and 05-29 0500 presumably\ndue to carcas laying on tag. Few data collected at recovery location\nbetween 05-29 1300 and 06-04 0100 (carcass moved to allow GPS signal?)", 
                             y = recovery_location[1, "latitude"], 
                             x = -8.92,
                             colour = "black", size = 2, type = "label")

# All_tags_frames2 <- add_text(All_tags_frames2, "Ceuta",
#                              y = sinaloa_cities[3, "laby"], 
#                              x = sinaloa_cities[3, "labx"],
#                              colour = "black", size = 3, type = "label")
All_tags_frames2[[1140]]

# this line opens an R Session in all of your cores (which makes it faster to create the single pictures)
cl = detectCores() %>% makePSOCKcluster; registerDoParallel(cl)

# animate frames
# animate_frames(frames[1:25], width = 350, height = 350, res = 50,
#                overwrite = TRUE, out_file = "R/animations/PinPoint_moveVis_Oct_21_test.gif")
animate_frames(All_tags_frames, width = 800, height = 800, 
               overwrite = TRUE, out_file = "products/animations/55719_21200_animation.mp4")

animate_frames(NFTag55831_move_align_frames, width = 800, height = 800, 
               overwrite = TRUE, out_file = "products/animations/55831_D59932_animation.mp4")

animate_frames(NFTag55808_move_align_frames, width = 800, height = 800, 
               overwrite = TRUE, out_file = "products/animations/55808_D59182_animation.mp4")
# closes the R Sessions in the other cores (not the one that you are working on)
stopCluster(cl)
registerDoSEQ()

view_spatial(Tag20996_align)
