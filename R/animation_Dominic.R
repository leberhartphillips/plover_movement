# R Script for processing Pathtrack nanoFix-mini and Lotek PinPoint-10 tag data
# and making an animation
# Author: Luke Eberhart-Hertel (luke.eberhart@orn.mpg.de)
# 12-May-2022

# install libraries
library(tidyverse)
library(sp)
library(mapview)
library(RColorBrewer)
library(moveVis)
library(move)

# function to read tagging data
import_plover_tag_spatial <- 
  function(data_loc, tag_ID, projection,
           time_zone = "America/Mazatlan",
           tag_model, bird_ID, bird_sex, bird_code, n_slice = 0){
    
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
        
        # remove first observation (calibration fix)
        # slice(n_slice * -1) %>%
        
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
        dplyr::select(tag_ID, timestamp_simple, fix_number, timestamp, 
                      Sats, Latitude, Longitude) %>% 
        
        `colnames<-` (tolower(names(.))) %>% 
        
        rename(satellites = sats,
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

# import tag data to visualize
ringo_Clara <- 
  import_plover_tag_spatial(data_loc = "data/Husum/PinPoint 50678 2022-05-05 03-56-57 for Luke female Ringed Plover NWR-WWM.txt",
                            tag_ID = "PinPoint_50678", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "GMT", tag_model = "PinPoint-10",
                            bird_ID = "Clara", bird_code = NA, bird_sex = "F")

ringo_NRN_WWM <- 
  import_plover_tag_spatial(data_loc = "data/Husum/PinPoint 50641 2022-05-10 14-05-21 for Luke.txt",
                            tag_ID = "PinPoint_50641", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "GMT", tag_model = "PinPoint-10",
                            bird_ID = "NRN_WWM", bird_code = NA, bird_sex = "F")

ringo_Bob <- 
  import_plover_tag_spatial(data_loc = "data/Husum/PinPoint 50629 2022-05-04 17-11-46 for Luke male Ringed Plover NNR-WWM Bob.txt",
                            tag_ID = "PinPoint_50629", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "GMT", tag_model = "PinPoint-10",
                            bird_ID = "Bob", bird_code = NA, bird_sex = "M")

ringo_Justus <- 
  import_plover_tag_spatial(data_loc = "data/Husum/PinPoint 50626 2022-05-05 14-36-33 for Luke male Ringed Plover NWR-NWM Justus.txt",
                            tag_ID = "PinPoint_50626", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "GMT", tag_model = "PinPoint-10",
                            bird_ID = "Justus", bird_code = NA, bird_sex = "M")

ringo_Vera <- 
  import_plover_tag_spatial(data_loc = "data/Husum/PinPoint 50615 2022-05-10 11-42-33 for Luke female Ringed plover NWR-GWM.txt",
                            tag_ID = "PinPoint_50615", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "GMT", tag_model = "PinPoint-10",
                            bird_ID = "Vera", bird_code = NA, bird_sex = "F")

# bind all long-term tags together
LT_tags <- bind(ringo_Clara,
                ringo_Bob,
                ringo_Justus,
                ringo_Vera)
# check the data (number of rows, head, and tail)
nrow(as.data.frame(LT_tags))
head(as.data.frame(LT_tags))
tail(as.data.frame(LT_tags))

# check the timespan between the first and last fix in the dataset
max(as.data.frame(LT_tags)$timestamp) - 
  min(as.data.frame(LT_tags)$timestamp)

# do the same for the short-term tag deployments
ST_tags <- ringo_NRN_WWM
nrow(as.data.frame(ST_tags))
head(as.data.frame(ST_tags))
tail(as.data.frame(ST_tags))
max(as.data.frame(ST_tags)$timestamp) - 
  min(as.data.frame(ST_tags)$timestamp)

# specify global map options
mapviewOptions(basemaps = c("Esri.WorldImagery","OpenStreetMap.BlackAndWhite","Thunderforest.OpenCycleMap","Esri.WorldShadedRelief"),
               raster.palette = colorRampPalette(brewer.pal(9, "Greys")),
               vector.palette = colorRampPalette(brewer.pal(9, "BrBG")[9:1]),
               na.color = "magenta",
               layers.control.pos = "topright")

# map tag data
mapview(LT_tags, zcol = "sex", 
        col.regions = brewer.pal(8, "Dark2")[c(2, 1)])#colorRampPalette(brewer.pal(8, "Dark2")))

mapview(ST_tags, zcol = "sex", 
        col.regions = brewer.pal(8, "Dark2")[c(2, 1)])#colorRampPalette(brewer.pal(8, "Dark2")))

# wangle longterm tagging data to subset to autumn migration period
LT_tags_wrangle <- 
  LT_tags %>%
  as.data.frame(.) %>% 
  arrange(timestamp_simple) %>% 
  filter(timestamp_simple > as.Date("2021-07-16") & timestamp_simple < as.Date("2021-10-19"))

# make move object for animation
LT_tags_move <- df2move(df = LT_tags_wrangle, track_id = "ring", 
                        proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                        x = "longitude", y = "latitude", time = "timestamp")

ST_tags_move <- df2move(df = arrange(as.data.frame(ST_tags), timestamp_simple), track_id = "ring", 
                        proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                        x = "longitude", y = "latitude", time = "timestamp")

# check the amount of time between fixes. The tracks in move_data have 
# irregular timestamps and sampling rates. print unique timestamps and timeLag 
# (might vary due to different tagging schedules and models)
unique(timestamps(LT_tags_move))
timeLag(LT_tags_move, unit = "hours")

unique(timestamps(ST_tags_move))
timeLag(ST_tags_move, unit = "hours")

# use align_move to correct move_data to a uniform time scale and lag using interpolation.
# resolution of 12 hours per timestamp (or 3 mins per timestamp for short-term tags):
LT_tags_move_align <- align_move(LT_tags_move, res = 12, unit = "hours")
ST_tags_move_align <- align_move(ST_tags_move, res = 3, unit = "mins")

# check the time between fixes of the aligned data
unique(unlist(timeLag(LT_tags_move_align, units = "hours")))
unique(unlist(timeLag(ST_tags_move_align, units = "mins")))

# make the frames of the animation
LT_tags_move_frames <- 
  frames_spatial(LT_tags_move_align, 
                 path_alpha = 0.8, path_size = 3,
                 trace_show = TRUE, tail_size = 0.75, 
                 path_legend = TRUE, path_fade = TRUE,
                 path_colours = brewer.pal(7, "Dark2")[c(1,2,3,4)],
                 map_service = "mapbox", map_type = "satellite",
                 map_token = "pk.eyJ1IjoibHVrZWViZXJoYXJ0IiwiYSI6ImNqeHA5bnUzaTBmZjUzbXF1ZHhwbjlzNXgifQ.zvOb_q-tUGOEtRKoyyki-Q",
                 alpha = 0.9,
                 margin_factor = 1.5,
                 equidistant = FALSE) %>%
  add_labels(title = "Common Ringed Plovers", 
             subtitle = "Tagged at Beltringharder Koog, Germany",
             x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_scalebar(colour = "white", distance = 1000,
               x = 0.5, y = 39, 
               height = 0.03) %>%
  add_timestamps(type = "label",
                 x = 0, y = 57,
                 size = 4) %>%
  add_progress()

# check the last frame to see if the annotations are nicely positioned
LT_tags_move_frames[[171]]

# do the same for the short-term tag data
ST_tags_move_frames <- 
  frames_spatial(ST_tags_move_align, 
                 path_alpha = 0.8, path_size = 3,
                 trace_show = TRUE, tail_size = 0.75, 
                 path_legend = FALSE, path_fade = TRUE,
                 path_colours = brewer.pal(7, "Dark2")[c(2)],
                 map_service = "mapbox", map_type = "satellite",
                 map_token = "pk.eyJ1IjoibHVrZWViZXJoYXJ0IiwiYSI6ImNqeHA5bnUzaTBmZjUzbXF1ZHhwbjlzNXgifQ.zvOb_q-tUGOEtRKoyyki-Q",
                 alpha = 0.9,
                 margin_factor = 1.5,
                 equidistant = FALSE) %>%
  add_labels(title = "Common Ringed Plover", 
             subtitle = "Tagged at Beltringharder Koog, Germany",
             x = "Longitude", y = "Latitude") %>%
  add_northarrow(colour = "white",label_size = 4, height = 0.1, size = 0.8,
                 x = 8.868, y = 54.5375) %>%
  add_scalebar(colour = "white", distance = 1,
               x = 8.868, y = 54.5365,
               height = 0.03) %>%
  add_timestamps(type = "label",
                 x = 8.885, y = 54.541,
                 size = 4) %>%
  add_progress()

ST_tags_move_frames[[466]]

# this line opens an R Session in all of your cores (which makes it faster to create the single pictures)
cl = detectCores() %>% makePSOCKcluster; registerDoParallel(cl)

# animate frames and export as a movie
animate_frames(LT_tags_move_frames, width = 800, height = 800, 
               overwrite = TRUE, out_file = "products/animations/husum_ringo_long_term_animation.mp4")
animate_frames(ST_tags_move_frames, width = 800, height = 800, 
               overwrite = TRUE, out_file = "products/animations/husum_ringo_short_term_animation.mp4")

# closes the R Sessions in the other cores (not the one that you are working on)
stopCluster(cl)
registerDoSEQ()

view_spatial(Tag20996_align)
