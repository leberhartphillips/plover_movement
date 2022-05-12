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
        slice(n_slice * -1) %>% 
        
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

PinPoint_51076_Male_CA3340_C2_2022 <- 
  import_plover_tag_spatial(data_loc = "data/Ceuta/MaleCA3340_C2_2022/Swift GPS Data Files/PinPoint 51076 2022-04-22 21-34-26_NestC2_MaleCA3440.txt",
                            tag_ID = "PinPoint_51076", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                            bird_ID = "CA3340", bird_code = "GX.RM|WX.GX", bird_sex = "M")

PinPoint_51064_Female_CA3224_C1_2022 <- 
  import_plover_tag_spatial(data_loc = "data/Ceuta/FemaleCA3224_C1_2022/Swift GPS Data Files/PinPoint 51064 2022-04-22 21-18-02_NestC1_FemaleCA3224.txt",
                            tag_ID = "PinPoint_51064", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "GMT", tag_model = "PinPoint-10", n_slice = c(1,2),
                            bird_ID = "CA3224", bird_code = "MX.RB|LX.OX", bird_sex = "F")

PinPoint_51065_Female_CV0195_WD1_2022 <- 
  import_plover_tag_spatial(data_loc = "data/Ceuta/FemaleCV0195_WD1_2022/Swift GPS Data Files/PinPoint 51065 2022-05-04 02-02-18_NestWD1FemaleCV0195.txt",
                            tag_ID = "PinPoint_51065", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                            bird_ID = "CV0195", bird_code = "UNK", bird_sex = "F")

PinPoint_51069_Male_CN0066_C301_2022 <- 
  import_plover_tag_spatial(data_loc = "data/Ceuta/MaleCN0066_C301_2022/Swift GPS Data Files/PinPoint 51069 2022-05-04 01-45-14_NestC301_MaleCN0066.txt",
                            tag_ID = "PinPoint_51069", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                            bird_ID = "CN0066", bird_code = "GX.RM|YX.OX", bird_sex = "M")

PinPoint_51073_Female_CN0937_C301_2022 <- 
  import_plover_tag_spatial(data_loc = "data/Ceuta/FemaleCN0937_C301_2022/Swift GPS Data Files/PinPoint 51073 2022-05-06 22-39-50_NestC301_FemaleCN0937.txt",
                            tag_ID = "PinPoint_51073", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                            bird_ID = "CN0937", bird_code = "GX.RM|YX.OX", bird_sex = "F")

PinPoint_51065_Male_CV0266_WD2_2022 <- 
  import_plover_tag_spatial(data_loc = "data/Ceuta/MaleCV0266_WD2_2022/Swift GPS Data Files/PinPoint 51065 2022-05-09 06-00-18_NestWD2_MaleCV0266.txt",
                            tag_ID = "PinPoint_51073", projection = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                            time_zone = "GMT", tag_model = "PinPoint-10", n_slice = 1,
                            bird_ID = "CV0266", bird_code = "GX.RM|YX.OX", bird_sex = "M")

nrow(as.data.frame(PinPoint_51065_Male_CV0266_WD2_2022))
head(as.data.frame(PinPoint_51065_Male_CV0266_WD2_2022))
tail(as.data.frame(PinPoint_51065_Male_CV0266_WD2_2022))

max(as.data.frame(PinPoint_51065_Male_CV0266_WD2_2022)$timestamp) - 
  min(as.data.frame(PinPoint_51065_Male_CV0266_WD2_2022)$timestamp)

UTM13n <- CRS("+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# specify global map options
mapviewOptions(basemaps = c("Esri.WorldImagery","OpenStreetMap.BlackAndWhite","Thunderforest.OpenCycleMap","Esri.WorldShadedRelief"),
               raster.palette = colorRampPalette(brewer.pal(9, "Greys")),
               vector.palette = colorRampPalette(brewer.pal(9, "BrBG")[9:1]),
               na.color = "magenta",
               layers.control.pos = "topright")

# map tag data
mapview(All_tags, zcol = "sex", 
        col.regions = brewer.pal(8, "Dark2")[c(2, 1)])#colorRampPalette(brewer.pal(8, "Dark2")))

# make move object for animation
Male_CA3340_move <- df2move(df = arrange(as.data.frame(PinPoint_51076_Male_CA3340_C2_2022), timestamp_simple), track_id = "ring", 
                            proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                            x = "longitude", y = "latitude", time = "timestamp")

Male_CN0066_move <- df2move(df = arrange(as.data.frame(PinPoint_51069_Male_CN0066_C301_2022), timestamp_simple), track_id = "ring", 
                              proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                              x = "longitude", y = "latitude", time = "timestamp")

Female_CA3224_move <- df2move(df = arrange(as.data.frame(PinPoint_51064_Female_CA3224_C1_2022), timestamp_simple), track_id = "ring", 
                              proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                              x = "longitude", y = "latitude", time = "timestamp")

Female_CV0195_move <- df2move(df = arrange(as.data.frame(PinPoint_51065_Female_CV0195_WD1_2022), timestamp_simple), track_id = "ring", 
                              proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                              x = "longitude", y = "latitude", time = "timestamp")

Female_CN0937_move <- df2move(df = arrange(as.data.frame(PinPoint_51073_Female_CN0937_C301_2022)[1:73,], timestamp_simple), track_id = "ring", 
                              proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                              x = "longitude", y = "latitude", time = "timestamp")

Male_CV0266_move <- df2move(df = arrange(as.data.frame(PinPoint_51065_Male_CV0266_WD2_2022)[1:73,], timestamp_simple), track_id = "ring", 
                              proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                              x = "longitude", y = "latitude", time = "timestamp")

# check the amount of time between fixes. The tracks in move_data have 
# irregular timestamps and sampling rates. print unique timestamps and timeLag 
# (might vary due to different tagging schedules and models)
unique(timestamps(Male_CV0266_move))
timeLag(Male_CV0266_move, unit = "hours")

# use align_move to correct move_data to a uniform time scale and lag using interpolation.
# resolution of 12 hours per timestamp:
Male_CA3340_move_align <- align_move(Male_CA3340_move, res = 5, unit = "mins")
Male_CN0066_move_align <- align_move(Male_CN0066_move, res = 5, unit = "mins")
Female_CA3224_move_align <- align_move(Female_CA3224_move, res = 5, unit = "mins")
Female_CV0195_move_align <- align_move(Female_CV0195_move, res = 5, unit = "mins")
Female_CN0937_move_align <- align_move(Female_CN0937_move, res = 5, unit = "mins")
Male_CV0266_move_align <- align_move(Male_CV0266_move, res = 5, unit = "mins")

unique(unlist(timeLag(Male_CA3340_move_align, units = "mins")))
unique(unlist(timeLag(Male_CN0066_move_align, units = "mins")))
unique(unlist(timeLag(Female_CA3224_move_align, units = "mins")))
unique(unlist(timeLag(Female_CV0195_move_align, units = "mins")))
unique(unlist(timeLag(Female_CN0937_move_align, units = "mins")))
unique(unlist(timeLag(Male_CV0266_move_align, units = "mins")))

# ext <- extent(-9.03, -8.89, 38.725, 38.77)

# ext <- extent(-106.983, -106.941, 23.895, 23.926)
ext <- extent(-106.990, -106.945, 23.895, 23.940)
# ext <- extent(-106.990, -106.880, 23.83, 23.940)


ext[1] - ((ext[1] - ext[2]) * 0.2)
ext[3] - ((ext[3] - ext[4]) * 0.9)

Male_CA3340_move_frames <- 
  frames_spatial(Male_CA3340_move_align, 
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
  add_labels(title = "Snowy Plover, Bahía de Ceuta, Mexico", 
             subtitle = "Male CA3340, nest C2",
             x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow(colour = "white",label_size = 4, height = 0.1, size = 0.8,
                 x = ext[1] - ((ext[1] - ext[2]) * 0.05), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.25)) %>%
  add_scalebar(colour = "white", distance = 2, 
               x = ext[1] - ((ext[1] - ext[2]) * 0.1), 
               y = ext[3] - ((ext[3] - ext[4]) * 0.1), 
               height = 0.03) %>%
  add_timestamps(type = "label", 
                 x = ext[1] - ((ext[1] - ext[2]) * 0.75), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.9), 
                 size = 4) %>%
  add_progress()

Female_CA3224_move_frames <- 
  frames_spatial(Female_CA3224_move_align, 
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
  add_labels(title = "Snowy Plover, Bahía de Ceuta, Mexico", 
             subtitle = "Female CA3224, nest C1",
             x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow(colour = "white",label_size = 4, height = 0.1, size = 0.8,
                 x = ext[1] - ((ext[1] - ext[2]) * 0.05), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.25)) %>%
  add_scalebar(colour = "white", distance = 2, 
               x = ext[1] - ((ext[1] - ext[2]) * 0.1), 
               y = ext[3] - ((ext[3] - ext[4]) * 0.1), 
               height = 0.03) %>%
  add_timestamps(type = "label", 
                 x = ext[1] - ((ext[1] - ext[2]) * 0.75), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.9), 
                 size = 4) %>%
  add_progress()

Male_CN0066_move_frames <- 
  frames_spatial(Male_CN0066_move_align, 
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
  add_labels(title = "Snowy Plover, Bahía de Ceuta, Mexico", 
             subtitle = "Male CN0066, nest C301",
             x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow(colour = "white",label_size = 4, height = 0.1, size = 0.8,
                 x = ext[1] - ((ext[1] - ext[2]) * 0.05), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.25)) %>%
  add_scalebar(colour = "white", distance = 2, 
               x = ext[1] - ((ext[1] - ext[2]) * 0.1), 
               y = ext[3] - ((ext[3] - ext[4]) * 0.1), 
               height = 0.03) %>%
  add_timestamps(type = "label", 
                 x = ext[1] - ((ext[1] - ext[2]) * 0.75), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.9), 
                 size = 4) %>%
  add_progress()

Female_CV0195_move_frames <- 
  frames_spatial(Female_CV0195_move_align, 
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
  add_labels(title = "Wilson's Plover, Bahía de Ceuta, Mexico", 
             subtitle = "Female CV0195, nest D1",
             x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow(colour = "white",label_size = 4, height = 0.1, size = 0.8,
                 x = ext[1] - ((ext[1] - ext[2]) * 0.05), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.25)) %>%
  add_scalebar(colour = "white", distance = 2, 
               x = ext[1] - ((ext[1] - ext[2]) * 0.1), 
               y = ext[3] - ((ext[3] - ext[4]) * 0.1), 
               height = 0.03) %>%
  add_timestamps(type = "label", 
                 x = ext[1] - ((ext[1] - ext[2]) * 0.75), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.9), 
                 size = 4) %>%
  add_progress()

Female_CN0937_move_frames <- 
  frames_spatial(Female_CN0937_move_align, 
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
  add_labels(title = "Snowy Plover, Bahía de Ceuta, Mexico", 
             subtitle = "Female CN0937, nest C301",
             x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow(colour = "white",label_size = 4, height = 0.1, size = 0.8,
                 x = ext[1] - ((ext[1] - ext[2]) * 0.05), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.25)) %>%
  add_scalebar(colour = "white", distance = 2, 
               x = ext[1] - ((ext[1] - ext[2]) * 0.1), 
               y = ext[3] - ((ext[3] - ext[4]) * 0.1), 
               height = 0.03) %>%
  add_timestamps(type = "label", 
                 x = ext[1] - ((ext[1] - ext[2]) * 0.75), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.9), 
                 size = 4) %>%
  add_progress()

Male_CV0266_move_frames <- 
  frames_spatial(Male_CV0266_move_align, 
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
  add_labels(title = "Wilson's Plover, Bahía de Ceuta, Mexico", 
             subtitle = "Male CV0266, nest D2",
             x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow(colour = "white",label_size = 4, height = 0.1, size = 0.8,
                 x = ext[1] - ((ext[1] - ext[2]) * 0.05), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.25)) %>%
  add_scalebar(colour = "white", distance = 2, 
               x = ext[1] - ((ext[1] - ext[2]) * 0.1), 
               y = ext[3] - ((ext[3] - ext[4]) * 0.1), 
               height = 0.03) %>%
  add_timestamps(type = "label", 
                 x = ext[1] - ((ext[1] - ext[2]) * 0.75), 
                 y = ext[3] - ((ext[3] - ext[4]) * 0.9), 
                 size = 4) %>%
  add_progress()

Male_CN0066_move_frames[[289]]
Female_CV0195_move_frames[[288]]
Female_CA3224_move_frames[[289]]
Male_CA3340_move_frames[[288]]
Female_CN0937_move_frames[[288]]
Male_CV0266_move_frames[[288]]

# recovery_location <- 
#   as.data.frame(NFTag55584)[c(23),]
# 
# sinaloa_cities <- 
#   data.frame(y = c(24.8091, 23.2494, 23.9019),
#              x = c(-107.3940, -106.4111, -106.9286)) %>% 
#   mutate(laby = y - 0.08,
#          labx = x - 0.1)
# 
# All_tags_frames2 <- 
#   add_gg(All_tags_frames, 
#          gg = expr(
#            geom_point(aes(x = longitude, y = latitude), 
#                       data = recovery_location, 
#                       color = "white", fill = "#7570B3", 
#                       shape = 21, size = 3)
#          ), 
#          data = recovery_location)
# 
# All_tags_frames2[[80]]
# 
# All_tags_frames2 <- add_text(All_tags_frames2, "Found dead on 06-01\nTag recovered from carcass  06-04", 
#                              y = recovery_location[1, "latitude"], 
#                              x = recovery_location[1, "longitude"] - 0.012,
#                              colour = "black", size = 2, type = "label")
# 
# All_tags_frames2 <- add_text(All_tags_frames2, "Notes: no data collected between 05-22 1300 and 05-29 0500 presumably\ndue to carcas laying on tag. Few data collected at recovery location\nbetween 05-29 1300 and 06-04 0100 (carcass moved to allow GPS signal?)", 
#                              y = recovery_location[1, "latitude"], 
#                              x = -8.92,
#                              colour = "black", size = 2, type = "label")
# 
# # All_tags_frames2 <- add_text(All_tags_frames2, "Ceuta",
# #                              y = sinaloa_cities[3, "laby"], 
# #                              x = sinaloa_cities[3, "labx"],
# #                              colour = "black", size = 3, type = "label")
# All_tags_frames2[[1140]]

# this line opens an R Session in all of your cores (which makes it faster to create the single pictures)
cl = detectCores() %>% makePSOCKcluster; registerDoParallel(cl)

# animate frames
animate_frames(Male_CA3340_move_frames, width = 800, height = 800, 
               overwrite = TRUE, out_file = "products/animations/Male_CA3340_2022_short_animation.mp4")
animate_frames(Male_CN0066_move_frames, width = 800, height = 800, 
               overwrite = TRUE, out_file = "products/animations/Male_CN0066_2022_short_animation.mp4")
animate_frames(Female_CV0195_move_frames, width = 800, height = 800, 
               overwrite = TRUE, out_file = "products/animations/Female_CV0195_2022_short_animation.mp4")
animate_frames(Female_CA3224_move_frames, width = 800, height = 800, 
               overwrite = TRUE, out_file = "products/animations/Female_CA3224_2022_short_animation.mp4")
animate_frames(Female_CN0937_move_frames, width = 800, height = 800, 
               overwrite = TRUE, out_file = "products/animations/Female_CN0937_2022_short_animation.mp4")
animate_frames(Male_CV0266_move_frames, width = 800, height = 800, 
               overwrite = TRUE, out_file = "products/animations/Male_CV0266_2022_short_animation.mp4")
# closes the R Sessions in the other cores (not the one that you are working on)
stopCluster(cl)
registerDoSEQ()

view_spatial(Tag20996_align)
