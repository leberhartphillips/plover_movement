library(RSQLite)
library(tidyverse)

# load data
Ceuta_CLOSED_mdd_v2_0_2 <- 
  RSQLite::dbConnect(SQLite(), 
                     dbname = "../../Mexico/Ceuta_CLOSED/data/Ceuta_CLOSED_version_releases/Ceuta_CLOSED_mdd_v2-0-2.sqlite")

# put all tables into a list
ceuta_list <- list(Nests = dbReadTable(Ceuta_CLOSED_mdd_v2_0_2, "Nests"),
                   Captures = dbReadTable(Ceuta_CLOSED_mdd_v2_0_2, "Captures"),
                   Broods = dbReadTable(Ceuta_CLOSED_mdd_v2_0_2, "Broods"),
                   Resights = dbReadTable(Ceuta_CLOSED_mdd_v2_0_2, "Resights"),
                   BirdRef = dbReadTable(Ceuta_CLOSED_mdd_v2_0_2, "BirdRef"))

ceuta_list$Captures %>% 
  filter(ring == "CA3340") %>% 
  dplyr::select(year, site, nest, ring, code, age, sex, date, time, weight, blood, observer, comments)
