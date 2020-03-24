library(tidyverse)
library(readxl)
library(leaflet)

ptm <- read_xlsx("01_db/2020Mar17_algaedb_ptm.xlsx")

#standardize ptm collector number to no spaces
ptm_col <- ptm %>% 
  mutate(s_coll_no = str_remove(`Collector Number`, "-"),
         space_coll_no = str_remove(s_coll_no, " "),
         no_ptm = str_remove(s_coll_no, "PTM"),
         number = as.numeric(no_ptm),
         final = if_else(no_ptm %in% c("207b","207a"),
                          no_ptm,
                          as.character(number)),
         `Collector Number` = paste0("PTM",final)) %>% 
  select(-s_coll_no,
         -space_coll_no,
         -no_ptm,
         -number,
         -final)

#plotting the points
leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  addCircleMarkers(lng = ptm$Geo_LongDecimal, lat = ptm$Geo_LatDecimal,
                   popup = paste(ptm$`Collector Number`, ptm$Country,
                                 ptm$Location),
                   clusterOptions = markerClusterOptions())

#Fix PTM-1192, PTM-1718
#fixed in the database March 23
#PTM-1192 missing negative
#PTM-1718 63 and 36 mixed up
ptm %>% 
  filter(`Collector Number` %in% c("PTM-1192", "PTM-1718")) %>% 
  select(`Accession Number`,
         `Collector Number`,
         Genus, Species, 
         VLatDegree,VLatMinute,VLatSecond,
         VLongDegree,VLongMinute, VLongSecond,
         Geo_LatDecimal,Geo_LongDecimal)
