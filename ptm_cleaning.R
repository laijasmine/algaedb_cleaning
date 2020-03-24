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

#Fixes for authorship can be made
ptm %>% 
  count(`taxon name for relation`,`Species Author`) %>% 
  View()

ptm %>% 
  count(Phylum, Class)

#plotting the points
leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  addCircleMarkers(lng = ptm$Geo_LongDecimal, lat = ptm$Geo_LatDecimal,
                   popup = paste(ptm$`Collector Number`, ptm$Country,
                                 ptm$Location),
                   clusterOptions = markerClusterOptions())

#upper case - habitat
#takes a dataframe and makes the first word in the column lowercase
capitalize <- function(ds){
  split <- str_split(ds$Habitat," ",n = 2) 
  
  cap <- map(split,~str_to_sentence(.x[1]))
  
  ds$Habitat <- map2(split,cap,~c(.y,.x[2]) %>% 
                       paste(.,collapse = " "))
  
  ds$Habitat <- gsub("NA","",ds$Habitat)
  
  return(ds)
}

ptm_cap <- capitalize(ptm_col)

#Remove coralline ""?
ptm_cap %>% 
  mutate(Genus2 = if_else(str_detect(Genus, "Coralline"),"Coralline", Genus)) %>% 
  select(Genus, Genus2, Species) %>% 
  View()

#fix depth???
#hand cleaning might be needed
ptm_cap %>% 
  select(Depth) %>% 
  arrange() %>% 
  View()

#microhabitat?
ptm_cap %>% 
  select(Microhabitat) %>% 
  count(Microhabitat) %>% 
  arrange() %>% 
  View()
#Katy vs Katherine R. Hind in determined by
ptm_cap %>% 
  mutate(Det = str_replace(`Determined By`,"Katy R. Hind", "Katherine R. Hind")) %>% 
  select(`Determined By`, Det) %>% 
  View()

#Giant barnable? - fix after asking Patrick?

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
