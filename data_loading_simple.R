######## loading data ##########
library(sf)
tract <- read_sf('data/ScoringOutputs/tract_cb_incomescoring.shp')
tract_cb_eduscore <- read_sf('data/tract_cb_eduscore.shp')  
  