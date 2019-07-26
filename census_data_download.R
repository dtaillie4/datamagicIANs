######## Downloading Census Data for CB Region ########
#install.packages('tidycensus')
library(tidycensus)
library(tidyverse)
library(sf)
#library(raster)
library(dplyr)
# codes for variables found here
#https://www.socialexplorer.com/data/ACS2014_5yr/metadata/?ds=ACS14_5yr
#and by adding 001 to each of the codes 
#survey = "acs5" is default argument for this dataset, gives 5 year estimate
#to clear environment: remove(list=ls())

#data <- get_acs(geography = "county", variables = c(
             # "B19113_001","B15003_001","B27001_001"),
             # state = c("MD","DE","VA","WV","NY","PA"),
             # geometry = TRUE, survey = "acs5")

data_income <- get_acs(geography = "county", 
  variables = "B19113_001",
  state = c("MD","DE","VA","WV","NY","PA", "DC"),
  geometry = TRUE, survey = "acs5")

data_education <- get_acs(geography = "county", 
                          variables = "B15003_001",
                          state = c("MD","DE","VA","WV","NY","PA", "DC"),
                          geometry = TRUE, survey = "acs5")

data_hi <- get_acs(geography = "county", 
                   variables = "B27001_001",
                   state = c("MD","DE","VA","WV","NY","PA", "DC"),
                   geometry = TRUE, survey = "acs5")

data_income <- rename(data_income, income = estimate) %>%
  rename(income_moe = moe) %>%
  select(-variable)


data_education <- st_drop_geometry(data_education) %>%
  rename(education_level = estimate) %>%
  rename(eucation_moe = moe) %>%
  select(-variable)

data_hi <- st_drop_geometry(data_hi) %>%
  rename(health_insurance = estimate) %>%
  rename(health_insurance_moe = moe) %>%
  select(-variable)


data_combined <- inner_join(data_education, data_hi) 
data_combined <- inner_join(data_combined, data_income)

####### tract data ##########


data_income_t <- get_acs(geography = "tract", 
                       variables = "B19113_001",
                       state = c("MD","DE","VA","WV","NY","PA", "DC"),
                       geometry = TRUE, survey = "acs5")

data_education_t <- get_acs(geography = "tract", 
                          variables = "B15003_001",
                          state = c("MD","DE","VA","WV","NY","PA", "DC"),
                          geometry = TRUE, survey = "acs5")

#data_ed_attainment_t <- get_acs(geography = "tract",
        #                        variables = "S1501",
         #                       state = c("MD","DE","VA","WV","NY","PA"),
         #                       geometry = FALSE, survey = "acs5")
  
data_hi_t <- get_acs(geography = "tract", 
                   variables = "B27001_001",
                   state = c("MD","DE","VA","WV","NY","PA", "DC"),
                   geometry = TRUE, survey = "acs5")

#downloading more education data 

data_hs_t <- get_acs(geography = "tract",
                     variables = "B15003_017", 
                     state = c("MD","DE","VA","WV","NY","PA", "DC"),
                     geometry = FALSE, survey = "acs5")
  
data_ged_t <- get_acs(geography = "tract",
                     variables = "B15003_018", 
                     state = c("MD","DE","VA","WV","NY","PA", "DC"),
                     geometry = FALSE, survey = "acs5") 

data_as_t <- get_acs(geography = "tract",
                               variables = "B15003_021", 
                               state = c("MD","DE","VA","WV","NY","PA", "DC"),
                               geometry = FALSE, survey = "acs5")

data_bac_t <- get_acs(geography = "tract",
                     variables = "B15003_022", 
                     state = c("MD","DE","VA","WV","NY","PA", "DC"),
                     geometry = FALSE, survey = "acs5")

data_mas_t <- get_acs(geography = "tract",
                      variables = "B15003_023", 
                      state = c("MD","DE","VA","WV","NY","PA", "DC"),
                      geometry = FALSE, survey = "acs5")

data_doc_t <- get_acs(geography = "tract",
                      variables = "B15003_025", 
                      state = c("MD","DE","VA","WV","NY","PA", "DC"),
                      geometry = FALSE, survey = "acs5")

data_pro_t <- get_acs(geography = "tract",
                      variables = "B15003_024", 
                      state = c("MD","DE","VA","WV","NY","PA", "DC"),
                      geometry = FALSE, survey = "acs5")
#professional school
data_pro_t <- rename(data_pro_t, num_pro = estimate) %>%
  rename(pro_moe = moe) %>%
  dplyr::select(-variable)
#doctorate 
data_doc_t <- rename(data_doc_t, num_doc = estimate) %>%
  rename(doc_moe = moe) %>%
  dplyr::select(-variable)
#masters
data_mas_t <- rename(data_mas_t, num_mas = estimate) %>%
  rename(mas_moe = moe) %>%
  dplyr::select(-variable)
#bachelors
data_bac_t <- rename(data_bac_t, num_bac = estimate) %>%
  rename(bac_moe = moe) %>%
  dplyr::select(-variable)
#associates
data_as_t <- rename(data_as_t, num_as = estimate) %>%
  rename(as_moe = moe) %>%
  dplyr::select(-variable)
#hs
data_hs_t <- rename(data_hs_t, num_hs = estimate) %>%
  rename(hs_moe = moe) %>%
  dplyr::select(-variable)
#ged
data_ged_t <- rename(data_ged_t, num_ged = estimate) %>%
  rename(ged_moe = moe) %>%
  dplyr::select(-variable)

data_income_t <- rename(data_income_t, income = estimate) %>%
  rename(income_moe = moe) %>%
  select(-variable)


data_education_t <- st_drop_geometry(data_education_t) %>%
  rename(education_level = estimate) %>%
  rename(eucation_moe = moe) %>%
  select(-variable)

data_hi_t <- st_drop_geometry(data_hi_t) %>%
  rename(health_insurance = estimate) %>%
  rename(health_insurance_moe = moe) %>%
  select(-variable)


data_combined_t <- inner_join(data_education_t, data_hi_t) 
data_combined_t <- inner_join(data_combined_t, data_income_t)
data_combined_t <- inner_join(data_combined_t, data_as_t)%>%
                    inner_join(data_bac_t) %>% 
                    inner_join(data_doc_t)%>% 
                    inner_join(data_ged_t)%>% 
                    inner_join(data_hs_t)%>% 
                    inner_join(data_mas_t)%>% 
                    inner_join(data_pro_t)

#### operator for testing membership = %in%
?st_merge


View(data_combined_t)
## saving data to SESYNC FOLDER FOR V AND KM TO ACCESS ##
st_write(data_combined, 'data/county_data.shp')
st_write(data_combined_t, 'data/tract_data_new.shp')


## look at the data in maps, crop to CB watershed region ##

ggplot(data_combined_t, 
       aes(geometry = geometry,
           fill = income))+
       geom_sf() 


### reading cb watershed shapefile 

#CB <- st_read('data/Chesapeake Bay and major watersheds.shp') %>%
  #summarise(SUM_AREA_M = sum(SUM_AREA_M))

CB <- st_read('data/Chesapeake Bay and major watersheds.shp')
tract <- st_read('data/tract_data_new.shp')  
prj <- '+proj=aea +lat_1=29.5 +lat_2=45.5 \
    +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0    \
    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0   \
    +units=m +no_defs'

CB_outline <- st_transform(
  CB,
  crs = prj)
tract <- st_transform(
  tract, 
  crs = prj
)

CB_tract <- st_intersection(CB_outline, tract)

ggplot(data = CB_tract, aes(geometry=geometry, fill = income))+
  geom_sf(size = .01)+
  scale_fill_gradient2(low = "red", mid = "yellow1", high = "darkgreen", 
                       midpoint = 100000)



geom_sf(data = na.omit(CB_outline),
          alpha = 0.7)
View(CB_tract)
st_write(CB_tract, 'data/census_tract_cb.shp')

#warning message: variables are assumed to be spatially constant throughout all geometries
#tract <- st_read('data/tract_data_new.shp') %>%
 # st_transform(tract, crs = st_crs(CB))

CB_tract <- CB %>%
  st_join(tract, inner = TRUE)

ggplot(data = CB_tract, 
       aes(geometry = geometry))+
  geom_sf()+
  geom_sf(data = na.omit(CB_outline),
          alpha = 0.7)


## CRS are the same, but both WGS84 for area calculations maynot be ideal

ggplot(data = tract, 
       aes(geometry = geometry))+
  geom_sf()+
  geom_sf(data = na.omit(CB_outline),
          alpha = 0.7)

#CB_tract_data <- st_join(tract, CB)

CB_outline <- st_union(CB)
plot(CB_outline)

#extent <- (matrix(st_bbox(CB_outline, nrow = 2)))

CB_tract <- st_intersection(tract, CB_outline)

plot(CB_tract)
#cropping census table to cb region... ??
View(CB)
cb_tract <- crop(data_combined_t$geometry, CB$geometry)

?crop

ggplot(CB)+
  geom_sf()



