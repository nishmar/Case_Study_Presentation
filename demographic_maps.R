#import data manipulation libraries
library("plyr")

#import geospatial libraries
library("rnaturalearth")
library("tmap")
library("tmaptools")
library("htmlwidgets")
library("raster")

#read biographical questionnaire data
bio_str <- read.csv("Documents/Trust/Biographical_Questionnaire_Logging_Sheet1_2.csv", stringsAsFactors = F)

### PREPARE US DATA ###

#Select participants from US from biographical questionnaire
us_bio <- bio_str[which(bio_str$Country=="USA"),]
# Aggregate state, participant counts
us_states<- count(us_bio, c('State_Province'))
# Relabel column
names(us_states)[names(us_states) == 'State_Province'] <- 'State'
# Reshape column 
us_states<- us_states[-1,]

# Retrieve state_level map of USA from rnaturalearth
# us_map : SpatialPolygonsDataFrame
us_map <- ne_states(country='united states of america')

#Combine aggregated data with map object
biomap_us <- append_data(us_map, us_states, key.shp = "name", key.data = "State")

### STATIC US MAP ###
#For static map, only include contiguous 48 states
exclude_states = c("Alaska", "Hawaii", "Puerto Rico", "Guam", "American Samoa", "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands")
contig_48<- subset(biomap_us, !(NAME %in% exclude_states))

#Create and save tmap object to file
tm<- tm_shape(contig_48) + tm_polygons("freq", id="name")+ tm_text("postal",size=1)
tmap_save(tm, filename="USmap.png", width=14)

### PREPARE CHINA DATA ###
# Select participants from China from biographical questionnaire
# Aggregate count over provinces, reshape, rename 
china_bio <- bio_str[which(bio_str$Country=="China"),]
china_provinces<- count(china_bio, c('State_Province'))
china_provinces<- china_provinces[-1,]
names(china_provinces)[names(china_provinces) == 'State_Province'] <- 'Province'

# Retrieve state-level map of China from rnaturalearth
# china_map : SpatialPolygonsDataFrame
china <- ne_states(country='china')

#Merge count data to map
biomap_china <- append_data(china_map, china_provinces, key.shp = "name", key.data = "Province")

### STATIC CHINA MAP ###
# Produce and save tmap of China
tm_china<- tm_shape(biomap_china) + tm_polygons("freq", id="name")+ tm_text("name")
tmap_save(tm_china, filename="China_map.png", width=14)

### INTERACTIVE VISUALIZATION ###

#merge US and China biomaps
biomap_union <-union(biomap_ne_us, biomap_china)
tm_map_comb <- tm_shape(biomap_union) + tm_polygons("freq", id="name")

#interactive tmap object, save 
interactive_map <- tmap_leaflet(tm_map_comb)
saveWidget(interactive_map_comb, "speaker_map.html")
