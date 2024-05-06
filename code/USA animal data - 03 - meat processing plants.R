#########################
### Load packages etc ###
#########################

setwd("~/Documents/USA Scoping") #Set working directory
library(ggplot2) #For graphing
theme_set(theme_bw()) #Because I'm fashionable
library(scales) #To help graphing
library(viridis) #To help graphing
library(gridExtra) #To help graphing
library(usmap) #For importing the US map spatial information
library(zipcodeR) #For converting US zip codes to coordinates
library(terra) #For spatial data
library(tidyterra) #For spatial data
library(ggnewscale) #To help graphing
library(readxl) #For loading XLSX files
#library(geojsonio) #For importing spatial JSON file
#library(sf) #For spatial data

###################
### Import data ###
###################

#From https://www.fsis.usda.gov/inspection/establishments/meat-poultry-and-egg-product-inspection-directory
# MPI Directory: Numerically by Establishment Number | XLSX (Feb 19, 2024)
# Dataset: Establishment Demographic Data (Feb 19, 2024, XLSX)

df_plants_byestablishmentnumber_raw <- read_excel("data/MPI_Directory_by_Establishment_Number.xlsx")
df_plants_byestablishmentnumber <- as.data.frame(df_plants_byestablishmentnumber_raw)

df_plants_demographic_raw <- read_excel("data/Dataset_Establishment_Demographic_Data.xlsx", skip=3)
df_plants_demographic <- as.data.frame(df_plants_demographic_raw)

##############################
### Clean and analyse data ###
##############################

#Zip code data frame
head(df_plants_byestablishmentnumber)
df_plants_zipcodes <- df_plants_byestablishmentnumber[,c(
  "Establishment\nID","Company","State","Zip")]
names(df_plants_zipcodes)[1] <- c("EstID")

#Demographic data frame
head(df_plants_demographic)
df_plants_demographic_2 <- df_plants_demographic[,c(
  "EstID","Company","Size","Beef\nSlaughter","Pork\nSlaughter",
  "Chicken\nSlaughter","Siluri\nformes\nProducts","Egg\nProducts")]
names(df_plants_demographic_2) <- c("EstID","Company","Size",
                                    "Beef_Slaughter","Pork_Slaughter",
                                    "Chicken_Slaughter","Catfish_Products",
                                    "Egg_Products")

#Change all the yucky "Yes" columns to TRUE columns
df_plants_demographic_2$Beef_SlaughterB <- NA
df_plants_demographic_2$Pork_SlaughterB <- NA
df_plants_demographic_2$Chicken_SlaughterB <- NA
df_plants_demographic_2$Catfish_ProductsB <- NA
df_plants_demographic_2$Egg_ProductsB <- NA

df_plants_demographic_2$Beef_SlaughterB[!is.na(df_plants_demographic_2$Beef_Slaughter)] <- TRUE
df_plants_demographic_2$Pork_SlaughterB[!is.na(df_plants_demographic_2$Pork_Slaughter)] <- TRUE
df_plants_demographic_2$Chicken_SlaughterB[!is.na(df_plants_demographic_2$Chicken_Slaughter)] <- TRUE
df_plants_demographic_2$Catfish_ProductsB[!is.na(df_plants_demographic_2$Catfish_Products)] <- TRUE
df_plants_demographic_2$Egg_ProductsB[!is.na(df_plants_demographic_2$Egg_Products)] <- TRUE

df_plants_demographic_2$Size2 <- ifelse(
  df_plants_demographic_2$Size=="Large","Large",
  "Small"
)

df_plants_demographic_3 <- df_plants_demographic_2[,c("EstID","Company","Size2",
                             "Beef_SlaughterB","Pork_SlaughterB",
                             "Chicken_SlaughterB","Catfish_ProductsB",
                             "Egg_ProductsB")]


#Merge zip code and demographic data into a single data frame
df_plants <- merge(df_plants_zipcodes, df_plants_demographic_3, by="EstID")

#For each zipcode, get the corresponding coordinates
df_plants$lat <- NA
df_plants$lng <- NA
for (i in c(1:nrow(df_plants))){
  tryCatch({
  zip_tmp <- df_plants[i,]$Zip
  coords <- geocode_zip(zip_tmp)
  df_plants[i,]$lat <- coords$lat
  df_plants[i,]$lng <- coords$lng},error=function(e){})
}

#Convert to a spatial vector
spatvector_plants <- vect(df_plants, geom=c("lng", "lat"), crs="epsg:4326", keepgeom=FALSE)

#Get map of US states
states_for_graphing <- us_map(regions=c("states"))

#Create CRS for the plants spatial vector
#crs(spatvector_plants) <- crs(states_for_graphing)

#Remove Hawaii Alaska and Puerto Rico for now
spatvector_plants_mainland <- spatvector_plants[-which(spatvector_plants$State %in% c("AK", "HI", "PR")),]

#Separate into separate vectors for each product type
spatvector_plants_mainland_beef <- spatvector_plants_mainland[which(spatvector_plants_mainland$Beef_SlaughterB),]
spatvector_plants_mainland_pork <- spatvector_plants_mainland[which(spatvector_plants_mainland$Pork_SlaughterB),]
spatvector_plants_mainland_chicken <- spatvector_plants_mainland[which(spatvector_plants_mainland$Chicken_SlaughterB),]
spatvector_plants_mainland_catfish <- spatvector_plants_mainland[which(spatvector_plants_mainland$Catfish_ProductsB),]
spatvector_plants_mainland_egg <- spatvector_plants_mainland[which(spatvector_plants_mainland$Egg_ProductsB),]

g_plants_beef <- ggplot() +
  geom_sf(data=states_for_graphing, fill="white") +
  geom_sf(aes(size=Size2, shape=Size2, fill=Size2), data=spatvector_plants_mainland_beef) +
  geom_sf_text(aes(label=abbr), colour="#40404042", data=states_for_graphing) + 
  theme_void() +
  ggtitle("Processing plants - Beef") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill="white")) +
  scale_size_manual(values = c("Large"=3,"Small"=2)) +
  scale_shape_manual(values = c("Large"=21,"Small"=16)) +
  scale_fill_manual(values = c("Large"="goldenrod3","Small"="black"))
g_plants_beef

  g_plants_pork <- ggplot() +
  geom_sf(data=states_for_graphing, fill="white") +
  geom_sf(aes(size=Size2, shape=Size2, fill=Size2), data=spatvector_plants_mainland_pork) +
  geom_sf_text(aes(label=abbr), colour="#40404042", data=states_for_graphing) + 
  theme_void() +
  ggtitle("Processing plants - Pork") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill="white")) +
  scale_size_manual(values = c("Large"=3,"Small"=2)) +
  scale_shape_manual(values = c("Large"=21,"Small"=16)) +
  scale_fill_manual(values = c("Large"="goldenrod3","Small"="black"))
g_plants_pork

g_plants_chicken <- ggplot() +
  geom_sf(data=states_for_graphing, fill="white") +
  geom_sf(aes(size=Size2, shape=Size2, fill=Size2), data=spatvector_plants_mainland_chicken) +
  geom_sf_text(aes(label=abbr), colour="#40404042", data=states_for_graphing) + 
  theme_void() +
  ggtitle("Processing plants - Chicken") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill="white")) +
  scale_size_manual(values = c("Large"=3,"Small"=2)) +
  scale_shape_manual(values = c("Large"=21,"Small"=16)) +
  scale_fill_manual(values = c("Large"="goldenrod3","Small"="black"))
g_plants_chicken

g_plants_catfish <- ggplot() +
  geom_sf(data=states_for_graphing, fill="white") +
  geom_sf(aes(size=Size2, shape=Size2, fill=Size2), data=spatvector_plants_mainland_catfish) +
  geom_sf_text(aes(label=abbr), colour="#40404042", data=states_for_graphing) + 
  theme_void() +
  ggtitle("Processing plants - Catfish") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill="white")) +
  scale_size_manual(values = c("Large"=3,"Small"=2)) +
  scale_shape_manual(values = c("Large"=21,"Small"=16)) +
  scale_fill_manual(values = c("Large"="goldenrod3","Small"="black"))
g_plants_catfish

g_plants_egg <- ggplot() +
  geom_sf(data=states_for_graphing, fill="white") +
  geom_sf(aes(size=Size2, shape=Size2, fill=Size2), data=spatvector_plants_mainland_egg) +
  geom_sf_text(aes(label=abbr), colour="#40404042", data=states_for_graphing) + 
  theme_void() +
  ggtitle("Processing plants - Egg") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill="white")) +
  scale_size_manual(values = c("Large"=3,"Small"=2)) +
  scale_shape_manual(values = c("Large"=21,"Small"=16)) +
  scale_fill_manual(values = c("Large"="goldenrod3","Small"="black"))
g_plants_egg


ggsave("results/g_plants_beef.png", g_plants_beef, width=16) 
ggsave("results/g_plants_pork.png", g_plants_pork, width=16) 
ggsave("results/g_plants_chicken.png", g_plants_chicken, width=16) 
ggsave("results/g_plants_catfish.png", g_plants_catfish, width=16) 
ggsave("results/g_plants_egg.png", g_plants_egg, width=16) 
