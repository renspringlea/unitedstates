#Credit to https://r-graph-gallery.com/328-hexbin-map-of-the-usa.html
#Hexbin map file from https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

#########################
### Load packages etc ###
#########################

setwd("~/Nextcloud/AA/USA") #Set working directory
library(ggplot2) #For graphing
theme_set(theme_bw()) #Because I'm fashionable
library(scales) #To help graphing
library(viridis) #To help graphing
library(gridExtra) #To help graphing
library(geojsonio) #For importing spatial JSON file
library(sf) #For spatial data
library(terra) #For spatial data
#####################################
### Import data on farmed animals ###
#####################################

#Load animal data
df_animals <- read.csv("Animals used_consumed in the USA - Key results by state.csv")

#Tidy up data
names(df_animals) <- c("state", "catfish", "trout", "baitfish", "ornamentalfish", "crawfish",
                       "shrimp", "cattle", "pigs", "layers", "broilers") 
df_animals[is.na(df_animals)] <- 0

#Convert animal data to long format
df_animals_usawhole <- df_animals[1,]
df_animals_statesonly <- df_animals[c(2:nrow(df_animals)),]

df_animals_statesonly_long <- reshape(df_animals_statesonly, direction="long", idvar="state", varying=c(2:ncol(df_animals_statesonly)),
                           v.names = "Value",
                           timevar = "Species",
                           times=names(df_animals_statesonly)[2:ncol(df_animals_statesonly)])
df_animals_usawhole_long <- reshape(df_animals_usawhole, direction="long", idvar="state", varying=c(2:ncol(df_animals_usawhole)),
                                      v.names = "Value",
                                      timevar = "Species",
                                      times=names(df_animals_usawhole)[2:ncol(df_animals_usawhole)])
row.names(df_animals_statesonly_long) <- c(1:nrow(df_animals_statesonly_long))
row.names(df_animals_usawhole_long) <- c(1:nrow(df_animals_usawhole_long))

###########################
### Import spatial data ###
###########################

#Load and format spatial data
#Note: first download from here
# https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
spdf_raw <- geojson_read("us_states_hexgrid.geojson",  what = "sp") #Load hexbin map file
spdf_raw@data$state <- gsub(" \\(United States\\)", "", spdf_raw@data$google_name) #Rename states
spdf <- st_as_sf(spdf_raw) #Migrate to sf
spdf <- spdf[spdf$state!="District of Columbia",] #Remove District of Columbia
spdf <- merge(spdf, df_animals_statesonly, by="state") #Merge spatial data with animal data


######################################
### Load wild-catch fisheries data ###
######################################

### Catch data

#Import NOAA data on fisheries catch by species, state and fleet
df_fisheries <- read.csv("FOSS_landings.csv", skip=1)
df_fisheries$Metric.Tons.Numeric <- as.numeric(gsub(",","",df_fisheries$Metric.Tons))

#The recreational catch doesn't have scientific names
#Fortunately, we're very smart, and we can match the scientific names
#from the commercial catch
df_fisheries_commercial <- df_fisheries[which(df_fisheries$Collection=="Commercial"),]
df_fisheries_recreational <- df_fisheries[which(df_fisheries$Collection=="Recreational"),]

for (i in c(1:nrow(df_fisheries_recreational))){
  tmp_NMFS.Name <- df_fisheries_recreational[i,]$NMFS.Name
  tmp_row <- which(df_fisheries_commercial$NMFS.Name==tmp_NMFS.Name)[1]
  tmp_scientific_name <- df_fisheries_commercial[tmp_row,]$Scientific.Name
  df_fisheries_recreational[i,]$Scientific.Name <- tmp_scientific_name
}

#Percentage of successful matches
100*(1-length(which(is.na(df_fisheries_recreational$Scientific.Name)))/nrow(df_fisheries_recreational))

#Bind recreational with commercial again
df_fisheries_matched <- rbind(df_fisheries_commercial, df_fisheries_recreational)
rownames(df_fisheries_matched) <- c(1:nrow(df_fisheries_matched))

### Estimated mean weight data

#Import Fishcount data on estimated mean weights for finfish
emw <- read.csv("fishcount.csv")
names(emw) <- c("country", "fao_species_category", "scientific_name",
                "fish_species", "class", "multispecies", "year",
                "emw_id", "emw_lower", "emw_upper", "gemw_lower",
                "gemw_upper", "mw_used_lower", "mw_used_upper",
                "n_lower_millions", "n_upper_millions")
emw$mw_used_lower.n <- as.numeric(emw$mw_used_lower)

#Aggregate fishcount data to make a data frame of used mean weights
emw_agg_lower <- aggregate(mw_used_lower.n~fao_species_category+scientific_name, FUN=mean, data=emw) #Note the difference
emw_agg_upper <- aggregate(mw_used_upper~fao_species_category+scientific_name, FUN=mean, data=emw)
emw_agg <- base::merge(emw_agg_lower, emw_agg_upper, by="fao_species_category", all=TRUE)
head(emw_agg)

#Import RP data with estimated mean weights for shrimp
emw_shrimp <- read.csv("rp_shrimp.csv")
emw_shrimp <- emw_shrimp[c(1:92),] #Delete irrelevant rows
emw_shrimp <- emw_shrimp[,c("Environment","Species","Generic.estimated.mean.weight..GEMW..lower..g.",
                            "Generic.estimated.mean.weight..GEMW..upper..g.","Clade")] #Delete irrelevant columns
names(emw_shrimp) <- c("Environment", "Species", "gemw_lower", "gemw_upper", "Clade") #Rename columns

#############################################
### Match wild-catch data to mean weights ###
#############################################

#For each row in the NOAA fisheries data, get the used mean weights from
#the fishcount data
df_fisheries_matched$mw_used_lower <- NA
df_fisheries_matched$mw_used_upper <- NA
df_fisheries_matched$finfish_or_shrimp <- NA
df_fisheries_matched$weightmatch <- FALSE

for (i in c(1:nrow(df_fisheries_matched))){
  #For shrimp rows
  if(substr(df_fisheries_matched$NMFS.Name[i], 1, 6)=="SHRIMP"){
    tmp_scientific_name <- df_fisheries_matched[i,]$Scientific.Name
    if(tmp_scientific_name=="" | is.na(tmp_scientific_name)){next}
    tmp_row <- which(emw_shrimp$Species==tmp_scientific_name)
    if(length(tmp_row)==0){next}
    tmp_mw_used_lower <- emw_shrimp[tmp_row,]$gemw_lower #Note the difference here
    tmp_mw_used_upper <- emw_shrimp[tmp_row,]$gemw_upper
    df_fisheries_matched[i,]$mw_used_lower <- tmp_mw_used_lower
    df_fisheries_matched[i,]$mw_used_upper <- tmp_mw_used_upper
    df_fisheries_matched[i,]$finfish_or_shrimp <- "shrimp"
    df_fisheries_matched[i,]$weightmatch <- TRUE
  }
  #For finfish rows
  if(substr(df_fisheries_matched$NMFS.Name[i], 1, 6)!="SHRIMP"){
    tmp_scientific_name <- df_fisheries_matched[i,]$Scientific.Name
    if(tmp_scientific_name=="" | is.na(tmp_scientific_name)){next}
    tmp_row <- which(emw_agg$scientific_name.x==tmp_scientific_name)[1]
    if(length(tmp_row)==0){next}
    tmp_mw_used_lower <- emw_agg[tmp_row,]$mw_used_lower.n #Note the difference here
    tmp_mw_used_upper <- emw_agg[tmp_row,]$mw_used_upper
    df_fisheries_matched[i,]$mw_used_lower <- tmp_mw_used_lower
    df_fisheries_matched[i,]$mw_used_upper <- tmp_mw_used_upper
    df_fisheries_matched[i,]$finfish_or_shrimp <- "finfish"
    df_fisheries_matched[i,]$weightmatch <- TRUE
  }
}

#Calculate percentage of tonnage matched with a mean weight successfully
fishcount_match_success <- aggregate(Metric.Tons.Numeric~weightmatch, FUN=sum, data=df_fisheries_matched)
100*fishcount_match_success$Metric.Tons.Numeric[2]/sum(fishcount_match_success$Metric.Tons.Numeric)
#Nice!
#Though this doesn't mean that we've actually got the EMWs for all those species
#Just that the species-name match didn't fail
#As fishcount only has finfish, has most but not all of the catch accounted for, etc

#Now we can calculate numbers of individuals for finfish
df_fisheries_matched$individuals_lower <- df_fisheries_matched$Metric.Tons.Numeric*10^6/df_fisheries_matched$mw_used_upper
df_fisheries_matched$individuals_upper <- df_fisheries_matched$Metric.Tons.Numeric*10^6/df_fisheries_matched$mw_used_lower
df_fisheries_matched$individuals_midpoint <- rowMeans(df_fisheries_matched[,c("individuals_lower","individuals_upper")])
head(df_fisheries_matched)

#Format nicely
df_fisheries_matched_floridafix <- df_fisheries_matched
df_fisheries_matched_floridafix[which(df_fisheries_matched_floridafix$State=="FLORIDA-EAST"),]$State <- "FLORIDA"
df_fisheries_matched_floridafix[which(df_fisheries_matched_floridafix$State=="FLORIDA-WEST"),]$State <- "FLORIDA"
names(df_fisheries_matched_floridafix)[2] <- "state"

#Remove "processed at sea" records (about 1.3% of individuals)
df_fisheries_matched_floridafix <- df_fisheries_matched_floridafix[-which(df_fisheries_matched_floridafix$state=="PROCESS AT SEA"),]

#State names to title case
df_fisheries_matched_floridafix$state <- tools::toTitleCase(tolower(df_fisheries_matched_floridafix$state))

#Separate by fleet and finfish/shrimp
df_fisheries_finfish_commercial <- df_fisheries_matched_floridafix[which(df_fisheries_matched_floridafix$Collection=="Commercial" &
                                                                           df_fisheries_matched_floridafix$finfish_or_shrimp=="finfish"),]
df_fisheries_finfish_recreational <- df_fisheries_matched_floridafix[which(df_fisheries_matched_floridafix$Collection=="Recreational" &
                                                                             df_fisheries_matched_floridafix$finfish_or_shrimp=="finfish"),]
df_fisheries_shrimp_commercial <- df_fisheries_matched_floridafix[which(df_fisheries_matched_floridafix$Collection=="Commercial" &
                                                                          df_fisheries_matched_floridafix$finfish_or_shrimp=="shrimp"),]
df_fisheries_shrimp_recreational <- df_fisheries_matched_floridafix[which(df_fisheries_matched_floridafix$Collection=="Recreational" &
                                                                            df_fisheries_matched_floridafix$finfish_or_shrimp=="shrimp"),]
#Note that df_fisheries_shrimp_recreational actually has 0 rows

#Aggregate by state
df_fisheries_finfish_commercial_agg <- aggregate(individuals_midpoint~state, FUN=sum, data=df_fisheries_finfish_commercial)
df_fisheries_finfish_recreational_agg <- aggregate(individuals_midpoint~state, FUN=sum, data=df_fisheries_finfish_recreational)
df_fisheries_shrimp_commercial_agg <- aggregate(individuals_midpoint~state, FUN=sum, data=df_fisheries_shrimp_commercial)

names(df_fisheries_finfish_commercial_agg)[2] <- "wildcaught_finfish_commerical_midpoint"
names(df_fisheries_finfish_recreational_agg)[2] <- "wildcaught_finfish_recreational_midpoint"
names(df_fisheries_shrimp_commercial_agg)[2] <- "wildcaught_shrimp_midpoint"

#Add to existing spatial data frame thingo
spdf <- merge(spdf, df_fisheries_finfish_commercial_agg, by="state", all=TRUE)
spdf <- merge(spdf, df_fisheries_finfish_recreational_agg, by="state", all=TRUE)
spdf <- merge(spdf, df_fisheries_shrimp_commercial_agg, by="state", all=TRUE)

#Add to existing non-spatial data frame (for bargraphing)
df_animals_usawhole_long_wildcatch <- data.frame("state"=rep("United States",3),
           "Species"=c("wildcaught_finfish_commerical_midpoint",
                       "wildcaught_finfish_recreational_midpoint",
                       "wildcaught_shrimp_midpoint"),
           "Value"=c(sum(df_fisheries_finfish_commercial_agg$wildcaught_finfish_commerical_midpoint, na.rm=TRUE),
             sum(df_fisheries_finfish_recreational_agg$wildcaught_finfish_recreational_midpoint, na.rm=TRUE),
             sum(df_fisheries_shrimp_commercial_agg$wildcaught_shrimp_midpoint, na.rm=TRUE)
))
df_animals_usawhole_long <- rbind(df_animals_usawhole_long, df_animals_usawhole_long_wildcatch)

df_animals_statesonly_long_wildcatch <- data.frame("state"=c(
  df_fisheries_finfish_commercial_agg$state,
  df_fisheries_finfish_recreational_agg$state,
  df_fisheries_shrimp_commercial_agg$state
  ),
  "Species"=c(
    rep("wildcaught_finfish_commerical_midpoint",nrow(df_fisheries_finfish_commercial_agg)),
    rep("wildcaught_finfish_recreational_midpoint",nrow(df_fisheries_finfish_recreational_agg)),
    rep("wildcaught_shrimp_midpoint",nrow(df_fisheries_shrimp_commercial_agg))),
  "Value"=c(
    df_fisheries_finfish_commercial_agg$wildcaught_finfish_commerical_midpoint,
    df_fisheries_finfish_recreational_agg$wildcaught_finfish_recreational_midpoint,
    df_fisheries_shrimp_commercial_agg$wildcaught_shrimp_midpoint)
    )
df_animals_statesonly_long <- rbind(df_animals_statesonly_long, df_animals_statesonly_long_wildcatch)

df_animals_statesonly_long_sum <- aggregate(Value~state, FUN=sum, data=df_animals_statesonly_long)
df_animals_statesonly_long_sum$Species <- "Sum"
df_animals_statesonly_long_sum <- df_animals_statesonly_long_sum[c(1,3,2)]
df_animals_statesonly_long <- rbind(df_animals_statesonly_long, df_animals_statesonly_long_sum)

#Set NA to 0
spdf[which(is.na(spdf$wildcaught_finfish_commerical_midpoint)),]$wildcaught_finfish_commerical_midpoint <- 0
spdf[which(is.na(spdf$wildcaught_finfish_recreational_midpoint)),]$wildcaught_finfish_recreational_midpoint <- 0
spdf[which(is.na(spdf$wildcaught_shrimp_midpoint)),]$wildcaught_shrimp_midpoint <- 0

df_animals_statesonly_long_sum

############################
### Graph farmed animals ###
############################

#Bar graph, by species
g1 <- ggplot(aes(x=reorder(Species, Value), y=Value), data=df_animals_usawhole_long) +
  geom_col() +
  xlab(NULL) +
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9),
                     name="Number killed in USA annually *") +
  coord_flip()
g1
ggsave("g1.png",g1,width=8,height=8)

#Bar graph, by species and state (different axis scale for each species)
states_agg <- aggregate(Value~state, FUN=sum, data=df_animals_statesonly_long)
states_agg[order(-states_agg$Value),]

species_order <- c("Sum",
                   df_animals_usawhole_long[order(-df_animals_usawhole_long$Value),]$Species)

g2 <- ggplot(aes(x=reorder(state, Value), y=Value), data=df_animals_statesonly_long) +
  geom_col() +
  facet_grid(~factor(Species, levels=species_order), scales="free") +
  xlab(NULL) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     name="Number killed in USA annually *") +
  coord_flip()
g2
ggsave("g2.png",g2,width=34,height=8)

#Map, by state
df_animals_statesonly_long_sum <- df_animals_statesonly_long_sum[,c(1,3)]
names(df_animals_statesonly_long_sum)[2] <- c("Sum")
spdf <- merge(spdf, df_animals_statesonly_long_sum, by="state")
g3 <- ggplot(aes(fill=Sum), data=spdf) +
  geom_sf(colour="white") +
  theme_void() +
  geom_sf_text(aes(label=iso3166_2), colour="white") +
  scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))
g3
ggsave("g3.png",g3,width=8,height=8)

#Map, by state and species (different colour scale range for each species)
#Not the most elegant thing I've ever done
#Sorry
species_order
g4_broilers <- ggplot(aes(fill=broilers), data=spdf) + geom_sf(colour="white") + theme_void() + geom_sf_text(aes(label=iso3166_2), colour="white") + scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))
g4_baitfish <- ggplot(aes(fill=baitfish), data=spdf) + geom_sf(colour="white") + theme_void() + geom_sf_text(aes(label=iso3166_2), colour="white") + scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))
g4_crawfish <- ggplot(aes(fill=crawfish), data=spdf) + geom_sf(colour="white") + theme_void() + geom_sf_text(aes(label=iso3166_2), colour="white") + scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))
g4_layers <- ggplot(aes(fill=layers), data=spdf) + geom_sf(colour="white") + theme_void() + geom_sf_text(aes(label=iso3166_2), colour="white") + scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))
g4_pigs <- ggplot(aes(fill=pigs), data=spdf) + geom_sf(colour="white") + theme_void() + geom_sf_text(aes(label=iso3166_2), colour="white") + scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))
g4_catfish <- ggplot(aes(fill=catfish), data=spdf) + geom_sf(colour="white") + theme_void() + geom_sf_text(aes(label=iso3166_2), colour="white") + scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))
g4_shrimp <- ggplot(aes(fill=shrimp), data=spdf) + geom_sf(colour="white") + theme_void() + geom_sf_text(aes(label=iso3166_2), colour="white") + scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))
g4_ornamentalfish <- ggplot(aes(fill=ornamentalfish), data=spdf) + geom_sf(colour="white") + theme_void() + geom_sf_text(aes(label=iso3166_2), colour="white") + scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))
g4_cattle <- ggplot(aes(fill=cattle), data=spdf) + geom_sf(colour="white") + theme_void() + geom_sf_text(aes(label=iso3166_2), colour="white") + scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))
g4_trout <- ggplot(aes(fill=trout), data=spdf) + geom_sf(colour="white") + theme_void() + geom_sf_text(aes(label=iso3166_2), colour="white") + scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))
g4_wildfinfishcommercial <- ggplot(aes(fill=wildcaught_finfish_commerical_midpoint), data=spdf) + geom_sf(colour="white") + theme_void() + geom_sf_text(aes(label=iso3166_2), colour="white") + scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))
g4_wildfinfishrecreational <- ggplot(aes(fill=wildcaught_finfish_recreational_midpoint), data=spdf) + geom_sf(colour="white") + theme_void() + geom_sf_text(aes(label=iso3166_2), colour="white") + scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))
g4_wildshrimp <- ggplot(aes(fill=wildcaught_shrimp_midpoint), data=spdf) + geom_sf(colour="white") + theme_void() + geom_sf_text(aes(label=iso3166_2), colour="white") + scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6))


#grid.arrange(g4_broilers,g4_baitfish,g4_crawfish,g4_layers,g4_pigs,
             #g4_catfish,g4_shrimp,g4_ornamentalfish,g4_cattle,g4_trout)
g4 <- arrangeGrob(g4_broilers,g4_baitfish,g4_crawfish,g4_layers,g4_pigs,
             g4_catfish,g4_shrimp,g4_ornamentalfish,g4_cattle,g4_trout,
             g4_wildfinfishcommercial,g4_wildfinfishrecreational,g4_wildshrimp)
ggsave("g4.png",g4,width=24,height=24)







