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
library(terra) #For spatial data
library(tidyterra) #For spatial data
library(ggnewscale) #To help graphing
library(xlsx) #For writing to a spreadsheet for further analysis
#library(geojsonio) #For importing spatial JSON file
#library(sf) #For spatial data


###################
### Import data ###
###################

##############
# Trade data #
##############

#Export data
#From https://apps.fas.usda.gov/gats/ExpressQuery1.aspx
#Product Type = Exports
#HS-4 product groups 01 to 05; all partners; annual for 2022;
#quantity FAS non converted
df_trade_export_raw <- read.csv("data/USA FAS Exports - Standard Query_00643.csv", skip=4)

#Import data
#Product Type = Imports (Consumption)
#From https://apps.fas.usda.gov/gats/ExpressQuery1.aspx
#HS-4 product groups 01 to 05; all partners; annual for 2022;
#quantity FAS non converted
df_trade_import_raw <- read.csv("data/USA FAS Imports - Standard Query_19353.csv", skip=4)

#Export and import data at HS-6 for these HS-4 categories: 0206, 0209, 0210, 
#As these are categories that contain multiple species each
#So we'll want to treat those categories separately
df_trade_export_hs6_raw <- read.csv("data/USA FAS Exports - HS6 for 0206 0209 0210 - Standard Query_27749.csv", skip=4)
df_trade_import_hs6_raw <- read.csv("data/USA FAS Imports - HS6 for 0206 0209 0210 - Standard Query_20618.csv", skip=4)

#Domestic production data, USA whole, to compare to trade data
#https://quickstats.nass.usda.gov/
#Census; Animals & Products
#National (US Total)
#2022; Annual
df_production_usa_total_raw <- read.csv("data/NASS USA Total - BF2D9DD8-1FBA-30B6-B2A9-39210B2CBADB.csv")

######################################
# Domestic production data by county #
######################################

#https://quickstats.nass.usda.gov/
#Census; Animals & Products
#County
#2022; Annual

df_production_usa_county_1 <- read.csv(
  "data/NASS County 1 - E1D7B1A7-F019-375A-B095-E9290A07CF2D.csv") # States beginning with the letter A - F
df_production_usa_county_2 <- read.csv(
  "data/NASS County 2 - 1AED18E0-ABFF-3804-8683-46A7C733203B.csv") # G - In
df_production_usa_county_3 <- read.csv(
  "data/NASS County 3 - A081B212-C206-3FAC-9A2D-6354F83D4DCE.csv") # Io - Mai
df_production_usa_county_4 <- read.csv(
  "data/NASS County 4 - 93E65477-AEBE-37F3-A089-273B364CDF1A.csv") # Mar - Misso
df_production_usa_county_5 <- read.csv(
  "data/NASS County 5 - D7C6BA48-2953-362A-BB0A-98160223CF4A.csv") # Mon - N
df_production_usa_county_6 <- read.csv(
  "data/NASS County 6 - 7FF30753-C3AD-37DA-BD15-57B228460B5E.csv") # O - S
df_production_usa_county_7 <- read.csv(
  "data/NASS County 7 - CCB4516C-188B-338A-9F22-7B3B255635F2.csv") # T - U
df_production_usa_county_8 <- read.csv(
  "data/NASS County 8 - 021C93D8-121C-3486-8168-12A2628EF569.csv") # V - W

#Bind into a single data frame
df_production_usa_county_raw <- rbind(
  df_production_usa_county_1, df_production_usa_county_2, df_production_usa_county_3,
  df_production_usa_county_4, df_production_usa_county_5, df_production_usa_county_6,
  df_production_usa_county_7, df_production_usa_county_8
)

#Remove all the individual component data frames
rm(df_production_usa_county_1, df_production_usa_county_2, df_production_usa_county_3,
          df_production_usa_county_4, df_production_usa_county_5, df_production_usa_county_6,
          df_production_usa_county_7, df_production_usa_county_8)

############################################
### Data restricting, cleaning, analysis ###
############################################

### Imports and exports ###

#For imports and exports, specify which products we are interested in
#(I wrote this list manually)
trade_codes_hs4 <- c(407,408,102,201,202,207,105,203,103)

#Categories that I DO want to include, except that we need to get them from the HS6 list
trade_codes_hs6_cattle <- c(20610, 20621, 20622, 20629, 21020)
trade_codes_hs6_swine <- c(20641, 20649, 20900, 20910, 20990, 21011, 21012, 21019)

trade_codes <- c(trade_codes_hs4, trade_codes_hs6_cattle, trade_codes_hs6_swine)

#Separate the HS6 import and export data frames by cattle vs swine,
#and then assign to HS4 labels accordingly
#df_trade_export_hs6_raw_cattle <- df_trade_export_hs6_raw[df_trade_export_hs6_raw$HS.Code %in% trade_codes_hs6_cattle,]
#df_trade_export_hs6_raw_swine <- df_trade_export_hs6_raw[df_trade_export_hs6_raw$HS.Code %in% trade_codes_hs6_swine,]
#df_trade_import_hs6_raw_cattle <- df_trade_import_hs6_raw[df_trade_import_hs6_raw$HS.Code %in% trade_codes_hs6_cattle,]
#df_trade_import_hs6_raw_swine <- df_trade_import_hs6_raw[df_trade_import_hs6_raw$HS.Code %in% trade_codes_hs6_swine,]

df_trade_export_raw <- rbind(df_trade_export_raw, df_trade_export_hs6_raw)
df_trade_import_raw <- rbind(df_trade_import_raw, df_trade_import_hs6_raw)

#Excluding aquaculture for now, might do that separately because it's very complicated: 301,302,303,304,305,306
#Other codes that may be counted, but in practice have negligible quantities: 502, 309, 308, 504, 106, 511

#Conversion factors for numbers of animals to meat weight
#from https://ourworldindata.org/meat-production#cattle-meat-per-animal
#MT per NO (that is, metric tonnes per individual animal)
#OWID gives the data in kg, hence dividing by 1,000 because we want to
#convert to tonnes, not kg
#and eggs roughly 60 grams from https://faunalytics.org/wp-content/uploads/2015/05/Citation2161_WorldEggProduction2011.pdf
conversion_cattle <- 370.6/1000
conversion_poultry <- 2.43/1000
conversion_swine <- 97.3/1000
conversion_egg <- 12*60/10^6 #from DZ to MT

#Imports
df_trade_import_raw$Qty_numeric <- as.numeric(gsub(",","",df_trade_import_raw$Qty))
df_trade_import_raw_restricted <- df_trade_import_raw[which(df_trade_import_raw$HS.Code %in% trade_codes),]

#Sum across all trade partners
df_trade_import_agg <- aggregate(Qty_numeric~Product+HS.Code+UOM, df_trade_import_raw_restricted, FUN=sum)
df_trade_import_agg <- df_trade_import_agg[order(df_trade_import_agg$HS.Code),]

#Exports
df_trade_export_raw$Qty_numeric <- as.numeric(gsub(",","",df_trade_export_raw$Qty))
df_trade_export_raw_restricted <- df_trade_export_raw[which(df_trade_export_raw$HS.Code %in% trade_codes),]

#Sum across all trade partners
df_trade_export_agg <- aggregate(Qty_numeric~Product+HS.Code+UOM, df_trade_export_raw_restricted, FUN=sum)
df_trade_export_agg <- df_trade_export_agg[order(df_trade_export_agg$HS.Code),]

#Combine into a single data frame with both imports and exports
df_trade_agg <- data.frame("Product"=df_trade_import_agg$Product,
                           "HS.Code"=df_trade_import_agg$HS.Code,
                           "UOM"=df_trade_import_agg$UOM,
                           "Qty_import_original"=df_trade_import_agg$Qty_numeric,
                           "Qty_export_original"=df_trade_export_agg$Qty_numeric)

#write.csv(df_trade_agg, file="results/df_trade_agg.csv")

#Convert tonnes (MT) to numbers (NO)
#(or for eggs, MT to dozens DZ)

#as well as the HS6 codes for bovine and swine
df_trade_agg$Qty_import <- df_trade_agg$Qty_import_original
df_trade_agg$Qty_export <- df_trade_agg$Qty_export_original

#201, 202, some HS6 codes: convert for bovine
df_trade_agg[df_trade_agg$HS.Code==201,c("Qty_import","Qty_export")] <-
  (1/conversion_cattle)*df_trade_agg[df_trade_agg$HS.Code==201,c("Qty_import_original","Qty_export_original")]  
df_trade_agg[df_trade_agg$HS.Code==202,c("Qty_import","Qty_export")] <-
  (1/conversion_cattle)*df_trade_agg[df_trade_agg$HS.Code==202,c("Qty_import_original","Qty_export_original")]  
df_trade_agg[df_trade_agg$HS.Code %in% trade_codes_hs6_cattle,c("Qty_import","Qty_export")] <-
  (1/conversion_cattle)*df_trade_agg[df_trade_agg$HS.Code %in% trade_codes_hs6_cattle,c("Qty_import_original","Qty_export_original")]  

#203, some HS6 codes: convert for swine
df_trade_agg[df_trade_agg$HS.Code==203,c("Qty_import","Qty_export")] <-
  (1/conversion_swine)*df_trade_agg[df_trade_agg$HS.Code==203,c("Qty_import_original","Qty_export_original")]  
df_trade_agg[df_trade_agg$HS.Code %in% trade_codes_hs6_swine,c("Qty_import","Qty_export")] <-
  (1/conversion_swine)*df_trade_agg[df_trade_agg$HS.Code %in% trade_codes_hs6_swine,c("Qty_import_original","Qty_export_original")]  


#207, convert for poultry
df_trade_agg[df_trade_agg$HS.Code==207,c("Qty_import","Qty_export")] <-
  (1/conversion_poultry)*df_trade_agg[df_trade_agg$HS.Code==207,c("Qty_import_original","Qty_export_original")]  

#408, convert for egg
df_trade_agg[df_trade_agg$HS.Code==408,c("Qty_import","Qty_export")] <-
  (1/conversion_egg)*df_trade_agg[df_trade_agg$HS.Code==408,c("Qty_import_original","Qty_export_original")]  

#Add up the different product codes that correspond to the same type of animal
df_trade <- data.frame("Product"=c("Bovine", "Swine", "Poultry", "Egg"),
           "UOM"=c("NO", "NO", "NO", "DZ"),
           "Qty_import" = c(
             sum(df_trade_agg[df_trade_agg$HS.Code %in% c(201,202,102,trade_codes_hs6_cattle),]$Qty_import),
             sum(df_trade_agg[df_trade_agg$HS.Code %in% c(203,103,trade_codes_hs6_swine),]$Qty_import),
             sum(df_trade_agg[df_trade_agg$HS.Code %in% c(207,105),]$Qty_import),
             sum(df_trade_agg[df_trade_agg$HS.Code %in% c(407,408),]$Qty_import)
           ),
           "Qty_export"=c(
             sum(df_trade_agg[df_trade_agg$HS.Code %in% c(201,202,102,trade_codes_hs6_cattle),]$Qty_export),
             sum(df_trade_agg[df_trade_agg$HS.Code %in% c(203,103,trade_codes_hs6_swine),]$Qty_export), 
             sum(df_trade_agg[df_trade_agg$HS.Code %in% c(207,105),]$Qty_export), 
             sum(df_trade_agg[df_trade_agg$HS.Code %in% c(407,408),]$Qty_export)
           ))


#USA total production
commodity_codes <- c("CATTLE","HOGS","CHICKENS","EGGS","CRUSTACEANS", "FOOD FISH")
domain_codes <- c("TOTAL")
dataitem_codes <- c("EGGS, PRODUCTION CONTRACT - PRODUCTION, MEASURED IN DOZEN",
                    "CATTLE, INCL CALVES - SALES, MEASURED IN HEAD", "CHICKENS, BROILERS - SALES, MEASURED IN HEAD", "HOGS - SALES, MEASURED IN HEAD",
                    "FOOD FISH, CATFISH - SALES & DISTRIBUTION, MEASURED IN $",
                    "FOOD FISH, TROUT - SALES & DISTRIBUTION, MEASURED IN $",
                    "CRUSTACEANS - SALES & DISTRIBUTION, MEASURED IN $")

#Set all (D) to 0, rather than being NA
df_production_usa_total_raw[which(df_production_usa_total_raw$Value==" (D)"),]$Value <- "0"
df_production_usa_total_raw$Value_numeric <- as.numeric(gsub(",","",df_production_usa_total_raw$Value))
df_production_usa_total <- 
  df_production_usa_total_raw[df_production_usa_total_raw$Commodity %in% commodity_codes &
                                df_production_usa_total_raw$Domain %in% domain_codes &
                                df_production_usa_total_raw$Data.Item %in% dataitem_codes,]


df_trade$Qty_domesticproduction <- c(
  df_production_usa_total[df_production_usa_total$Commodity=="CATTLE",]$Value_numeric,
  df_production_usa_total[df_production_usa_total$Commodity=="HOGS",]$Value_numeric,
  df_production_usa_total[df_production_usa_total$Commodity=="CHICKENS",]$Value_numeric,
  df_production_usa_total[df_production_usa_total$Commodity=="EGGS",]$Value_numeric
)

df_trade$import_percent_of_domestic <- 100*df_trade$Qty_import/df_trade$Qty_domesticproduction
df_trade$export_percent_of_domestic <- 100*df_trade$Qty_export/df_trade$Qty_domesticproduction

df_trade

#write.csv(df_trade, file="results/df_trade.csv")

#Now I want to look at the detailed trade flows for poultry and eggs
df_g_imports_207 <- df_trade_import_raw[df_trade_import_raw$HS.Code==207,]
df_g_imports_207$percent <- 100*df_g_imports_207$Qty_numeric/sum(df_g_imports_207$Qty_numeric,na.rm=T)
df_g_imports_207 <- df_g_imports_207[df_g_imports_207$percent>1,]
df_g_imports_207 <- df_g_imports_207[-which(is.na(df_g_imports_207$Partner)),]
g_imports_207 <- ggplot(aes(x=reorder(Partner,-percent),y=percent), data=df_g_imports_207) +
  geom_col(fill="#E4ABB8") +
  geom_text(aes(label=paste("(",round(percent,1),"%)",sep="")), vjust=-0.5) +
  geom_text(aes(label=paste(Qty," t",sep=""), vjust=-2)) +
  xlab("Trade partner") + ylab("Percent") +
  ggtitle("Imported poultry meat", subtitle="Individuals imported by United States in 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = expansion(add = c(0,10)))
g_imports_207

df_g_imports_105 <- df_trade_import_raw[df_trade_import_raw$HS.Code==105,]
df_g_imports_105$percent <- 100*df_g_imports_105$Qty_numeric/sum(df_g_imports_105$Qty_numeric,na.rm=T)
df_g_imports_105 <- df_g_imports_105[df_g_imports_105$percent>1,]
df_g_imports_105 <- df_g_imports_105[-which(is.na(df_g_imports_105$Partner)),]
g_imports_105 <- ggplot(aes(x=reorder(Partner,-percent),y=percent), data=df_g_imports_105) +
  geom_col(fill="#E4ABB8") +
  geom_text(aes(label=paste("(",round(percent,1),"%)",sep="")), vjust=-0.5) +
  geom_text(aes(label=Qty, vjust=-2)) +
  xlab("Trade partner") + ylab("Percent") +
  ggtitle("Imported live chickens", subtitle="Individuals imported by United States in 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = expansion(add = c(0,10)))
g_imports_105

df_g_exports_207 <- df_trade_export_raw[df_trade_export_raw$HS.Code==207,]
df_g_exports_207$percent <- 100*df_g_exports_207$Qty_numeric/sum(df_g_exports_207$Qty_numeric,na.rm=T)
df_g_exports_207 <- df_g_exports_207[df_g_exports_207$percent>1,]
df_g_exports_207 <- df_g_exports_207[-which(is.na(df_g_exports_207$Partner)),]
g_exports_207 <- ggplot(aes(x=reorder(Partner,-percent),y=percent), data=df_g_exports_207) +
  geom_col(fill="#E67B94") +
  geom_text(aes(label=paste("(",round(percent,1),"%)",sep="")), vjust=-0.5) +
  geom_text(aes(label=paste(Qty," t",sep=""), vjust=-2)) +
  xlab("Trade partner") + ylab("Percent") +
  ggtitle("Exported poultry meat", subtitle="Tonnes exported by United States in 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = expansion(add = c(0,3)))
g_exports_207

df_g_exports_105 <- df_trade_export_raw[df_trade_export_raw$HS.Code==105,]
df_g_exports_105$percent <- 100*df_g_exports_105$Qty_numeric/sum(df_g_exports_105$Qty_numeric,na.rm=T)
df_g_exports_105 <- df_g_exports_105[df_g_exports_105$percent>1,]
df_g_exports_105 <- df_g_exports_105[-which(is.na(df_g_exports_105$Partner)),]
g_exports_105 <- ggplot(aes(x=reorder(Partner,-percent),y=percent), data=df_g_exports_105) +
  geom_col(fill="#E67B94") +
  geom_text(aes(label=paste("(",round(percent,1),"%)",sep="")), vjust=-0.5) +
  geom_text(aes(label=Qty, vjust=-2)) +
  xlab("Trade partner") + ylab("Percent") +
  ggtitle("Exported live chickens", subtitle="Individuals exported by United States in 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = expansion(add = c(0,5)))
g_exports_105

grid.arrange(arrangeGrob(g_imports_207,g_imports_105, nrow=1),
             g_exports_207,
             g_exports_105,
             nrow=3)

g_poultry_trade <- arrangeGrob(arrangeGrob(g_imports_207,g_imports_105, nrow=1),
             g_exports_207,
             g_exports_105,
             nrow=3)

ggsave("results/g_poultry_trade.png",g_poultry_trade,width=16,height=16)





# eggs
df_g_imports_407 <- df_trade_import_raw[df_trade_import_raw$HS.Code==407,]
df_g_imports_407$percent <- 100*df_g_imports_407$Qty_numeric/sum(df_g_imports_407$Qty_numeric,na.rm=T)
df_g_imports_407 <- df_g_imports_407[df_g_imports_407$percent>1,]
df_g_imports_407 <- df_g_imports_407[-which(is.na(df_g_imports_407$Partner)),]
g_imports_407 <- ggplot(aes(x=reorder(Partner,-percent),y=percent), data=df_g_imports_407) +
  geom_col(fill="#FFE5AA") +
  geom_text(aes(label=paste("(",round(percent,1),"%)",sep="")), vjust=-0.5) +
  geom_text(aes(label=paste(Qty," dz",sep=""), vjust=-2)) +
  xlab("Trade partner") + ylab("Percent") +
  ggtitle("Imported shell eggs", subtitle="Dozens imported by United States in 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = expansion(add = c(0,5)))
g_imports_407

df_g_imports_408 <- df_trade_import_raw[df_trade_import_raw$HS.Code==408,]
df_g_imports_408$percent <- 100*df_g_imports_408$Qty_numeric/sum(df_g_imports_408$Qty_numeric,na.rm=T)
df_g_imports_408 <- df_g_imports_408[df_g_imports_408$percent>1,]
df_g_imports_408 <- df_g_imports_408[-which(is.na(df_g_imports_408$Partner)),]
g_imports_408 <- ggplot(aes(x=reorder(Partner,-percent),y=percent), data=df_g_imports_408) +
  geom_col(fill="#FFE5AA") +
  geom_text(aes(label=paste("(",round(percent,1),"%)",sep="")), vjust=-0.5) +
  geom_text(aes(label=paste(Qty," t",sep=""), vjust=-2)) +
  xlab("Trade partner") + ylab("Percent") +
  ggtitle("Imported non-shell eggs", subtitle="Tonnes imported by United States in 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = expansion(add = c(0,10)))
g_imports_408

df_g_exports_407 <- df_trade_export_raw[df_trade_export_raw$HS.Code==407,]
df_g_exports_407$percent <- 100*df_g_exports_407$Qty_numeric/sum(df_g_exports_407$Qty_numeric,na.rm=T)
df_g_exports_407 <- df_g_exports_407[df_g_exports_407$percent>1,]
df_g_exports_407 <- df_g_exports_407[-which(is.na(df_g_exports_407$Partner)),]
g_exports_407 <- ggplot(aes(x=reorder(Partner,-percent),y=percent), data=df_g_exports_407) +
  geom_col(fill="#FCD375") +
  geom_text(aes(label=paste("(",round(percent,1),"%)",sep="")), vjust=-0.5) +
  geom_text(aes(label=paste(Qty," dz",sep=""), vjust=-2)) +
  xlab("Trade partner") + ylab("Percent") +
  ggtitle("Exported shell eggs", subtitle="Dozens exported by United States in 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = expansion(add = c(0,3)))
g_exports_407

df_g_exports_408 <- df_trade_export_raw[df_trade_export_raw$HS.Code==408,]
df_g_exports_408$percent <- 100*df_g_exports_408$Qty_numeric/sum(df_g_exports_408$Qty_numeric,na.rm=T)
df_g_exports_408 <- df_g_exports_408[df_g_exports_408$percent>1,]
df_g_exports_408 <- df_g_exports_408[-which(is.na(df_g_exports_408$Partner)),]
g_exports_408 <- ggplot(aes(x=reorder(Partner,-percent),y=percent), data=df_g_exports_408) +
  geom_col(fill="#FCD375") +
  geom_text(aes(label=paste("(",round(percent,1),"%)",sep="")), vjust=-0.5) +
  geom_text(aes(label=paste(Qty," t",sep=""), vjust=-2)) +
  xlab("Trade partner") + ylab("Percent") +
  ggtitle("Exported non-shell eggs", subtitle="Tonnes exported by United States in 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(expand = expansion(add = c(0,5)))
g_exports_408

grid.arrange

g_egg_trade <- arrangeGrob(g_imports_407,
                           g_imports_408,
                           g_exports_407,
                           g_exports_408,
                           nrow=2)

#ggsave("results/g_egg_trade.png",g_egg_trade,width=22,height=12)





############################
### Production by county ###
############################
#Set all (D) to 0, then convert production to numeric
#Note (D) = confidential; (Z) = small
df_production_usa_county_raw[which(df_production_usa_county_raw$Value==" (D)"),]$Value <- "0"
df_production_usa_county_raw[which(df_production_usa_county_raw$Value==" (Z)"),]$Value <- "0"
df_production_usa_county_raw$Value_numeric <- as.numeric(gsub(",","",df_production_usa_county_raw$Value))
df_production_usa_county <- 
  df_production_usa_county_raw[df_production_usa_county_raw$Commodity %in% commodity_codes &
                                 df_production_usa_county_raw$Domain %in% domain_codes &
                                 df_production_usa_county_raw$Data.Item %in% dataitem_codes,]

#Get the GEOID codes from state and county codes, which we'll use for matching
df_production_usa_county$State.ANSI.twodigits <- sprintf("%02d", df_production_usa_county$State.ANSI)
df_production_usa_county$County.ANSI.threedigits <- sprintf("%03d", df_production_usa_county$County.ANSI)
df_production_usa_county$GEOID <- paste(df_production_usa_county$State.ANSI.twodigits,
                                        df_production_usa_county$County.ANSI.threedigits,
                                        sep="")

#Separate into the commodities
df_production_usa_county_cattle <- df_production_usa_county[df_production_usa_county$Commodity=="CATTLE",c("GEOID","Value_numeric")]
names(df_production_usa_county_cattle) <- c("fips", "cattle")
df_production_usa_county_chickens <- df_production_usa_county[df_production_usa_county$Commodity=="CHICKENS",c("GEOID","Value_numeric")]
names(df_production_usa_county_chickens) <- c("fips", "chickens")
df_production_usa_county_hogs <- df_production_usa_county[df_production_usa_county$Commodity=="HOGS",c("GEOID","Value_numeric")]
names(df_production_usa_county_hogs) <- c("fips", "hogs")
df_production_usa_county_eggs <- df_production_usa_county[df_production_usa_county$Commodity=="EGGS",c("GEOID","Value_numeric")]
names(df_production_usa_county_eggs) <- c("fips", "eggs")
df_production_usa_county_crustaceans <- df_production_usa_county[df_production_usa_county$Commodity=="CRUSTACEANS",c("GEOID","Value_numeric")]
names(df_production_usa_county_crustaceans) <- c("fips", "crustaceans")
df_production_usa_county_trout <- df_production_usa_county[df_production_usa_county$Data.Item=="FOOD FISH, TROUT - SALES & DISTRIBUTION, MEASURED IN $",c("GEOID","Value_numeric")]
names(df_production_usa_county_trout) <- c("fips", "trout")
df_production_usa_county_catfish <- df_production_usa_county[df_production_usa_county$Data.Item=="FOOD FISH, CATFISH - SALES & DISTRIBUTION, MEASURED IN $",c("GEOID","Value_numeric")]
names(df_production_usa_county_catfish) <- c("fips", "catfish")

#Format the county-level production data for further analysis
#We'll make this into a prettier spreadsheet for saving to file
head(df_production_usa_county)
df_production_usa_county_pretty <- df_production_usa_county

#Report geography in the desired format
df_production_usa_county_pretty$Geography <- paste0(
  "0500000US",
  df_production_usa_county_pretty$State.ANSI.twodigits,
  df_production_usa_county_pretty$County.ANSI.threedigits
)

#Report geographic area name in the desired format
df_production_usa_county_pretty$Geographic_Area_Name <- paste0(
  stringr::str_to_title(df_production_usa_county_pretty$County),
  " County, ",
  stringr::str_to_title(df_production_usa_county_pretty$State)
)

#Keep only the columns we want
df_production_usa_county_pretty <- df_production_usa_county_pretty[
  ,
  c("Geography","Geographic_Area_Name","Year","Commodity","Data.Item","Value_numeric")
]
head(df_production_usa_county_pretty)
unique(df_production_usa_county_pretty$Data.Item)
df_production_usa_county_pretty
county_spreadsheet_cattle <- df_production_usa_county_pretty[which(
  df_production_usa_county_pretty$Commodity=="CATTLE"),]
county_spreadsheet_chickens <- df_production_usa_county_pretty[which(
  df_production_usa_county_pretty$Commodity=="CHICKENS"),]
county_spreadsheet_hogs <- df_production_usa_county_pretty[which(
  df_production_usa_county_pretty$Commodity=="HOGS"),]
county_spreadsheet_eggs <- df_production_usa_county_pretty[which(
  df_production_usa_county_pretty$Commodity=="EGGS"),]
county_spreadsheet_crustaceans <- df_production_usa_county_pretty[which(
  df_production_usa_county_pretty$Commodity=="CRUSTACEANS"),]
county_spreadsheet_trout <- df_production_usa_county_pretty[which(
  df_production_usa_county_pretty$Data.Item==
    "FOOD FISH, TROUT - SALES & DISTRIBUTION, MEASURED IN $"),]
county_spreadsheet_catfish <- df_production_usa_county_pretty[which(
  df_production_usa_county_pretty$Data.Item==
    "FOOD FISH, CATFISH - SALES & DISTRIBUTION, MEASURED IN $"),]

#Write to a single spreadsheet
write.xlsx(county_spreadsheet_cattle, file="results/county_production_spreadsheet.xlsx", 
           sheetName="cattle", row.names=FALSE)
write.xlsx(county_spreadsheet_chickens, file="results/county_production_spreadsheet.xlsx", 
           sheetName="chickens", append=TRUE, row.names=FALSE)
write.xlsx(county_spreadsheet_hogs, file="results/county_production_spreadsheet.xlsx", 
           sheetName="hogs", append=TRUE, row.names=FALSE)
write.xlsx(county_spreadsheet_eggs, file="results/county_production_spreadsheet.xlsx", 
           sheetName="eggs", append=TRUE, row.names=FALSE)
write.xlsx(county_spreadsheet_crustaceans, file="results/county_production_spreadsheet.xlsx", 
           sheetName="crustaceans", append=TRUE, row.names=FALSE)
write.xlsx(county_spreadsheet_trout, file="results/county_production_spreadsheet.xlsx", 
           sheetName="trout", append=TRUE, row.names=FALSE)
write.xlsx(county_spreadsheet_catfish, file="results/county_production_spreadsheet.xlsx", 
           sheetName="catfish", append=TRUE, row.names=FALSE)


#Graph
map_cattle <- map_with_data(df_production_usa_county_cattle, value="cattle")
map_chickens <- map_with_data(df_production_usa_county_chickens, value="chickens")
map_hogs <- map_with_data(df_production_usa_county_hogs, value="hogs")
map_eggs <- map_with_data(df_production_usa_county_eggs, value="eggs")
map_crustaceans <- map_with_data(df_production_usa_county_crustaceans, value="crustaceans")
map_trout <- map_with_data(df_production_usa_county_trout, value="trout")
map_catfish <- map_with_data(df_production_usa_county_catfish, value="catfish")

g_counties_cattle <- ggplot() +
  geom_sf(aes(colour=cattle,fill=cattle), data=map_cattle) +
  theme_void() +
  scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_colour_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  new_scale_fill() +
  new_scale_colour() +
  geom_sf(data=us_map(regions=c("states")),fill="#FFFFFF00",colour="#404040") +
  geom_sf_text(aes(label=abbr), colour="#40404042", data=us_map(regions=c("states"))) + 
  ggtitle("Cattle produced by county", subtitle="Unit = Individuals sold by farms; Year = 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill="white"))
g_counties_cattle

g_counties_chickens <- ggplot() +
  geom_sf(aes(colour=chickens,fill=chickens), data=map_chickens) +
  theme_void() +
  scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_colour_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  new_scale_fill() +
  new_scale_colour() +
  geom_sf(data=us_map(regions=c("states")),fill="#FFFFFF00",colour="#404040") +
  geom_sf_text(aes(label=abbr), colour="#40404042", data=us_map(regions=c("states"))) + 
  ggtitle("Chickens produced by county", subtitle="Unit = Individuals sold by farms; Year = 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill="white"))
g_counties_chickens

g_counties_hogs <- ggplot() +
  geom_sf(aes(colour=hogs,fill=hogs), data=map_hogs) +
  theme_void() +
  scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_colour_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  new_scale_fill() +
  new_scale_colour() +
  geom_sf(data=us_map(regions=c("states")),fill="#FFFFFF00",colour="#404040") +
  geom_sf_text(aes(label=abbr), colour="#40404042", data=us_map(regions=c("states"))) + 
  ggtitle("Hogs produced by county", subtitle="Unit = Individuals sold by farms; Year = 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill="white"))
g_counties_hogs

g_counties_eggs <- ggplot() +
  geom_sf(aes(colour=eggs,fill=eggs), data=map_eggs) +
  theme_void() +
  scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_colour_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  new_scale_fill() +
  new_scale_colour() +
  geom_sf(data=us_map(regions=c("states")),fill="#FFFFFF00",colour="#404040") +
  geom_sf_text(aes(label=abbr), colour="#40404042", data=us_map(regions=c("states"))) + 
  ggtitle("Eggs produced by county", subtitle="Unit = Dozen produced by farms; Year = 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill="white"))
g_counties_eggs

g_counties_crustaceans <- ggplot() +
  geom_sf(aes(colour=crustaceans,fill=crustaceans), data=map_crustaceans) +
  theme_void() +
  scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_colour_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  new_scale_fill() +
  new_scale_colour() +
  geom_sf(data=us_map(regions=c("states")),fill="#FFFFFF00",colour="#404040") +
  geom_sf_text(aes(label=abbr), colour="#40404042", data=us_map(regions=c("states"))) + 
  ggtitle("Crustaceans produced by county, value", subtitle="Unit = US Dollar; Year = 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill="white"))
g_counties_crustaceans

g_counties_trout <- ggplot() +
  geom_sf(aes(colour=trout,fill=trout), data=map_trout) +
  theme_void() +
  scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_colour_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  new_scale_fill() +
  new_scale_colour() +
  geom_sf(data=us_map(regions=c("states")),fill="#FFFFFF00",colour="#404040") +
  geom_sf_text(aes(label=abbr), colour="#40404042", data=us_map(regions=c("states"))) + 
  ggtitle("Trout produced by county, value", subtitle="Unit = US Dollar; Year = 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill="white"))
g_counties_trout

g_counties_catfish <- ggplot() +
  geom_sf(aes(colour=catfish,fill=catfish), data=map_catfish) +
  theme_void() +
  scale_fill_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_colour_viridis(option="rocket", direction=-1, labels = unit_format(unit = "M", scale = 1e-6)) +
  new_scale_fill() +
  new_scale_colour() +
  geom_sf(data=us_map(regions=c("states")),fill="#FFFFFF00",colour="#404040") +
  geom_sf_text(aes(label=abbr), colour="#40404042", data=us_map(regions=c("states"))) + 
  ggtitle("Catfish produced by county, value", subtitle="Unit = US Dollar; Year = 2022") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect(fill="white"))
g_counties_catfish

#ggsave("results/g_counties_cattle.png", g_counties_cattle, width=16)
#ggsave("results/g_counties_chickens.png", g_counties_chickens, width=16) 
#ggsave("results/g_counties_hogs.png", g_counties_hogs, width=16) 
#ggsave("results/g_counties_eggs.png", g_counties_eggs, width=16) 
#ggsave("results/g_counties_crustaceans.png", g_counties_crustaceans, width=16) 
#ggsave("results/g_counties_trout.png", g_counties_trout, width=16) 
#ggsave("results/g_counties_catfish.png", g_counties_catfish, width=16) 












