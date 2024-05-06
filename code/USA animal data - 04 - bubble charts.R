#Credit to https://stackoverflow.com/questions/37186172/bubble-chart-without-axis-in-r

setwd("~/Documents/USA Scoping") #Set working directory
library(ggplot2) #For graphing
library(packcircles) #For generating circles
library(scales) #To help graphing
library(viridis) #To help graphing
theme_set(theme_bw()) #Because I'm fashionable

#Generate vector of circle sizes
#This will be for layer farm sizes
#from Table 30 
#https://www.nass.usda.gov/Publications/AgCensus/2017/Full_Report/Volume_1,_Chapter_1_US/usv1.pdf

df_bubble_layers <- data.frame(
  "number_of_farms" = c(202403,17066,7871,1482,513,1095,1484,266,320),
  "layers_in_each_category" = c(3260087,1063287,1253342,1464135,
                               3499325,16185116,42059552,
                               17973810,281482739))
df_bubble_layers$layers_per_farm <- df_bubble_layers$layers_in_each_category/
  df_bubble_layers$number_of_farms

vector_bubble_layers <- rep(df_bubble_layers$layers_per_farm,
                            df_bubble_layers$number_of_farms)
#vector_bubble_layers_rand <- vector_bubble_layers[order(-vector_bubble_layers)]

#Generate circles
p <- circleProgressiveLayout(vector_bubble_layers)
d <- circleLayoutVertices(p)
d$radius <- p$radius
d$area <- pi*(d$radius^2)

g_bubble_layers <- ggplot(d, aes(x, y)) + 
  geom_polygon(aes(group = id, fill=area), show.legend = T) +
  scale_fill_viridis(option="rocket", direction=-1,
                     trans="log",
                     breaks=c(100,1000,10000,100000,500000),
                     labels=c("100","1k","10k","100k","500k")) +
  labs(fill="No. hens",title="Distribution of hens on egg farms in the United States",
       caption="Each circle represents a farm, and each circle's area is proportional to the approx. number of laying hens on that farm.")+
  #geom_text(data = p, aes(x, y), label = LETTERS) +
  #scale_fill_distiller(palette = "RdGy") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill="white"))+ 
  guides(fill = guide_colorbar(barheight = 10))

g_bubble_layers
ggsave("results/g_bubble_layers.png",g_bubble_layers,width=8,height=8)
