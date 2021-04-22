########################################################### ##
##   Data manipulation and presentation for systematic map ###

## 1.0 Setup environment           ####

rm(list = ls())
getwd()

dir_data <-"C:/Users/chess/OneDrive - Newcastle University/PhD homework/Other Work and Demonstrating/BIO8068/git-example"
setwd(dir_data)


library(dplyr)
library(tidyr)
library(sf)
library(raster)
library(RColorBrewer)
library(reshape2)
library(plyr)
library(ggplot2)
library(ggpol)

## 1.1 Import and tidy data ####

# remove number codes from variable names first
data <- read.csv("git_test_data.csv", header = TRUE, na.strings = c(""))
data$Comments <- NULL
summary(as.factor(data$Inclusion))

#Remove studies excluded
data <- data %>% 
  filter(Inclusion == 1) #118 coded

summary(data)



#################################### ###
## 2.0 Distribution among threats  ####
## 2.1 Frequencies of each threat 

# Select only threat and ID column
# Separate out individual threats using ";" as separator 
# Lengthen dataframe 
# count 


## 2.2 Mean number of threats studied 


##################################### ##
## 3.0 Spatial distribution        ####
## 3.1 Frequency of different spatial scales ####


## 3.1.1 Pyramid Plot ####

head(data)

## Wrangle data 

Pyr1 <- data %>% dplyr::select(ArticleID, Spatial_Scale, tax_kingdom) %>% 
  filter(tax_kingdom != "na") %>% 
  separate(tax_kingdom, c("King1", "King2"), sep = ";", extra = "warn") %>% 
  gather(key = "index", value = "Kingdom", King1, King2 ) %>% 
  na.omit() %>% 
  dplyr::count(Spatial_Scale,Kingdom)
Pyr1$Spatial_Scale <- gsub("Multi-National", "Multi-national", Pyr1$Spatial_Scale)

## whichever variable will be on the left - give negative values 
Pyr1$n <- ifelse(Pyr1$Kingdom == "Animalia ", -1*Pyr1$n, Pyr1$n)

## Plot ##
Pyr_plot <- ggplot(Pyr1, aes(x = Spatial_Scale, y = n, fill = Kingdom)) +
  geom_bar(position = position_dodge(width=1), stat='identity') + 
  scale_fill_manual(values=c("dodgerblue3", "darkorange2")) + 
  #scale_y_continuous(trans = "reverse") +
  facet_share(~Kingdom, dir = "h", scales = "free") +
  scale_x_discrete(limits= c("Global", "Continental", "Multi-national","National", "Sub-national", "Local")) +
  coord_flip() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.line.x  = element_line(color = "black"),
        #axis.ticks.y = element_blank(),
        axis.text.x  = element_text(colour = "black", size = 8, face = "bold", angle=0, hjust=0.5),
        axis.text.y  = element_text(colour = "black", size = 8, face = "bold"),
        axis.title.x = element_text(size = 14,  face="bold", margin = margin(t = 30, r = 20, b = 10, l = 20)),
        #plot.margin  = unit(c(1,1,1,1),"cm")
  ) + 
  labs(y = "Articles",x="")
Pyr_plot

# Still to-do: ####
# 1. remove "-" from left side axis labels without changing orientation of the plot
# 2. Make the axis the same scale 



###################################### ###
## 3.2 Geographic distribution ####

dir_shapefiles <- "C:/Users/chess/OneDrive - Newcastle University/PhD homework/Chapter 1 Systematic Map etc/Systematic Map/Systematic Map Analysis/Data visualisation/CountryBoundaries"
setwd(dir_shapefiles)


country_boundaries = shapefile("99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp")
plot(country_boundaries)

# #Coordinate reference system
# crs(country_boundaries) #+proj=longlat +datum=WGS84 +no_defs 
# #Describes type of vector data stored in object
# class(country_boundaries)
# #How many features 
# length(country_boundaries)
# #spatial extent
# extent(country_boundaries)
# #view meta-data summary 
# country_boundaries
# # view particular variable 


#Separate out country information from SM data
country_data <- data %>%
  dplyr::select(ArticleID, Country)

country_data <- country_data[complete.cases(country_data),]
country_data <- country_data[!(country_data$Country == "na"),]

country_data <- country_data %>% separate(Country, c("C1", "C2", "C3", "C4", "C5"), sep =";", extra = "warn") %>% 
  gather(key = "Placement", value = "country", C1, C2, C3, C4, C5 ) %>% 
  na.omit()

## check no errors in naming - only need to run first time

#Make list of countries in shapefile
 lsCntry_shapefile <- country_boundaries$CNTRY_NAME
 lsCntry_data <- unique(country_data$country)
 lsCntry_data
lsCntry_data[!(lsCntry_data %in% lsCntry_shapefile)]
 
 lsCntry_shapefile
 countries <- as.data.frame(lsCntry_shapefile)
 
# write.csv(countries, "list_countries.csv")


## count articles per country
geograph_dist <- country_data %>% 
  dplyr::count(country) %>% 
  dplyr::rename(CNTRY_NAME = country)

countries

article_dist <- countries %>% dplyr::rename(CNTRY_NAME = lsCntry_shapefile) %>% 
  left_join(geograph_dist,by = "CNTRY_NAME", keep = TRUE) %>% 
  mutate_if(is.integer, ~replace(., is.na(.), 0))


geog_merge <- merge(country_boundaries, article_dist, by = "CNTRY_NAME")
geog_merge

## Colour
display.brewer.all() # "Reds", "BluGn", "OrRd"

my.palette <- brewer.pal(n=7, name = "BuPu")

# plot - colour discretely 
# col.regions expects a vector with the names of colours
spplot(geog_merge, "n", col.regions = my.palette, cuts = 6, main = "Threat mapping articles per country", col = "transparent")

spplot(geog_merge, "n", main = "Threat mapping articles per country", col.regions = colorRampPalette(brewer.pal(7, "BuPu"))(16), col = "white")

##  Chloropleth map # colour by numeric variable

summary(as.factor(data$Spatial_Scale))

## Still To do: ####
#1. Split and facet by threat category and ecoregion
#2. Incorporate the marine information using EEZs or otherwise 
#3. Add boundaries between countries in a relevant colour
  
#################################### ###
## 4.0 Taxonomic distribution       ####
## 4.1 Taxonomic group vs taxonomic resolution

library(networkD3)

## 4.1.0 Make taxa dataframe ####
taxa <- data %>%  dplyr::select(ArticleID, tax_kingdom, animal_group, plant_group) %>% 
  filter(tax_kingdom != "na") %>% 
  tidyr::separate(tax_kingdom, c("K1", "K2"), sep = ";", extra = "drop") %>% 
  tidyr::gather(key ="Placement", value = "Kingdom", c("K1", "K2")) %>% 
  na.omit()

for (i in 1:length(taxa$ArticleID)) {
  
if (taxa$Kingdom[i] == "Animalia ") {
  taxa$plant_group[i] <- NA
} else if (taxa$Kingdom[i] == "Plantae") {
  taxa$animal_group[i] <- NA
}
  }

taxa <-taxa %>% gather(key="key",value = "tax_group", c("animal_group", "plant_group")) %>% 
  separate(tax_group, c("A1", "A2", "A3", "A4", "A5"), sep =";", extra = "warn") %>% 
  gather(key = "key2", value = "tax_group", c("A1", "A2", "A3", "A4", "A5")) %>% 
  na.omit() %>% 
  dplyr::select(ArticleID, Kingdom, tax_group)

taxa <- data %>% dplyr::select(ArticleID, tax_res, tax_scope) %>% 
  right_join(taxa, by= "ArticleID")


## 4.1.1 Sankey diagram - taxonomic distribution ####
vector <- as.character(seq(from =1, to =15, by=1 ))


## Convert data into incidence matrix of two variables ##
Tax_Sank <- taxa %>% dplyr::count(tax_res, Kingdom) %>% 
  spread(Kingdom, value = n, fill = NA)
  
  ?spread()

tax_vec <- Tax_Sank$tax_res
Tax_Sank[,1] <-NULL
rownames(Tax_Sank) <- tax_vec

library(tibble)

## transform to connection dataframe ##
links <- Tax_Sank %>% 
  as.data.frame() %>% 
  rownames_to_column(var="source") %>% 
  gather(key="target", value="value", -1) %>%
  filter(value != 0)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)
#Change order
nodes$name
nodes$name <- c("Species ","Genus ","Family ","Order","Class","Phylum","Kingdom","Domain","Animalia ","Plantae")


# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
plot.tax <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", fontSize = 15,
                   sinksRight=TRUE, iterations = 0)

plot.tax


############################################# ##
## 4.1.2 Alluvial diagram                   ####
## 3 levels - resolution - kingdom - group    ##

## Tidy taxonomic groups 
install.packages("ggalluvial")
library(ggalluvial)

allu_tax <- taxa %>% group_by(Kingdom) %>% 
  dplyr::count(tax_res, tax_group)

is_alluvia_form(as.data.frame(allu_tax), axes = 1:3, silent = TRUE)

## Example 1 
ggplot(as.data.frame(allu_tax),
       aes(y = n, axis1 = Kingdom, axis2 = tax_group)) +
  geom_alluvium(aes(fill = Kingdom), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Kingdom", "Group"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Taxonomic distribution of threat mapping literature")


## Example 2
plot1 <- ggplot(as.data.frame(allu_tax),
                aes(y = n, axis1 = tax_res, axis2 = tax_group)) +
  geom_alluvium(aes(fill = Kingdom), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Taxonomic Resolution", "Taxonomic Group"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Taxonomic distribution of threat mapping literature")

plot1

## Example 3
ggplot(as.data.frame(allu_tax),
       aes(y = n,
           axis1 = tax_res, axis2 = Kingdom, axis3 = tax_group)) +
  geom_alluvium(aes(fill = Kingdom),
                width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(geom = "text", width = 1/3, reverse = FALSE, alpha = .5) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_x_discrete(limits = c("Resolution", "Kingdom", "Group")) +
  ylab("Number of articles")+
  #coord_flip() +
  ggtitle("Taxonomic distribution of threat mapping literature")+
  theme_minimal()

## Still to do: ####
# 1. Re-order the groups such that plants and animals are together and it is logical
# 2. Is there a better way to present this - still looks messy 


###################################### #
## 5.0 Co-occurence matrices        ####
## 5.1 Sp Scale vs taxonomy
## 5.2 Sp scale vs threats 

## Sankey - spatial scale to threats
# 1 separate threats 

vector <- as.character(seq(from =1, to =15, by=1 ))

data$Spatial_Scale <- gsub("Multi-National", "Multi-national", data$Spatial_Scale)

## Convert data into incidence matrix of two variables ##
Sank1_dat <- data %>%  dplyr::select(ArticleID, Spatial_Scale, Threat) %>% 
  separate(Threat, vector, sep = ";", extra = "warn") %>% 
  gather(key = "Placement", value = "threat", vector ) %>% 
  na.omit()  %>% 
  dplyr::count(Spatial_Scale, threat) %>% 
  spread(Spatial_Scale, value = n, fill = NA)
  

threat_vec <- Sank1_dat$threat
Sank1_dat[,1] <-NULL
rownames(Sank1_dat) <- threat_vec

library(tibble)

## transform to connection dataframe ##
links <- Sank1_dat %>% 
  as.data.frame() %>% 
  rownames_to_column(var="source") %>% 
  gather(key="target", value="value", -1) %>%
  filter(value != 0)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)
nodes$name <- c("1.1:DevResid","1.2:DevComInd","1.3:DevRnR","1.4:DevUnspec","2.1:AgNTC", "2.2:AgWood","2.3:AgLivestock","2.4:Aqua","2.5:AgUncpec",   
                "3.1:OilGas","3.2:MinQuar","3.3:Renew","4.1:RoadRail","4.2:UtilityLine","4.3:ShipLane", "4.4:FlightPath",
                "5.1:UseAnimals","5.2:UsePlant","5.3:UseWood","5.4:UseAquatic","6.1:ActivRnR","6.2:WarConflict","6.3:Other",
                "7.1:FireSystem","7.2:WaterSystem","8.1:SDAlienInv","8.6:DUnknownC","9.1:PollDWW","9.2:PollIndMil","9.3:PollAgrFor","9.4:PollGarbSW","9.6:PollEnergy",
                "9.7:PollUnspec","11.1:CC_Habitat","11.2:CC_Drought","11.3:CC_Temp","11.4:CC_WetWeather","11.5:CC_other",
                "Global","Continental", "Multi-national","National","Sub-national","Local")

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

nodes$group <-c("1", "1","1", "1", "2", "2", "2", "2", "2", "3", "3","3","4","4","4","4","5","5","5","5",
                "6","6","6","7","7","8","8","9","9","9","9","9","9","11","11","11","11","11",
                "Scale","Scale","Scale","Scale","Scale","Scale") 
test <- c("1.1:DevResid","1.2:DevComInd","1.3:DevRnR","1.4:DevUnspec","2.1:AgNTC", "2.2:AgWood","2.3:AgLivestock","2.4:Aqua","2.5:AgUncpec",   
          "3.1:OilGas","3.2:MinQuar","3.3:Renew","4.1:RoadRail","4.2:UtilityLine","4.3:ShipLane", "4.4:FlightPath",
          "5.1:UseAnimals","5.2:UsePlant","5.3:UseWood","5.4:UseAquatic","6.1:ActivRnR","6.2:WarConflict","6.3:Other",
          "7.1:FireSystem","7.2:WaterSystem","8.1:SDAlienInv","8.6:DUnknownC","9.1:PollDWW","9.2:PollIndMil","9.3:PollAgrFor","9.4:PollGarbSW","9.6:PollEnergy",
          "9.7:PollUnspec","11.1:CC_Habitat","11.2:CC_Drought","11.3:CC_Temp","11.4:CC_WetWeather","11.5:CC_other")
test <- as.data.frame(test) %>% 
  dplyr::rename(source = test)

linkGroup <- c("1", "1","1", "1", "2", "2", "2", "2", "2", "3", "3","3","4","4","4","4","5","5","5","5",
               "6","6","6","7","7","8","8","9","9","9","9","9","9","11","11","11","11","11")

links <- links %>% right_join(test, by = "source")


my_color <- 'd3.scaleOrdinal() .domain(["1", "2", "3", "4", "5", "6", "7", "8", "9", "11", "Scale"]) .range(["cadetblue",
                                  "steelblue", "darkslategray", "darkolivegreen", "coral", "tomato", "darkgoldenrod", "indianred", "dodgerblue", "firebrick","violetred" ])'

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=TRUE, iterations = 0, fontSize = 15, colourScale = my_color, NodeGroup="group", LinkGroup = "group")

p

NodeColours <-c("cadetblue1","cadetblue2", "cadetblue3", "cadetblue4", "darkslategray1", "darkslategray2","darkslategray3", "darkslategray4", "darkslategrey",
                "darkolivegreen2","darkolivegreen3","darkolivegreen4", "darkgoldenrod1","darkgoldenrod2","darkgoldenrod3","darkgoldenrod4",
                "slateblue1", "slateblue2", "slateblue3", "slateblue4", "tomato1","tomato3","tomato4",
                "firebrick3", "firebrick4", "dodgerblue3", "dodgerblue4", "coral", "coral1", "coral2", "coral3",
                "coral4", "indianred","indianred1", "indianred2","indianred3","indianred4",
                "yellow", "springgreen", "violetred1", "turqoise2", "red1", "magenta")



## 5.3 Threats  vs taxonomy