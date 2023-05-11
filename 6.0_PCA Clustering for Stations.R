#----------------------SET UP
#load packages
library(terra)
library(sf)
library(sp)
library(tidyverse)
library(ggplot2)

#set working directory
setwd("C:/Users/casey/OneDrive - Tufts/Documents/2022 Thesis/Data/Data_Clean") 

#----------------------IMPORT DATA
#import study area with epa sld data
studyarea <- read_sf("studyarea_595.shp")
studyarea_crs <- st_crs(studyarea)
#rename SLD column names
names(studyarea)[1]<- paste("residential_density")
names(studyarea)[2]<- paste("employment_density")
names(studyarea)[3]<- paste("employment_entropy")
names(studyarea)[4]<- paste("employment_household_entropy")
names(studyarea)[5]<- paste("network_density_auto")
names(studyarea)[6]<- paste("network_density_mm")
names(studyarea)[7]<- paste("network_density_ped")
names(studyarea)[8]<- paste("intsect_density_auto")
names(studyarea)[9]<- paste("intsect_density_mm")
names(studyarea)[10]<- paste("intsect_density_ped")
names(studyarea)[11]<- paste("distance_to_transit")
names(studyarea)[12]<- paste("transit_freq")
names(studyarea)[13]<- paste("job_access_auto")
names(studyarea)[14]<- paste("job_access_transit")

#import all trips sorted by station ID
df = read_csv('trips_sorted_1000.csv')

#import station locations
stations = read_sf("stations_sld.shp")
plot(st_geometry(studyarea)) 
plot(st_geometry(stations), add=T)
stations_crs <- st_crs(stations)

#rename station column names
names(stations)[2]<- paste("residential_density")
names(stations)[3]<- paste("employment_density")
names(stations)[4]<- paste("employment_entropy")
names(stations)[5]<- paste("employment_household_entropy")
names(stations)[6]<- paste("network_density_auto")
names(stations)[7]<- paste("network_density_mm")
names(stations)[8]<- paste("network_density_ped")
names(stations)[9]<- paste("intsect_density_auto")
names(stations)[10]<- paste("intsect_density_mm")
names(stations)[11]<- paste("intsect_density_ped")
names(stations)[12]<- paste("distance_to_transit")
names(stations)[13]<- paste("transit_freq")
names(stations)[14]<- paste("job_access_auto")
names(stations)[15]<- paste("job_access_transit")

#check projection of spatial data
stations_crs == studyarea_crs #FALSE
stations_pr <- stations %>% st_transform(studyarea_crs)
stations_crs == studyarea_crs #TRUE

#attribute join trips data to stations sf object
stations_wtrips <- merge(stations_pr, df, by='station_id', how='left')

#----------------------MAP VARIABLES
# library(RColorBrewer)
# library(tmap)
# my.palette1 <- brewer.pal(n = 7, name = "RdYlBu")
# 
# map10 <- tm_shape(studyarea) +tm_fill(col="job_access_auto", palette=my.palette1, title="Job Access by Auto")+tm_borders() + 
#   tm_shape(stations_wtrips) +tm_bubbles("arrival_trip_parity", col = "grey30", alpha=.5, border.col = "black", border.alpha = .5, sizes.legend=seq(0, 1, by=.2), scale=.7) +tm_layout(legend.outside=T)
# map10
# map11 <- tm_shape(studyarea) +tm_fill(col="network_density_ped", palette=my.palette2, title="Pedestrian Network Density")+tm_borders() + 
#   tm_shape(stations) +tm_bubbles("arrival_trip_parity", col = "grey30", alpha=.5, border.col = "black", border.alpha = .5, sizes.legend=seq(0, 1, by=.2), scale=.7) +tm_layout(legend.outside=T)
# map11
# map12 <- tm_shape(studyarea) +tm_fill(col="employment_household_entropy", palette=my.palette2, title="Employment and Household Entropy")+tm_borders() + 
#   tm_shape(stations) +tm_bubbles("arrival_trip_parity", col = "grey30", alpha=.5, border.col = "black", border.alpha = .5, sizes.legend=seq(0, 1, by=.2), scale=.7) +tm_layout(legend.outside=T)
# map12

# #scatter plot matrix 
# x <- ggpairs(stations_df, columns = c(2:15,19:22), 
#         lower = list(continuous = "smooth"), 
#         title = "Scatter Plot Matrix", axisLabels = "show")
# 
# svg("myPlotMatrix.svg", height = 20, width = 40)
# print(x)
# dev.off()
# 
# #parallel plot 
# library(GGally)
# ggparcoord(data= stations, 
#            columns = 13:26, 
#            groupColumn = "arrival_trip_parity", 
#            order = "allClass", 
#            scale = "std", 
#            title="Parallel coordinate plot of station attributes grouped by parity rate", 
#            mapping = ggplot2::aes(size = 1)) +  
#   ggplot2::scale_size_identity() + xlab("") + ylab("Standardized value")
# 
stations_df <- data.frame(select_if(stations, is.numeric))
stations_df1 <- stations_df %>% mutate_all(function(x) as.numeric(as.character(x)))

#summary statistics
library(stargazer)
#text file saved to table1.txt 
stargazer(stations_df1,type = "html", title="Descriptive Statistics", digits=1, out="stations_sld_descriptive_stats.html")

#checking correlations to see if we need to use PCA and clustering
library(psych)
describe(stations_df1, na.rm = TRUE, ranges = TRUE)
corr_matrix = cor(stations_df1, use="pairwise.complete.obs")
# See intercorrelations
round(corr_matrix, 2)

#Scaled to make the units of variables comparable
stations_scaled <- scale(stations_df1)

# Compute dissimilarity matrix "euclidean distances" 
stations_dist <- dist(stations_scaled, method = "euclidean")

# Hierarchical clustering using Ward's method
stations_hc <- hclust(stations_dist, method = "ward.D2" )
plot(stations_hc)

# Cut tree into 3 groups and plot
grp <- cutree(stations_hc, k = 3)
rect.hclust(stations_hc, k = 3, border = "red") 


######################################################################
# Clustering Section 2.2 visualizing and statistics
#####################################################################

#cut the tree into the number of suggested groups
stations_wardgrp <- cutree(stations_hc, k = 3)
plot(stations_hc, cex = 0.6) # plot tree
# add rectangles for each group
rect.hclust(stations_hc, k = 3, border = "blue") 

#how many stations in each cluster
table(stations_wardgrp)

#join this to the rest of the atlanta data assumes dplyr library
stations_cl <- mutate(stations, cluster = stations_wardgrp )
table(stations_cl$cluster)

studyarea %>% as.data.frame() %>% stargazer(type = "html", digits=1, out="studyarea_sld.html", title='Descriptive Statistics of Study Area')
stations_cl[1:15] %>% as.data.frame() %>% stargazer(type = "html", digits=1, out="stations_sld.html", title='Descriptive Statistics of Built Environment of Stations')

cluster_id <- stations_cl %>% subset(select=c(17))
stations_cl <- stations_wtrips %>% st_join(cluster_id)
stations_cl_rd <- data.frame(select_if(stations_cl, is.numeric)) %>% subset(select=c(1,16,19,24)) %>% mutate_all(function(x) as.numeric(as.character(x)))

library(ggplot2)
library(plotly)
library(gapminder)

p <- stations_cl_rd %>%
  # filter(cluster==2) %>%
  ggplot( aes(arrivals, arrival_trip_parity, color=cluster)) +
  geom_point() +
  theme_bw()

ggplotly(p)

plot(x=stations_cl_rd$arrivals, y=stations_cl_rd$arrival_trip_parity, type='p', log='x')

stations_cl_df <- data.frame(stations_cl) 

stations_cl %>% group_by(cluster)

# using tmap to make this interactive to see the neighborhood names
# ignore projection warning
#change basemap to openstreetmap

library(tmap)
map_cluster <-tm_shape(stations_cl)+ tm_bubbles(col="cluster", scale=.5)
map_cluster

#parallel plot for clusters
# ggparcoord(data= studyarea_cl, 
#            columns = 1:13,
#            groupColumn = "cluster", 
#            order = "allClass", 
#            scale = "std", 
#            title="Parallel coordinate plot of clustered block groups", 
#            mapping = ggplot2::aes(size = 1)) +  
#   ggplot2::scale_size_identity() + xlab("") + ylab("Standardized value")


#use this if plots don't appear separately
# dev.off()

#calculate statistics
# use wb.ratio average.within/average.between (should be small)
#average.between: average distance between clusters as large as possible
#average.within: average distance within clusters as small as possible
#Dunn index should be maximized
library(fpc)
ward3 <- cutree(hclust(stations_dist, method = "ward.D2"), k=3)
complete3 <- cutree(hclust(stations_dist, method = "complete"), k=3)
ward3_stat <- cluster.stats(stations_dist, ward3)
complete3_stat <- cluster.stats(stations_dist, complete3)
ward3_stat$wb.ratio
complete3_stat$wb.ratio
ward3_stat$dunn
complete3_stat$dunn
ward3_stat$average.between
complete3_stat$average.between
ward3_stat$average.within
complete3_stat$average.within
#suggests that complete clustering method is similar to ward clusters

# to get summary statistics for each variable in every ward cluster
stations_clusterswithVars <- mutate(stations_wtrips, cluster = stations_wardgrp) %>% na.exclude()
aggregate(stations_clusterswithVars, list(stations_clusterswithVars$cluster), median)

######################################################################
#  PCA Section 3.0 without rotation 
# 
#####################################################################
studyarea_df2 <- studyarea_df1[1:14]
stations_num <- st_drop_geometry(stations[2:15])

#without rotation pca
pca <- principal(stations_num, rotate="none", nfactors=6, scores=TRUE)
#shows the eigenvalues 
# keep those over 1 or close to 1 as useful components
pca$values 
#components 1-5 are above 1, PC6 is close with 0.992

#communality closer to 1 means variable is better explained by the components
pca$communality

#screeplot suggests keep 5 components 
VSS.scree(cor(stations_num), main = "scree plot")

##loadings show the correlation of the variable to the component
#high values suggest higher correlation
#can be negative or positive
pca$loadings
#first four components explain 60%
#first six explain 75.6%

#save the scores for each location to the sf object
stations_pca <- cbind(stations, pca$scores)

#Mapping the first three components 
library(tmap)
map_pc1 <- tm_shape(studyarea_pca) +tm_fill(col="PC1", palette=my.palette1, title="PC1")+tm_borders()
map_pc1 
map_pc2 <- tm_shape(studyarea_pca) +tm_fill(col="PC2", palette=my.palette1,  alpha=0.7, title="PC2")+tm_borders()
map_pc2
map_pc3 <- tm_shape(studyarea_pca) +tm_fill(col="PC3", palette=my.palette1,  alpha=0.7, title="PC3")+tm_borders()
map_pc3
map_pc4 <- tm_shape(studyarea_pca) +tm_fill(col="PC4", palette=my.palette1,  alpha=0.7, title="PC4")+tm_borders()
map_pc4
map_pc5 <- tm_shape(studyarea_pca) +tm_fill(col="PC5", palette=my.palette1, alpha=0.7, title="PC5")+tm_borders()
map_pc5
# #To makmap_pc3 <- tm_shape(studyarea_pca) +tm_fill(col="PC3", palette=my.palette1, title="PC3")+tm_borders()
# map_pc3e maps in Geoda or QGIS or ArcGIS
# #st_wrimap_pc3 <- tm_shape(studyarea_pca) +tm_fill(col="PC3", palette=my.palette1, title="PC3")+tm_borders()
# map_pc3te(atlanta_pca, "final_Atlanta.shp", delete_layer = TRUE) 


######################################################################
#  Section 3.1 with rotation 
# 
#####################################################################

pca_varimax <- principal(stations_num, rotate="varimax", nfactors=6, scores=TRUE)
pca_varimax$values
pca_varimax$loadings
#first six components explain 75.6% variance

stations_pca_rot <- cbind(stations, pca_varimax$scores)

my.palette1 <- brewer.pal(n = 7, name = "OrRd")

#Mapping the first three components with eigenvalues greater than 1 
library(tmap)
map_rc1 <- tm_shape(studyarea) +tm_fill(palette=my.palette1)+tm_shape(stations_pca_rot) +tm_bubbles(col="RC1", size=.25, style='jenks') +tm_layout(legend.outside=T, title='Rotated PC1 for Stations')
map_rc1 #job access by transit, density  (explaining 22% of variance)
map_rc2 <- tm_shape(studyarea) +tm_fill(palette=my.palette1)+tm_shape(stations_pca_rot) +tm_bubbles(col="RC2", size=.25, style='jenks') +tm_layout(legend.outside=T, title='Rotated PC2 for Stations')
map_rc2 #pedestrian-oriented for network and intersection; sidewalks! parks! 
map_rc3 <- tm_shape(studyarea) +tm_fill(palette=my.palette1)+tm_shape(stations_pca_rot) +tm_bubbles(col="RC3", size=.25, style='jenks') +tm_layout(legend.outside=T, title='Rotated PC3 for Stations')
map_rc3 #less auto-oriented, less land use mix, walkable & residential 
map_rc4 <- tm_shape(studyarea) +tm_fill(palette=my.palette1)+tm_shape(stations_pca_rot) +tm_bubbles(col="RC4", size=.25, style='jenks') +tm_layout(legend.outside=T, title='Rotated PC4 for Stations')
map_rc4 #job access by transit, density  (explaining 22% of variance)
map_rc5 <- tm_shape(studyarea) +tm_fill(palette=my.palette1)+tm_shape(stations_pca_rot) +tm_bubbles(col="RC5", size=.25, style='jenks') +tm_layout(legend.outside=T, title='Rotated PC5 for Stations')
map_rc5 #pedestrian-oriented for network and intersection; sidewalks! parks! 
map_rc6 <- tm_shape(studyarea) +tm_fill(palette=my.palette1)+tm_shape(stations_pca_rot) +tm_bubbles(col="RC6", size=.25, style='jenks') +tm_layout(legend.outside=T, title='Rotated PC6 for Stations')
map_rc6 #less auto-oriented, less land use mix, walkable & residential 


tmap_arrange(map_rc1, map_rc2, map_rc3, map_rc4, map_rc5, map_rc6)

#to save to a file
tmap_save(map_rc1, filename = "../Figures/map_rc1.png")
tmap_save(map_rc2, filename = "../Figures/map_rc2.png")
tmap_save(map_rc3, filename = "../Figures/map_rc3.png")
tmap_save(map_rc4, filename = "../Figures/map_rc4.png")


############################################
# 4 (Optional) Regression predicting HIV with 
#  components as explanatory variables
############################################
stations_wtrips_pca_rot <- stations_pca_rot %>% left_join(df, by='station_id')

stations_rot_lm_parity <- lm(arrival_trip_parity ~ RC1 + RC2 + RC3 +RC4 +RC5 +RC6, data=stations_wtrips_pca_rot)
summary(stations_rot_lm_parity)

stations_rot_lm_female <- lm(female_arrivals ~ RC1 + RC2 + RC3 +RC4+RC5 +RC6, data=stations_wtrips_pca_rot)
summary(stations_rot_lm_female)

stations_rot_lm_male <- lm(male_arrivals ~ RC1 + RC2 + RC3 +RC4+RC5 +RC6, data=stations_wtrips_pca_rot)
summary(stations_rot_lm_male)

stargazer(stations_rot_lm_parity, stations_rot_lm_female, stations_rot_lm_male, type = "html", digits=1, out="pca_rot.html", title='PCA Rotation Regression Results')

#Koenker-Bassett 
library(lmtest)
bptest(stations_rot_lm_female)
#Breusch-Pagan Test
bptest(stations_OLS, studentize=FALSE)
#VIF 
#higher than 2 indicates multicollinearity
library(car)
vif(stations_rot_lm_female)
#Condition number 
library(mctest)
omcdiag(stations_rot_lm_female)
#most results suggest no multicollinearity issues

