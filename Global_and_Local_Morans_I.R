#----------------------SET UP
#load packages
#for reading in shapefile
library(sf)
#for weight matrices, local moran and bivariate local moran
library(rgeoda)
#for global moran and weight matrices
library(spdep)

#setwd to thesis/data/r folder
setwd("C:/Users/casey/OneDrive - Tufts/Documents/2022 Thesis/Data/R") 

#----------------------IMPORT DATA
stations <- read_sf("../Data_clean/stations_1000.shp")
studyarea <- read_sf("../Data_clean/studyarea_595.shp")
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

#convert the sf to sp for stargazer to work 
stations_pr <- st_transform(stations, studyarea_crs)
stations_sp <- as_Spatial(stations_pr)
stations_df <- stations_sp@data
st_crs(stations_pr) == st_crs(studyarea)
plot(st_geometry(studyarea))

#Creating weight matrices in spdep
#Queens case first order
studyarea_nbq1 <- poly2nb(studyarea, queen=TRUE)
studyarea_nbq1

# rooks case first order
studyarea_nbr1 <- poly2nb(studyarea, queen=FALSE)
studyarea_nbr1

#plotting the graph
#convert  mass sf to sp Spatial object for coordinates function to work 
studyarea_sp <- as(studyarea, "Spatial")
class(studyarea_sp)
plot(studyarea_sp, border="grey60")
plot(studyarea_nbr1, coordinates(studyarea_sp), add=TRUE, pch=19, cex=0.6)

#generating higher order queens 
studyarea_nbq <- nblag(studyarea_nbq1, 3)
#summary of orders 1, 2 and 3rd order
summary(studyarea_nbq[[1]])
summary(studyarea_nbq[[2]])
summary(studyarea_nbq[[3]])

#saving the 2nd and 3rd order queens cases
studyarea_nbq2 <- studyarea_nbq[[2]]
studyarea_nbq3 <- studyarea_nbq[[3]]

#to plot higher order 
plot(studyarea_sp)
plot(studyarea_nbq2, coordinates(studyarea_sp), add=TRUE, pch=19, cex=0.6)

# weight matrix based on 1 2 and 4 nearest neighbors 
#generate coordinates for centroid
studyarea_coords <- coordinates(studyarea_sp)
#generating ID for each block group
studyareaIDs <- row.names(as(studyarea_sp, "data.frame")) 

#generate nearest 1 2 and 4 neighbors 
studyarea_nn1 <- knn2nb(knearneigh(studyarea_coords, k=1), row.names= studyareaIDs)
studyarea_nn2 <- knn2nb(knearneigh(studyarea_coords, k=2), row.names= studyareaIDs)
studyarea_nn4 <- knn2nb(knearneigh(studyarea_coords, k=4), row.names= studyareaIDs)

summary(studyarea_nn1)
summary(studyarea_nn2)
summary(studyarea_nn4)

#distance based neighbors 
#generate distances between centroids for the nearest neighbors
studyarea_dist <-unlist(nbdists(studyarea_nn1, studyarea_coords))
#shows the min mean and max distance between centroids
summary(studyarea_dist)
#distance threshold to ensure all towns have a neighbor
dist_threshold <-max(studyarea_dist) 

#generating distance based neighbors 
studyarea_distn_lessthan_threshold <-dnearneigh(studyarea_coords, d1=0, d2=0.75*dist_threshold, row.names= studyareaIDs)
studyarea_distn_threshold <-dnearneigh(studyarea_coords, d1=0, d2=1*dist_threshold, row.names= studyareaIDs)
studyarea_distn_morethan_threshold<-dnearneigh(studyarea_coords, d1=0, d2=1.5*dist_threshold, row.names= studyareaIDs) 
studyarea_distn_20km <-dnearneigh(studyarea_coords, d1=0, d2=20000, row.names= studyareaIDs) 

#as distance increases each town has more neighbors
summary(studyarea_distn_lessthan_threshold)
summary(studyarea_distn_threshold)
summary(studyarea_distn_morethan_threshold)
summary(studyarea_distn_20km)

#changing it to a weight matrix format that can be used for global moran's I calculations

W_sa_nbqueen_rowstd  <- nb2listw(studyarea_nbq1, zero.policy=TRUE)
W_sa_nbrook_rowstd <- nb2listw(studyarea_nbr1, zero.policy=TRUE)
W_sa_nbqueen2_rowstd  <- nb2listw(studyarea_nbq2, zero.policy=TRUE)
W_sa_nbqueen3_rowstd  <- nb2listw(studyarea_nbq3, zero.policy=TRUE)
W_sa_kn4_rowstd  <- nb2listw(studyarea_nn4, zero.policy=TRUE)
W_sa_d1_rowstd  <- nb2listw(studyarea_distn_threshold, zero.policy=TRUE)
W_sa_d20Km_rowstd  <- nb2listw(studyarea_distn_20km, zero.policy=TRUE)
W_sa_d3_rowstd  <- nb2listw(studyarea_distn_morethan_threshold, zero.policy=TRUE)

#Global Moran's I for different weight matrices 
moran.test(studyarea$residential_density, W_sa_nbqueen_rowstd, zero.policy=T)
moran.test(studyarea$residential_density, W_sa_nbrook_rowstd, zero.policy=T)
moran.test(studyarea$residential_density, W_sa_nbqueen2_rowstd, zero.policy=T)
moran.test(studyarea$residential_density, W_sa_nbqueen3_rowstd, zero.policy=T)

moran.test(studyarea$residential_density, W_sa_kn4_rowstd, zero.policy=T)
moran.test(studyarea$residential_density, W_sa_d1_rowstd, zero.policy=T)
moran.test(studyarea$residential_density, W_sa_d20Km_rowstd, zero.policy=T)
moran.test(studyarea$residential_density, W_sa_d3_rowstd, zero.policy=T)

#using MC randomization to test for significance
moran.mc(studyarea$residential_density, W_sa_nbqueen_rowstd, nsim=499, zero.policy=T)
moran.mc(studyarea$residential_density, W_sa_nbqueen2_rowstd, nsim=499, zero.policy=T)
moran.mc(studyarea$residential_density, W_sa_d1_rowstd, nsim=499, zero.policy=T)
moran.mc(studyarea$residential_density, W_sa_kn4_rowstd, nsim=499, zero.policy=T)

#plotting the Moran's I 
options(scipen=999)
moran.plot(studyarea$residential_density, W_sa_nbqueen_rowstd, zero.policy=T, xlab="Residential Density", ylab="Lagged Residential Density")

###################################will not work in the data lab below this############################
library(spdep)
#bivariate global moran's I 
biv_studyarea <- moran_bv(studyarea$job_access_transit, studyarea$employment_density, W_sa_nbqueen_rowstd, nsim=499)
#value suggests very little correlation between income in a town and surrounding onsite releases
biv_studyarea$t0
#its also not significant 
plot(biv_studyarea)

biv_table <- biv_studyarea$data
plot(biv_table$x, biv_table$y, main="Scatterplot of residential density and employment density",
     xlab="Residential Density", ylab="Lag of Employment Density", pch=19)
abline(lm(biv_table$y~biv_table$x), col="red") # regression line (y~x)
lines(lowess(biv_table$x,biv_table$y), col="blue") # lowess line (x,y)

########################SECTION 2.3 #############################
#Local Moran's I in rgeoda 

#create weight matrix in rgeoda 
#Queen and rooks weight matrix
studyarea_queen  <- queen_weights(studyarea)
summary(studyarea_queen)

studyarea_rook   <-rook_weights(studyarea)
summary(studyarea_rook)

#checking if there are islands
has_isolates(studyarea_queen)

#higher order weights matrix using the order parameter
studyarea_queen3rd  <- queen_weights(studyarea, order=3,include_lower_order=TRUE, precision_threshold = 0)

#notice that you have more neighbors when you use the 2nd order 
summary(studyarea_queen)
summary(studyarea_queen3rd)

#-------------------------------------------- CALCULATE LAG VARIABLES 
# Calculating the lag of network density mm for each block group
lag_networkm <- spatial_lag(studyarea_queen, studyarea["network_density_mm"])
lag_networkm
#joining it back to mass
studyarea <- cbind(studyarea, "Lag_networkm_1" = lag_networkm[, 1])

#plotting the variable and its lag together 
#suggests clustering since neighbors values are correlated with values at the location
plot(studyarea$network_density_mm,studyarea$Lag_networkm_1)
abline(lm(Lag_networkm_1 ~ network_density_mm, data = studyarea), col = "red")

#----
# Calculating the lag of total job access by auto for each block group
lag_jobacsa <- spatial_lag(studyarea_queen, studyarea["job_access_auto"])
lag_jobacsa 
#joining it back to mass
studyarea <- cbind(studyarea, "Lag_jobacsa_1" = lag_jobacsa[, 1])

#plotting the variable and its lag together 
#suggests clustering since neighbors values are correlated with values at the location
plot(studyarea$job_access_auto,studyarea$Lag_jobacsa_1)
abline(lm(Lag_jobacsa_1 ~ job_access_auto, data = studyarea), col = "red")

# distance based weights 

#function min_dist finds an optimized distance threshold 
#that guarantees that every observation has at least one neighbor
studyarea_dist_thres <- min_distthreshold(studyarea)
studyarea_dist_thres  
studyarea_distw1 <- distance_weights(studyarea, studyarea_dist_thres, power=1.0,  is_inverse=FALSE, is_arc=FALSE)
#note that there are no isolates here because we ensured that all the towns had a neighbor
summary(studyarea_distw1)

#setting the threshold lower than distance threshold
studyarea_distw2 <- distance_weights(studyarea, 20000, power=1.0,  is_inverse=FALSE, is_arc=FALSE)
summary(studyarea_distw2)

#setting the threshold higher than distance threshold
studyarea_distw3 <- distance_weights(studyarea, 50000, power=1.0,  is_inverse=FALSE, is_arc=FALSE)
summary(studyarea_distw3)


#k nearest neighbors nearest 1, 2, and 4 neighbors
studyarea_knn1 <- knn_weights(studyarea, 1, power = 1.0,is_inverse = FALSE, is_arc = FALSE)
studyarea_knn2 <- knn_weights(studyarea, 2, power = 1.0,is_inverse = FALSE, is_arc = FALSE)
studyarea_knn4 <- knn_weights(studyarea, 4, power = 1.0,is_inverse = FALSE, is_arc = FALSE)

summary(studyarea_knn1)
summary(studyarea_knn2)
summary(studyarea_knn4)

#Local Moran's I for JOB ACCESS BY TRANSIT using queen 1st order 
jobacst = studyarea["job_access_transit"]
lisa_jobacst <- rgeoda::local_moran(studyarea_queen,jobacst)
summary(lisa_jobacst)

lisa_colors <- lisa_colors(lisa_jobacst)
lisa_labels <- lisa_labels(lisa_jobacst)
lisa_clusters <- lisa_clusters(lisa_jobacst)
plot(st_geometry(studyarea), 
     col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#333333", lwd=0.1)
title(main = "Local Moran Map of Job Access by Transit (Q1)")
legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")

#----------------------MAP VARIABLES
library(RColorBrewer)
library(tmap)
my.palette1 <- brewer.pal(n = 7, name = "PuBu")

map1 <- tm_shape(studyarea) +tm_fill(col="job_access_transit", palette=my.palette1, style='fisher', title="Job Access by Transit")+tm_borders() + 
  tm_shape(stations_1000) +tm_bubbles(alpha=.5, border.col = "black", border.alpha = .5, scale=.5) +tm_layout(legend.outside=T)
map1
map2 <- tm_shape(studyarea) +tm_fill(col="employment_density", palette=my.palette1, style='fisher', title="Employment Density")+tm_borders() + 
  tm_shape(stations_1000) +tm_bubbles(alpha=.5, border.col = "black", border.alpha = .5, scale=.5) +tm_layout(legend.outside=T)
map2

map2 <- tm_shape(studyarea) +tm_fill(col="residential_density", palette=my.palette1, style='fisher', title="Residential Density")+tm_borders() + 
  tm_shape(stations_1000) +tm_bubbles(alpha=.5, border.col = "black", border.alpha = .5, scale=.5) +tm_layout(legend.outside=T)
map3

map4 <- tm_shape(studyarea) +tm_fill(col="intsect_density_ped", palette=my.palette1, style='fisher', title="Ped. Intersection Density")+tm_borders() + 
  tm_shape(stations_1000) +tm_bubbles(alpha=.5, border.col = "black", border.alpha = .5, scale=.5) +tm_layout(legend.outside=T)
map4

map5 <- tm_shape(studyarea) +tm_fill(col="network_density_ped", palette=my.palette1, style='fisher', title="Ped. Network Density")+tm_borders() + 
  tm_shape(stations_1000) +tm_bubbles(alpha=.5, border.col = "black", border.alpha = .5, scale=.5) +tm_layout(legend.outside=T)
map5

map6 <- tm_shape(studyarea) +tm_fill(col="network_density_mm", palette=my.palette1, style='fisher', title="MM. Network Density")+tm_borders() + 
  tm_shape(stations_1000) +tm_bubbles(alpha=.5, border.col = "black", border.alpha = .5, scale=.5) +tm_layout(legend.outside=T)
map6

map7 <- tm_shape(studyarea) +tm_fill(col="intsect_density_mm", palette=my.palette1, style='fisher', title="MM. Intersection Density")+tm_borders() + 
  tm_shape(stations_1000) +tm_bubbles(alpha=.5, border.col = "black", border.alpha = .5, scale=.5) +tm_layout(legend.outside=T)
map7


#Local Moran's I for NETWORK DENSITY using queen 1st order 
mm_network = studyarea["network_density_mm"]
lisa_networkm <- rgeoda::local_moran(studyarea_queen,mm_network)
summary(lisa_networkm)

lisa_colors <- lisa_colors(lisa_networkm)
lisa_labels <- lisa_labels(lisa_networkm)
lisa_clusters <- lisa_clusters(lisa_networkm)
plot(st_geometry(studyarea), 
     col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#333333", lwd=0.1)
title(main = "Local Moran Map of Multimodal Network Density (Q1)")
legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")


#Local Moran's I for TOTAL EMPLOYMENT using queen 1st order 
ped_network = studyarea["job_access_auto"]
lisa_networkp <- rgeoda::local_moran(studyarea_queen,ped_network)
summary(lisa_networkp)

lisa_colors <- lisa_colors(lisa_networkp)
lisa_labels <- lisa_labels(lisa_networkp)
lisa_clusters <- lisa_clusters(lisa_networkp)
plot(st_geometry(studyarea), 
     col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#333333", lwd=0.1)
title(main = "Local Moran Map of Job Access by Auto (Q1)")
legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")


