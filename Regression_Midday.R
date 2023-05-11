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

#import station locations
stations = read_sf("stations_sld.shp")
plot(st_geometry(stations))
stations_crs <- st_crs(stations)
# stations<- stations_sld #above won't load, so using saved data from sld processing R script

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

#import all trips sorted by station ID
df = read_csv('midday_sorted.csv')

#attribute join trips data to stations sf object
stations_wtrips <- merge(stations_pr, df, by.x='station_id', by.y='station.id', how='left')

#make spatial object 
stations_sp <- as_Spatial(stations_wtrips)
#make df for stargazer 
stations_df <- stations_sp@data

#text file saved to table1.txt
library(stargazer)
stargazer(stations_df,type = "text", title="Descriptive statistics for Midday Trips", digits=1, out="tablefinal.txt")

#map trip arrivals and departures
library(RColorBrewer)

pal <- brewer.pal(7, "OrRd") # we select 7 colors from the palette
plot(st_geometry(blockgroups1))
plot(df1['arrivals'], add = T,
     breaks = "jenks", nbreaks = 7,
     pal = pal, 
     pch = 16)

plot(st_geometry(blockgroups1))
plot(df1['departures'], add = T,
     breaks = "jenks", nbreaks = 7,
     pal = pal, 
     pch = 16)

#----------------------ANALYSIS
library(spdep)

#----NEAREST NEIGHBORS 
#generate coordinates for centroid
stations_coords <- coordinates(stations_sp)
#generating ID for each tract
stationsIDs <- row.names(as(stations_sp, "data.frame")) 
#generate nearest 4 neighbors 
stations_nn4 <- knn2nb(knearneigh(stations_coords, k=4), row.names= stationsIDs)
#generate nearest 2 neighbors
stations_nn2 <- knn2nb(knearneigh(stations_coords, k=2), row.names= stationsIDs)

##distance based neighbors 
#generate distances between centroids for the nearest neighbors
stations_nn1 <- knn2nb(knearneigh(stations_coords, k=1), row.names=stationsIDs)
stations_dist <-unlist(nbdists(stations_nn1, stations_coords))
#shows the min mean and max distance between centroids
summary(stations_dist)
#distance threshold to ensure all towns have a neighbor 22.771km
dist_threshold <-max(stations_dist) 

#generating distance based neighbors 
stations_dist_threshold <-dnearneigh(stations_coords, d1=0, d2=1*dist_threshold, row.names= stationsIDs)
summary(stations_dist_threshold)

#histogram of neighbors
stations_dist_card <- card(stations_dist_threshold)
hist(stations_dist_card, breaks=30)

#changing it to a weight matrix format that can be used for global moran's I calculations
W_kn4_rowstd  <- nb2listw(stations_nn4, zero.policy=TRUE)
W_kn2_rowstd <- nb2listw(stations_nn2, zero.policy=T)
W_dist_rowstd  <- nb2listw(stations_dist_threshold, zero.policy=TRUE)

#----------------------LINEAR REGRESSION for trip parity
arrival_OLS <- lm(arrival_trip_parity ~  residential_density +employment_entropy + employment_density + employment_household_entropy + network_density_auto + network_density_mm + network_density_ped + intsect_density_auto + intsect_density_mm + intsect_density_ped + distance_to_transit + transit_freq + job_access_auto + job_access_transit, data=stations_df)
summary(arrival_OLS)

#update regression with significant variables only
small_OLS <- lm(arrival_trip_parity ~ employment_entropy + employment_density + employment_household_entropy + job_access_auto, data=stations_sp)
summary(small_OLS)
#results: employment_density***, job_access_auto*** are very significant

#----------------------DIAGNOSTICS
library(lmtest)
#Koenker-Bassett 
bptest(small_OLS)
#Breusch-Pagan Test
bptest(small_OLS, studentize=FALSE)
#VIF 
#higher than 2 indicates multicollinearity
library(car)
vif(small_OLS)
#Condition number 
library(mctest)
omcdiag(small_OLS)

# saving residuals and fitted values
stations_sp$olsresid <-residuals(small_OLS)
stations_sp$ols_fitted <- fitted(small_OLS)

# Moran test for residuals with second order Queens
lm.morantest(small_OLS, W_kn4_rowstd, zero.policy=T)
lm.morantest(small_OLS, W_kn2_rowstd, zero.policy=T)
# positive Morans I with significant p-value indicates non-random clustering

lisaRslt <- spdep::localmoran(stations_sp$arrival_trip_parity, W_kn4_rowstd, zero.policy = TRUE, na.action = na.omit)
dim(lisaRslt); dim(stations_sp);

# Lagrange multiplier tests for residuals for 4 neighbors
lm.LMtests(small_OLS, W_kn4_rowstd, test="all", zero.policy=T)
# RLMlag = 10.18, df = 1, p-value = 0.00142

#----------------------SPATIAL LAG MODEL
#for 4 neighbors
stations_spatial_lag <- spatialreg::lagsarlm(arrivals ~ employment_entropy + employment_density + employment_household_entropy + job_access_auto, data=stations_sp, W_kn4_rowstd, zero.policy=T)
summary(stations_spatial_lag)

# saving residuals and fitted values
stations_sp$splag_resid<-residuals(stations_spatial_lag)
stations_sp$splag_fitted <- fitted(stations_spatial_lag)


#----------------------LINEAR REGRESSION 2 for female trips
arrival_OLS <- lm(female_arrivals ~  residential_density + employment_entropy + employment_density + employment_household_entropy + network_density_auto + network_density_mm + network_density_ped + intsect_density_auto + intsect_density_mm + intsect_density_ped + distance_to_transit + transit_freq + job_access_auto + job_access_transit, data=stations_df)
summary(arrival_OLS)

#update regression with significant variables only
small_OLS <- lm(female_arrivals ~ network_density_ped + job_access_transit, data=stations_sp)
summary(small_OLS)
#results: network_density_ped* and job_access_transit*** are very significant
#----------------------DIAGNOSTICS
library(lmtest)
#Koenker-Bassett 
bptest(small_OLS)
#Breusch-Pagan Test
bptest(small_OLS, studentize=FALSE)
#VIF 
#higher than 2 indicates multicollinearity
library(car)
vif(small_OLS)
#Condition number 
library(mctest)
omcdiag(small_OLS)

# saving residuals and fitted values
stations_sp$olsresid <-residuals(small_OLS)
stations_sp$ols_fitted <- fitted(small_OLS)

# Moran test for residuals with second order Queens
lm.morantest(small_OLS, W_kn4_rowstd, zero.policy=T)
lm.morantest(small_OLS, W_kn2_rowstd, zero.policy=T)
# positive Morans I with significant p-value indicates non-random clustering

lisaRslt <- spdep::localmoran(stations_sp$female_arrivals, W_kn4_rowstd, zero.policy = TRUE, na.action = na.omit)
dim(lisaRslt); dim(stations_sp);

# Lagrange multiplier tests for residuals for 4 neighbors
lm.LMtests(small_OLS, W_kn4_rowstd, test="all", zero.policy=T)
# RLMlag = 12.295, df = 1, p-value = 0.0004541

#----------------------SPATIAL LAG MODEL for female trips
#for 4 neighbors
stations_spatial_lag <- spatialreg::lagsarlm(female_arrivals ~ distance_to_transit+ job_access_transit, data=stations_sp, W_kn4_rowstd, zero.policy=T)
summary(stations_spatial_lag)
#results: network_density_mm, network_density_ped are significant < 0.05

# saving residuals and fitted values
stations_sp$splag_resid<-residuals(stations_spatial_lag)
stations_sp$splag_fitted <- fitted(stations_spatial_lag)

#----------------------MAPPING RESIDUAL RESULTS
stations_sf = st_as_sf(stations_sp)

#use color brewer
library(RColorBrewer)
pal <- brewer.pal(7, "OrRd") # we select 7 colors from the 
plot(st_geometry(blockgroups1))
plot(stations_sf["splag_resid"],
     main='Residuals from spatial lag', 
     breaks="jenks",
     nbreaks = 7,
     col = pal, 
     add=T)

#----------------------LOCAL MORAN I
lisaRslt_splag <- spdep::localmoran(stations_sp$splag_resid, W_kn4_rowstd, zero.policy = TRUE, na.action = na.omit)
dim(lisaRslt_splag); dim(stations_sp);

# Now we can derive the cluster/outlier types (COType in ArcGIS term) for each spatial feature in the data
significanceLevel <- 0.05; # 95% confidence
meanVal <- mean(stations_sp$arrival_trip_parity);

lisaRslt_splag %<>% tibble::as_tibble() %>%
  magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr(z > 0)")) %>%
  dplyr::mutate(coType = dplyr::case_when(
    `Pr(z > 0)` > 0.05 ~ "Insignificant",
    `Pr(z > 0)` <= 0.05 & Ii >= 0 & stations_sp$arrival_trip_parity >= meanVal ~ "HH",
    `Pr(z > 0)` <= 0.05 & Ii >= 0 & stations_sp$arrival_trip_parity < meanVal ~ "LL",
    `Pr(z > 0)` <= 0.05 & Ii < 0 & stations_sp$arrival_trip_parity >= meanVal ~ "HL",
    `Pr(z > 0)` <= 0.05 & Ii < 0 & stations_sp$arrival_trip_parity < meanVal ~ "LH"
  ))

# Now add this coType to original sf data
stations_sp$coType <- lisaRslt_splag$coType %>% tidyr::replace_na("Insignificant")

#----------------------LINEAR REGRESSION 3 for male trips
arrival_OLS <- lm(male_arrivals ~  residential_density + employment_entropy + employment_density + employment_household_entropy + network_density_auto + network_density_mm + network_density_ped + intsect_density_auto + intsect_density_mm + intsect_density_ped + distance_to_transit + transit_freq + job_access_auto + job_access_transit, data=stations_df)
summary(arrival_OLS)

a2_OLS <- lm(male_arrivals ~ network_density_mm + network_density_ped + intsect_density_mm + intsect_density_ped + job_access_transit, data=stations_df)
summary(a2_OLS)
small_OLS <- a2_OLS

#update regression with significant variables only
small_OLS <- lm(male_arrivals ~ network_density_ped + job_access_transit, data=stations_sp)
summary(small_OLS)
#results: network_density_ped, distance_to_transit and job_access_transit are significant
#----------------------DIAGNOSTICS
library(lmtest)
#Koenker-Bassett 
bptest(small_OLS)
#Breusch-Pagan Test
bptest(small_OLS, studentize=FALSE)
#VIF 
#higher than 2 indicates multicollinearity
library(car)
vif(small_OLS)
#Condition number 
library(mctest)
omcdiag(small_OLS)

# saving residuals and fitted values
stations_sp$olsresid <-residuals(small_OLS)
stations_sp$ols_fitted <- fitted(small_OLS)

# Moran test for residuals with second order Queens
lm.morantest(small_OLS, W_kn4_rowstd, zero.policy=T)
lm.morantest(small_OLS, W_kn2_rowstd, zero.policy=T)
# positive Morans I with significant p-value indicates non-random clustering

lisaRslt <- spdep::localmoran(stations_sp$male_arrivals, W_kn4_rowstd, zero.policy = TRUE, na.action = na.omit)
dim(lisaRslt); dim(stations_sp);

# Lagrange multiplier tests for residuals for 4 neighbors
lm.LMtests(small_OLS, W_kn4_rowstd, test="all", zero.policy=T)
# RLMlag model is more significant 

#----------------------SPATIAL LAG MODEL
#for 4 neighbors
stations_spatial_lag <- spatialreg::lagsarlm(male_arrivals ~ network_density_ped + job_access_transit, data=stations_sp, W_kn4_rowstd, zero.policy=T)
summary(stations_spatial_lag)
#results: job_access_transit*

# saving residuals and fitted values
stations_sp$splag_resid<-residuals(stations_spatial_lag)
stations_sp$splag_fitted <- fitted(stations_spatial_lag)

#----------------------MAPPING RESIDUAL RESULTS
stations_sf = st_as_sf(stations_sp)

#use color brewer
library(RColorBrewer)
pal <- brewer.pal(7, "OrRd") # we select 7 colors from the 
plot(st_geometry(blockgroups1))
plot(stations_sf["splag_resid"],
     main='Residuals from spatial lag', 
     breaks="jenks",
     nbreaks = 7,
     col = pal, 
     add=T)

#----------------------LOCAL MORAN I
lisaRslt_splag <- spdep::localmoran(stations_sp$splag_resid, W_kn4_rowstd, zero.policy = TRUE, na.action = na.omit)
dim(lisaRslt_splag); dim(stations_sp);

# Now we can derive the cluster/outlier types (COType in ArcGIS term) for each spatial feature in the data
significanceLevel <- 0.05; # 95% confidence
meanVal <- mean(stations_sp$arrival_trip_parity);

lisaRslt_splag %<>% tibble::as_tibble() %>%
  magrittr::set_colnames(c("Ii","E.Ii","Var.Ii","Z.Ii","Pr(z > 0)")) %>%
  dplyr::mutate(coType = dplyr::case_when(
    `Pr(z > 0)` > 0.05 ~ "Insignificant",
    `Pr(z > 0)` <= 0.05 & Ii >= 0 & stations_sp$arrival_trip_parity >= meanVal ~ "HH",
    `Pr(z > 0)` <= 0.05 & Ii >= 0 & stations_sp$arrival_trip_parity < meanVal ~ "LL",
    `Pr(z > 0)` <= 0.05 & Ii < 0 & stations_sp$arrival_trip_parity >= meanVal ~ "HL",
    `Pr(z > 0)` <= 0.05 & Ii < 0 & stations_sp$arrival_trip_parity < meanVal ~ "LH"
  ))

# Now add this coType to original sf data
stations_sp$coType <- lisaRslt_splag$coType %>% tidyr::replace_na("Insignificant")

