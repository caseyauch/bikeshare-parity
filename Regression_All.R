#import all trips sorted by station ID
df = read_csv('./Tables/trips_sorted_276.csv')
df1 <- na.omit(df)
nrow(df1)

#attribute join trips data to stations 
df1 <- stations %>% merge(df1, by='station_id')
nrow(df1)

#make spatial object 
stations_sp <- as_Spatial(df1)

#make df for stargazer 
stations_df <- stations_sp@data

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

#----------------------LINEAR REGRESSION 1 
parity_OLS <- lm(arrival_trip_parity ~ residential_density +employment_entropy + employment_density + employment_household_entropy + network_density_auto + network_density_mm + network_density_ped + intsect_density_auto + intsect_density_mm + intsect_density_ped + distance_to_transit + transit_freq + job_access_auto + job_access_transit, data=stations_df)
summary(parity_OLS)
farrival_OLS <- lm(female_arrivals ~ residential_density + employment_entropy + employment_density + employment_household_entropy + network_density_auto + network_density_mm + network_density_ped + intsect_density_auto + intsect_density_mm + intsect_density_ped + distance_to_transit + transit_freq + job_access_auto + job_access_transit, data=stations_df)
summary(farrival_OLS)
marrival_OLS <- lm(male_arrivals ~ residential_density +employment_entropy + employment_density + employment_household_entropy + network_density_auto + network_density_mm + network_density_ped + intsect_density_auto + intsect_density_mm + intsect_density_ped + distance_to_transit + transit_freq + job_access_auto + job_access_transit, data=stations_df)
summary(marrival_OLS)
arrivals_OLS <- lm(arrivals ~ residential_density +employment_entropy + employment_density + employment_household_entropy + network_density_auto + network_density_mm + network_density_ped + intsect_density_auto + intsect_density_mm + intsect_density_ped + distance_to_transit + transit_freq + job_access_auto + job_access_transit, data=stations_df)
summary(arrivals_OLS)

stargazer(parity_OLS, farrival_OLS, marrival_OLS, arrivals_OLS, type='html', out='OLS_regression_results.html')

#----------------DIAGNOSTICS for arrivals_OLS
library(lmtest)
#Koenker-Bassett 
bptest(arrivals_OLS)
#Breusch-Pagan Test
bptest(arrivals_OLS, studentize=FALSE)
#VIF 
#higher than 2 indicates multicollinearity
library(car)
vif(arrivals_OLS)
#Condition number 
library(mctest)
omcdiag(arrivals_OLS)

# Trend surface model for housing value
#Use coordinates to get XY if not already in the layer 
stations_trendsurface <- lm(arrival_trip_parity ~latitude + longitude, data=stations_sp)

summary(stations_trendsurface)
#----------------------LINEAR REGRESSION 2 
#update regression with significant variables only
a2_OLS <- lm(arrivals ~ network_density_mm + network_density_ped + job_access_transit, data=stations_sp)
parity2_OLS <- lm(arrival_trip_parity ~ employment_household_entropy + employment_density +job_access_auto*100000, data=stations_sp)
fa2_OLS <- lm(female_arrivals ~ network_density_mm + network_density_mm + job_access_transit, data=stations_sp)
ma2_OLS <- lm(male_arrivals ~ network_density_mm + network_density_ped + job_access_transit, data=stations_sp)

stargazer(parity2_OLS, fa2_OLS, ma2_OLS, a2_OLS, type='html', out='OLS_regression_results2.html',  digits=2)

#----------------DIAGNOSTICS for arrivals2_OLS
library(lmtest)
#Koenker-Bassett 
bptest(a2_OLS)
#Breusch-Pagan Test
bptest(a2_OLS, studentize=FALSE)
#VIF 
library(car)
#higher than 2 indicates multicollinearity
vif(parity2_OLS)
#Condition number 
library(mctest)
omcdiag(fa2_OLS)
#majority does not indicate multicollinearity issues

#---------------------------------------------BREAK
small_OLS <- a2_OLS

# saving residuals and fitted values
stations_sp$olsresid <-residuals(small_OLS)
stations_sp$ols_fitted <- fitted(small_OLS)

# Moran test for residuals with second order Queens
lm.morantest(small_OLS, W_kn4_rowstd, zero.policy=T)
lm.morantest(small_OLS, W_kn2_rowstd, zero.policy=T)
# positive Morans I with significant p-value indicates non-random clustering

lisaRslt <- spdep::localmoran(stations_sp$arrival_trip_parity, W_kn4_rowstd, zero.policy = TRUE, na.action = na.omit)
dim(lisaRslt); dim(stations_sp);

#calculating a lagged variable
stations_sp$parity_qlag <- lag.listw(W_kn4_rowstd,stations_sp$arrival_trip_parity, zero.policy = TRUE)

# OLS using lagged parityvalue as predictor
parity_lagOLS <- lm(arrival_trip_parity~parity_qlag, data=stations_sp)
summary(parity_lagOLS)

# Lagrange multiplier tests for residuals for 4 neighbors
lm.LMtests(small_OLS, W_kn2_rowstd, test="all", zero.policy=T)
# both LM err and LMlag are significant, so time to look at Robust LM Diagnostics
# RLMlag is significant meaning spatial lag model should be used
# RLMlag = 6.8311, df = 1, p-value = 0.008958

#----------------------SPATIAL LAG MODEL
#for 4 neighbors
stations_spatial_lag <- spatialreg::lagsarlm(female_arrivals ~ residential_density +employment_entropy + employment_density + employment_household_entropy + network_density_auto + network_density_mm + network_density_ped + intsect_density_auto + intsect_density_mm + intsect_density_ped + distance_to_transit + transit_freq + job_access_auto + job_access_transit, data=stations_sp, W_kn4_rowstd, zero.policy=T)
summary(stations_spatial_lag)


parity2_spl <- spatialreg::lagsarlm(arrival_trip_parity ~ employment_household_entropy + employment_density+ job_access_auto, data=stations_sp, W_kn4_rowstd, zero.policy=T)
summary(parity2_spl)
fa2_spl <- spatialreg::lagsarlm(female_arrivals ~  network_density_mm + intsect_density_ped + job_access_transit, data=stations_sp, W_kn4_rowstd, zero.policy=T)
summary(fa2_spl)
ma2_spl <- spatialreg::lagsarlm(male_arrivals ~ employment_household_entropy + network_density_ped + intsect_density_ped + job_access_transit, data=stations_sp, W_kn4_rowstd, zero.policy=T)
summary(ma2_spl)
a2_spl <- spatialreg::lagsarlm(arrivals ~ residential_density + network_density_ped + job_access_transit, data=stations_sp, W_kn4_rowstd, zero.policy=T)
summary(a2_spl)

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

#----------------------LINEAR REGRESSION for female trips
arrival_OLS <- lm(female_arrivals ~  residential_density +employment_entropy + employment_density + employment_household_entropy + network_density_auto + network_density_mm + network_density_ped + intsect_density_auto + intsect_density_mm + intsect_density_ped + distance_to_transit + transit_freq + job_access_auto + job_access_transit, data=stations_df)
summary(arrival_OLS)

#update regression with significant variables only
small_OLS <- lm(arrival_trip_parity ~ employment_density + employment_household_entropy + network_density_ped + job_access_auto + job_access_transit, data=stations_sp)
summary(small_OLS)
#results: network_density_ped**, network_density_mm*, job_access_transit** are very significant

a2_OLS <- lm(female_arrivals ~ network_density_ped + job_access_transit, data=stations_sp)
summary(a2_OLS)
small_OLS <- a2_OLS
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
lm.LMtests(small_OLS, W_kn2_rowstd, test="all", zero.policy=T)
# both LM err and LMlag are significant, so time to look at Robust LM Diagnostics
# RLMLag is significant 


#----------------------SPATIAL LAG MODEL
#for 4 neighbors
stations_spatial_lag <- spatialreg::lagsarlm(female_arrivals ~ network_density_ped + job_access_transit, data=stations_sp, W_kn4_rowstd, zero.policy=T)
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
meanVal <- mean(stations_sp$female_arrivals);

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


#----------------------LINEAR REGRESSION for male trips
arrival_OLS <- lm(male_arrivals ~  residential_density +employment_entropy + employment_density + employment_household_entropy + network_density_auto + network_density_mm + network_density_ped + intsect_density_auto + intsect_density_mm + intsect_density_ped + distance_to_transit + transit_freq + job_access_auto + job_access_transit, data=stations_df)
summary(arrival_OLS)

#update regression with significant variables only
small_OLS <- lm(male_arrivals ~ network_density_ped + job_access_transit, data=stations_sp)
summary(small_OLS)
#results: network_density_ped*, network_density_mm*, and job_access_transit*** are very significant
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
lm.LMtests(small_OLS, W_kn2_rowstd, test="all", zero.policy=T)
# both LM err and LMlag are significant, so time to look at Robust LM Diagnostics
# both robust models are significant? RLMLag is more sig.

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
meanVal <- mean(stations_sp$female_arrivals);

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
