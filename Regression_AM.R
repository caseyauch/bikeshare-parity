#set working directory
setwd("C:/Users/casey/OneDrive - Tufts/Documents/2022 Thesis/Data/Data_Clean") 

#import all trips sorted by station ID
df = read_csv('./Tables/am_sorted.csv')

#attribute join trips data to stations sf object
stations_wtrips <- merge(stations, df, by='station_id')
#make spatial object 
stations_sp <- as_Spatial(stations_wtrips)
#make df for stargazer 
stations_df <- stations_sp@data

#text file saved to table1.txt
library(stargazer)
stargazer(stations_df,type = "text", title="Descriptive statistics for AM Trips", digits=1, out="tablefinal.txt")

#map trip arrivals and departures
pal <- brewer.pal(7, "OrRd") # we select 7 colors from the palette
plot(st_geometry(studyarea))
plot(stations_wtrips['arrivals'], add = T,
     breaks = "jenks", nbreaks = 7,
     pal = pal, 
     pch = 16)

#----------------------LINEAR REGRESSION for trip parity
arrival_OLS <- lm(arrival_trip_parity ~  residential_density +employment_entropy + employment_density + employment_household_entropy + network_density_auto + network_density_mm + network_density_ped + intsect_density_auto + intsect_density_mm + intsect_density_ped + distance_to_transit + transit_freq + job_access_auto + job_access_transit, data=stations_df)
summary(arrival_OLS)

#update regression with significant variables only
small_OLS <- lm(arrival_trip_parity ~ employment_density + employment_household_entropy + job_access_auto, data=stations_sp)
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
stations_spatial_lag <- spatialreg::lagsarlm(arrival_trip_parity ~employment_density + employment_household_entropy + job_access_auto, data=stations_sp, W_kn4_rowstd, zero.policy=T)
summary(stations_spatial_lag)

# saving residuals and fitted values
stations_sp$splag_resid<-residuals(stations_spatial_lag)
stations_sp$splag_fitted <- fitted(stations_spatial_lag)


#----------------------LINEAR REGRESSION 2 for female trips
arrival_OLS <- lm(female_arrivals ~  residential_density + employment_entropy + employment_density + employment_household_entropy + network_density_auto + network_density_mm + network_density_ped + intsect_density_auto + intsect_density_mm + intsect_density_ped + distance_to_transit + transit_freq + job_access_auto + job_access_transit, data=stations_df)
summary(arrival_OLS)

#update regression with significant variables only
small_OLS <- lm(female_arrivals ~ residential_density + network_density_ped + job_access_transit, data=stations_sp)
summary(small_OLS)
#results: nothing is significant 
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
stations_spatial_lag <- spatialreg::lagsarlm(female_arrivals ~ network_density_ped + job_access_transit, data=stations_df, W_kn4_rowstd, zero.policy = TRUE)
summary(stations_spatial_lag)
#results: nothing is significant!

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
stations_spatial_lag <- spatialreg::lagsarlm(male_arrivals ~ employment_density + employment_entropy + network_density_ped + job_access_transit, data=stations_sp, W_kn4_rowstd, zero.policy=T)
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

