
######################## WEIGHT MATRICES #############################
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

######################## LOCAL MORAN'S I #############################
#Local Moran's I in rgeoda 
library(rgeoda)
#create queens weight matrix in rgeoda 
#higher order weights matrix using the order parameter
boston_queen2nd  <- queen_weights(studyarea, order=2,include_lower_order=TRUE, precision_threshold = 0)
boston_queen1st  <- queen_weights(studyarea, order=1,include_lower_order=TRUE, precision_threshold = 0)
summary(boston_queen2nd)

lisa_jobaccst <- rgeoda::local_moran(boston_queen1st, studyarea["job_access_transit"])
summary(lisa_jobaccst)

#mapping the lisa
lisa_colors <- lisa_colors(lisa_jobaccst)
lisa_labels <- lisa_labels(lisa_jobaccst)
lisa_clusters <- lisa_clusters(lisa_jobaccst)

plot(st_geometry(studyarea), 
     col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}))
title(main = "Local Moran Map of Job Access by Transit")
legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")

#bivariate Local Moran's I total employment surrounded by job access by auto
biv_lisa <- rgeoda::local_bimoran(boston_queen1st, studyarea[c('employment_density', 'job_access_transit')])
lisa_colors <- lisa_colors(biv_lisa)
lisa_labels <- lisa_labels(biv_lisa)
lisa_clusters <- lisa_clusters(biv_lisa)

#interpret as not High high but High surrounded by High
plot(st_geometry(studyarea),
     col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#333333", lwd=0.1)
title(main = "Local Bivariate Employment Density & Job Access by Transit")
legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")


#bivariate Local Moran's I 
biv_lisa <- rgeoda::local_bimoran(boston_queen1st, studyarea[c('job_access_auto', 'job_access_transit')])
lisa_colors <- lisa_colors(biv_lisa)
lisa_labels <- lisa_labels(biv_lisa)
lisa_clusters <- lisa_clusters(biv_lisa)

#interpret as not High high but High surrounded by High
plot(st_geometry(studyarea),
     col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#333333", lwd=0.1)
title(main = "Local Bivariate Job Access by Auto & Job Access by Transit")
legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")


########################GLOBAL MORANS #############################
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
mt<-moran.test(studyarea$residential_density, W_sa_nbqueen_rowstd, zero.policy=T) 
mt2<-moran.test(studyarea$employment_density, W_sa_nbqueen_rowstd, zero.policy=T)
mt3<-moran.test(studyarea$employment_entropy, W_sa_nbqueen_rowstd, zero.policy=T)
mt4<-moran.test(studyarea$employment_household_entropy, W_sa_nbqueen_rowstd, zero.policy=T)
mt5<-moran.test(studyarea$network_density_auto, W_sa_nbqueen_rowstd, zero.policy=T)
mt6<-moran.test(studyarea$network_density_mm, W_sa_nbqueen_rowstd, zero.policy=T)
mt7<-moran.test(studyarea$network_density_ped, W_sa_nbqueen_rowstd, zero.policy=T)
mt8<-moran.test(studyarea$intsect_density_auto, W_sa_nbqueen_rowstd, zero.policy=T)
mt9<-moran.test(studyarea$intsect_density_mm, W_sa_nbqueen_rowstd, zero.policy=T)
mt10<-moran.test(studyarea$intsect_density_ped, W_sa_nbqueen_rowstd, zero.policy=T)
mt11<-moran.test(studyarea$distance_to_transit, W_sa_nbqueen_rowstd, zero.policy=T)
mt12<-moran.test(studyarea$transit_freq, W_sa_nbqueen_rowstd, zero.policy=T)
mt13<-moran.test(studyarea$job_access_auto, W_sa_nbqueen_rowstd, zero.policy=T)
mt14<-moran.test(studyarea$job_access_transit, W_sa_nbqueen_rowstd, zero.policy=T)

mt_results_q1 <- list(mt,mt2,mt3,mt4,mt5,mt6,mt7,mt8,mt9,mt10,mt11,mt12,mt13,mt14) %>% map(broom::tidy) %>% bind_rows() %>% select(estimate1, p.value) %>% rename(q1 = 1, pval_q1 = 2)

mt<-moran.test(studyarea$residential_density, W_sa_nbqueen2_rowstd, zero.policy=T) 
mt2<-moran.test(studyarea$employment_density, W_sa_nbqueen2_rowstd, zero.policy=T)
mt3<-moran.test(studyarea$employment_entropy, W_sa_nbqueen2_rowstd, zero.policy=T)
mt4<-moran.test(studyarea$employment_household_entropy, W_sa_nbqueen2_rowstd, zero.policy=T)
mt5<-moran.test(studyarea$network_density_auto, W_sa_nbqueen2_rowstd, zero.policy=T)
mt6<-moran.test(studyarea$network_density_mm, W_sa_nbqueen2_rowstd, zero.policy=T)
mt7<-moran.test(studyarea$network_density_ped, W_sa_nbqueen2_rowstd, zero.policy=T)
mt8<-moran.test(studyarea$intsect_density_auto, W_sa_nbqueen2_rowstd, zero.policy=T)
mt9<-moran.test(studyarea$intsect_density_mm,W_sa_nbqueen2_rowstd, zero.policy=T)
mt10<-moran.test(studyarea$intsect_density_ped, W_sa_nbqueen2_rowstd, zero.policy=T)
mt11<-moran.test(studyarea$distance_to_transit, W_sa_nbqueen2_rowstd, zero.policy=T)
mt12<-moran.test(studyarea$transit_freq, W_sa_nbqueen2_rowstd, zero.policy=T)
mt13<-moran.test(studyarea$job_access_auto, W_sa_nbqueen2_rowstd, zero.policy=T)
mt14<-moran.test(studyarea$job_access_transit, W_sa_nbqueen2_rowstd, zero.policy=T)

mt_results_q2 <- list(mt,mt2,mt3,mt4,mt5,mt6,mt7,mt8,mt9,mt10,mt11,mt12,mt13,mt14) %>% map(broom::tidy) %>% bind_rows() %>% select(estimate1, p.value) %>% rename(q2 = 1, pval_q2 = 2)

mt<-moran.test(studyarea$residential_density, W_sa_kn4_rowstd, zero.policy=T) 
mt2<-moran.test(studyarea$employment_density, W_sa_kn4_rowstd, zero.policy=T)
mt3<-moran.test(studyarea$employment_entropy, W_sa_kn4_rowstd, zero.policy=T)
mt4<-moran.test(studyarea$employment_household_entropy, W_sa_kn4_rowstd, zero.policy=T)
mt5<-moran.test(studyarea$network_density_auto, W_sa_kn4_rowstd, zero.policy=T)
mt6<-moran.test(studyarea$network_density_mm, W_sa_kn4_rowstd, zero.policy=T)
mt7<-moran.test(studyarea$network_density_ped, W_sa_kn4_rowstd, zero.policy=T)
mt8<-moran.test(studyarea$intsect_density_auto, W_sa_kn4_rowstd, zero.policy=T)
mt9<-moran.test(studyarea$intsect_density_mm,W_sa_kn4_rowstd, zero.policy=T)
mt10<-moran.test(studyarea$intsect_density_ped, W_sa_kn4_rowstd, zero.policy=T)
mt11<-moran.test(studyarea$distance_to_transit, W_sa_kn4_rowstd, zero.policy=T)
mt12<-moran.test(studyarea$transit_freq, W_sa_kn4_rowstd, zero.policy=T)
mt13<-moran.test(studyarea$job_access_auto, W_sa_kn4_rowstd, zero.policy=T)
mt14<-moran.test(studyarea$job_access_transit, W_sa_kn4_rowstd, zero.policy=T)

mt_results_kn4 <- list(mt,mt2,mt3,mt4,mt5,mt6,mt7,mt8,mt9,mt10,mt11,mt12,mt13,mt14) %>% map(broom::tidy) %>% bind_rows() %>% select(estimate1, p.value) %>% rename(kn4 = 1, pval_kn4 = 2)

mt<-moran.test(studyarea$residential_density,W_sa_d1_rowstd, zero.policy=T) 
mt2<-moran.test(studyarea$employment_density,W_sa_d1_rowstd, zero.policy=T)
mt3<-moran.test(studyarea$employment_entropy,W_sa_d1_rowstd, zero.policy=T)
mt4<-moran.test(studyarea$employment_household_entropy,W_sa_d1_rowstd, zero.policy=T)
mt5<-moran.test(studyarea$network_density_auto,W_sa_d1_rowstd, zero.policy=T)
mt6<-moran.test(studyarea$network_density_mm,W_sa_d1_rowstd, zero.policy=T)
mt7<-moran.test(studyarea$network_density_ped,W_sa_d1_rowstd, zero.policy=T)
mt8<-moran.test(studyarea$intsect_density_auto,W_sa_d1_rowstd, zero.policy=T)
mt9<-moran.test(studyarea$intsect_density_mm,W_sa_kn4_rowstd, zero.policy=T)
mt10<-moran.test(studyarea$intsect_density_ped,W_sa_d1_rowstd, zero.policy=T)
mt11<-moran.test(studyarea$distance_to_transit,W_sa_d1_rowstd, zero.policy=T)
mt12<-moran.test(studyarea$transit_freq,W_sa_d1_rowstd, zero.policy=T)
mt13<-moran.test(studyarea$job_access_auto,W_sa_d1_rowstd, zero.policy=T)
mt14<-moran.test(studyarea$job_access_transit,W_sa_d1_rowstd, zero.policy=T)

mt_results_d1 <- list(mt,mt2,mt3,mt4,mt5,mt6,mt7,mt8,mt9,mt10,mt11,mt12,mt13,mt14) %>% map(broom::tidy) %>% bind_rows() %>% select(estimate1, p.value) %>% rename(d1 = 1, pval_d1 = 2)


#prepare csv with morafor exporting 
names <- names(studyarea)
mt_results <- cbind(mt_results_q1,mt_results_q2, mt_results_kn4, mt_results_d1, names[1:14]) %>% arrange(desc(q1)) %>% rename(var_name = 'names[1:14]') %>% format(digits=2)
write_csv(mt_results, 'mt_results.csv', append=F)
                                                         