#Local Moran's I in rgeoda 
library(rgeoda)
#create queens weight matrix in rgeoda 
#higher order weights matrix using the order parameter
boston_queen2nd  <- queen_weights(stations_wtrips, order=2,include_lower_order=TRUE, precision_threshold = 0)
summary(boston_queen2nd)


#Local Moran's I  for gender parity 
lisa_parity <- rgeoda::local_moran(boston_queen2nd, stations_wtrips["arrival_trip_parity"])
summary(lisa_parity)
lisa_colors <- lisa_colors(lisa_parity)
lisa_labels <- lisa_labels(lisa_parity)
lisa_clusters <- lisa_clusters(lisa_parity)
breaks = c(0, 1, 2, 3, 4, 5)  
stations_lisa <- cbind(stations_wtrips, lisa_clusters)
tm_shape(studyarea)+tm_fill(alpha=0.5)+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2), title="Gender parity clusters")+tm_shape(stations_lisa) + tm_bubbles(col='lisa_clusters', palette=c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4)), scale=0.5, labels =c("Not significant", "High-High","Low-Low","Low-High","High-Low"))

#Local Moran's I  for female arrivals
lisa_parity <- rgeoda::local_moran(boston_queen2nd, stations_wtrips["female_arrivals"])
summary(lisa_parity)
lisa_colors <- lisa_colors(lisa_parity)
lisa_labels <- lisa_labels(lisa_parity)
lisa_clusters <- lisa_clusters(lisa_parity)
breaks = c(0, 1, 2, 3, 4, 5)  
stations_lisa <- cbind(stations_wtrips, lisa_clusters)
ttm()
tm_shape(studyarea)+tm_fill(alpha=0.5)+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2), title="Clusters of female arrivals")+tm_shape(stations_lisa) + tm_bubbles(col='lisa_clusters', palette=c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4)), scale=0.5, labels =c("Not significant", "High-High","Low-Low","Low-High","High-Low"))

#mapping bivariate for female arrivals
obs_sites_lisa <- stations_lisa %>% filter(station_id %in% c(68, 22, 61, 47, 131, 97, 81))
tm_shape(studyarea)+tm_fill()+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2), title="Female Ridership")+tm_shape(obs_sites_lisa)+tm_bubbles(col='lisa_clusters', palette=c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4)), scale=0.5, labels =c("Not significant", "High-High","Low-Low","Low-High","High-Low"))


# plot(st_geometry(studyarea))
# plot(st_geometry(stations_wtrips), 
#      col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}), add=T)
# title(main = "Local Moran Map of Gender Parity")
# legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")


#bivariate Local Moran's I for gender parity and arrivals
biv_lisa <- rgeoda::local_bimoran(boston_queen2nd, stations_wtrips[c('arrivals', 'arrival_trip_parity')])
lisa_colors <- lisa_colors(biv_lisa)
lisa_labels <- lisa_labels(biv_lisa)
lisa_clusters <- lisa_clusters(biv_lisa)
breaks = c(0, 1, 2, 3, 4, 5) 
lisa_df <- cbind(stations_wtrips, lisa_clusters)
tm_shape(studyarea)+tm_fill(alpha=0.5)+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2), title="Ridership & Gender Parity")+tm_shape(lisa_df) + tm_bubbles(col='lisa_clusters', palette=c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4)), scale=0.5, labels =c("Not significant", "High-High","Low-Low","Low-High","High-Low"))

#mapping bivariate for obs sites
obs_sites_lisa <- lisa_df %>% filter(station_id %in% c(68, 22, 61, 47, 131, 97, 81))
tm_shape(studyarea)+tm_fill()+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2), title="Ridership & Gender Parity")+tm_shape(obs_sites_lisa)+tm_bubbles(col='lisa_clusters', palette=c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4)), scale=0.5, labels =c("Not significant", "High-High","Low-Low","Low-High","High-Low"))

# LOCAL MORANS FOR ARRIVALS
lisa_parity <- rgeoda::local_moran(boston_queen2nd, stations_wtrips["arrival_trip_parity"])
summary(lisa_parity)

#mapping the lisa for gender parity
lisa_colors <- lisa_colors(lisa_parity)
lisa_labels <- lisa_labels(lisa_parity)
lisa_clusters <- lisa_clusters(lisa_parity)

breaks = c(0, 1, 2, 3, 4, 5) 
stations_lisa <- cbind(stations_wtrips, lisa_clusters)
stations_lisa$'Gender Parity' <- stations_lisa$lisa_clusters
LISA2<- tm_shape(studyarea)+tm_fill()+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2), title="LISA Clusters")+tm_shape(stations_lisa) + tm_bubbles(col='Gender Parity', palette=c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4)), scale=0.5, labels =c("Not significant", "High-High","Low-Low","Low-High","High-Low"))
LISA2

#interpret as not High high but High surrounded by High
plot(st_geometry(studyarea))
plot(st_geometry(stations_wtrips),
     col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}), 
     border = "#333333", lwd=0.1, add=T)
title(main = "Local Bivariate LISA Gender Parity Surrounded by Arrivals")
legend('bottomleft', legend = lisa_labels, fill = lisa_colors, border = "#eeeeee")
