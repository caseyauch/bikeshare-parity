########################SECTION 2.1#############################
#text file saved to table1.txt
stargazer(stations_df,type = "text", title="Descriptive statistics", digits=1, out="tablefinal.txt")
#html file which looks nicer 
stargazer(stations_df, median=TRUE, type = "html", title="Descriptive statistics", digits=1, out="table1.html")

#histogram of gender parity
ggplot(stations_wtrips, aes(x='arrival_trip_parity')) + 
  geom_histogram() + 
  ggtitle("Histogram of Trip Parity Rates by Station") + 
  xlab("Trip Parity (Female/Male Trips)") + 
  ylab("Count of Stations")

#histogram of trip counts
ggplot(stations_wtrips, aes(x=arrivals)) +  
  geom_histogram(bins=50) + 
  ggtitle("Histogram of Bikeshare Demand by Station") + 
  xlab("Number of Trips") + 
  ylab("Count of Stations")

#histogram of gender parity
ggplot(stations_wtrips, 
       aes(x=`arrival_trip_parity`)) + 
  geom_histogram(color="black", alpha=.5) + 
  ggtitle("Histogram of Gender Parity") + 
  xlab("Gender Parity") + 
  ylab("Count of Stations")

#histogram of male trips
ggplot(stations_wtrips, 
       aes(x=`male_arrivals`)) + 
  geom_histogram() + 
  ggtitle("Histogram of Male Trips by Station") + 
  xlab("Number of Male Trips") + 
  ylab("Count of Stations") + theme_classic()

stations_wtrips1 <- stations_wtrips
stations_wtrips1$'Gender Parity' <- stations_wtrips1$arrival_trip_parity
stations_wtrips1$'Arrivals' <- stations_wtrips1$arrivals


map1<-tm_shape(studyarea) +tm_fill(alpha=.8,col="job_access_transit", style='fisher', palette=my.palette1, title='Job Access by Transit')+tm_borders()+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 0))
map1
map2<-tm_shape(studyarea) +tm_fill(alpha=.8, col="job_access_auto", style='fisher', palette=my.palette1, title='Job Access by Auto')+tm_borders()+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 0))
map2
map3<-tm_shape(studyarea) +tm_fill(col="employment_density", style='fisher', palette=my.palette1, title='Employment Density')+tm_borders()+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 0))
map4<-tm_shape(studyarea) +tm_fill(alpha=.8,col="residential_density", style='fisher', palette=my.palette1, title='Residential Density')+tm_borders()+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 0))
map5<-tm_shape(studyarea) +tm_fill(col="transit_freq", style='fisher', palette=my.palette1, title='Transit Frequency')+tm_borders()+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 0))
map5
map6<-tm_shape(studyarea) +tm_fill(alpha=.5, col="intsect_density_ped", style='fisher', palette=my.palette1, title='Intersection Density (Ped.)', alpha=0.5)+tm_borders()+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 0)) + tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2), title="")+tm_shape(obs_sites)+tm_bubbles(scale=.25)
map6
map7<-tm_shape(studyarea) +tm_fill(alpha=.8,col="distance_to_transit", style='fisher', palette=my.palette1, title='Distance to Transit')+tm_borders()+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 0)) 
map7
map8<-tm_shape(studyarea) +tm_fill(col="network_density_auto", style='fisher', palette=my.palette1, title='Network Density (Auto)')+tm_borders()+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 0))
map10<-tm_shape(studyarea) +tm_fill(alpha=.8, col='employment_household_entropy', style='fisher', palette=my.palette1, title='Land Use Diversity')+tm_borders()+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2))
map10
map11<-tm_shape(studyarea) +tm_fill(alpha=.8,col='employment_entropy', style='fisher', palette=my.palette1, title='Employment Diversity')+tm_borders()+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2))
map11

tmap_save(map1, width=1800, filename = "map1.png")
tmap_save(map2, width=1800, filename = "map2.png")
tmap_save(map3, width=1800, filename = "map3.png")
tmap_save(map4, width=1800, filename = "map4.png")
tmap_save(map5, width=1800, filename = "map5.png")
tmap_save(map6, width=1800, filename = "map6.png")
tmap_save(map7, width=1800, filename = "map7.png")
tmap_save(map8, width=1800, filename = "map8.png")
tmap_save(map10, width=1800, filename = "map10.png")
tmap_save(map11, width=1800, filename = "map11.png")

stations_cl$cluster <- as.factor(stations_cl$cluster)
#histogram of gender parity by cluster
ggplot(stations_cl, 
       aes(x=arrival_trip_parity, fill=cluster)) + 
  geom_histogram(position='identity', color="black", alpha=.5) + 
  ggtitle("Histogram of Gender Parity by Cluster") + 
  xlab("Gender Parity") + 
  ylab("Count of Stations") + scale_fill_brewer(palette='Paired')

#histogram of arrivals 
ggplot(stations_cl, 
       aes(x=arrivals)) + 
  geom_histogram(position='identity', color="black", alpha=.5) + 
  ggtitle("Histogram of Arrivals") + 
  xlab("Trip Arrivals") + 
  ylab("Count of Stations") + scale_fill_brewer(palette='Paired')

p <- stations_wtrips  %>%
  ggplot(aes(job_access_auto, employment_density, id=station_id)) +
  geom_point() + geom_smooth(method = 'lm', formula = y ~ x) +
  theme_bw() + ggtitle("")
p
ggplotly(p, source='select', tooltip = c('id'))

#histogram of arrivals by cluster
ggplot(stations_cl, 
       aes(x=arrivals, fill=cluster)) + 
  geom_histogram(position='identity', color="black", alpha=.5) + 
  ggtitle("Histogram of Arrivals by Cluster") + 
  xlab("Gender Parity") + 
  ylab("Count of Stations") + scale_fill_brewer(palette='Paired')


#scatter plot matrix
x <- ggpairs(stations_df, columns = c(2:15),
             lower = list(continuous = "smooth"),
             title = "Scatter Plot Matrix", axisLabels = "show")
x

library(corrplot)
stations.cor <- cor(stations_df[2:15])
corrplot(stations.cor, type='upper', tl.col = "black")

trips.cor <- cor(trips_sorted[2:9])
corrplot(trips.cor, type='upper', tl.col = "black")

p <- stations_pca_rot  %>%
  ggplot(aes(arrivals, arrival_trip_parity, id=station_id)) +
  geom_point() +
  theme_bw() + scale_colour_brewer(palette = "Paired") + ggtitle("")
ggplotly(p, source='select', tooltip = c('id'))

tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2), title="")+tm_shape(stations_cl)+tm_bubbles(col='female_arrivals', palette=seq.palette7, scale=.75)

#observation sites for components
obs_sites <- list(22,
                  47,
                  61,
                  68,
                  81,
                  85,
                  104
)
obs_sites_df <- stations_wtrips %>% filter(station_id %in% obs_sites)
map_obs_sites <- stations_pca_rot %>% filter(station_id %in% obs_sites)
tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2), title="RC1 values")+tm_shape(map_obs_sites)+tm_bubbles(col='RC1', palette=seq.palette7, scale=.75)
tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2), title="RC2 values")+tm_shape(map_obs_sites)+tm_bubbles(col='RC2', palette=seq.palette7, scale=.75)
tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2), title="RC3 values")+tm_shape(map_obs_sites)+tm_bubbles(col='RC3', palette=seq.palette7, scale=.75)
tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2), title="RC4 values")+tm_shape(map_obs_sites)+tm_bubbles(col='RC4', palette=seq.palette7, scale=.75)

#station attributes of observation sites 
p <- stations_cl  %>% filter(cluster==2) %>%
  ggplot(aes(female_arrivals, arrival_trip_parity, id=station_id)) +
  geom_point() +
  theme_bw() + scale_colour_brewer(palette = "Paired") + ggtitle("")
ggplotly(p, source='select', tooltip = c('id'))

table <- stations_pca_rot %>% filter(station_id %in% c(67,53, 12)) %>% select('station_id', 'arrivals', 'female_arrivals', 'arrival_trip_parity', 'RC1', 'RC2', 'RC3', 'RC4') %>% arrange(station_id) %>% data.frame() %>% format(digit=2)
write_csv(table, 'observation_sites.csv', append=T)

table2 <- stations_cl%>% filter(station_id %in% c(67,53, 12)) %>% select('station_id', 'arrivals', 'female_arrivals', 'arrival_trip_parity', 'cluster') %>% arrange(station_id) %>% data.frame() %>% format(digit=2)

#visualizing component values at the station level
tm_shape(stations_pca_rot)+tm_bubbles(col='RC3', palette=seq.palette, scale=.5)+tm_layout(legend.outside=T, legend.format = list(format="f", digits = 2))
