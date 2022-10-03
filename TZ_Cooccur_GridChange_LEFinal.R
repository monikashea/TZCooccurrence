setwd("C:/Users/mshea3/Desktop/TZCooccurrence_DataCode")

library(sp)
library(rgdal)
library(tidyverse)

################### prepare data #######################
## import tree data
all_trees_pred <- read.csv(file="./all_trees_pred_12Aug2019.csv",header=TRUE)

coords <- cbind(all_trees_pred$X,all_trees_pred$Y)

ptree_diff_2018 <- SpatialPointsDataFrame(coords,all_trees_pred)
proj4string(ptree_diff_2018) <- CRS("+init=epsg:3070") # add projection NAD83/Wisconsin Transverse Mercator

## import grids
Grid_1000m <- readOGR(dsn="./Grids",layer="1000m_Grid")
Grid_1000m <- spTransform(Grid_1000m,CRS=CRS("+init=epsg:3070"))
Grid_1000m$Grid_ID <- as.integer(as.character(Grid_1000m$Grid_ID))

Grid_1200m <- readOGR(dsn="./Grids",layer="1200m_Grid")
Grid_1200m <- spTransform(Grid_1200m,CRS=CRS("+init=epsg:3070"))
Grid_1200m$Grid_ID <- as.integer(as.character(Grid_1200m$Grid_ID))

Grid_1500m <- readOGR(dsn="./Grids",layer="1500m_Grid")
Grid_1500m <- spTransform(Grid_1500m,CRS=CRS("+init=epsg:3070"))
Grid_1500m$Grid_ID <- as.integer(as.character(Grid_1500m$Grid_ID))

Grid_2000m <- readOGR(dsn="./Grids",layer="2000m_Grid")
Grid_2000m <- spTransform(Grid_2000m,CRS=CRS("+init=epsg:3070"))
Grid_2000m$Grid_ID <- as.integer(as.character(Grid_2000m$Grid_ID))

Grid_2500m <- readOGR(dsn="./Grids",layer="2500m_Grid")
Grid_2500m <- spTransform(Grid_2500m,CRS=CRS("+init=epsg:3070"))
Grid_2500m$Grid_ID <- as.integer(as.character(Grid_2500m$Grid_ID))

Grid_3000m <- readOGR(dsn="./Grids",layer="3000m_Grid")
Grid_3000m <- spTransform(Grid_3000m,CRS=CRS("+init=epsg:3070"))
Grid_3000m$Grid_ID <- as.integer(as.character(Grid_3000m$Grid_ID))

Grid_3750m <- readOGR(dsn="./Grids",layer="3750m_Grid")
Grid_3750m <- spTransform(Grid_3750m,CRS=CRS("+init=epsg:3070"))
Grid_3750m$Grid_ID <- as.integer(as.character(Grid_3750m$Grid_ID))

Grid_4285m <- readOGR(dsn="./Grids",layer="4285m_Grid")
Grid_4285m <- spTransform(Grid_4285m,CRS=CRS("+init=epsg:3070"))
Grid_4285m$Grid_ID <- as.integer(as.character(Grid_4285m$Grid_ID))

Grid_5000m <- readOGR(dsn="./Grids",layer="5000m_Grid")
Grid_5000m <- spTransform(Grid_5000m,CRS=CRS("+init=epsg:3070"))
Grid_5000m$Grid_ID <- as.integer(as.character(Grid_5000m$Grid_ID))

Grid_6000m <- readOGR(dsn="./Grids",layer="6000m_Grid")
Grid_6000m <- spTransform(Grid_6000m,CRS=CRS("+init=epsg:3070"))
Grid_6000m$Grid_ID <- as.integer(as.character(Grid_6000m$Grid_ID))

Grid_7500m <- readOGR(dsn="./Grids",layer="7500m_Grid")
Grid_7500m <- spTransform(Grid_7500m,CRS=CRS("+init=epsg:3070"))
Grid_7500m$Grid_ID <- as.integer(as.character(Grid_7500m$Grid_ID))

Grid_30km <- readOGR(dsn="./Grids",layer="30km_Grid")
Grid_30km <- spTransform(Grid_30km,CRS=CRS("+init=epsg:3070"))
Grid_30km$Grid_ID <- as.integer(as.character(Grid_30km$Grid_ID))



## overlay points on grids
pg_overlay_1000m <- over(ptree_diff_2018,Grid_1000m)
pg_overlay_1200m <- over(ptree_diff_2018,Grid_1200m)
pg_overlay_1500m <- over(ptree_diff_2018,Grid_1500m)
pg_overlay_2000m <- over(ptree_diff_2018,Grid_2000m)
pg_overlay_2500m <- over(ptree_diff_2018,Grid_2500m)
pg_overlay_3000m <- over(ptree_diff_2018,Grid_3000m)
pg_overlay_3750m <- over(ptree_diff_2018,Grid_3750m)
pg_overlay_4285m <- over(ptree_diff_2018,Grid_4285m)
pg_overlay_5000m <- over(ptree_diff_2018,Grid_5000m)
pg_overlay_6000m <- over(ptree_diff_2018,Grid_6000m)
pg_overlay_7500m <- over(ptree_diff_2018,Grid_7500m)
pg_overlay_30km <- over(ptree_diff_2018,Grid_30km)

# rename Grid_ID and remove Id field
pg_overlay_1000m <- pg_overlay_1000m %>% rename(Grid_1000m=Grid_ID) %>% select(-Id)
pg_overlay_1200m <- pg_overlay_1200m %>% rename(Grid_1200m=Grid_ID) %>% select(-Id)
pg_overlay_1500m <- pg_overlay_1500m %>% rename(Grid_1500m=Grid_ID) %>% select(-Id)
pg_overlay_2000m <- pg_overlay_2000m %>% rename(Grid_2000m=Grid_ID) %>% select(-Id)
pg_overlay_2500m <- pg_overlay_2500m %>% rename(Grid_2500m=Grid_ID) %>% select(-Id)
pg_overlay_3000m <- pg_overlay_3000m %>% rename(Grid_3000m=Grid_ID) %>% select(-Id)
pg_overlay_3750m <- pg_overlay_3750m %>% rename(Grid_3750m=Grid_ID) %>% select(-Id)
pg_overlay_4285m <- pg_overlay_4285m %>% rename(Grid_4285m=Grid_ID) %>% select(-Id)
pg_overlay_5000m <- pg_overlay_5000m %>% rename(Grid_5000m=Grid_ID) %>% select(-Id)
pg_overlay_6000m <- pg_overlay_6000m %>% rename(Grid_6000m=Grid_ID) %>% select(-Id)
pg_overlay_7500m <- pg_overlay_7500m %>% rename(Grid_7500m=Grid_ID) %>% select(-Id)
pg_overlay_30km <- pg_overlay_30km %>% rename(Grid_30km=Grid_ID) %>% select(-Id)

# put them all together
pg_overlay_trees <- cbind(pg_overlay_1000m,
                          pg_overlay_1200m,
                          pg_overlay_1500m,
                          pg_overlay_2000m,
                          pg_overlay_2500m,
                          pg_overlay_3000m,
                          pg_overlay_3750m,
                          pg_overlay_4285m,
                          pg_overlay_5000m,
                          pg_overlay_6000m,
                          pg_overlay_7500m,
                          pg_overlay_30km)

Trees <- all_trees_pred %>% bind_cols(pg_overlay_trees)

summary(as.factor(Trees$SP_new))

# remove UK, RR (unknown species and very rare species with <10 points)

Trees<-Trees %>% filter(!SP_new %in% c('UK','RR')) %>% filter(!is.na(SP_new))

Trees$SP_new<-as.factor(Trees$SP_new)
Trees$SP_new<-droplevels(Trees$SP_new)

# Relative BA and density calculations
Trees$BA=pi*((Trees$diam/2)^2)


#### bring in TZ layer and larger grid 
## bring in TZ layer and larger grid 
Zones4_polygon<-readOGR('./mean_FS_TZ4_PolygonZones','mean_FS_TZ4_PolygonZones')
Zones4_polygon <- spTransform(Zones4_polygon,CRS=CRS("+init=epsg:3070"))

# subset TZ polygon
TZ4_polygon<-Zones4_polygon[Zones4_polygon$Zone=='TZ',]

plot(TZ4_polygon)

## Northern and Southern indicator species
NSpp_TZ4<-read.csv('./NSpp_TZ4_FS.csv',header=TRUE)
SSpp_TZ4<-read.csv('./SSpp_TZ4_FS.csv',header=TRUE)

# Select N and S species that are siginificant indicator species at least 50% of the time
NSpp<-NSpp_TZ4 %>% mutate(percent=count/50) %>% filter(percent>=0.5)
SSpp<-SSpp_TZ4 %>% mutate(percent=count/50) %>% filter(percent>=0.5)

## Get spatial data ready
coords<-SpatialPoints(Trees[,c("X","Y")])
TreesSp<-SpatialPointsDataFrame(coords,Trees)
proj4string(TreesSp)<-CRS("+init=epsg:3070")

TreesSp$N<-2
TreesSp@data<-within(TreesSp@data,N[SP_new %in% NSpp$SP_new]<-1)
TreesSp@data<-within(TreesSp@data,N[SP_new %in% SSpp$SP_new]<-0)



############# overall data summary ############
## species proportions
TreesSp_data<-TreesSp@data
TreesSp_data$Ind_Zone<-ifelse(TreesSp_data$N==1,"North",ifelse(TreesSp_data$N==2,"Both","South"))

Grid_prop<-TreesSp_data %>% group_by(Grid_30km,Ind_Zone) %>% summarise(count=n()) %>% spread(Ind_Zone,count) %>% ungroup
Grid_prop[is.na(Grid_prop)]<-0

Grid_prop<-Grid_prop %>% mutate(Nprop=North/(North+South))

# export
#write.csv(Grid_prop,file="./Output/Grid_prop.csv",row.names = FALSE)



######################################  co-occurrence patterns #####################################
#################### Grid 1000m ####################

### Grid 30 km
Grid_30km_Grid_1000m_prop_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_1000m_obs_hist_List<-vector("list",nrow(Grid_30km))

## calculate observed and expected
for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  if(nrow(Point_grid_subset@data)==0) next
  
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  Grid_1000m_prop<-PGS_data %>% group_by(Grid_1000m,Ind_Zone) %>% summarise(count=n()) %>% spread(Ind_Zone,count) %>% ungroup
  Grid_1000m_prop[is.na(Grid_1000m_prop)]<-0
  
  names<-c("Grid_1000m","Both","North","South")
  
  Missing<-setdiff(names,names(Grid_1000m_prop))
  Grid_1000m_prop[Missing]<-0
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  Grid_1000m_prop<-Grid_1000m_prop %>% mutate(Nprop=North/(North+South)) %>% filter(Nprop>=0)
  Grid_1000m_prop$Grid<-i
  
  Grid_1000m_obs_hist=hist(Grid_1000m_prop$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
  Grid_1000m_obs_hist_set<-data.frame(do.call("cbind",Grid_1000m_obs_hist[2:4]))
  Grid_1000m_obs_hist_set<-Grid_1000m_obs_hist_set %>% mutate(percent=counts/sum(counts))
  
  index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
  
  Grid_1000m_obs_hist_set$index<-index
  Grid_1000m_obs_hist_set<-mutate(Grid_1000m_obs_hist_set,product=percent*index) %>% mutate(Grid=i)
  
  Grid_30km_Grid_1000m_prop_List[[i]]<-Grid_1000m_prop
  Grid_30km_Grid_1000m_obs_hist_List[[i]]<-Grid_1000m_obs_hist_set
}

Grid30_Grid_1000m_prop<-do.call("rbind",Grid_30km_Grid_1000m_prop_List)
Grid30_Grid_1000m_obs_hist<-do.call("rbind",Grid_30km_Grid_1000m_obs_hist_List)


## random relabeling
#Grid_30km_Grid_1000m_prop_relabel_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_1000m_relabel_hist_summary_List<-vector("list",nrow(Grid_30km))

for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  # get data ready for relabeling
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  niter=1000
  NS_Grid_1000m_prop_List<-vector("list",niter)
  NS_Grid_1000m_prop_relabel_List<-vector("list",niter)
  NS_Grid_1000m_relabel_hist_List<-vector("list",niter)
  
  for (z in 1:niter){
    V<-sample(PGS_data$Ind_Zone)
    NS_Grid_1000m_relabel<-PGS_data %>% mutate(relabel=V) %>% 
      group_by(Grid_1000m,relabel) %>% summarise(count=n()) %>% spread(relabel,count) %>% ungroup
    NS_Grid_1000m_relabel[is.na(NS_Grid_1000m_relabel)]<-0
    
    if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
    if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
    
    NS_Grid_1000m_relabel<-NS_Grid_1000m_relabel %>% mutate(Nprop=North/(North+South))
    NS_Grid_1000m_relabel$Grid<-i
    NS_Grid_1000m_relabel$iter<-z
    
    Grid_1000m_relabel_hist=hist(NS_Grid_1000m_relabel$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
    Grid_1000m_relabel_hist_set<-data.frame(do.call("cbind",Grid_1000m_relabel_hist[2:4]))
    Grid_1000m_relabel_hist_set<-Grid_1000m_relabel_hist_set %>% mutate(percent=counts/sum(counts))
    
    index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
    
    Grid_1000m_relabel_hist_set$index<-index
    Grid_1000m_relabel_hist_set<-mutate(Grid_1000m_relabel_hist_set,product=percent*index) %>% mutate(iter=z)
    
    NS_Grid_1000m_prop_relabel_List[[z]]<-NS_Grid_1000m_relabel
    NS_Grid_1000m_relabel_hist_List[[z]]<-Grid_1000m_relabel_hist_set
  }
  
  NS_Grid_1000m_prop_relabel<-do.call("rbind",NS_Grid_1000m_prop_relabel_List)
  
  NS_Grid_1000m_relabel_hist<-do.call("rbind",NS_Grid_1000m_relabel_hist_List)
  NS_Grid_1000m_relabel_hist$Grid<-i
  
  relabel_hist_summary<-NS_Grid_1000m_relabel_hist %>% group_by(iter) %>% summarise(sum=sum(product))
  relabel_hist_summary$Grid<-i
  
  #Grid_30km_Grid_1000m_prop_relabel_List[[i]]<-NS_Grid_1000m_prop_relabel
  Grid_30km_Grid_1000m_relabel_hist_summary_List[[i]]<-relabel_hist_summary
}

#Grid30_Grid_1000m_prop_relabel<-do.call("rbind",Grid_30km_Grid_1000m_prop_relabel_List)
Grid30_Grid_1000m_relabel_hist_summary<-do.call("rbind",Grid_30km_Grid_1000m_relabel_hist_summary_List)



### summarizing and comparing span of data
# Grid 30
Grid30_Grid_1000m_obs_hist_summary<-Grid30_Grid_1000m_obs_hist %>% group_by(Grid) %>% summarize(sum=sum(product))
Grid30_Grid_1000m_relabel_hist_summary2<-Grid30_Grid_1000m_relabel_hist_summary %>% group_by(Grid) %>% summarize(relabel_mean=mean(sum),upper=quantile(sum,probs=0.975),lower=quantile(sum,probs=0.025))
Grid30_Grid_1000m_hist_summary<-Grid30_Grid_1000m_obs_hist_summary %>% left_join(Grid30_Grid_1000m_relabel_hist_summary2,by="Grid") %>% 
  mutate(upper_diff=sum-upper, lower_diff=sum-lower, diff=sum-relabel_mean) %>%  
  mutate(upper_sign=sign(upper_diff),lower_sign=sign(lower_diff)) %>% mutate(sig=upper_sign+lower_sign)


### export results
write.csv(Grid30_Grid_1000m_hist_summary,file="./Output/Grid30_Grid_1000m_hist_summary.csv",row.names = FALSE)


#################### Grid 1200m ####################
### Grid 30 km
Grid_30km_Grid_1200m_prop_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_1200m_obs_hist_List<-vector("list",nrow(Grid_30km))

## calculate observed and expected
for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  if(nrow(Point_grid_subset@data)==0) next
  
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  Grid_1200m_prop<-PGS_data %>% group_by(Grid_1200m,Ind_Zone) %>% summarise(count=n()) %>% spread(Ind_Zone,count) %>% ungroup
  Grid_1200m_prop[is.na(Grid_1200m_prop)]<-0
  
  names<-c("Grid_1200m","Both","North","South")
  
  Missing<-setdiff(names,names(Grid_1200m_prop))
  Grid_1200m_prop[Missing]<-0
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  Grid_1200m_prop<-Grid_1200m_prop %>% mutate(Nprop=North/(North+South)) %>% filter(Nprop>=0)
  Grid_1200m_prop$Grid<-i
  
  Grid_1200m_obs_hist=hist(Grid_1200m_prop$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
  Grid_1200m_obs_hist_set<-data.frame(do.call("cbind",Grid_1200m_obs_hist[2:4]))
  Grid_1200m_obs_hist_set<-Grid_1200m_obs_hist_set %>% mutate(percent=counts/sum(counts))
  
  index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
  
  Grid_1200m_obs_hist_set$index<-index
  Grid_1200m_obs_hist_set<-mutate(Grid_1200m_obs_hist_set,product=percent*index) %>% mutate(Grid=i)
  
  Grid_30km_Grid_1200m_prop_List[[i]]<-Grid_1200m_prop
  Grid_30km_Grid_1200m_obs_hist_List[[i]]<-Grid_1200m_obs_hist_set
}

Grid30_Grid_1200m_prop<-do.call("rbind",Grid_30km_Grid_1200m_prop_List)
Grid30_Grid_1200m_obs_hist<-do.call("rbind",Grid_30km_Grid_1200m_obs_hist_List)


## random relabeling
#Grid_30km_Grid_1200m_prop_relabel_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_1200m_relabel_hist_summary_List<-vector("list",nrow(Grid_30km))

for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  # get data ready for relabeling
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
   
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  niter=1000
  NS_Grid_1200m_prop_List<-vector("list",niter)
  NS_Grid_1200m_prop_relabel_List<-vector("list",niter)
  NS_Grid_1200m_relabel_hist_List<-vector("list",niter)
  
  for (z in 1:niter){
    V<-sample(PGS_data$Ind_Zone)
    NS_Grid_1200m_relabel<-PGS_data %>% mutate(relabel=V) %>% 
      group_by(Grid_1200m,relabel) %>% summarise(count=n()) %>% spread(relabel,count) %>% ungroup
    NS_Grid_1200m_relabel[is.na(NS_Grid_1200m_relabel)]<-0
    
    if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
    if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
    
    NS_Grid_1200m_relabel<-NS_Grid_1200m_relabel %>% mutate(Nprop=North/(North+South))
    NS_Grid_1200m_relabel$Grid<-i
    NS_Grid_1200m_relabel$iter<-z
    
    Grid_1200m_relabel_hist=hist(NS_Grid_1200m_relabel$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
    Grid_1200m_relabel_hist_set<-data.frame(do.call("cbind",Grid_1200m_relabel_hist[2:4]))
    Grid_1200m_relabel_hist_set<-Grid_1200m_relabel_hist_set %>% mutate(percent=counts/sum(counts))
    
    index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
    
    Grid_1200m_relabel_hist_set$index<-index
    Grid_1200m_relabel_hist_set<-mutate(Grid_1200m_relabel_hist_set,product=percent*index) %>% mutate(iter=z)
    
    NS_Grid_1200m_prop_relabel_List[[z]]<-NS_Grid_1200m_relabel
    NS_Grid_1200m_relabel_hist_List[[z]]<-Grid_1200m_relabel_hist_set
  }
  
  NS_Grid_1200m_prop_relabel<-do.call("rbind",NS_Grid_1200m_prop_relabel_List)
  
  NS_Grid_1200m_relabel_hist<-do.call("rbind",NS_Grid_1200m_relabel_hist_List)
  NS_Grid_1200m_relabel_hist$Grid<-i
  
  relabel_hist_summary<-NS_Grid_1200m_relabel_hist %>% group_by(iter) %>% summarise(sum=sum(product))
  relabel_hist_summary$Grid<-i
  
  #Grid_30km_Grid_1200m_prop_relabel_List[[i]]<-NS_Grid_1200m_prop_relabel
  Grid_30km_Grid_1200m_relabel_hist_summary_List[[i]]<-relabel_hist_summary
}

#Grid30_Grid_1200m_prop_relabel<-do.call("rbind",Grid_30km_Grid_1200m_prop_relabel_List)
Grid30_Grid_1200m_relabel_hist_summary<-do.call("rbind",Grid_30km_Grid_1200m_relabel_hist_summary_List)



### summarizing and comparing span of data
# Grid 30
Grid30_Grid_1200m_obs_hist_summary<-Grid30_Grid_1200m_obs_hist %>% group_by(Grid) %>% summarize(sum=sum(product))
Grid30_Grid_1200m_relabel_hist_summary2<-Grid30_Grid_1200m_relabel_hist_summary %>% group_by(Grid) %>% summarize(relabel_mean=mean(sum),upper=quantile(sum,probs=0.975),lower=quantile(sum,probs=0.025))
Grid30_Grid_1200m_hist_summary<-Grid30_Grid_1200m_obs_hist_summary %>% left_join(Grid30_Grid_1200m_relabel_hist_summary2,by="Grid") %>% 
  mutate(upper_diff=sum-upper, lower_diff=sum-lower, diff=sum-relabel_mean) %>%  
  mutate(upper_sign=sign(upper_diff),lower_sign=sign(lower_diff)) %>% mutate(sig=upper_sign+lower_sign)


### export results
write.csv(Grid30_Grid_1200m_hist_summary,file="./Output/Grid30_Grid_1200m_hist_summary.csv",row.names = FALSE)



#################### Grid 1500m ####################
### Grid 30
Grid_30km_Grid_1500m_prop_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_1500m_obs_hist_List<-vector("list",nrow(Grid_30km))

## calculate observed and expected
for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  if(nrow(Point_grid_subset@data)==0) next
  
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  Grid_1500m_prop<-PGS_data %>% group_by(Grid_1500m,Ind_Zone) %>% summarise(count=n()) %>% spread(Ind_Zone,count) %>% ungroup
  Grid_1500m_prop[is.na(Grid_1500m_prop)]<-0
  
  names<-c("Grid_1500m","Both","North","South")
  
  Missing<-setdiff(names,names(Grid_1500m_prop))
  Grid_1500m_prop[Missing]<-0
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  Grid_1500m_prop<-Grid_1500m_prop %>% mutate(Nprop=North/(North+South)) %>% filter(Nprop>=0)
  Grid_1500m_prop$Grid<-i
  
  Grid_1500m_obs_hist=hist(Grid_1500m_prop$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
  Grid_1500m_obs_hist_set<-data.frame(do.call("cbind",Grid_1500m_obs_hist[2:4]))
  Grid_1500m_obs_hist_set<-Grid_1500m_obs_hist_set %>% mutate(percent=counts/sum(counts))
  
  index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
  
  Grid_1500m_obs_hist_set$index<-index
  Grid_1500m_obs_hist_set<-mutate(Grid_1500m_obs_hist_set,product=percent*index) %>% mutate(Grid=i)
  
  Grid_30km_Grid_1500m_prop_List[[i]]<-Grid_1500m_prop
  Grid_30km_Grid_1500m_obs_hist_List[[i]]<-Grid_1500m_obs_hist_set
}

Grid30_Grid_1500m_prop<-do.call("rbind",Grid_30km_Grid_1500m_prop_List)
Grid30_Grid_1500m_obs_hist<-do.call("rbind",Grid_30km_Grid_1500m_obs_hist_List)


## random relabeling
#Grid_30km_Grid_1500m_prop_relabel_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_1500m_relabel_hist_summary_List<-vector("list",nrow(Grid_30km))

for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  # get data ready for relabeling
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  niter=1000
  NS_Grid_1500m_prop_List<-vector("list",niter)
  NS_Grid_1500m_prop_relabel_List<-vector("list",niter)
  NS_Grid_1500m_relabel_hist_List<-vector("list",niter)
  
  for (z in 1:niter){
    V<-sample(PGS_data$Ind_Zone)
    NS_Grid_1500m_relabel<-PGS_data %>% mutate(relabel=V) %>% 
      group_by(Grid_1500m,relabel) %>% summarise(count=n()) %>% spread(relabel,count) %>% ungroup
    NS_Grid_1500m_relabel[is.na(NS_Grid_1500m_relabel)]<-0
    
    if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
    if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
    
    NS_Grid_1500m_relabel<-NS_Grid_1500m_relabel %>% mutate(Nprop=North/(North+South))
    NS_Grid_1500m_relabel$Grid<-i
    NS_Grid_1500m_relabel$iter<-z
    
    Grid_1500m_relabel_hist=hist(NS_Grid_1500m_relabel$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
    Grid_1500m_relabel_hist_set<-data.frame(do.call("cbind",Grid_1500m_relabel_hist[2:4]))
    Grid_1500m_relabel_hist_set<-Grid_1500m_relabel_hist_set %>% mutate(percent=counts/sum(counts))
    
    index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
    
    Grid_1500m_relabel_hist_set$index<-index
    Grid_1500m_relabel_hist_set<-mutate(Grid_1500m_relabel_hist_set,product=percent*index) %>% mutate(iter=z)
    
    NS_Grid_1500m_prop_relabel_List[[z]]<-NS_Grid_1500m_relabel
    NS_Grid_1500m_relabel_hist_List[[z]]<-Grid_1500m_relabel_hist_set
  }
  
  NS_Grid_1500m_prop_relabel<-do.call("rbind",NS_Grid_1500m_prop_relabel_List)
  
  NS_Grid_1500m_relabel_hist<-do.call("rbind",NS_Grid_1500m_relabel_hist_List)
  NS_Grid_1500m_relabel_hist$Grid<-i
  
  relabel_hist_summary<-NS_Grid_1500m_relabel_hist %>% group_by(iter) %>% summarise(sum=sum(product))
  relabel_hist_summary$Grid<-i
  
  #Grid_30km_Grid_1500m_prop_relabel_List[[i]]<-NS_Grid_1500m_prop_relabel
  Grid_30km_Grid_1500m_relabel_hist_summary_List[[i]]<-relabel_hist_summary
}

#Grid30_Grid_1500m_prop_relabel<-do.call("rbind",Grid_30km_Grid_1500m_prop_relabel_List)
Grid30_Grid_1500m_relabel_hist_summary<-do.call("rbind",Grid_30km_Grid_1500m_relabel_hist_summary_List)



### summarizing and comparing span of data
# Grid 30
Grid30_Grid_1500m_obs_hist_summary<-Grid30_Grid_1500m_obs_hist %>% group_by(Grid) %>% summarize(sum=sum(product))
Grid30_Grid_1500m_relabel_hist_summary2<-Grid30_Grid_1500m_relabel_hist_summary %>% group_by(Grid) %>% summarize(relabel_mean=mean(sum),upper=quantile(sum,probs=0.975),lower=quantile(sum,probs=0.025))
Grid30_Grid_1500m_hist_summary<-Grid30_Grid_1500m_obs_hist_summary %>% left_join(Grid30_Grid_1500m_relabel_hist_summary2,by="Grid") %>% 
  mutate(upper_diff=sum-upper, lower_diff=sum-lower, diff=sum-relabel_mean) %>%  
  mutate(upper_sign=sign(upper_diff),lower_sign=sign(lower_diff)) %>% mutate(sig=upper_sign+lower_sign)


### export results
write.csv(Grid30_Grid_1500m_hist_summary,file="./Output/Grid30_Grid_1500m_hist_summary.csv",row.names = FALSE)



#################### Grid 2000m ####################
### Grid 30
Grid_30km_Grid_2000m_prop_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_2000m_obs_hist_List<-vector("list",nrow(Grid_30km))

## calculate observed and expected
for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  if(nrow(Point_grid_subset@data)==0) next
  
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  Grid_2000m_prop<-PGS_data %>% group_by(Grid_2000m,Ind_Zone) %>% summarise(count=n()) %>% spread(Ind_Zone,count) %>% ungroup
  Grid_2000m_prop[is.na(Grid_2000m_prop)]<-0
  
  names<-c("Grid_2000m","Both","North","South")
  
  Missing<-setdiff(names,names(Grid_2000m_prop))
  Grid_2000m_prop[Missing]<-0
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  Grid_2000m_prop<-Grid_2000m_prop %>% mutate(Nprop=North/(North+South)) %>% filter(Nprop>=0)
  Grid_2000m_prop$Grid<-i
  
  Grid_2000m_obs_hist=hist(Grid_2000m_prop$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
  Grid_2000m_obs_hist_set<-data.frame(do.call("cbind",Grid_2000m_obs_hist[2:4]))
  Grid_2000m_obs_hist_set<-Grid_2000m_obs_hist_set %>% mutate(percent=counts/sum(counts))
  
  index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
  
  Grid_2000m_obs_hist_set$index<-index
  Grid_2000m_obs_hist_set<-mutate(Grid_2000m_obs_hist_set,product=percent*index) %>% mutate(Grid=i)
  
  Grid_30km_Grid_2000m_prop_List[[i]]<-Grid_2000m_prop
  Grid_30km_Grid_2000m_obs_hist_List[[i]]<-Grid_2000m_obs_hist_set
}

Grid30_Grid_2000m_prop<-do.call("rbind",Grid_30km_Grid_2000m_prop_List)
Grid30_Grid_2000m_obs_hist<-do.call("rbind",Grid_30km_Grid_2000m_obs_hist_List)


## random relabeling
#Grid_30km_Grid_2000m_prop_relabel_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_2000m_relabel_hist_summary_List<-vector("list",nrow(Grid_30km))

for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  # get data ready for relabeling
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  niter=1000
  NS_Grid_2000m_prop_List<-vector("list",niter)
  NS_Grid_2000m_prop_relabel_List<-vector("list",niter)
  NS_Grid_2000m_relabel_hist_List<-vector("list",niter)
  
  for (z in 1:niter){
    V<-sample(PGS_data$Ind_Zone)
    NS_Grid_2000m_relabel<-PGS_data %>% mutate(relabel=V) %>% 
      group_by(Grid_2000m,relabel) %>% summarise(count=n()) %>% spread(relabel,count) %>% ungroup
    NS_Grid_2000m_relabel[is.na(NS_Grid_2000m_relabel)]<-0
    
    if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
    if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
    
    NS_Grid_2000m_relabel<-NS_Grid_2000m_relabel %>% mutate(Nprop=North/(North+South))
    NS_Grid_2000m_relabel$Grid<-i
    NS_Grid_2000m_relabel$iter<-z
    
    Grid_2000m_relabel_hist=hist(NS_Grid_2000m_relabel$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
    Grid_2000m_relabel_hist_set<-data.frame(do.call("cbind",Grid_2000m_relabel_hist[2:4]))
    Grid_2000m_relabel_hist_set<-Grid_2000m_relabel_hist_set %>% mutate(percent=counts/sum(counts))
    
    index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
    
    Grid_2000m_relabel_hist_set$index<-index
    Grid_2000m_relabel_hist_set<-mutate(Grid_2000m_relabel_hist_set,product=percent*index) %>% mutate(iter=z)
    
    NS_Grid_2000m_prop_relabel_List[[z]]<-NS_Grid_2000m_relabel
    NS_Grid_2000m_relabel_hist_List[[z]]<-Grid_2000m_relabel_hist_set
  }
  
  NS_Grid_2000m_prop_relabel<-do.call("rbind",NS_Grid_2000m_prop_relabel_List)
  
  NS_Grid_2000m_relabel_hist<-do.call("rbind",NS_Grid_2000m_relabel_hist_List)
  NS_Grid_2000m_relabel_hist$Grid<-i
  
  relabel_hist_summary<-NS_Grid_2000m_relabel_hist %>% group_by(iter) %>% summarise(sum=sum(product))
  relabel_hist_summary$Grid<-i
  
  #Grid_30km_Grid_2000m_prop_relabel_List[[i]]<-NS_Grid_2000m_prop_relabel
  Grid_30km_Grid_2000m_relabel_hist_summary_List[[i]]<-relabel_hist_summary
}

#Grid30_Grid_2000m_prop_relabel<-do.call("rbind",Grid_30km_Grid_2000m_prop_relabel_List)
Grid30_Grid_2000m_relabel_hist_summary<-do.call("rbind",Grid_30km_Grid_2000m_relabel_hist_summary_List)



### summarizing and comparing span of data
# Grid 30
Grid30_Grid_2000m_obs_hist_summary<-Grid30_Grid_2000m_obs_hist %>% group_by(Grid) %>% summarize(sum=sum(product))
Grid30_Grid_2000m_relabel_hist_summary2<-Grid30_Grid_2000m_relabel_hist_summary %>% group_by(Grid) %>% summarize(relabel_mean=mean(sum),upper=quantile(sum,probs=0.975),lower=quantile(sum,probs=0.025))
Grid30_Grid_2000m_hist_summary<-Grid30_Grid_2000m_obs_hist_summary %>% left_join(Grid30_Grid_2000m_relabel_hist_summary2,by="Grid") %>% 
  mutate(upper_diff=sum-upper, lower_diff=sum-lower, diff=sum-relabel_mean) %>%  
  mutate(upper_sign=sign(upper_diff),lower_sign=sign(lower_diff)) %>% mutate(sig=upper_sign+lower_sign)


### export results
write.csv(Grid30_Grid_2000m_hist_summary,file="./Output/Grid30_Grid_2000m_hist_summary.csv",row.names = FALSE)



#################### Grid 2500m ####################
### Grid 30
Grid_30km_Grid_2500m_prop_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_2500m_obs_hist_List<-vector("list",nrow(Grid_30km))

## calculate observed and expected
for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  if(nrow(Point_grid_subset@data)==0) next
  
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  Grid_2500m_prop<-PGS_data %>% group_by(Grid_2500m,Ind_Zone) %>% summarise(count=n()) %>% spread(Ind_Zone,count) %>% ungroup
  Grid_2500m_prop[is.na(Grid_2500m_prop)]<-0
  
  names<-c("Grid_2500m","Both","North","South")
  
  Missing<-setdiff(names,names(Grid_2500m_prop))
  Grid_2500m_prop[Missing]<-0
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  Grid_2500m_prop<-Grid_2500m_prop %>% mutate(Nprop=North/(North+South)) %>% filter(Nprop>=0)
  Grid_2500m_prop$Grid<-i
  
  Grid_2500m_obs_hist=hist(Grid_2500m_prop$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
  Grid_2500m_obs_hist_set<-data.frame(do.call("cbind",Grid_2500m_obs_hist[2:4]))
  Grid_2500m_obs_hist_set<-Grid_2500m_obs_hist_set %>% mutate(percent=counts/sum(counts))
  
  index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
  
  Grid_2500m_obs_hist_set$index<-index
  Grid_2500m_obs_hist_set<-mutate(Grid_2500m_obs_hist_set,product=percent*index) %>% mutate(Grid=i)
  
  Grid_30km_Grid_2500m_prop_List[[i]]<-Grid_2500m_prop
  Grid_30km_Grid_2500m_obs_hist_List[[i]]<-Grid_2500m_obs_hist_set
}

Grid30_Grid_2500m_prop<-do.call("rbind",Grid_30km_Grid_2500m_prop_List)
Grid30_Grid_2500m_obs_hist<-do.call("rbind",Grid_30km_Grid_2500m_obs_hist_List)


## random relabeling
#Grid_30km_Grid_2500m_prop_relabel_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_2500m_relabel_hist_summary_List<-vector("list",nrow(Grid_30km))

for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  # get data ready for relabeling
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  niter=1000
  NS_Grid_2500m_prop_List<-vector("list",niter)
  NS_Grid_2500m_prop_relabel_List<-vector("list",niter)
  NS_Grid_2500m_relabel_hist_List<-vector("list",niter)
  
  for (z in 1:niter){
    V<-sample(PGS_data$Ind_Zone)
    NS_Grid_2500m_relabel<-PGS_data %>% mutate(relabel=V) %>% 
      group_by(Grid_2500m,relabel) %>% summarise(count=n()) %>% spread(relabel,count) %>% ungroup
    NS_Grid_2500m_relabel[is.na(NS_Grid_2500m_relabel)]<-0
    
    if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
    if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
    
    NS_Grid_2500m_relabel<-NS_Grid_2500m_relabel %>% mutate(Nprop=North/(North+South))
    NS_Grid_2500m_relabel$Grid<-i
    NS_Grid_2500m_relabel$iter<-z
    
    Grid_2500m_relabel_hist=hist(NS_Grid_2500m_relabel$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
    Grid_2500m_relabel_hist_set<-data.frame(do.call("cbind",Grid_2500m_relabel_hist[2:4]))
    Grid_2500m_relabel_hist_set<-Grid_2500m_relabel_hist_set %>% mutate(percent=counts/sum(counts))
    
    index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
    
    Grid_2500m_relabel_hist_set$index<-index
    Grid_2500m_relabel_hist_set<-mutate(Grid_2500m_relabel_hist_set,product=percent*index) %>% mutate(iter=z)
    
    NS_Grid_2500m_prop_relabel_List[[z]]<-NS_Grid_2500m_relabel
    NS_Grid_2500m_relabel_hist_List[[z]]<-Grid_2500m_relabel_hist_set
  }
  
  NS_Grid_2500m_prop_relabel<-do.call("rbind",NS_Grid_2500m_prop_relabel_List)
  
  NS_Grid_2500m_relabel_hist<-do.call("rbind",NS_Grid_2500m_relabel_hist_List)
  NS_Grid_2500m_relabel_hist$Grid<-i
  
  relabel_hist_summary<-NS_Grid_2500m_relabel_hist %>% group_by(iter) %>% summarise(sum=sum(product))
  relabel_hist_summary$Grid<-i
  
  #Grid_30km_Grid_2500m_prop_relabel_List[[i]]<-NS_Grid_2500m_prop_relabel
  Grid_30km_Grid_2500m_relabel_hist_summary_List[[i]]<-relabel_hist_summary
}

#Grid30_Grid_2500m_prop_relabel<-do.call("rbind",Grid_30km_Grid_2500m_prop_relabel_List)
Grid30_Grid_2500m_relabel_hist_summary<-do.call("rbind",Grid_30km_Grid_2500m_relabel_hist_summary_List)



### summarizing and comparing span of data
# Grid 30
Grid30_Grid_2500m_obs_hist_summary<-Grid30_Grid_2500m_obs_hist %>% group_by(Grid) %>% summarize(sum=sum(product))
Grid30_Grid_2500m_relabel_hist_summary2<-Grid30_Grid_2500m_relabel_hist_summary %>% group_by(Grid) %>% summarize(relabel_mean=mean(sum),upper=quantile(sum,probs=0.975),lower=quantile(sum,probs=0.025))
Grid30_Grid_2500m_hist_summary<-Grid30_Grid_2500m_obs_hist_summary %>% left_join(Grid30_Grid_2500m_relabel_hist_summary2,by="Grid") %>% 
  mutate(upper_diff=sum-upper, lower_diff=sum-lower, diff=sum-relabel_mean) %>%  
  mutate(upper_sign=sign(upper_diff),lower_sign=sign(lower_diff)) %>% mutate(sig=upper_sign+lower_sign)


### export results
#write.csv(Grid12_Grid_2500m_hist_summary,file="./Output/Grid12_Grid_2500m_hist_summary.csv",row.names = FALSE)
write.csv(Grid30_Grid_2500m_hist_summary,file="./Output/Grid30_Grid_2500m_hist_summary.csv",row.names = FALSE)


#################### Grid 3000m ####################
### Grid 30
Grid_30km_Grid_3000m_prop_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_3000m_obs_hist_List<-vector("list",nrow(Grid_30km))

## calculate observed and expected
for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  if(nrow(Point_grid_subset@data)==0) next
  
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  Grid_3000m_prop<-PGS_data %>% group_by(Grid_3000m,Ind_Zone) %>% summarise(count=n()) %>% spread(Ind_Zone,count) %>% ungroup
  Grid_3000m_prop[is.na(Grid_3000m_prop)]<-0
  
  names<-c("Grid_3000m","Both","North","South")
  
  Missing<-setdiff(names,names(Grid_3000m_prop))
  Grid_3000m_prop[Missing]<-0
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  Grid_3000m_prop<-Grid_3000m_prop %>% mutate(Nprop=North/(North+South)) %>% filter(Nprop>=0)
  Grid_3000m_prop$Grid<-i
  
  Grid_3000m_obs_hist=hist(Grid_3000m_prop$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
  Grid_3000m_obs_hist_set<-data.frame(do.call("cbind",Grid_3000m_obs_hist[2:4]))
  Grid_3000m_obs_hist_set<-Grid_3000m_obs_hist_set %>% mutate(percent=counts/sum(counts))
  
  index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
  
  Grid_3000m_obs_hist_set$index<-index
  Grid_3000m_obs_hist_set<-mutate(Grid_3000m_obs_hist_set,product=percent*index) %>% mutate(Grid=i)
  
  Grid_30km_Grid_3000m_prop_List[[i]]<-Grid_3000m_prop
  Grid_30km_Grid_3000m_obs_hist_List[[i]]<-Grid_3000m_obs_hist_set
}

Grid30_Grid_3000m_prop<-do.call("rbind",Grid_30km_Grid_3000m_prop_List)
Grid30_Grid_3000m_obs_hist<-do.call("rbind",Grid_30km_Grid_3000m_obs_hist_List)


## random relabeling
#Grid_30km_Grid_3000m_prop_relabel_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_3000m_relabel_hist_summary_List<-vector("list",nrow(Grid_30km))

for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  # get data ready for relabeling
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  niter=1000
  NS_Grid_3000m_prop_List<-vector("list",niter)
  NS_Grid_3000m_prop_relabel_List<-vector("list",niter)
  NS_Grid_3000m_relabel_hist_List<-vector("list",niter)
  
  for (z in 1:niter){
    V<-sample(PGS_data$Ind_Zone)
    NS_Grid_3000m_relabel<-PGS_data %>% mutate(relabel=V) %>% 
      group_by(Grid_3000m,relabel) %>% summarise(count=n()) %>% spread(relabel,count) %>% ungroup
    NS_Grid_3000m_relabel[is.na(NS_Grid_3000m_relabel)]<-0
    
    if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
    if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
    
    NS_Grid_3000m_relabel<-NS_Grid_3000m_relabel %>% mutate(Nprop=North/(North+South))
    NS_Grid_3000m_relabel$Grid<-i
    NS_Grid_3000m_relabel$iter<-z
    
    Grid_3000m_relabel_hist=hist(NS_Grid_3000m_relabel$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
    Grid_3000m_relabel_hist_set<-data.frame(do.call("cbind",Grid_3000m_relabel_hist[2:4]))
    Grid_3000m_relabel_hist_set<-Grid_3000m_relabel_hist_set %>% mutate(percent=counts/sum(counts))
    
    index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
    
    Grid_3000m_relabel_hist_set$index<-index
    Grid_3000m_relabel_hist_set<-mutate(Grid_3000m_relabel_hist_set,product=percent*index) %>% mutate(iter=z)
    
    NS_Grid_3000m_prop_relabel_List[[z]]<-NS_Grid_3000m_relabel
    NS_Grid_3000m_relabel_hist_List[[z]]<-Grid_3000m_relabel_hist_set
  }
  
  NS_Grid_3000m_prop_relabel<-do.call("rbind",NS_Grid_3000m_prop_relabel_List)
  
  NS_Grid_3000m_relabel_hist<-do.call("rbind",NS_Grid_3000m_relabel_hist_List)
  NS_Grid_3000m_relabel_hist$Grid<-i
  
  relabel_hist_summary<-NS_Grid_3000m_relabel_hist %>% group_by(iter) %>% summarise(sum=sum(product))
  relabel_hist_summary$Grid<-i
  
  #Grid_30km_Grid_3000m_prop_relabel_List[[i]]<-NS_Grid_3000m_prop_relabel
  Grid_30km_Grid_3000m_relabel_hist_summary_List[[i]]<-relabel_hist_summary
}

#Grid30_Grid_3000m_prop_relabel<-do.call("rbind",Grid_30km_Grid_3000m_prop_relabel_List)
Grid30_Grid_3000m_relabel_hist_summary<-do.call("rbind",Grid_30km_Grid_3000m_relabel_hist_summary_List)



### summarizing and comparing span of data
# Grid 30
Grid30_Grid_3000m_obs_hist_summary<-Grid30_Grid_3000m_obs_hist %>% group_by(Grid) %>% summarize(sum=sum(product))
Grid30_Grid_3000m_relabel_hist_summary2<-Grid30_Grid_3000m_relabel_hist_summary %>% group_by(Grid) %>% summarize(relabel_mean=mean(sum),upper=quantile(sum,probs=0.975),lower=quantile(sum,probs=0.025))
Grid30_Grid_3000m_hist_summary<-Grid30_Grid_3000m_obs_hist_summary %>% left_join(Grid30_Grid_3000m_relabel_hist_summary2,by="Grid") %>% 
  mutate(upper_diff=sum-upper, lower_diff=sum-lower, diff=sum-relabel_mean) %>%  
  mutate(upper_sign=sign(upper_diff),lower_sign=sign(lower_diff)) %>% mutate(sig=upper_sign+lower_sign)


### export results
write.csv(Grid30_Grid_3000m_hist_summary,file="./Output/Grid30_Grid_3000m_hist_summary.csv",row.names = FALSE)


#################### Grid 3750m ####################
### Grid 30
Grid_30km_Grid_3750m_prop_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_3750m_obs_hist_List<-vector("list",nrow(Grid_30km))

## calculate observed and expected
for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  if(nrow(Point_grid_subset@data)==0) next
  
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  Grid_3750m_prop<-PGS_data %>% group_by(Grid_3750m,Ind_Zone) %>% summarise(count=n()) %>% spread(Ind_Zone,count) %>% ungroup
  Grid_3750m_prop[is.na(Grid_3750m_prop)]<-0
  
  names<-c("Grid_3750m","Both","North","South")
  
  Missing<-setdiff(names,names(Grid_3750m_prop))
  Grid_3750m_prop[Missing]<-0
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  Grid_3750m_prop<-Grid_3750m_prop %>% mutate(Nprop=North/(North+South)) %>% filter(Nprop>=0)
  Grid_3750m_prop$Grid<-i
  
  Grid_3750m_obs_hist=hist(Grid_3750m_prop$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
  Grid_3750m_obs_hist_set<-data.frame(do.call("cbind",Grid_3750m_obs_hist[2:4]))
  Grid_3750m_obs_hist_set<-Grid_3750m_obs_hist_set %>% mutate(percent=counts/sum(counts))
  
  index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
  
  Grid_3750m_obs_hist_set$index<-index
  Grid_3750m_obs_hist_set<-mutate(Grid_3750m_obs_hist_set,product=percent*index) %>% mutate(Grid=i)
  
  Grid_30km_Grid_3750m_prop_List[[i]]<-Grid_3750m_prop
  Grid_30km_Grid_3750m_obs_hist_List[[i]]<-Grid_3750m_obs_hist_set
}

Grid30_Grid_3750m_prop<-do.call("rbind",Grid_30km_Grid_3750m_prop_List)
Grid30_Grid_3750m_obs_hist<-do.call("rbind",Grid_30km_Grid_3750m_obs_hist_List)


## random relabeling
#Grid_30km_Grid_3750m_prop_relabel_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_3750m_relabel_hist_summary_List<-vector("list",nrow(Grid_30km))

for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  # get data ready for relabeling
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  niter=1000
  NS_Grid_3750m_prop_List<-vector("list",niter)
  NS_Grid_3750m_prop_relabel_List<-vector("list",niter)
  NS_Grid_3750m_relabel_hist_List<-vector("list",niter)
  
  for (z in 1:niter){
    V<-sample(PGS_data$Ind_Zone)
    NS_Grid_3750m_relabel<-PGS_data %>% mutate(relabel=V) %>% 
      group_by(Grid_3750m,relabel) %>% summarise(count=n()) %>% spread(relabel,count) %>% ungroup
    NS_Grid_3750m_relabel[is.na(NS_Grid_3750m_relabel)]<-0
    
    if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
    if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
    
    NS_Grid_3750m_relabel<-NS_Grid_3750m_relabel %>% mutate(Nprop=North/(North+South))
    NS_Grid_3750m_relabel$Grid<-i
    NS_Grid_3750m_relabel$iter<-z
    
    Grid_3750m_relabel_hist=hist(NS_Grid_3750m_relabel$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
    Grid_3750m_relabel_hist_set<-data.frame(do.call("cbind",Grid_3750m_relabel_hist[2:4]))
    Grid_3750m_relabel_hist_set<-Grid_3750m_relabel_hist_set %>% mutate(percent=counts/sum(counts))
    
    index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
    
    Grid_3750m_relabel_hist_set$index<-index
    Grid_3750m_relabel_hist_set<-mutate(Grid_3750m_relabel_hist_set,product=percent*index) %>% mutate(iter=z)
    
    NS_Grid_3750m_prop_relabel_List[[z]]<-NS_Grid_3750m_relabel
    NS_Grid_3750m_relabel_hist_List[[z]]<-Grid_3750m_relabel_hist_set
  }
  
  NS_Grid_3750m_prop_relabel<-do.call("rbind",NS_Grid_3750m_prop_relabel_List)
  
  NS_Grid_3750m_relabel_hist<-do.call("rbind",NS_Grid_3750m_relabel_hist_List)
  NS_Grid_3750m_relabel_hist$Grid<-i
  
  relabel_hist_summary<-NS_Grid_3750m_relabel_hist %>% group_by(iter) %>% summarise(sum=sum(product))
  relabel_hist_summary$Grid<-i
  
  #Grid_30km_Grid_3750m_prop_relabel_List[[i]]<-NS_Grid_3750m_prop_relabel
  Grid_30km_Grid_3750m_relabel_hist_summary_List[[i]]<-relabel_hist_summary
}

#Grid30_Grid_3750m_prop_relabel<-do.call("rbind",Grid_30km_Grid_3750m_prop_relabel_List)
Grid30_Grid_3750m_relabel_hist_summary<-do.call("rbind",Grid_30km_Grid_3750m_relabel_hist_summary_List)



### summarizing and comparing span of data
# Grid 30
Grid30_Grid_3750m_obs_hist_summary<-Grid30_Grid_3750m_obs_hist %>% group_by(Grid) %>% summarize(sum=sum(product))
Grid30_Grid_3750m_relabel_hist_summary2<-Grid30_Grid_3750m_relabel_hist_summary %>% group_by(Grid) %>% summarize(relabel_mean=mean(sum),upper=quantile(sum,probs=0.975),lower=quantile(sum,probs=0.025))
Grid30_Grid_3750m_hist_summary<-Grid30_Grid_3750m_obs_hist_summary %>% left_join(Grid30_Grid_3750m_relabel_hist_summary2,by="Grid") %>% 
  mutate(upper_diff=sum-upper, lower_diff=sum-lower, diff=sum-relabel_mean) %>%  
  mutate(upper_sign=sign(upper_diff),lower_sign=sign(lower_diff)) %>% mutate(sig=upper_sign+lower_sign)


### export results
write.csv(Grid30_Grid_3750m_hist_summary,file="./Output/Grid30_Grid_3750m_hist_summary.csv",row.names = FALSE)



#################### Grid 4285m ####################
### Grid 30
Grid_30km_Grid_4285m_prop_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_4285m_obs_hist_List<-vector("list",nrow(Grid_30km))

## calculate observed and expected
for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  if(nrow(Point_grid_subset@data)==0) next
  
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  Grid_4285m_prop<-PGS_data %>% group_by(Grid_4285m,Ind_Zone) %>% summarise(count=n()) %>% spread(Ind_Zone,count) %>% ungroup
  Grid_4285m_prop[is.na(Grid_4285m_prop)]<-0
  
  names<-c("Grid_4285m","Both","North","South")
  
  Missing<-setdiff(names,names(Grid_4285m_prop))
  Grid_4285m_prop[Missing]<-0
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  Grid_4285m_prop<-Grid_4285m_prop %>% mutate(Nprop=North/(North+South)) %>% filter(Nprop>=0)
  Grid_4285m_prop$Grid<-i
  
  Grid_4285m_obs_hist=hist(Grid_4285m_prop$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
  Grid_4285m_obs_hist_set<-data.frame(do.call("cbind",Grid_4285m_obs_hist[2:4]))
  Grid_4285m_obs_hist_set<-Grid_4285m_obs_hist_set %>% mutate(percent=counts/sum(counts))
  
  index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
  
  Grid_4285m_obs_hist_set$index<-index
  Grid_4285m_obs_hist_set<-mutate(Grid_4285m_obs_hist_set,product=percent*index) %>% mutate(Grid=i)
  
  Grid_30km_Grid_4285m_prop_List[[i]]<-Grid_4285m_prop
  Grid_30km_Grid_4285m_obs_hist_List[[i]]<-Grid_4285m_obs_hist_set
}

Grid30_Grid_4285m_prop<-do.call("rbind",Grid_30km_Grid_4285m_prop_List)
Grid30_Grid_4285m_obs_hist<-do.call("rbind",Grid_30km_Grid_4285m_obs_hist_List)


## random relabeling
#Grid_30km_Grid_4285m_prop_relabel_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_4285m_relabel_hist_summary_List<-vector("list",nrow(Grid_30km))

for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  # get data ready for relabeling
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  niter=1000
  NS_Grid_4285m_prop_List<-vector("list",niter)
  NS_Grid_4285m_prop_relabel_List<-vector("list",niter)
  NS_Grid_4285m_relabel_hist_List<-vector("list",niter)
  
  for (z in 1:niter){
    V<-sample(PGS_data$Ind_Zone)
    NS_Grid_4285m_relabel<-PGS_data %>% mutate(relabel=V) %>% 
      group_by(Grid_4285m,relabel) %>% summarise(count=n()) %>% spread(relabel,count) %>% ungroup
    NS_Grid_4285m_relabel[is.na(NS_Grid_4285m_relabel)]<-0
    
    if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
    if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
    
    NS_Grid_4285m_relabel<-NS_Grid_4285m_relabel %>% mutate(Nprop=North/(North+South))
    NS_Grid_4285m_relabel$Grid<-i
    NS_Grid_4285m_relabel$iter<-z
    
    Grid_4285m_relabel_hist=hist(NS_Grid_4285m_relabel$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
    Grid_4285m_relabel_hist_set<-data.frame(do.call("cbind",Grid_4285m_relabel_hist[2:4]))
    Grid_4285m_relabel_hist_set<-Grid_4285m_relabel_hist_set %>% mutate(percent=counts/sum(counts))
    
    index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
    
    Grid_4285m_relabel_hist_set$index<-index
    Grid_4285m_relabel_hist_set<-mutate(Grid_4285m_relabel_hist_set,product=percent*index) %>% mutate(iter=z)
    
    NS_Grid_4285m_prop_relabel_List[[z]]<-NS_Grid_4285m_relabel
    NS_Grid_4285m_relabel_hist_List[[z]]<-Grid_4285m_relabel_hist_set
  }
  
  NS_Grid_4285m_prop_relabel<-do.call("rbind",NS_Grid_4285m_prop_relabel_List)
  
  NS_Grid_4285m_relabel_hist<-do.call("rbind",NS_Grid_4285m_relabel_hist_List)
  NS_Grid_4285m_relabel_hist$Grid<-i
  
  relabel_hist_summary<-NS_Grid_4285m_relabel_hist %>% group_by(iter) %>% summarise(sum=sum(product))
  relabel_hist_summary$Grid<-i
  
  #Grid_30km_Grid_4285m_prop_relabel_List[[i]]<-NS_Grid_4285m_prop_relabel
  Grid_30km_Grid_4285m_relabel_hist_summary_List[[i]]<-relabel_hist_summary
}

#Grid30_Grid_4285m_prop_relabel<-do.call("rbind",Grid_30km_Grid_4285m_prop_relabel_List)
Grid30_Grid_4285m_relabel_hist_summary<-do.call("rbind",Grid_30km_Grid_4285m_relabel_hist_summary_List)



### summarizing and comparing span of data
# Grid 30
Grid30_Grid_4285m_obs_hist_summary<-Grid30_Grid_4285m_obs_hist %>% group_by(Grid) %>% summarize(sum=sum(product))
Grid30_Grid_4285m_relabel_hist_summary2<-Grid30_Grid_4285m_relabel_hist_summary %>% group_by(Grid) %>% summarize(relabel_mean=mean(sum),upper=quantile(sum,probs=0.975),lower=quantile(sum,probs=0.025))
Grid30_Grid_4285m_hist_summary<-Grid30_Grid_4285m_obs_hist_summary %>% left_join(Grid30_Grid_4285m_relabel_hist_summary2,by="Grid") %>% 
  mutate(upper_diff=sum-upper, lower_diff=sum-lower, diff=sum-relabel_mean) %>%  
  mutate(upper_sign=sign(upper_diff),lower_sign=sign(lower_diff)) %>% mutate(sig=upper_sign+lower_sign)


### export results
write.csv(Grid30_Grid_4285m_hist_summary,file="./Output/Grid30_Grid_4285m_hist_summary.csv",row.names = FALSE)



#################### Grid 5000m ####################
### Grid 30
Grid_30km_Grid_5000m_prop_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_5000m_obs_hist_List<-vector("list",nrow(Grid_30km))

## calculate observed and expected
for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  if(nrow(Point_grid_subset@data)==0) next
  
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  Grid_5000m_prop<-PGS_data %>% group_by(Grid_5000m,Ind_Zone) %>% summarise(count=n()) %>% spread(Ind_Zone,count) %>% ungroup
  Grid_5000m_prop[is.na(Grid_5000m_prop)]<-0
  
  names<-c("Grid_5000m","Both","North","South")
  
  Missing<-setdiff(names,names(Grid_5000m_prop))
  Grid_5000m_prop[Missing]<-0
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  Grid_5000m_prop<-Grid_5000m_prop %>% mutate(Nprop=North/(North+South)) %>% filter(Nprop>=0)
  Grid_5000m_prop$Grid<-i
  
  Grid_5000m_obs_hist=hist(Grid_5000m_prop$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
  Grid_5000m_obs_hist_set<-data.frame(do.call("cbind",Grid_5000m_obs_hist[2:4]))
  Grid_5000m_obs_hist_set<-Grid_5000m_obs_hist_set %>% mutate(percent=counts/sum(counts))
  
  index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
  
  Grid_5000m_obs_hist_set$index<-index
  Grid_5000m_obs_hist_set<-mutate(Grid_5000m_obs_hist_set,product=percent*index) %>% mutate(Grid=i)
  
  Grid_30km_Grid_5000m_prop_List[[i]]<-Grid_5000m_prop
  Grid_30km_Grid_5000m_obs_hist_List[[i]]<-Grid_5000m_obs_hist_set
}

Grid30_Grid_5000m_prop<-do.call("rbind",Grid_30km_Grid_5000m_prop_List)
Grid30_Grid_5000m_obs_hist<-do.call("rbind",Grid_30km_Grid_5000m_obs_hist_List)


## random relabeling
#Grid_30km_Grid_5000m_prop_relabel_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_5000m_relabel_hist_summary_List<-vector("list",nrow(Grid_30km))

## calculate observed and expected
for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  # get data ready for relabeling
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  niter=1000
  NS_Grid_5000m_prop_List<-vector("list",niter)
  NS_Grid_5000m_prop_relabel_List<-vector("list",niter)
  NS_Grid_5000m_relabel_hist_List<-vector("list",niter)
  
  for (z in 1:niter){
    V<-sample(PGS_data$Ind_Zone)
    NS_Grid_5000m_relabel<-PGS_data %>% mutate(relabel=V) %>% 
      group_by(Grid_5000m,relabel) %>% summarise(count=n()) %>% spread(relabel,count) %>% ungroup
    NS_Grid_5000m_relabel[is.na(NS_Grid_5000m_relabel)]<-0
    
    if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
    if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
    
    NS_Grid_5000m_relabel<-NS_Grid_5000m_relabel %>% mutate(Nprop=North/(North+South))
    NS_Grid_5000m_relabel$Grid<-i
    NS_Grid_5000m_relabel$iter<-z
    
    Grid_5000m_relabel_hist=hist(NS_Grid_5000m_relabel$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
    Grid_5000m_relabel_hist_set<-data.frame(do.call("cbind",Grid_5000m_relabel_hist[2:4]))
    Grid_5000m_relabel_hist_set<-Grid_5000m_relabel_hist_set %>% mutate(percent=counts/sum(counts))
    
    index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
    
    Grid_5000m_relabel_hist_set$index<-index
    Grid_5000m_relabel_hist_set<-mutate(Grid_5000m_relabel_hist_set,product=percent*index) %>% mutate(iter=z)
    
    NS_Grid_5000m_prop_relabel_List[[z]]<-NS_Grid_5000m_relabel
    NS_Grid_5000m_relabel_hist_List[[z]]<-Grid_5000m_relabel_hist_set
  }
  
  NS_Grid_5000m_prop_relabel<-do.call("rbind",NS_Grid_5000m_prop_relabel_List)
  
  NS_Grid_5000m_relabel_hist<-do.call("rbind",NS_Grid_5000m_relabel_hist_List)
  NS_Grid_5000m_relabel_hist$Grid<-i
  
  relabel_hist_summary<-NS_Grid_5000m_relabel_hist %>% group_by(iter) %>% summarise(sum=sum(product))
  relabel_hist_summary$Grid<-i
  
  #Grid_30km_Grid_5000m_prop_relabel_List[[i]]<-NS_Grid_5000m_prop_relabel
  Grid_30km_Grid_5000m_relabel_hist_summary_List[[i]]<-relabel_hist_summary
}

#Grid30_Grid_5000m_prop_relabel<-do.call("rbind",Grid_30km_Grid_5000m_prop_relabel_List)
Grid30_Grid_5000m_relabel_hist_summary<-do.call("rbind",Grid_30km_Grid_5000m_relabel_hist_summary_List)



### summarizing and comparing span of data
# Grid 30
Grid30_Grid_5000m_obs_hist_summary<-Grid30_Grid_5000m_obs_hist %>% group_by(Grid) %>% summarize(sum=sum(product))
Grid30_Grid_5000m_relabel_hist_summary2<-Grid30_Grid_5000m_relabel_hist_summary %>% group_by(Grid) %>% summarize(relabel_mean=mean(sum),upper=quantile(sum,probs=0.975),lower=quantile(sum,probs=0.025))
Grid30_Grid_5000m_hist_summary<-Grid30_Grid_5000m_obs_hist_summary %>% left_join(Grid30_Grid_5000m_relabel_hist_summary2,by="Grid") %>% 
  mutate(upper_diff=sum-upper, lower_diff=sum-lower, diff=sum-relabel_mean) %>%  
  mutate(upper_sign=sign(upper_diff),lower_sign=sign(lower_diff)) %>% mutate(sig=upper_sign+lower_sign)


### export results
write.csv(Grid30_Grid_5000m_hist_summary,file="./Output/Grid30_Grid_5000m_hist_summary.csv",row.names = FALSE)



#################### Grid 6000m ####################
### Grid 30
Grid_30km_Grid_6000m_prop_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_6000m_obs_hist_List<-vector("list",nrow(Grid_30km))

## calculate observed and expected
for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  if(nrow(Point_grid_subset@data)==0) next
  
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  Grid_6000m_prop<-PGS_data %>% group_by(Grid_6000m,Ind_Zone) %>% summarise(count=n()) %>% spread(Ind_Zone,count) %>% ungroup
  Grid_6000m_prop[is.na(Grid_6000m_prop)]<-0
  
  names<-c("Grid_6000m","Both","North","South")
  
  Missing<-setdiff(names,names(Grid_6000m_prop))
  Grid_6000m_prop[Missing]<-0
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  Grid_6000m_prop<-Grid_6000m_prop %>% mutate(Nprop=North/(North+South)) %>% filter(Nprop>=0)
  Grid_6000m_prop$Grid<-i
  
  Grid_6000m_obs_hist=hist(Grid_6000m_prop$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
  Grid_6000m_obs_hist_set<-data.frame(do.call("cbind",Grid_6000m_obs_hist[2:4]))
  Grid_6000m_obs_hist_set<-Grid_6000m_obs_hist_set %>% mutate(percent=counts/sum(counts))
  
  index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
  
  Grid_6000m_obs_hist_set$index<-index
  Grid_6000m_obs_hist_set<-mutate(Grid_6000m_obs_hist_set,product=percent*index) %>% mutate(Grid=i)
  
  Grid_30km_Grid_6000m_prop_List[[i]]<-Grid_6000m_prop
  Grid_30km_Grid_6000m_obs_hist_List[[i]]<-Grid_6000m_obs_hist_set
}

Grid30_Grid_6000m_prop<-do.call("rbind",Grid_30km_Grid_6000m_prop_List)
Grid30_Grid_6000m_obs_hist<-do.call("rbind",Grid_30km_Grid_6000m_obs_hist_List)


## random relabeling
#Grid_30km_Grid_6000m_prop_relabel_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_6000m_relabel_hist_summary_List<-vector("list",nrow(Grid_30km))

for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  # get data ready for relabeling
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  niter=1000
  NS_Grid_6000m_prop_List<-vector("list",niter)
  NS_Grid_6000m_prop_relabel_List<-vector("list",niter)
  NS_Grid_6000m_relabel_hist_List<-vector("list",niter)
  
  for (z in 1:niter){
    V<-sample(PGS_data$Ind_Zone)
    NS_Grid_6000m_relabel<-PGS_data %>% mutate(relabel=V) %>% 
      group_by(Grid_6000m,relabel) %>% summarise(count=n()) %>% spread(relabel,count) %>% ungroup
    NS_Grid_6000m_relabel[is.na(NS_Grid_6000m_relabel)]<-0
    
    if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
    if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
    
    NS_Grid_6000m_relabel<-NS_Grid_6000m_relabel %>% mutate(Nprop=North/(North+South))
    NS_Grid_6000m_relabel$Grid<-i
    NS_Grid_6000m_relabel$iter<-z
    
    Grid_6000m_relabel_hist=hist(NS_Grid_6000m_relabel$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
    Grid_6000m_relabel_hist_set<-data.frame(do.call("cbind",Grid_6000m_relabel_hist[2:4]))
    Grid_6000m_relabel_hist_set<-Grid_6000m_relabel_hist_set %>% mutate(percent=counts/sum(counts))
    
    index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
    
    Grid_6000m_relabel_hist_set$index<-index
    Grid_6000m_relabel_hist_set<-mutate(Grid_6000m_relabel_hist_set,product=percent*index) %>% mutate(iter=z)
    
    NS_Grid_6000m_prop_relabel_List[[z]]<-NS_Grid_6000m_relabel
    NS_Grid_6000m_relabel_hist_List[[z]]<-Grid_6000m_relabel_hist_set
  }
  
  NS_Grid_6000m_prop_relabel<-do.call("rbind",NS_Grid_6000m_prop_relabel_List)
  
  NS_Grid_6000m_relabel_hist<-do.call("rbind",NS_Grid_6000m_relabel_hist_List)
  NS_Grid_6000m_relabel_hist$Grid<-i
  
  relabel_hist_summary<-NS_Grid_6000m_relabel_hist %>% group_by(iter) %>% summarise(sum=sum(product))
  relabel_hist_summary$Grid<-i
  
  #Grid_30km_Grid_6000m_prop_relabel_List[[i]]<-NS_Grid_6000m_prop_relabel
  Grid_30km_Grid_6000m_relabel_hist_summary_List[[i]]<-relabel_hist_summary
}

#Grid30_Grid_6000m_prop_relabel<-do.call("rbind",Grid_30km_Grid_6000m_prop_relabel_List)
Grid30_Grid_6000m_relabel_hist_summary<-do.call("rbind",Grid_30km_Grid_6000m_relabel_hist_summary_List)



### summarizing and comparing span of data
# Grid 30
Grid30_Grid_6000m_obs_hist_summary<-Grid30_Grid_6000m_obs_hist %>% group_by(Grid) %>% summarize(sum=sum(product))
Grid30_Grid_6000m_relabel_hist_summary2<-Grid30_Grid_6000m_relabel_hist_summary %>% group_by(Grid) %>% summarize(relabel_mean=mean(sum),upper=quantile(sum,probs=0.975),lower=quantile(sum,probs=0.025))
Grid30_Grid_6000m_hist_summary<-Grid30_Grid_6000m_obs_hist_summary %>% left_join(Grid30_Grid_6000m_relabel_hist_summary2,by="Grid") %>% 
  mutate(upper_diff=sum-upper, lower_diff=sum-lower, diff=sum-relabel_mean) %>%  
  mutate(upper_sign=sign(upper_diff),lower_sign=sign(lower_diff)) %>% mutate(sig=upper_sign+lower_sign)


### export results
write.csv(Grid30_Grid_6000m_hist_summary,file="./Output/Grid30_Grid_6000m_hist_summary.csv",row.names = FALSE)



#################### Grid 7500m ####################
### Grid 30
Grid_30km_Grid_7500m_prop_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_7500m_obs_hist_List<-vector("list",nrow(Grid_30km))

## calculate observed and expected
for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  if(nrow(Point_grid_subset@data)==0) next
  
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  Grid_7500m_prop<-PGS_data %>% group_by(Grid_7500m,Ind_Zone) %>% summarise(count=n()) %>% spread(Ind_Zone,count) %>% ungroup
  Grid_7500m_prop[is.na(Grid_7500m_prop)]<-0
  
  names<-c("Grid_7500m","Both","North","South")
  
  Missing<-setdiff(names,names(Grid_7500m_prop))
  Grid_7500m_prop[Missing]<-0
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  Grid_7500m_prop<-Grid_7500m_prop %>% mutate(Nprop=North/(North+South)) %>% filter(Nprop>=0)
  Grid_7500m_prop$Grid<-i
  
  Grid_7500m_obs_hist=hist(Grid_7500m_prop$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
  Grid_7500m_obs_hist_set<-data.frame(do.call("cbind",Grid_7500m_obs_hist[2:4]))
  Grid_7500m_obs_hist_set<-Grid_7500m_obs_hist_set %>% mutate(percent=counts/sum(counts))
  
  index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
  
  Grid_7500m_obs_hist_set$index<-index
  Grid_7500m_obs_hist_set<-mutate(Grid_7500m_obs_hist_set,product=percent*index) %>% mutate(Grid=i)
  
  Grid_30km_Grid_7500m_prop_List[[i]]<-Grid_7500m_prop
  Grid_30km_Grid_7500m_obs_hist_List[[i]]<-Grid_7500m_obs_hist_set
}

Grid30_Grid_7500m_prop<-do.call("rbind",Grid_30km_Grid_7500m_prop_List)
Grid30_Grid_7500m_obs_hist<-do.call("rbind",Grid_30km_Grid_7500m_obs_hist_List)


## random relabeling
#Grid_30km_Grid_7500m_prop_relabel_List<-vector("list",nrow(Grid_30km))
Grid_30km_Grid_7500m_relabel_hist_summary_List<-vector("list",nrow(Grid_30km))

for (i in 1:nrow(Grid_30km)){
  ## break up grid
  Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]
  Point_grid_subset<-TreesSp[Grid_subset,]
  
  # get data ready for relabeling
  PGS_data<-Point_grid_subset@data
  PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))
  
  if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
  if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
  
  niter=1000
  NS_Grid_7500m_prop_List<-vector("list",niter)
  NS_Grid_7500m_prop_relabel_List<-vector("list",niter)
  NS_Grid_7500m_relabel_hist_List<-vector("list",niter)
  
  for (z in 1:niter){
    V<-sample(PGS_data$Ind_Zone)
    NS_Grid_7500m_relabel<-PGS_data %>% mutate(relabel=V) %>% 
      group_by(Grid_7500m,relabel) %>% summarise(count=n()) %>% spread(relabel,count) %>% ungroup
    NS_Grid_7500m_relabel[is.na(NS_Grid_7500m_relabel)]<-0
    
    if(nrow(filter(PGS_data,Ind_Zone=="North"))==0) next
    if(nrow(filter(PGS_data,Ind_Zone=="South"))==0) next
    
    NS_Grid_7500m_relabel<-NS_Grid_7500m_relabel %>% mutate(Nprop=North/(North+South))
    NS_Grid_7500m_relabel$Grid<-i
    NS_Grid_7500m_relabel$iter<-z
    
    Grid_7500m_relabel_hist=hist(NS_Grid_7500m_relabel$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
    Grid_7500m_relabel_hist_set<-data.frame(do.call("cbind",Grid_7500m_relabel_hist[2:4]))
    Grid_7500m_relabel_hist_set<-Grid_7500m_relabel_hist_set %>% mutate(percent=counts/sum(counts))
    
    index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)
    
    Grid_7500m_relabel_hist_set$index<-index
    Grid_7500m_relabel_hist_set<-mutate(Grid_7500m_relabel_hist_set,product=percent*index) %>% mutate(iter=z)
    
    NS_Grid_7500m_prop_relabel_List[[z]]<-NS_Grid_7500m_relabel
    NS_Grid_7500m_relabel_hist_List[[z]]<-Grid_7500m_relabel_hist_set
  }
  
  NS_Grid_7500m_prop_relabel<-do.call("rbind",NS_Grid_7500m_prop_relabel_List)
  
  NS_Grid_7500m_relabel_hist<-do.call("rbind",NS_Grid_7500m_relabel_hist_List)
  NS_Grid_7500m_relabel_hist$Grid<-i
  
  relabel_hist_summary<-NS_Grid_7500m_relabel_hist %>% group_by(iter) %>% summarise(sum=sum(product))
  relabel_hist_summary$Grid<-i
  
  #Grid_30km_Grid_7500m_prop_relabel_List[[i]]<-NS_Grid_7500m_prop_relabel
  Grid_30km_Grid_7500m_relabel_hist_summary_List[[i]]<-relabel_hist_summary
}

#Grid30_Grid_7500m_prop_relabel<-do.call("rbind",Grid_30km_Grid_7500m_prop_relabel_List)
Grid30_Grid_7500m_relabel_hist_summary<-do.call("rbind",Grid_30km_Grid_7500m_relabel_hist_summary_List)


### summarizing and comparing span of data
# Grid 30
Grid30_Grid_7500m_obs_hist_summary<-Grid30_Grid_7500m_obs_hist %>% group_by(Grid) %>% summarize(sum=sum(product))
Grid30_Grid_7500m_relabel_hist_summary2<-Grid30_Grid_7500m_relabel_hist_summary %>% group_by(Grid) %>% summarize(relabel_mean=mean(sum),upper=quantile(sum,probs=0.975),lower=quantile(sum,probs=0.025))
Grid30_Grid_7500m_hist_summary<-Grid30_Grid_7500m_obs_hist_summary %>% left_join(Grid30_Grid_7500m_relabel_hist_summary2,by="Grid") %>% 
  mutate(upper_diff=sum-upper, lower_diff=sum-lower, diff=sum-relabel_mean) %>%  
  mutate(upper_sign=sign(upper_diff),lower_sign=sign(lower_diff)) %>% mutate(sig=upper_sign+lower_sign)


### export results
write.csv(Grid30_Grid_7500m_hist_summary,file="./Output/Grid30_Grid_7500m_hist_summary.csv",row.names = FALSE)


## determining "scale of co-occurrence"
Grid_1000m_hist_sig<-Grid30_Grid_1000m_hist_summary %>% select(Grid,sig) %>% mutate(scale="1000m") %>% 
  mutate(sig2=ifelse(sig==2,2,ifelse(sig==-2,-2,0))) %>% select(-sig)
Grid_1200m_hist_sig<-Grid30_Grid_1200m_hist_summary %>% select(Grid,sig) %>% mutate(scale="1200m") %>% 
  mutate(sig2=ifelse(sig==2,2,ifelse(sig==-2,-2,0))) %>% select(-sig)
Grid_1500m_hist_sig<-Grid30_Grid_1500m_hist_summary %>% select(Grid,sig) %>% mutate(scale="1500m") %>% 
  mutate(sig2=ifelse(sig==2,2,ifelse(sig==-2,-2,0))) %>% select(-sig)
Grid_2000m_hist_sig<-Grid30_Grid_2000m_hist_summary %>% select(Grid,sig) %>% mutate(scale="2000m") %>% 
  mutate(sig2=ifelse(sig==2,2,ifelse(sig==-2,-2,0))) %>% select(-sig)
Grid_2500m_hist_sig<-Grid30_Grid_2500m_hist_summary %>% select(Grid,sig) %>% mutate(scale="2500m") %>% 
  mutate(sig2=ifelse(sig==2,2,ifelse(sig==-2,-2,0))) %>% select(-sig)
Grid_3000m_hist_sig<-Grid30_Grid_3000m_hist_summary %>% select(Grid,sig) %>% mutate(scale="3000m") %>% 
  mutate(sig2=ifelse(sig==2,2,ifelse(sig==-2,-2,0))) %>% select(-sig)
Grid_3750m_hist_sig<-Grid30_Grid_3750m_hist_summary %>% select(Grid,sig) %>% mutate(scale="3750m") %>% 
  mutate(sig2=ifelse(sig==2,2,ifelse(sig==-2,-2,0))) %>% select(-sig)
Grid_4285m_hist_sig<-Grid30_Grid_4285m_hist_summary %>% select(Grid,sig) %>% mutate(scale="4285m") %>% 
  mutate(sig2=ifelse(sig==2,2,ifelse(sig==-2,-2,0))) %>% select(-sig)
Grid_5000m_hist_sig<-Grid30_Grid_5000m_hist_summary %>% select(Grid,sig) %>% mutate(scale="5000m") %>% 
  mutate(sig2=ifelse(sig==2,2,ifelse(sig==-2,-2,0))) %>% select(-sig)
Grid_6000m_hist_sig<-Grid30_Grid_6000m_hist_summary %>% select(Grid,sig) %>% mutate(scale="6000m") %>% 
  mutate(sig2=ifelse(sig==2,2,ifelse(sig==-2,-2,0))) %>% select(-sig)
Grid_7500m_hist_sig<-Grid30_Grid_7500m_hist_summary %>% select(Grid,sig) %>% mutate(scale="7500m") %>% 
  mutate(sig2=ifelse(sig==2,2,ifelse(sig==-2,-2,0))) %>% select(-sig)

Grid_hist_sig<-Grid_1000m_hist_sig %>% bind_rows(Grid_1200m_hist_sig,Grid_1500m_hist_sig,Grid_2000m_hist_sig,Grid_2500m_hist_sig,
                                                 Grid_3000m_hist_sig,Grid_3750m_hist_sig,Grid_4285m_hist_sig,Grid_5000m_hist_sig,
                                                 Grid_6000m_hist_sig,Grid_7500m_hist_sig) %>% spread(scale,sig2)

Grid_hist_scale_list<-vector("list",nrow(Grid_hist_sig))

for (i in 1:nrow(Grid_hist_sig)){
  Grid <- Grid_hist_sig[i,]
  Grid <- Grid %>% gather("scale","sig",2:12) %>% mutate(scale2=as.numeric(gsub('m','',scale))) %>% 
    filter(sig<2) %>% group_by(Grid) %>% summarise(min(scale2)) %>% rename(min_scale=`min(scale2)`)
  Grid_hist_scale_list[[i]]<-Grid
}

Grid_hist_scale<-do.call("rbind",Grid_hist_scale_list)
Grid_hist_sig_scale<-left_join(Grid_hist_sig,Grid_hist_scale,by="Grid")

# changing NA to 9999: these are the Grids that are significantly separated past at least the 6000m scale
Grid_hist_sig_scale$min_scale[is.na(Grid_hist_sig_scale$min_scale)]<-9999

## export
write.csv(Grid_hist_sig_scale,file="./Output/Grid_hist_sig_scale.csv",row.names = FALSE)












