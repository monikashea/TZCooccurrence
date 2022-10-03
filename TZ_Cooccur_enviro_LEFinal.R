setwd("U:/Shea/Research/GIS/Projects/TZ_Cooccurrence/Data")

library(plotly)
library(sp)
library(gstat)
library(rgdal)
library(rgeos)
library(raster)
library(leaps)
library(spatstat)
library(maptools)
library(spdep)
library(MASS)
library(vegan)
library(RColorBrewer)
library(car)
library(sf)
library(rgl)
library(emmeans)
library(bestglm)
library(glmulti)
library(tidyverse)
library(ggspatial)
library(cowplot)
library(stringr)

########################## BRING IN DATA ###############################

########### Co-occurrence results ############

# Grid 30
Grid30_overall<-read.csv(file="./Output/23Jan2021/Grid_hist_sig_scale.csv",header = TRUE)

########### Environmental Data #############
#### summaries processed in ArcGIS, File = U:\Shea\Research\GIS\Projects\TZ_Cooccurrence\Maps\Cooccur_Enviro_20191230
#### rasters processed using Zonal Statistics as Table
elev_full <- read.csv(file="./Enviro/30km_ZonalStats/elev_30km.csv",header=TRUE)
ppet_full <- read.csv(file="./Enviro/30km_ZonalStats/ppet_30km.csv",header=TRUE)
sand_full <- read.csv(file="./Enviro/30km_ZonalStats/sand_30km.csv",header=TRUE)
DI_full <- read.csv(file="./Enviro/30km_ZonalStats/DI_30km.csv",header=TRUE)

# formatting
elev <- elev_full %>% rename(elev_range=RANGE,elev_std=STD,Grid_ID=GRID_ID) %>% select(Grid_ID,elev_range,elev_std)

ppet <- ppet_full %>% rename(ppet_mean=MEAN,ppet_std=STD,Grid_ID=GRID_ID) %>% select(Grid_ID,ppet_mean,ppet_std)

sand <- sand_full %>% rename(sand_mean=MEAN,sand_std=STD,Grid_ID=GRID_ID) %>% select(Grid_ID,sand_mean,sand_std)

DI <- DI_full %>% rename(DI_mean=MEAN,DI_std=STD,DI_maj=MAJORITY,Grid_ID=GRID_ID) %>% select(Grid_ID,DI_mean,DI_std,DI_maj)


## proportion of grid cell in state boundary
# import WI boundary
wisc<-readOGR(dsn="./shapefiles/WI_clip",layer="WI_HARN_mask")
wisc_HARN <- spTransform(wisc,CRS=CRS("+init=epsg:3070"))

# bring in grid
Grid_30km <- readOGR(dsn="./shapefiles/Grids",layer="30km_Grid")
Grid_30km <- spTransform(Grid_30km,CRS=CRS("+init=epsg:3070"))
Grid_30km$Grid_ID <- as.integer(as.character(Grid_30km$Grid_ID))

# intersect grid polygons with WI boundary
# transform to sf
Grid_sf <- as(Grid_30km,"sf")
wisc_sf <- as(wisc_HARN,"sf")

# run the intersect function, converting the output to a tibble
Grid_wisc_int <- as_tibble(st_intersection(Grid_sf,wisc_sf))

# add in an area count column to the tibble (area of each arable poly in intersect layer)
Grid_wisc_int$areaIn <- st_area(Grid_wisc_int$geometry)
WI_prop <- data.frame(cbind(Grid_wisc_int$Grid_ID,round(Grid_wisc_int$areaIn,0))) %>% rename(Grid_ID=1,areaIn=2) %>% 
  mutate(WI_prop=areaIn/max(areaIn)) %>% select(-areaIn)

Enviro_30km_pre1<-WI_prop %>%  
  left_join(sand,by="Grid_ID") %>% 
  left_join(elev,by="Grid_ID") %>% 
  left_join(ppet,by="Grid_ID") %>%
  left_join(DI,by="Grid_ID")

#### bring in TZ layer
Zones4_polygon<-readOGR('U:/Shea/Research/GIS/Projects/TZ_Boundary/Data/shapefiles/TZ_Boundary','mean_FS_TZ4_PolygonZones')
Zones4_polygon <- spTransform(Zones4_polygon,CRS=CRS("+init=epsg:3070"))

Zones_Grid_30km<-raster::intersect(Zones4_polygon,Grid_30km)

### get the area to select the zone with maximum area in border regions
# Extract areas from polygon objects 
areas <- data.frame(area=sapply(Zones_Grid_30km@polygons, FUN=function(x) {slot(x, 'area')}))
row.names(areas) <- sapply(Zones_Grid_30km@polygons, FUN=function(x) {slot(x, 'ID')})
# Combine attributes info and areas 
Zones_Grid_30km_area <- spCbind(Zones_Grid_30km, areas)

# all grid cells touching TZ boundary will be considered TZ
Zones_Grid_30km_TZ4<-data.frame(Zones_Grid_30km@data) %>% select(Grid_ID,Zone) %>% group_by(Grid_ID) %>% summarise(numzones=n()) %>% 
  left_join(Zones_Grid_30km@data,by="Grid_ID") %>% select(Grid_ID,Zone,numzones) %>% rename(oldZone=Zone) %>% 
  mutate(Zone=ifelse(Grid_ID==92,"South",ifelse(numzones>1,"TZ",ifelse(oldZone=="North","North",ifelse(oldZone=="TZ","TZ","South"))))) %>% 
  filter(Zone==oldZone) %>% select(Grid_ID,Zone) 

Enviro_30km_pre2<-Enviro_30km_pre1 %>% left_join(Zones_Grid_30km_TZ4,by="Grid_ID") %>% mutate(Zone=as.factor(Zone))

### add relprop data
Grid_prop <- read.csv("./Output/23Jan2021/Grid_prop.csv",header=TRUE)
Grid30_Nprop <- Grid_prop %>% rename(Grid_ID=Grid_30km) %>% select(Grid_ID,Nprop,Bprop) 

# make proportion relative to dominant in zone
Grid30_Nprop <- Grid30_Nprop %>% mutate(relprop=ifelse(Nprop<0.5,1-Nprop,Nprop))

# Add rest to enviro data
Enviro_30km<-Enviro_30km_pre2 %>% 
  left_join(Grid30_Nprop,by="Grid_ID") %>% 
  filter(WI_prop>0.01) 

## add to SPDF to make some maps
Grid_30km_Enviro <- merge(Grid_30km,Enviro_30km,by='Grid_ID')
TZ_poly <- list("sp.polygons",Zones4_polygon,first=F,col="black")

# TZ only
Grid_30km_Enviro_TZ <- Grid_30km_Enviro[Grid_30km_Enviro@data$Zone=="TZ" & !is.na(Grid_30km_Enviro@data$Zone),]

### Add summaries 
cluster_data_Grid30_overall <- Grid30_overall %>% left_join(Enviro_30km,by=c("Grid"="Grid_ID"))

#write.csv(cluster_data_Grid30_overall,file="cluster_data_Grid30_overall.csv",row.names = FALSE)
#writeOGR(Grid_30km_Enviro_TZ,"./shapefiles/TZ_Grid","Grid_30km_TZ",driver="ESRI Shapefile")





################################ BEGIN ANALYSIS ###################################

######## Detection of co-occurrence analysis ###########
##### Grid 30
### overall results
# reduce dataset to include only the Tension Zone
cluster_data_Grid30_overall_TZ <- filter(cluster_data_Grid30_overall, Zone=="TZ")

## plot of co-occurrence scale
Grid_30km_Enviro_TZ_cooccur <- merge(Grid_30km_Enviro_TZ,cluster_data_Grid30_overall_TZ,by.x='Grid_ID',by.y='Grid')

#### test co-occur vs non-co-occur by 7500m
## process data
cluster_data_Grid30_overall_TZ$cooccur <- ifelse(cluster_data_Grid30_overall_TZ$min_scale>7500,0,1)

# scatterplots
pairs(cluster_data_Grid30_overall_TZ[,c(28,27,19,18,21,15)])

## NOTE: DI may be coorelated with elevation SD and sand mean.
# VIF tests for no-interaction lms suggest that this may not be an issue
# leave in DI mean

### analysis
## full model, no interactions
cooccur.1 <- glm(cooccur~relprop+ppet_mean+elev_std+DI_mean+sand_mean, family = binomial(link = "logit"), data=cluster_data_Grid30_overall_TZ)

## backward to eliminate main effects, then forward to include interactions
cooccur.1.back <- step(cooccur.1,direction="backward",k=log(nrow(cluster_data_Grid30_overall_TZ)),
                       scope=~relprop+ppet_mean+elev_std+DI_mean+sand_mean)

summary(cooccur.1.back)

## only relprop is included in the model

cooccur.1.final <- glm(cooccur~relprop, family = binomial(link = "logit"), data=cluster_data_Grid30_overall_TZ)

summary(cooccur.1.final)

cluster_data_Grid30_overall_TZ_fig <- cluster_data_Grid30_overall_TZ %>% mutate(cooccur=as.factor(cooccur))
ggplot(data=cluster_data_Grid30_overall_TZ_fig,aes(x=relprop,fill=cooccur,color=cooccur))+
  geom_histogram(alpha=0.5,position="identity")

cooccur_group_mean <- cluster_data_Grid30_overall_TZ %>% group_by(cooccur) %>% summarize(relprop=mean(relprop))


############ Scale of Co-occurrence analysis #############

#### test variables influencing scale of co-occurrence up to 7500m
# process data
cluster_data_Grid30_overall_TZ2<-cluster_data_Grid30_overall_TZ %>% filter(min_scale<8000)

# scatterplots
pairs(cluster_data_Grid30_overall_TZ2[,c(13,27,19,18,21,15)])

### analyses
## best model
cooccur_scale.glmulti <- glmulti(min_scale~relprop+ppet_mean+elev_std+DI_mean+sand_mean+
                                   ppet_mean*elev_std+ppet_mean*DI_mean+ppet_mean*sand_mean+
                                   elev_std*DI_mean+elev_std*sand_mean+DI_mean*sand_mean,data=cluster_data_Grid30_overall_TZ2,
                                 level=2,
                                 method="h",
                                 crit="bic",
                                 plotty = F, report = F,
                                 fitfunction = "lm")

summary(cooccur_scale.glmulti@objects[[2]]) # 2 is best complete model
bic(cooccur_scale.glmulti@objects[[2]]) 

cooccur_scale.1 <- lm(min_scale~relprop+ppet_mean+sand_mean+
                          ppet_mean*sand_mean,
                      data=cluster_data_Grid30_overall_TZ2)

summary(cooccur_scale.1)

par(mfrow=c(2,2))
plot(cooccur_scale.1)
par(mfrow=c(1,1))


### log-transform min_scale
## process data
cluster_data_Grid30_overall_TZ2 <- cluster_data_Grid30_overall_TZ2 %>% mutate(log_min_scale=log(min_scale))

## best model 
cooccur_scale.glmulti.2 <- glmulti(log_min_scale~relprop+ppet_mean+elev_std+DI_mean+sand_mean+
                                     ppet_mean*elev_std+ppet_mean*DI_mean+ppet_mean*sand_mean+
                                     elev_std*DI_mean+elev_std*sand_mean+DI_mean*sand_mean,data=cluster_data_Grid30_overall_TZ2,
                                   level=2,
                                   method="h",
                                   crit="bic",
                                   plotty = F, report = F,
                                   fitfunction = "lm")

summary(cooccur_scale.glmulti.2@objects[[1]]) # 1 is best
bic(cooccur_scale.glmulti.2@objects[[1]]) 

cooccur_scale.2 <- lm(log_min_scale~relprop+ppet_mean+sand_mean+
                        ppet_mean*sand_mean,
                      data=cluster_data_Grid30_overall_TZ2)

summary(cooccur_scale.2)

par(mfrow=c(2,2))
plot(cooccur_scale.2)
par(mfrow=c(1,1))


#### interaction plots
## get data ready
cluster_data_Grid30_overall_TZ3 <- cluster_data_Grid30_overall_TZ2 %>% 
  mutate(sand_group_num=ntile(sand_mean,3)) %>% mutate(ppet_group_num=ntile(ppet_mean,3)) %>% 
  mutate(sand_group=ifelse(sand_group_num==1, "Low (28.0 %)",ifelse(sand_group_num==3,"High (61.9 %)","Medium (45.0 %)"))) %>% 
  mutate(ppet_group=ifelse(ppet_group_num==1,"Low (198.4 mm)",ifelse(ppet_group_num==3,"High (247.3 mm)","Medium (222.9 mm)")))

### cooccur_scale.6  
## prediction lines using procedure with package "emmeans"
## details on approach here: https://stats.idre.ucla.edu/r/seminars/interactions-r/#s3
sanda <- mean(cluster_data_Grid30_overall_TZ2$sand_mean)+sd(cluster_data_Grid30_overall_TZ2$sand_mean)
sandmean <- mean(cluster_data_Grid30_overall_TZ2$sand_mean)
sandb <- mean(cluster_data_Grid30_overall_TZ2$sand_mean)-sd(cluster_data_Grid30_overall_TZ2$sand_mean)

ppeta <- mean(cluster_data_Grid30_overall_TZ2$ppet_mean)+sd(cluster_data_Grid30_overall_TZ2$ppet_mean)
ppetmean <- mean(cluster_data_Grid30_overall_TZ2$ppet_mean)
ppetb <- mean(cluster_data_Grid30_overall_TZ2$ppet_mean)-sd(cluster_data_Grid30_overall_TZ2$ppet_mean)

## slopes of simple regression lines
mylist <- list(ppet_mean=c(ppeta,ppetmean,ppetb))
emtrends(cooccur_scale.2,pairwise~ppet_mean,var="sand_mean",at=mylist)

mylist <- list(sand_mean=c(sanda,sandmean,sandb))
emtrends(cooccur_scale.2,pairwise~sand_mean,var="ppet_mean",at=mylist)

## get ready for plots
mylist <- list(ppet_mean=c(ppeta,ppetmean,ppetb),sand_mean = c(15.0, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 84.8))
cooccur_scale.2_ppet_sand <- emmip(cooccur_scale.2,ppet_mean~sand_mean|relprop,at=mylist,CIs=TRUE,plotit = FALSE)
cooccur_scale.2_ppet_sand$fppet <- factor(cooccur_scale.2_ppet_sand$ppet_mean)

## ggplot with points and line
ggplot(data=cooccur_scale.2_ppet_sand, aes(x=sand_mean))+
  geom_point(data=cluster_data_Grid30_overall_TZ3,aes(x=sand_mean,y=log_min_scale, col=ppet_group),alpha=0.5)+
  geom_line(aes(x=sand_mean,y=yvar, color=fppet))+
  geom_ribbon(aes(ymax=UCL,ymin=LCL,fill=fppet),alpha=0.4)+
  scale_color_manual(breaks=c("High (247.3 mm)","Medium (222.9 mm)","Low (198.4 mm)"),values=c("#56B4E9","#E69F00","#009E73","#009E73","#56B4E9","#E69F00"))+
  scale_fill_manual(values=c("#56B4E9","#E69F00","#009E73"),guide="none")+
  theme_classic()+
  theme(plot.title=element_text(hjust = 0.5))+
  labs(x="Percent sand mean", y="log scale of co-occurrence (km)",color="P-PET group")


### map residuals 
cooccur.resid_data <- setNames(data.frame(cluster_data_Grid30_overall_TZ$Grid,cooccur.1.final$residuals),c("Grid_ID","co.resid"))
cooccur_scale.resid_data <- setNames(data.frame(cluster_data_Grid30_overall_TZ2$Grid,cooccur_scale.2$residuals),c("Grid_ID","scale.resid"))

Grid_30km_TZ_cooccur_resid <- merge(Grid_30km_Enviro_TZ,cooccur.resid_data,by='Grid_ID')
Grid_30km_TZ_cooccur_resid <- merge(Grid_30km_TZ_cooccur_resid,cooccur_scale.resid_data,by='Grid_ID')

spplot(Grid_30km_TZ_cooccur_resid,'co.resid',sp.layout=TZ_poly,xlim=Zones4_polygon@bbox[1,],ylim=Zones4_polygon@bbox[2,],main="Residuals - co-occurrence logistic regression")
spplot(Grid_30km_TZ_cooccur_resid,'scale.resid',sp.layout=TZ_poly,xlim=Zones4_polygon@bbox[1,],ylim=Zones4_polygon@bbox[2,],main="Residuals - scale of co-occurrence linear regression")


############### examine spatial pattern of model residuals ###############
## calculate semivariogram of residuals
# Grid centroids
Grid_30km_centroids<-SpatialPointsDataFrame(gCentroid(Grid_30km,byid=TRUE),Grid_30km@data)
Grid_30km_centroids_data<-as.data.frame(Grid_30km_centroids) %>% mutate(Grid_ID=as.numeric(as.character(Grid_ID)))

## cooccur
resid_Grid30_cooccur<-data.frame(cluster_data_Grid30_overall_TZ$Grid,cooccur.1.final$residuals)
colnames(resid_Grid30_cooccur)<-c("Grid_ID","resid")
#resid_Grid30_cooccur$Grid<-as.character(resid_Grid30_cooccur$Grid)

Grid30_cooccur_centroids_resid<-right_join(Grid_30km_centroids_data,resid_Grid30_cooccur,by="Grid_ID")

coordinates(Grid30_cooccur_centroids_resid)<-~x+y
v6<-variogram(resid~1,data=Grid30_cooccur_centroids_resid,cloud=TRUE,boundaries=seq(30000,240000,by=30000))
plot(v6)

## cooccur scale
resid_Grid30_cooccur_scale<-data.frame(cluster_data_Grid30_overall_TZ2$Grid,cooccur_scale.2$residuals)
colnames(resid_Grid30_cooccur_scale)<-c("Grid_ID","resid")
#resid_Grid30_cooccur_scale$Grid<-as.character(resid_Grid30_cooccur_scale$Grid)

Grid30_cooccur_scale_centroids_resid<-right_join(Grid_30km_centroids_data,resid_Grid30_cooccur_scale,by="Grid_ID")

coordinates(Grid30_cooccur_scale_centroids_resid)<-~x+y
v7<-variogram(resid~1,data=Grid30_cooccur_scale_centroids_resid,boundaries=seq(30000,180000,by=15000))#,boundaries=c(30000,42426.41,60000.00,67082.04,84852.81,90000.00,94868.33,108166.54,120000.00,123693.17,127279.2,134164.1,150000,152970.6,161554.9,180000))
plot(v7)

### no apparent patterns in semivariogram

################# NMS of species ###############
## import tree data
all_trees_pred <- read.csv(file="U:/Shea/Research/GIS/Projects/Differentiation/analysis/data/output/all_trees_pred_12Aug2019.csv",header=TRUE)

coords <- cbind(all_trees_pred$X,all_trees_pred$Y)

ptree_diff_2019 <- SpatialPointsDataFrame(coords,all_trees_pred)
proj4string(ptree_diff_2019) <- CRS("+init=epsg:3070") # add projection NAD83/Wisconsin Transferse Mercator

pg_overlay <- over(ptree_diff_2019,Grid_30km)
pg_overlay_trees <- cbind(pg_overlay,ptree_diff_2019@data)
pg_overlay_trees <- pg_overlay_trees %>% rename(Grid_30km=Grid_ID)

## format tree data for 30 km grid
Trees<-pg_overlay_trees

summary(as.factor(Trees$SP_new))

# remove NA, UK, RR (NA, unknown species, and very rare species with <10 points)

Trees<-Trees %>% filter(!SP_new %in% c('UK','RR')) %>% filter(!is.na(SP_new)) %>% 
  mutate(Grid_30km=as.numeric((as.character(Grid_30km))))

# remove other rare species (AL, BW, BX, CO, HA, SO, TH, CH, WI) with < 100 points in TZ
Trees<-Trees %>% filter(!SP_new %in% c('AL','BW','BX','CO','HA','SO','TH','CH','WI')) 

Trees$SP_new<-as.factor(Trees$SP_new)
Trees$SP_new<-droplevels(Trees$SP_new)

# Relative density calculations

Trees_spsite<-Trees %>% group_by(SP_new,Grid_30km) %>% summarise(count=n())
Trees_site<-Trees %>% group_by(Grid_30km) %>% summarise(tcount=n())
Trees_mutate<-Trees_spsite %>% left_join(Trees_site,by="Grid_30km") %>% mutate(RDom=count/tcount)

# make species x site matrix
Trees_count<-as.data.frame(unclass(xtabs(count~Grid_30km+SP_new,data=Trees_mutate)))
Trees_count$Grid_30km<-as.numeric(rownames(Trees_count))

# remove grid cells outside of Tension Zone
Trees_count_TZ<-Enviro_30km %>% select(Grid_ID,Zone) %>% left_join(Trees_count,by=c("Grid_ID"="Grid_30km")) %>% filter(Zone=="TZ") %>% select(-Zone)
row.names(Trees_count_TZ)<-Trees_count_TZ$Grid_ID

colSums(Trees_count_TZ)

### only cells that have co-occurrence
# try multiple axes
Trees_count_TZ_cooccur<- cluster_data_Grid30_overall_TZ2 %>% select(Grid) %>% left_join(Trees_count,by=c("Grid"="Grid_30km"))
row.names(Trees_count_TZ_cooccur)<-Trees_count_TZ_cooccur$Grid

Trees_count_NMS_cooccur<-Trees_count_TZ_cooccur[,2:29]
Grid30_NMS.result1=metaMDS(Trees_count_NMS_cooccur,distance="bray", k=1,trymax=5000) 
Grid30_NMS.result2=metaMDS(Trees_count_NMS_cooccur,distance="bray", k=2,trymax=5000) 
Grid30_NMS.result3=metaMDS(Trees_count_NMS_cooccur,distance="bray", k=3,trymax=5000) 
Grid30_NMS.result4=metaMDS(Trees_count_NMS_cooccur,distance="bray", k=4,trymax=5000) 

Grid30_NMS.result1
Grid30_NMS.result2
Grid30_NMS.result3
Grid30_NMS.result4

k<-c(1,2,3,4)
stress<-c(Grid30_NMS.result1$stress,Grid30_NMS.result2$stress,Grid30_NMS.result3$stress,Grid30_NMS.result4$stress)
scree_stress<-data.frame(k,stress)

plot(stress~k)

## go with 2 axis solution
plot(Grid30_NMS.result2,choices=c(1,2), type="n") 

text(Grid30_NMS.result2,display="species", cex=0.7, col="black",choices=c(1,2)) # show species as names, cex for size 

## relate environmental variables
Enviro_30km_NMS <- semi_join(Enviro_30km,Trees_count_TZ_cooccur,by=c("Grid_ID"="Grid")) %>% left_join(Grid30_overall,by=c("Grid_ID"="Grid")) %>% rename(scale=min_scale) %>% 
  mutate(cooccur=ifelse(scale>7500,0,1),log_scale=log(scale),
         Sand=sand_mean,PPET=ppet_mean,DI=DI_mean,ElevSD=elev_std,Log_scale=log_scale,Relprop=relprop)

Env.fit_30km=envfit(Grid30_NMS.result2~Log_scale+Sand+DI+ElevSD+PPET+Relprop, data= Enviro_30km_NMS, perm=1000)

Env.fit_30km
plot(Env.fit_30km)

######################### END ANALYSIS ###########################

############################ FIGURES ###########################

####### Plot Fig 3 ###############
## example Grid cell
i=133

Grid_subset<-Grid_30km[Grid_30km$Grid_ID==i,]

# where is it located
plot(Grid_30km)
plot(TZ4_polygon,add=TRUE)
plot(Grid_subset,col="red",add=TRUE)

### Fig 3a
### map of analysis unit grid cells overlaying

### convert to sf
wisc_HARN_sf <- as(wisc_HARN,"sf")
Zones4_polygon_sf <- as(Zones4_polygon,"sf")
Grid_30km_Enviro_TZ_sf <- as(Grid_30km_Enviro_TZ,"sf")

# create sf of example grid
Grid <- Grid_30km[Grid_30km$Grid_ID==i,]
Grid2_sf <- as(Grid,"sf")

# plot
Fig3a <- ggplot(data=Zones4_polygon_sf)+
  geom_sf(fill="white",col="gray20",lwd=0.25)+
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1, "cm"), #how tall the arrow should be
                         width= unit(0.5, "cm"), 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering)+
  geom_sf(data=Grid_30km_Enviro_TZ_sf,col="black",fill="white",alpha=0,lwd=0.75)+
  geom_sf(data=Grid2_sf,fill="purple",col="black",lwd=0.75,alpha=0.5)+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())+
  theme(legend.title = element_blank(),
        legend.text=element_text(size=12))

Fig3a

#### save plot
dpi=300
ggsave("U:/Shea/Research/GIS/Projects/TZ_Cooccurrence/Images/Fig3a_pre_new.png", plot=Fig3a, width = 3.25, height = 3.75, dpi = dpi, units="in")

############# Fig 3b 
########### plot of selection area grid cells within analysis unit
###### Load data from TZ_Cooccur_GridChange_20200410.R

##### extract 3000m grid cells
### identify smaller grid cells that occur in 30km grid cell
## no simple method, so we have to do multiple selections then remove those that actually aren't in grid cell
# extract  example grid
# select grid cells in polygon; will end up with excess
Grid2_3000m_1 <- Grid_3000m[which(gOverlaps(Grid,Grid_3000m,byid=TRUE)),] # selects some of the grid cells on the edges
Grid2_3000m_2 <- Grid_3000m[which(gContains(Grid,Grid_3000m,byid=TRUE)),] # selects the grid cells completely inside Grid polygon
Grid2_3000m_extra <- rbind(Grid2_3000m_1,Grid2_3000m_2)

# change to sf for next steps
Grid2_3000m_sf <- as(Grid2_3000m_extra,"sf")

## remove grid cells that have most area outside polygon
# find area of overlap
Grid2_3000m_int <- as_tibble(st_intersection(Grid2_sf,Grid2_3000m_sf))
Grid2_3000m_int$areaIn <- st_area(Grid2_3000m_int$geometry)
Grid2_3000m_int <- Grid2_3000m_int %>% rename(Grid_3000m=Grid_ID.1)

# remove cells with small overlap area 
Grid2_3000m_in <- Grid2_3000m_int %>% filter(as.vector(areaIn)>10000) %>% select(Grid_3000m) %>% rename(Grid_ID=Grid_3000m)

# correct grid subset
Grid2_3000m <- Grid2_3000m_extra[Grid2_3000m_extra@data$Grid_ID %in% Grid2_3000m_in$Grid_ID,]
Grid2_3000m <- Grid2_3000m[,-1]

# convert to sf 
Grid_3000m_sf <- as(Grid2_3000m,"sf")

#### extract tree data
## change crs
TreesSp <- spTransform(TreesSp,CRS=CRS("+init=epsg:3070"))
Grid_subset <- spTransform(Grid_subset,CRS=CRS("+init=epsg:3070"))

Point_grid_subset<-TreesSp[Grid_subset,]

# add text column
Point_grid_subset@data$Sp_grp <- ifelse(Point_grid_subset@data$N==0,"Southern",ifelse(Point_grid_subset@data$N==1,"Northern","Both"))

# convert to sf
PGS_sf <- as(Point_grid_subset,"sf")

Fig3b <- ggplot()+
  geom_sf(data=Grid2_sf,fill="white")+
  annotation_scale(location = "tr", width_hint = 0.25) +
  geom_sf(data=Grid_3000m_sf,fill="white")+
  geom_sf(data=PGS_sf,aes(fill=Sp_grp,col=Sp_grp),size=0.2)+
  scale_fill_manual(values=c("gray","blue","red"),name="Species group",breaks=c("Northern","Southern","Both"))+
  scale_color_manual(values=c("gray","blue","red"),name="Species group",breaks=c("Northern","Southern","Both"))+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,0,0,-10))

Fig3b_scale <- ggplot()+
  geom_sf(data=Grid2_sf,fill="white")+
  #geom_sf(data=Grid_3000m_sf,fill="white")+
  #geom_sf(data=PGS_sf,aes(fill=Sp_grp,col=Sp_grp),size=0.2)+
  #scale_fill_manual(values=c("gray","blue","red"),name="Species group",breaks=c("Northern","Southern","Both"))+
  #scale_color_manual(values=c("gray","blue","red"),name="Species group",breaks=c("Northern","Southern","Both"))+
  annotation_scale(location = "br", width_hint = 0.25) +
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,0,0,-10))

Fig3b
Fig3b_scale

#### save plot
ggsave("U:/Shea/Research/GIS/Projects/TZ_Cooccurrence/Images/Fig3b_pre_new.png", plot=Fig3b, width = 3.25, height = 2.25, units="in")
ggsave("U:/Shea/Research/GIS/Projects/TZ_Cooccurrence/Images/Fig3b_scale.png", plot=Fig3b_scale, width = 3.25, height = 2.25, units="in")


##### make histogram
PGS_data<-Point_grid_subset@data
PGS_data$Ind_Zone<-ifelse(PGS_data$N==1,"North",ifelse(PGS_data$N==2,"Both","South"))

Grid_3000m_prop<-PGS_data %>% group_by(Grid_3000m,Ind_Zone) %>% summarise(count=n()) %>% spread(Ind_Zone,count) %>% ungroup
Grid_3000m_prop[is.na(Grid_3000m_prop)]<-0

names<-c("Grid_3000m","Both","North","South")

Missing<-setdiff(names,names(Grid_3000m_prop))
Grid_3000m_prop[Missing]<-0

Grid_3000m_prop<-Grid_3000m_prop %>% mutate(Nprop=North/(North+South)) %>% filter(Nprop>=0)
Grid_3000m_prop$Grid<-i

Grid_3000m_obs_hist=hist(Grid_3000m_prop$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
Grid_3000m_obs_hist_data <- data.frame(Grid_3000m_obs_hist$mids,Grid_3000m_obs_hist$counts)
colnames(Grid_3000m_obs_hist_data) <- c("mids","count")

plot(Grid_3000m_obs_hist,xlab="Proportion northern trees",ylab="Number of grid cells",main=NULL)

Fig3c <- ggplot(Grid_3000m_obs_hist_data,aes(x=mids,y=count))+
  geom_bar(stat="identity",fill="white",color="black",width=0.0526)+
  theme_classic()+
  xlab("Proportion of northern trees")+
  ylab("Number of grid cells")+
  theme(axis.text = element_text(size=10,color="black"),
        axis.title = element_text(size=10,color="black"))

Fig3c

#### save plot
dpi=300
ggsave("U:/Shea/Research/GIS/Projects/TZ_Cooccurrence/Images/Fig3c_pre.png", plot=Fig3c, width = 3.25, height = 2.25, units="in")

Grid_3000m_obs_hist_set<-data.frame(do.call("cbind",Grid_3000m_obs_hist[2:4]))
Grid_3000m_obs_hist_set<-Grid_3000m_obs_hist_set %>% mutate(percent=counts/sum(counts))

index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)

Grid_3000m_obs_hist_set$index<-index
Grid_3000m_obs_hist_set<-mutate(Grid_3000m_obs_hist_set,product=percent*index) %>% mutate(Grid=i)

index_value <- sum(Grid_3000m_obs_hist_set$product)
## index is 6.81

### relabel
PGS_data_NS<-filter(PGS_data,Ind_Zone %in% c("North","South"))
PGS_data_both<-filter(PGS_data,Ind_Zone %in% c("Both"))

V<-sample(PGS_data_NS$Ind_Zone)

## map relabel
PGS_data_relabel <- PGS_data_NS %>% mutate(relabel=V) %>% bind_rows(PGS_data_both) %>% 
  mutate(relabel=ifelse(is.na(relabel),"Both",relabel)) %>% select(dtrscot,relabel)

PGS_sf_relabel <- PGS_sf %>% left_join(PGS_data_relabel,by="dtrscot")

  
Fig3b3 <- ggplot()+
  geom_sf(data=Grid2_sf,fill="white")+
  geom_sf(data=Grid_3000m_sf,fill="white")+
  geom_sf(data=PGS_sf_relabel,aes(fill=relabel,col=relabel),size=0.2)+
  scale_fill_manual(values=c("blue","red","gray"),name="Species group",breaks=c("North","South","Both"))+
  scale_color_manual(values=c("blue","red","gray"),name="Species group",breaks=c("North","South","Both"))+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,0,0,-10))

Fig3b3

#### save plot
ggsave("U:/Shea/Research/GIS/Projects/TZ_Cooccurrence/Images/Fig3b_random.png", plot=Fig3b3, width = 4.5, height = 3, units="in")


## relabel histogram
NS_Grid_3000m_relabel<-PGS_data_NS %>% mutate(relabel=V) %>% 
  group_by(Grid_3000m,relabel) %>% summarise(count=n()) %>% spread(relabel,count) %>% ungroup
NS_Grid_3000m_relabel[is.na(NS_Grid_3000m_relabel)]<-0

NS_Grid_3000m_relabel<-NS_Grid_3000m_relabel %>% mutate(Nprop=North/(North+South))
NS_Grid_3000m_relabel$Grid<-i

Grid_3000m_relabel_hist=hist(NS_Grid_3000m_relabel$Nprop,breaks=seq(0,1,length.out = 20),plot=FALSE)
Grid_3000m_relabel_hist_data <- data.frame(Grid_3000m_relabel_hist$mids,Grid_3000m_relabel_hist$counts)
colnames(Grid_3000m_relabel_hist_data) <- c("mids","count")

Fig3c3 <- ggplot(Grid_3000m_relabel_hist_data,aes(x=mids,y=count))+
  geom_bar(stat="identity",fill="white",color="black",width=0.0526)+
  theme_classic()+
  xlab("Proportion of northern trees")+
  ylab("Number of grid cells")+
  theme(axis.text = element_text(size=10,color="black"),
        axis.title = element_text(size=10,color="black"))

Fig3c3

#### save plot
dpi=300
ggsave("U:/Shea/Research/GIS/Projects/TZ_Cooccurrence/Images/Fig3c_relabel.png", plot=Fig3c3, width = 3.25, height = 2.25, units="in")

Grid_3000m_relabel_hist_set<-data.frame(do.call("cbind",Grid_3000m_relabel_hist[2:4]))
Grid_3000m_relabel_hist_set<-Grid_3000m_relabel_hist_set %>% mutate(percent=counts/sum(counts))

index<-c(10,9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10)

Grid_3000m_relabel_hist_set$index<-index
Grid_3000m_relabel_hist_set<-mutate(Grid_3000m_relabel_hist_set,product=percent*index) 

sum(Grid_3000m_relabel_hist_set$product)

# plot without red cell
Fig3a2 <- ggplot(data=Zones4_polygon_sf)+
  geom_sf(fill="white",col="gray20",lwd=0.25)+
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1, "cm"), #how tall the arrow should be
                         width= unit(0.5, "cm"), 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering)+
  geom_sf(data=Grid_30km_Enviro_TZ_sf,col="black",fill="white",alpha=0,lwd=0.75)+
  #geom_sf(data=Grid2_sf,fill="red",col="black",lwd=0.75,alpha=0.5)+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())+
  theme(legend.title = element_blank(),
        legend.text=element_text(size=12))

Fig3a2

#### save plot
dpi=300
#ggsave("U:/Shea/Research/GIS/Projects/TZ_Cooccurrence/Images/Fig3a_pre.png", plot=Fig3a, width = 3.25, height = 3.75, dpi = dpi, units="in")
ggsave("U:/Shea/Research/GIS/Projects/TZ_Cooccurrence/Images/Fig3a_all_pre.png", plot=Fig3a2, width = 6.5, height = 7.5, dpi = dpi, units="in")

########## finish making Fig. 3 in PowerPoint


################## Plot Fig 4 #####################
#### enviro variables
Fig4a <- ggplot(data=Grid_30km_Enviro_TZ_sf)+
  geom_sf(aes(fill=ppet_mean),col="black",lwd=0.3)+
  scale_fill_viridis_c(direction=1)+
  labs(fill="P-PET\n(mm)")+
  geom_sf(data=Zones4_polygon_sf,fill="white",col="black",lwd=0.25,alpha=0)+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.margin = grid::unit(c(0,0,0,0), "mm"))+
  theme(legend.title = element_text(size=9),
        legend.text=element_text(size=9),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,5,0,-15))

Fig4a

Fig4b <- ggplot(data=Grid_30km_Enviro_TZ_sf)+
  geom_sf(aes(fill=elev_std),col="black",lwd=0.3)+
  scale_fill_viridis_c(direction=1)+
  labs(fill="ElevSD\n(m)")+
  geom_sf(data=Zones4_polygon_sf,fill="white",col="black",lwd=0.25,alpha=0)+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.margin = grid::unit(c(0,0,0,0), "mm"))+
  theme(legend.title = element_text(size=9),
        legend.text=element_text(size=9),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,5,0,-15))

Fig4b

Fig4c <- ggplot(data=Grid_30km_Enviro_TZ_sf)+
  geom_sf(aes(fill=DI_mean),col="black",lwd=0.3)+
  scale_fill_viridis_c(direction=1)+
  labs(fill="DI")+
  geom_sf(data=Zones4_polygon_sf,fill="white",col="black",lwd=0.25,alpha=0)+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.margin = grid::unit(c(0,0,0,0), "mm"))+
  theme(legend.title = element_text(size=9),
        legend.text=element_text(size=9),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,5,0,-15))

Fig4c

Fig4d <- ggplot(data=Grid_30km_Enviro_TZ_sf)+
  geom_sf(aes(fill=sand_mean),col="black",lwd=0.3)+
  scale_fill_viridis_c()+
  labs(fill="PctSand")+
  geom_sf(data=Zones4_polygon_sf,fill="white",col="black",lwd=0.25,alpha=0)+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.margin = grid::unit(c(0,0,0,0), "mm"))+
  theme(legend.title = element_text(size=9),
        legend.text=element_text(size=9),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,5,0,-15))

Fig4d

Fig4e <- ggplot(data=Grid_30km_Enviro_TZ_sf)+
  geom_sf(aes(fill=relprop),col="black",lwd=0.3)+
  scale_fill_viridis_c()+
  labs(fill="RelProp")+
  geom_sf(data=Zones4_polygon_sf,fill="white",col="black",lwd=0.25,alpha=0)+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.margin = grid::unit(c(0,0,0,0), "mm"))+
  theme(legend.title = element_text(size=9),
        legend.text=element_text(size=9),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,5,0,-15))

Fig4e


Fig4f <- ggplot(data=Grid_30km_Enviro_TZ_sf)+
  geom_sf(aes(fill=relprop),col="white",alpha=0)+
  scale_fill_viridis_c()+
  annotation_scale(location = "bl", width_hint = 0.25, pad_x = unit(0.0, "in"), pad_y = unit(0.25, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(1, "cm"), #how tall the arrow should be
                         width= unit(0.5, "cm"), 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_orienteering)+
  geom_sf(data=Zones4_polygon_sf,fill="white",col=NA,lwd=0.25,alpha=0)+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.margin = grid::unit(c(0,0,0,0), "mm"))+
  theme(legend.position = "None")

Fig4f


#### try plot grid
Fig4 <- plot_grid(Fig4a,Fig4b,Fig4c,Fig4d,Fig4e,Fig4f,align="v",ncol=3,labels=c("a","b","c","d","e"),label_size = 12,label_fontface = "plain")

Fig4

dpi=600
ggsave("U:/Shea/Research/GIS/Projects/TZ_Cooccurrence/Images/Fig4_pre_new2_9.tiff", device = "tiff", plot=Fig4, width = 6.5, height = 3.75, units="in", dpi = 600)


################## Plot Fig 5 ####################
Grid_30km_Enviro_TZ_cooccur_sf <- as(Grid_30km_Enviro_TZ_cooccur,"sf") %>% 
  mutate(min_scale_factor = factor(min_scale, levels = c("9999", "7500", "6000", "5000", "4285", "3750", "3000", "2500", "2000", "1500", "1200", "1000")))

TZ_poly_sf <- as(TZ_poly, "sf")

Fig5 <- ggplot(Grid_30km_Enviro_TZ_cooccur_sf)+
  geom_sf(aes(fill=min_scale_factor),col="black",lwd=.3)+
  scale_fill_brewer(palette = "RdYlBu", direction = 1, 
                    labels = c("> 7.5", "7.5", "6.0", "5.0", "4.285", "3.75", "3.0", "2.5", "2.0", "1.5", "1.2", "1"),
                    name = "Scale of\nCo-occurrence\n(km)") +
  geom_sf(data=Zones4_polygon_sf,fill="white",col="black",lwd=.25,alpha=0)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.margin = grid::unit(c(0,0,0,0), "mm"))+
  theme(legend.title = element_text(size=10),
        legend.title.align = 0.5,
        legend.text=element_text(size=10),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,5,0,-5))

Fig5

dpi = 600
ggsave("U:/Shea/Research/GIS/Projects/TZ_Cooccurrence/Images/Fig5_new2_2.tiff", device = "tiff", plot=Fig5, width = 4.5, height = 3.75, units="in", dpi = 600)


################ Plot Fig. 6 ###################
#### interaction plot of scale of co-occurrence analysis
Fig6 <- ggplot(data=cooccur_scale.2_ppet_sand, aes(x=sand_mean))+
  geom_point(data=cluster_data_Grid30_overall_TZ3,aes(x=sand_mean,y=log_min_scale, col=ppet_group),alpha=0.5)+
  geom_line(aes(x=sand_mean,y=yvar, color=fppet))+
  geom_ribbon(aes(ymax=UCL,ymin=LCL,fill=fppet),alpha=0.4)+
  scale_color_manual(breaks=c("High (247.3 mm)","Medium (222.9 mm)","Low (198.4 mm)"),
                     values=c("#56B4E9","#E69F00","#009E73","#009E73","#56B4E9","#E69F00"),
                     labels = c("High", "Medium", "Low"))+
  scale_fill_manual(values=c("#56B4E9","#E69F00","#009E73"),guide="none")+
  theme_classic()+
  theme(plot.title=element_text(hjust = 0.5),
        axis.text = element_text(size=10,color="black"),
        axis.title = element_text(size=10,color="black"),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,0,0,-10))+
  labs(x="PctSand", y="log scale of co-occurrence (km)",color="P-PET group")

Fig6

#### save plot
dpi=600
ggsave("U:/Shea/Research/GIS/Projects/TZ_Cooccurrence/Images/Fig6_new_6.tiff", device = "tiff", plot=Fig6, width = 4.5, height = 2.5, dpi = dpi, units="in")



################ Fig 7 ########################
##### NMDS results

plot(Grid30_NMS.result2,choices=c(1,2), type="n") 
text(Grid30_NMS.result2,display="species", cex=1, col="black",choices=c(1,2)) # show species as names, cex for size 
plot(Env.fit_30km)

data.scores = as.data.frame(scores(Grid30_NMS.result2$points))
data.scores$Nprop=cluster_data_Grid30_overall_TZ2$Nprop
data.scores$Bprop=cluster_data_Grid30_overall_TZ2$Bprop

sp.scores = as.data.frame(Grid30_NMS.result2$species)
sp.scores$SP <- row.names(sp.scores)

en_coord_cont = as.data.frame(scores(Env.fit_30km, "vectors")) * ordiArrowMul(Env.fit_30km)

en_coord_cont <- en_coord_cont %>% 
  mutate(names = c("Scale of\nco-occurrence", "PctSand", "DI", "ElevSD", "P-PET", "RelProp"))


Fig7 <- ggplot(data = data.scores, aes(x = MDS1, y = MDS2)) + 
  geom_point(data = data.scores, col = "gray", size = 2, alpha = 0.75) + 
  #scale_colour_manual(values = c("orange", "steelblue"))  + 
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont, size =0.5, alpha = 0.5, colour = "grey30") +
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "plain", label = en_coord_cont$names, size = (10*0.36), lineheight = .75) + 
  geom_text(data=sp.scores, aes(x =MDS1, y = MDS2), colour = "black", 
            fontface = "bold", label = sp.scores$SP, size = (10*0.36), lineheight = .75)+
  theme(axis.title = element_text(size = 10, face = "plain", colour = "black"), 
        panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"), 
        axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "plain", colour = "black"), 
        legend.text = element_text(size = 9, colour = "black")) + 
  labs(colour = "Prop. of\nNorthern\nTrees")

Fig7

dpi=600
ggsave("U:/Shea/Research/GIS/Projects/TZ_Cooccurrence/Images/Fig7_new_3.tiff", device = "tiff", plot=Fig7, width = 3.3, height = 3.1, dpi = dpi, units="in")


