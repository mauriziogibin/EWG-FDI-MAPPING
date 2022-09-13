#-------------------------------------------------------------------------------
#
# Script to clean, analyse and map the spatial effort and spatial landings
# datasets of the FDI EWG21-12 20210913 - 20210917
# Tor 3 team : Maciej, Maksims, Maurizio, Stefanos. Every 
# contribution is highlighted.
# Contact: maurizio.gibin@gmail.com
#
# Date: 2021-09-13 - 2021-09-17
#
#
#-------------------------------------------------------------------------------
###################
# Maciej Adamowicz
# 14.09.2018
###################
library(data.table)
library(sf)
library(ggplot2)
library(dplyr)

options(scipen = 999)
options(digits = 9)

#- Clear workspace
rm(list=ls())
#- Settings paths
cDIR = '~/work/EWG-FDI-21-12'
setwd(cDIR)
#- Settings paths
codePath         <- paste0(cDIR, "/scripts/")    # R scripts location
dataF            <- paste0(cDIR, "/data/")# data folder
csqF             <- paste0(cDIR, "/csquares/")
icesrF           <- paste0(cDIR, "/ices_rects/")
fshzn            <- paste0(cDIR, "/fishing_zones/")
outPath          <- paste0(cDIR, "/output/")   # output

setwd(csqF)
load(file = "grids.RData")
# FDI DATA ----
setwd(dataF)
gc()
####################
#Maciej 19.09.2019 #
####################
load(file="fdi_Tableau_Table.H.RData")

fdi.tableau <- fdi.tableau[year==2014,]

gc()
# Please use data.table! In the landings script I achieve the same result 
# using DT in less time. I left this but I suggest to use DT in the future
# especially when the number of rows ia considerable, like Table H.

# fdi.tableau[,cscode:=as.character(c_square)]
setwd(fshzn)
#Loading the file with subregions assigned to fishing zones
fishing_zones           <- fread("fishing_zones_2021.csv", stringsAsFactors = F)

setwd(dataF)
nrow(fdi.tableau[is.na(c_square),])
#Assign fishing zones to the fdi.tableau data
fdi.tableau <- left_join(fdi.tableau,fishing_zones,by="sub_region")
fishing_zones <- NULL;gc()
fdi.tableau <-data.table(fdi.tableau)
gc()
# #Remove rows with sub_region = NK and remove BSAs
# fdi.tableau<-fdi.tableau[!sub_region %in% c("NK","BSA")]
# Check if all rows have a fishing zone assigned
unique(fdi.tableau[is.na(fishing_zone),.(sub_region)])

#Remove  incorrect data
fdi.tableau <- fdi.tableau[sub_region!='NK',]

fdi.tableau<-fdi.tableau[valid=="Y"]

unique(fdi.tableau$rectangle_type)
fdi.tableau[is.na(rectangle_type),rectangle_type:="05*05"]
fdi.tableau.rectangle.na.csq<-fdi.tableau[is.na(rectangle_type)]

gc()
#Create id for each lon/lat combination in the fdi.tableau data
fdi.tableau <- mutate(fdi.tableau,
              rect_id = paste(as.character(rectangle_lon),as.character(rectangle_lat),sep = '/'))
#Create id containing lon and lat of the centroid for each ICES rectangle
icesr <-
  mutate(icesr, rect_id = paste(as.character(ices_x), as.character(ices_y), sep = '/'))
gc()

#Join fdi.tableau data with ices rectangles dataset
fdi.tableau <- left_join(fdi.tableau,icesr,by="rect_id")
fdi.tableau <- mutate(fdi.tableau,cscode=as.character(c_square))
fdi.tableau <- select(fdi.tableau,country:icesname,cscode,rect_id)
gc()
#Keep the the data with the ICES rectangles assigned in a separate dataset
fdi.tableau.ices<-filter(fdi.tableau,!is.na(icesname))
sum(fdi.tableau.ices$totwghtlandg)
sum(fdi.tableau.ices$totvallandg)

#Join the fdi.tableau.ices dataset with c-squares dataset. Warning! Fishing days will be doubled.
fdi.tableau.ices<-left_join(fdi.tableau.ices,csq05,by="icesname") 
#Divide fishing days by 2 (each ICES rectangle has 2 c-squares)
fdi.tableau.ices<-mutate(fdi.tableau.ices,
                         totwghtlandg=totwghtlandg/2,
                         totvallandg=totvallandg/2,
                         cscode = cscode.y)
#Check if the total fishing days remained the same
sum(fdi.tableau.ices$totwghtlandg)
sum(fdi.tableau.ices$totvallandg)
gc()
fdi.tableau.ices<-select(fdi.tableau.ices,country:icesname,cscode,rect_id)

#Keep the the data with the ICES rectangles NOT assigned in a separate dataset 
fdi.tableau.not.ices<-filter(fdi.tableau,is.na(icesname))
fdi.tableau.not.ices.05.1<-filter(fdi.tableau.not.ices,rectangle_type=="05*1")
#Handle 0.5x1 rectangles outside ICES area
#Create the ids containing lon and lat of the centre left and centre right of the csquare
sum(fdi.tableau.not.ices.05.1$totwghtlandg)
sum(fdi.tableau.not.ices.05.1$totvallandg)

csq05<-mutate(csq05,
              rect_id=paste(as.character(w_csq),as.character(csq_y),sep='/'))#rect_id = centre/left
fdi.tableau.not.ices.05.1.left <-
  left_join(fdi.tableau.not.ices.05.1, csq05, by = "rect_id")
fdi.tableau.not.ices.05.1.left <-
  mutate(
    fdi.tableau.not.ices.05.1.left,
    totwghtlandg = totwghtlandg /
      2,
    totvallandg = totvallandg / 2,
    cscode = cscode.y
  )

csq05<-mutate(csq05,
              rect_id=paste(as.character(e_csq),as.character(csq_y),sep='/'))#rect_id = centre/right
fdi.tableau.not.ices.05.1.right<-left_join(fdi.tableau.not.ices.05.1,csq05,by="rect_id")
fdi.tableau.not.ices.05.1.right <- mutate(fdi.tableau.not.ices.05.1.right,
                                          totwghtlandg=totwghtlandg/2,
                                          totvallandg=totvallandg/2,
                                          cscode = cscode.y)
fdi.tableau.not.ices.05.1<-rbind(fdi.tableau.not.ices.05.1.left,fdi.tableau.not.ices.05.1.right)

fdi.tableau.not.ices.05.1<-select(fdi.tableau.not.ices.05.1,country:rect_id,icesname = icesname.y,cscode)

# fdi.tableau.not.ices.05.1<-rename(fdi.tableau.not.ices.05.1,icesname=icesname.x)
# fdi.tableau.not.ices.05.1<-mutate(fdi.tableau.not.ices.05.1,c_square = cscode)

sum(fdi.tableau.not.ices.05.1$totwghtlandg)
sum(fdi.tableau.not.ices.05.1$totvallandg)

print(paste0("fdi.tableau.not.ices.05.1 number of rows without cscode: ",nrow(filter(fdi.tableau.not.ices.05.1,is.na(cscode)))))
# temp<-filter(fdi.tableau.not.ices.05.1,is.na(cscode)) %>% 
#   select(country:icesname)
fdi.tableau.not.ices.05.1<-filter(fdi.tableau.not.ices.05.1,!is.na(cscode))

# Check what rectangle types are present in the rest of the data
fdi.tableau.not.ices %>% 
  filter(rectangle_type!="05*1") %>% 
  group_by(rectangle_type) %>% 
  summarise(n=n())

# The rest of the data has 05*05, 1*1 and 5*5 rectangle_type

# Handle 05*05 rectangles
fdi.tableau.csq.05.05 <- filter(fdi.tableau.not.ices,rectangle_type=="05*05") %>% 
  rename(csq_c_id=rect_id)

# Create the id containing lon and lat of the centroid for each c-square
csq05<-mutate(csq05,
              csq_c_id=paste(as.character(csq_x),as.character(csq_y),sep='/'))


# Join the fdi.tableau.csq.c dataset with c-squares dataset.
fdi.tableau.csq.05.05<-left_join(fdi.tableau.csq.05.05,csq05,by="csq_c_id")
gc()
fdi.tableau.csq.05.05<-select(fdi.tableau.csq.05.05,country:csq_c_id,icesname = icesname.y,cscode = cscode.y)
gc()
# Check if there are any rows without csquare assigned
print(paste0("fdi.tableau.csq.05.05 number of rows without cscode: ",nrow(filter(fdi.tableau.csq.05.05,is.na(cscode)))))

# Handle 1*1 rectangles
fdi.tableau.csq.1.1 <- filter(fdi.tableau.not.ices,rectangle_type=="1*1") %>% 
  rename(csq_c_id=rect_id)

csquares.1.1 <- select(fdi.tableau.csq.1.1,csq_c_id,rectangle_lon,rectangle_lat) %>% 
  mutate(key=1) %>% 
  distinct()
grid.for.1.1 <- data.frame(lon_diff = c(0,0.5,0.5,0),
                           lat_diff = c(0,0,0.5,0.5),
                           key= c(1))
csquares.1.1 <- inner_join(csquares.1.1,grid.for.1.1,by="key") %>% 
  mutate(bl_lon = rectangle_lon - lon_diff,
         bl_lat = rectangle_lat - lat_diff) %>% 
  select(csq_c_id, bl_lon, bl_lat)

sum(fdi.tableau.csq.1.1$totwghtlandg)
sum(fdi.tableau.csq.1.1$totvallandg)

fdi.tableau.csq.1.1 <- fdi.tableau.csq.1.1 %>% 
  inner_join(csquares.1.1,by="csq_c_id") %>% 
  mutate(totwghtlandg=totwghtlandg/4,
         totvallandg=totvallandg/4,
         rect_id=paste(as.character(bl_lon),as.character(bl_lat),sep='/'))
sum(fdi.tableau.csq.1.1$totwghtlandg)
sum(fdi.tableau.csq.1.1$totvallandg)

# create bottom-left id of csquare
csq05<-mutate(csq05,
              rect_id=paste(as.character(w_csq),as.character(s_csq),sep='/'))
fdi.tableau.csq.1.1<-left_join(fdi.tableau.csq.1.1,csq05,by="rect_id")

fdi.tableau.csq.1.1<-select(fdi.tableau.csq.1.1,country:fishing_zone,rect_id,icesname = icesname.y,cscode = cscode.y)
print(paste0("fdi.tableau.csq.1.1 number of rows without cscode: ",nrow(filter(fdi.tableau.csq.1.1,is.na(cscode)))))

# Handle 5*5 rectangles
fdi.tableau.csq.5.5 <- filter(fdi.tableau.not.ices,rectangle_type=="5*5") %>% 
  rename(csq_c_id=rect_id)

csquares.5.5 <- select(fdi.tableau.csq.5.5,csq_c_id,rectangle_lon,rectangle_lat) %>% 
  mutate(key=1) %>% 
  distinct()

grid.for.5.5 <- inner_join(data.frame(lon_diff=seq(-2, by=0.5, length.out = 10), key=1),
                          data.frame(lat_diff=seq(-2, by=0.5, length.out = 10), key=1), 
                          by="key")

csquares.5.5 <- inner_join(csquares.5.5,grid.for.5.5,by="key") %>% 
  mutate(bl_lon = rectangle_lon - lon_diff,
         bl_lat = rectangle_lat - lat_diff) %>% 
  select(csq_c_id, bl_lon, bl_lat)

sum(fdi.tableau.csq.5.5$totwghtlandg)
sum(fdi.tableau.csq.5.5$totvallandg)

fdi.tableau.csq.5.5 <- fdi.tableau.csq.5.5 %>% 
  inner_join(csquares.5.5,by="csq_c_id") %>% 
  mutate(totwghtlandg=totwghtlandg/100,
         totvallandg=totvallandg/100,
         rect_id=paste(as.character(bl_lon),as.character(bl_lat),sep='/'))
sum(fdi.tableau.csq.5.5$totwghtlandg)
sum(fdi.tableau.csq.5.5$totvallandg)

# create bottom-left id of csquare
csq05<-mutate(csq05,
              rect_id=paste(as.character(w_csq),as.character(s_csq),sep='/'))
fdi.tableau.csq.5.5<-left_join(fdi.tableau.csq.5.5,csq05,by="rect_id")

fdi.tableau.csq.5.5<-select(fdi.tableau.csq.5.5,country:fishing_zone,rect_id, icesname = icesname.y,cscode = cscode.y)
print(paste0("fdi.tableau.csq.5.5 number of rows without cscode: ",nrow(filter(fdi.tableau.csq.5.5,is.na(cscode)))))

csq05 <- NULL;gc()
fdi.tableau.rectangle.na.csq     <- NULL;gc();
fdi.tableau.not.ices.05.1.left   <- NULL;gc();
fdi.tableau.not.ices.05.1.right  <- NULL;gc();

fdi.tableau.ices          <-rename(fdi.tableau.ices,geo_id=rect_id)
fdi.tableau.not.ices.05.1 <-rename(fdi.tableau.not.ices.05.1,geo_id=rect_id)
fdi.tableau.csq.05.05     <-rename(fdi.tableau.csq.05.05,geo_id=csq_c_id)
fdi.tableau.csq.1.1       <-rename(fdi.tableau.csq.1.1,geo_id=rect_id)
fdi.tableau.csq.5.5       <-rename(fdi.tableau.csq.5.5,geo_id=rect_id)

FDItotalweightlandg <- round(sum(fdi.tableau$totwghtlandg),0)
FDItotalvallandg <- round(sum(fdi.tableau$totvallandg),0)
fdi.tableau   <- NULL;gc()
csq05Land     <- NULL;gc()
csquares.1.1  <- NULL;gc()
csquares.5.5  <- NULL;gc()

result<-as.data.table(rbind(fdi.tableau.ices,
                            fdi.tableau.csq.05.05,
                            fdi.tableau.not.ices.05.1,
                            fdi.tableau.csq.1.1,
                            fdi.tableau.csq.5.5))
fdi.tableau.ices                 <- NULL;gc();
fdi.tableau.csq.05.05            <- NULL;gc();
fdi.tableau.not.ices.05.1        <- NULL;gc();
fdi.tableau.csq.1.1              <- NULL;gc();
fdi.tableau.csq.5.5              <- NULL;gc();
fdi.tableau.not.ices             <- NULL;gc();

print(paste0("number of rows without cscode: ",nrow(filter(result,is.na(geo_id)))))
print(paste0("Are total landings correct? : ",round(sum(result$totwghtlandg),0)==FDItotalweightlandg))
print(paste0("Are total value of landings correct? : ",round(sum(result$totvallandg),0)==FDItotalvallandg))

# Omitting the Portugal data as per request

# Confidentiality and last errors removal
country_by_csquare <- result[,.('country'=unique(country)),by=.(cscode)]
country_by_csquare <- country_by_csquare[,nrows:=nrow(.SD),by=.(cscode)]

# NLD asked to omit the confidential records for those csquares where there is
# only the Dutch fleet
NLD_csquares <- country_by_csquare[country=='NLD'&nrows==1,]$cscode
# Now I will remove the confidential records for those c squares where there is only
# the dutch fleet
result <- result[!(cscode %in% NLD_csquares & country=='NLD'& (confidential=='A'|confidential=='V')),]
# PRT
result <- result[!(country == 'PRT' & (confidential=='A'|confidential=='V') & supra_region =='OFR'), ]
# ITA
result <- result[!(country == 'ITA' & year=='2020'), ]

# Creating the two versions:
# resultEU28 with UK 
# resultEU27 without UK 
names(result)
#result <- result[valid=='Y',]
resultEU28 <- result
resultEU27 <- result[!(country %in% c('ENG','SCO','GBG','GBJ','IOM','NIR')),]
result <- NULL;gc()



fields.to.keep <- c("year","quarter","vessel_length","fishing_tech","gear_type","mesh_size_range",
                    "target_assemblage","metier","supra_region","sub_region","specon_tech",
                    "deep","rectangle_type","rectangle_lon","rectangle_lat", "species",
                    "totwghtlandg","totvallandg","icesname","cscode")   



resultEU28 <- resultEU28[,fields.to.keep, with = FALSE]


resultEU28.cs <- resultEU28[,.(sum(totwghtlandg,na.rm = T),
                       sum(totvallandg,na.rm = T),
                       unique(rectangle_lon),
                       unique(rectangle_lat)),
                    by = c("year","quarter","vessel_length","fishing_tech","gear_type","mesh_size_range",
                           "target_assemblage","metier","supra_region","sub_region","specon_tech", "species",
                           "deep","rectangle_type","icesname","cscode")]

resultEU28 <- NULL;gc()
setnames(resultEU28.cs,c('V1','V2','V3','V4') ,c("totwghtlandg","totvallandg","rectangle_lon","rectangle_lat"))
# Number of c_sqaures with weight and vlaue = 0
nrow(resultEU28.cs[(totwghtlandg == 0 & totvallandg == 0),])
# Omitting the records
resultEU28.cs <- resultEU28.cs[!(totwghtlandg == 0 & totvallandg == 0),]

setwd(outPath)
csq05Coast <- NULL;
csq05grid  <- NULL;
csq05Land  <- NULL;
csq05Sea   <- NULL;
csq05pts   <-  NULL;
resultEU28DT   <- as.data.table(resultEU28.cs)
fwrite(resultEU28DT, 'spatial_landings_tableau_pts.csv')
resultEU28DT <- NULL;
load(file='../csquares/grids.RData')
resultEU28_sf <- left_join(resultEU28.cs, csq05[, c("cscode", "geometry")], by = 'cscode')
resultEU28.cs <- NULL;gc()
resultEU28_sf$rectangle_lat <- NULL;
resultEU28_sf$rectangle_lon <- NULL;
resultEU28_sf <- st_sf(
  resultEU28_sf,
  sf_column_name = 'geometry',
  crs = 4326)

# for (year in unique(resultEU28_sf$year)) {
#   st_write(resultEU28_sf[resultEU28_sf$year == as.numeric(year), ],
#            paste0("spatial_landings_tableau_", year, ".geojson"))
#   gc()
# }
for (year in unique(resultEU28_sf$year)) {
  st_write(resultEU28_sf[resultEU28_sf$year == as.numeric(year), ],
           paste0("spatial_landings_tableau_", year, "_EU28.shp"))
  gc()
}
# For some reason year 2018 has a feature with an error, feature 676321.
resultEU28_sf_2014 <- resultEU28_sf[resultEU28_sf$year ==2014,]
resultEU28_sf_2015 <- resultEU28_sf[resultEU28_sf$year ==2015,]
resultEU28_sf[resultEU28_sf$year ==2015,] <- NULL;gc()
resultEU28_sf_2016 <- resultEU28_sf[resultEU28_sf$year ==2016,]
resultEU28_sf[resultEU28_sf$year ==2016,] <- NULL;gc()
resultEU28_sf_2017 <- resultEU28_sf[resultEU28_sf$year ==2017,]
resultEU28_sf[resultEU28_sf$year ==2017,] <- NULL;gc()
resultEU28_sf_2018 <- resultEU28_sf[resultEU28_sf$year ==2018,]
resultEU28_sf[resultEU28_sf$year ==2018,] <- NULL;gc()
resultEU28_sf_2019 <- resultEU28_sf[resultEU28_sf$year ==2019,]
resultEU28_sf_2020 <- resultEU28_sf[resultEU28_sf$year ==2020,]

resultEU28_sf <- NULL;gc()
# plot(resultEU28_sf_2018[676321,])
# st_is_valid(resultEU28_sf_2018[676321,])
st_write(resultEU28_sf_2014,'spatial_landings_tableau_2014_EU28.shp',delete_dsn = TRUE)
st_write(resultEU28_sf_2015,'spatial_landings_tableau_2015_EU28.shp',delete_dsn = TRUE)
st_write(resultEU28_sf_2016,'spatial_landings_tableau_2016_EU28.shp',delete_dsn = TRUE)
st_write(resultEU28_sf_2017,'spatial_landings_tableau_2017_EU28.shp',delete_dsn = TRUE)
st_write(resultEU28_sf_2018,'spatial_landings_tableau_2018_EU28.shp',delete_dsn = TRUE)
st_write(resultEU28_sf_2019,'spatial_landings_tableau_2019_EU28.shp',delete_dsn = TRUE)
st_write(resultEU28_sf_2020,'spatial_landings_tableau_2020_EU28.shp',delete_dsn = TRUE)

result_sf <- NULL;gc()
# spatial_landings_tableau <- fread('spatial_landings_tableau_pts_EU28.csv')
# totvallandgcountryyear <- spatial_landings_tableau[,sum(totvallandg),by=.(year,quarter)]

# Deleting objects
resultEU28 <- NULL;
resultEU28DT <- NULL;
resultEU28.cs <- NULL;
resultEU28_sf <- NULL;gc()
resultEU28_sf_2014 <- NULL;gc()
resultEU28_sf_2015 <- NULL;gc()
resultEU28_sf_2016 <- NULL;gc()
resultEU28_sf_2017 <- NULL;gc()
resultEU28_sf_2018 <- NULL;gc()
resultEU28_sf_2019 <- NULL;gc()
resultEU28_sf_2020 <- NULL;gc()
gc()

resultEU27 <- resultEU27[,fields.to.keep, with = FALSE]
resultEU27.cs <- resultEU27[,.(sum(totwghtlandg,na.rm = T),
                       sum(totvallandg,na.rm = T),
                       unique(rectangle_lon),
                       unique(rectangle_lat)),
                    by = c("year","quarter","vessel_length","fishing_tech","gear_type","mesh_size_range",
                           "target_assemblage","metier","supra_region","sub_region","specon_tech", "species",
                           "deep","rectangle_type","icesname","cscode")]

resultEU27 <- NULL;gc()
setnames(resultEU27.cs,c('V1','V2','V3','V4') ,c("totwghtlandg","totvallandg","rectangle_lon","rectangle_lat"))
# Number of c_sqaures with weight and vlaue = 0
nrow(resultEU27.cs[(totwghtlandg == 0 & totvallandg == 0),])
# Omitting the records
#resultEU27.cs <- resultEU27.cs[!(totwghtlandg == 0 & totvallandg == 0),]

setwd(outPath)
csq05Coast <- NULL;
csq05grid  <- NULL;
csq05Land  <- NULL;
csq05Sea   <- NULL;
csq05pts   <-  NULL;
resultEU27DT   <- as.data.table(resultEU27.cs)
fwrite(resultEU27DT, 'spatial_landings_tableau_pts_EU27.csv')
resultEU27DT <- NULL;
load(file='../csquares/grids.RData')
resultEU27_sf <- left_join(resultEU27.cs, csq05[, c("cscode", "geometry")], by = 'cscode')
resultEU27.cs <- NULL;gc()
resultEU27_sf$rectangle_lat <- NULL;
resultEU27_sf$rectangle_lon <- NULL;
resultEU27_sf <- st_sf(
  resultEU27_sf,
  sf_column_name = 'geometry',
  crs = 4326)

# for (year in unique(resultEU27_sf$year)) {
#   st_write(resultEU27_sf[resultEU27_sf$year == as.numeric(year), ],
#            paste0("spatial_landings_tableau_", year, ".geojson"))
#   gc()
# }
for (year in unique(resultEU27_sf$year)) {
  st_write(resultEU27_sf[resultEU27_sf$year == as.numeric(year), ],
           paste0("spatial_landings_tableau_", year, "_EU27.shp"))
  gc()
}
# For some reason year 2018 has a feature with an error, fearutre 676321.
resultEU27_sf_2014 <- resultEU27_sf[resultEU27_sf$year ==2014,]
resultEU27_sf_2015 <- resultEU27_sf[resultEU27_sf$year ==2015,]
resultEU27_sf[resultEU27_sf$year ==2015,] <- NULL;gc()
resultEU27_sf_2016 <- resultEU27_sf[resultEU27_sf$year ==2016,]
resultEU27_sf[resultEU27_sf$year ==2016,] <- NULL;gc()
resultEU27_sf_2017 <- resultEU27_sf[resultEU27_sf$year ==2017,]
resultEU27_sf[resultEU27_sf$year ==2017,] <- NULL;gc()
resultEU27_sf_2018 <- resultEU27_sf[resultEU27_sf$year ==2018,]
resultEU27_sf[resultEU27_sf$year ==2018,] <- NULL;gc()
resultEU27_sf_2019 <- resultEU27_sf[resultEU27_sf$year ==2019,]
resultEU27_sf_2020 <- resultEU27_sf[resultEU27_sf$year ==2020,]

resultEU27_sf <- NULL;gc()
# plot(resultEU27_sf_2018[676321,])
# st_is_valid(resultEU27_sf_2018[676321,])
st_write(resultEU27_sf_2014,'spatial_landings_tableau_2014_EU27.shp',delete_dsn = TRUE)
st_write(resultEU27_sf_2015,'spatial_landings_tableau_2015_EU27.shp',delete_dsn = TRUE)
st_write(resultEU27_sf_2016,'spatial_landings_tableau_2016_EU27.shp',delete_dsn = TRUE)
st_write(resultEU27_sf_2017,'spatial_landings_tableau_2017_EU27.shp',delete_dsn = TRUE)
st_write(resultEU27_sf_2018,'spatial_landings_tableau_2018_EU27.shp',delete_dsn = TRUE)
st_write(resultEU27_sf_2019,'spatial_landings_tableau_2019_EU27.shp',delete_dsn = TRUE)
st_write(resultEU27_sf_2020,'spatial_landings_tableau_2020_EU27.shp',delete_dsn = TRUE)

resultEU27_sf <- NULL;gc()
resultEU27_sf_2014 <- NULL;gc()
resultEU27_sf_2015 <- NULL;gc()
resultEU27_sf_2016 <- NULL;gc()
resultEU27_sf_2017 <- NULL;gc()
resultEU27_sf_2018 <- NULL;gc()
resultEU27_sf_2019 <- NULL;gc()
resultEU27_sf_2020 <- NULL;gc()

# spatial_landings_tableau <- fread('spatial_landings_tableau_pts_EU27.csv')
# totvallandgcountryyear <- spatial_landings_tableau[,sum(totvallandg),by=.(year,quarter)]
