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
#########
#Maurizio#
#########
library(data.table)
library(sf)
library(ggplot2)
library(dplyr)
#- Clear workspace
rm(list=ls())
cDIR = '/home/maurizio/work/EWG-FDI-21-12'
setwd(cDIR)
#- Settings paths
codePath         <- paste0(cDIR, "/scripts/")    # R scripts location
dataF            <- paste0(cDIR, "/data/")# data folder
csqF             <- paste0(cDIR, "/csquares/")
icesrF           <- paste0(cDIR, "/ices_rects/")
outPath          <- paste0(cDIR, "/output/")   # output
# FDI DATA ----
setwd(dataF)
# Loading the spatial effort and landings data from ftp
fnames <- c("TABLE_I", "TABLE_H")
# for(i in fnames){

# for(i in fnames){
setwd('./original/')
fList <- list.files(path='.',pattern=glob2rx('table_h_????.csv'))
fdi   <- rbindlist(lapply(fList,fread,stringsAsFactors=F,nThread=3)) 
setwd('../')
gc()

i<-"Table.H"
#fdi            <- fread(paste(i,".csv", sep=''), stringsAsFactors = F)
fdi.n<-nrow(fdi)
# Converting NA text to NA value
fdi[fdi=="NA"] <- NA
gc()
fdi[,c_square:=as.character(c_square)]
fdi <- setorder(fdi,year,quarter)
fdi[,id := 1:.N,]
###########################
# MACIEK 2019 adjustments #
###########################
if("country_code" %in% colnames(fdi)){
  setnames(fdi,old=c("country_code"),new=c("country"))  
}
fdi[,':='(rectangle_lat = as.numeric(rectangle_lat),
          rectangle_lon = as.numeric(rectangle_lon))]

# Number of rows not having both coordinates or c_square
errors.no.lat.lon.no.csq <-
  fdi[is.na(rectangle_lon) & is.na(rectangle_lat)& is.na(c_square)]
# Errors in min max coords
errors.lat.lon.bounds <- fdi[(rectangle_lon < -180 | rectangle_lon > 180)
                             |(rectangle_lat < -90 | rectangle_lat > 90),]
# Errors in only rectangle type max coords
errors.rect.only <- fdi[is.na(rectangle_type) &!is.na(rectangle_lat)
                        &!is.na(rectangle_lon) & is.na(c_square),]
# Number of rows having only one coordinate. Terrible case but luckily no rows.
errors.one.coord <- fdi[is.na(rectangle_lon) != is.na(rectangle_lat)]
# Number of rows with csquare only and wrong rect type
errors.csq.rectangle_type <- fdi[is.na(rectangle_lon) & is.na(rectangle_lat)
                                 & !is.na(c_square) & rectangle_type != '05*05',]
# Data subset having csquares and coords
fdi.csq.coords<-fdi[!is.na(rectangle_lat) & !is.na(rectangle_lon) & !is.na(c_square)]
# Data subset having csquares only
fdi.csq<-fdi[is.na(rectangle_lat) & is.na(rectangle_lon) & !is.na(c_square)]
# Data subset having coords only
fdi.coords<-fdi[!is.na(rectangle_lat) & !is.na(rectangle_lon) & is.na(c_square)]
nrow(fdi)-nrow(fdi.coords)-nrow(fdi.csq)-nrow(fdi.csq.coords)
setwd(csqF)
load(file = "grids.RData")
csq05$geometry <- NULL
gc()
# Checking if coords are consistent with csquares
fdi.csq.coords<-merge(fdi.csq.coords,csq05[,c("cscode","type","csq_x","csq_y")], all.x=T,by.x="c_square", by.y="cscode")
fdi.csq.coords[rectangle_lat == csq_y & rectangle_lon == csq_x,valid:="YES"]
nrow(fdi.csq.coords[valid=='YES',])
# Number of records omitted
nrow(fdi.csq.coords)- nrow(fdi.csq.coords[valid=='YES',]) # 5120 to omit
errors.csq.coords <- fdi.csq.coords[is.na(valid),]
fdi.csq.coords<-fdi.csq.coords[valid=="YES",.(country,year,quarter,vessel_length,fishing_tech,gear_type,
                                              target_assemblage,mesh_size_range,metier,supra_region,
                                              sub_region,eez_indicator,geo_indicator,specon_tech,deep,species,
                                              rectangle_type,rectangle_lat,rectangle_lon,c_square,valid,totwghtlandg,
                                              totvallandg,confidential,id)]

fdi.csq.coords$valid <- NA;gc()
# Assigning coords to csquares
fdi.csq<-merge(fdi.csq,csq05[,c("cscode","type","csq_x","csq_y")], all.x=T,by.x="c_square", by.y="cscode")
gc()
nrow(fdi.csq[is.na(csq_x),]) # the join was a 100% match
fdi.csq<-fdi.csq[,.(country,year,quarter,vessel_length,fishing_tech,gear_type,
                    target_assemblage,mesh_size_range,metier,supra_region,
                    sub_region,eez_indicator,geo_indicator,specon_tech,deep,species,
                    rectangle_type,csq_y,csq_x,c_square,totwghtlandg,totvallandg,
                    confidential,id)]
setnames(fdi.csq,old = c("csq_y","csq_x"), new = c("rectangle_lat","rectangle_lon"))
nrow(fdi.csq[is.na(rectangle_lon),])
nrow(fdi.csq[is.na(rectangle_lat),])
# Checking if coords are consistent with the rectangle type
# Check coordinates according to the type of rectangle
fdi.coords[,`:=`(remainder_lon=rectangle_lon%%1,
                 remainder_lat=rectangle_lat%%1)]
fdi.coords[rectangle_type == "5*5",`:=`(remainder_lon=rectangle_lon%%5,
                                        remainder_lat=rectangle_lat%%5)]
fdi.coords[(rectangle_type=="05*05" & (!remainder_lon %in% c(0.25,0.75) | !remainder_lat %in% c(0.25,0.75))) |
             (rectangle_type=="05*1" & (remainder_lon != 0.50 | !remainder_lat %in% c(0.25,0.75))) |
             (rectangle_type=="1*1" & (remainder_lon != 0.50 | remainder_lat != 0.50)) |
             (rectangle_type=="5*5" & (remainder_lon != 2.5 | remainder_lat != 2.5)),
           valid:="NO"]

rect.check<-fdi.coords[,.(nrows=.N),by=.(country,rectangle_type,remainder_lon,remainder_lat,valid)]

# Points on land
csq05Land$geometry <- NULL
fdi.coords <- merge(fdi.coords,csq05Land[,c("type","csq_x","csq_y")],
                    by.x = c("rectangle_lon","rectangle_lat"),
                    by.y = c("csq_x","csq_y"),
                    all.x = TRUE)
gc()
fdi.coords[type=='land',valid:='NO']
fdi.coords.on.land <- fdi.coords[type=='land',]
fdi.csq <- merge(fdi.csq,csq05Land[,c("type","cscode")],
                 by.x = "c_square",
                 by.y = "cscode",
                 all.x = TRUE)
fdi.csq[type=='land',valid:='NO']
fdi.csq.on.land <- fdi.csq[type=='land',]
fdi.csq.coords <- merge(fdi.csq.coords,csq05Land[,c("type","cscode")],
                        by.x = "c_square",
                        by.y = "cscode",
                        all.x = TRUE)
fdi.csq.coords[type=='land',valid:='NO']
fdi.csq.coords.on.land <- fdi.csq.coords[type=='land',]
fdi.csq.on.land    <- fdi.csq.on.land[,lapply(.SD,as.character)]
fdi.coords.on.land <- fdi.coords.on.land[,lapply(.SD,as.character)]
cols <- names(fdi.coords.on.land)[names(fdi.coords.on.land)%in% names(fdi.csq.on.land)]

points.on.land <- rbind(fdi.csq.on.land,
                        fdi.coords.on.land[,.SD,.SDcols = cols])

errors.csq.rectangle_type$valid <-'NO'
errors.csq.rectangle_type$type  <- NA
errors.csq.coords$valid         <-'NO'

# At the end we have the following errors
cols <- names(fdi)
errors.rect.check <- fdi.coords[valid=='NO',]

errors.ids <- unique(
  c(
    errors.lat.lon.bounds$id,
    errors.no.lat.lon.no.csq$id,
    errors.one.coord$id,
    errors.rect.only$id,
    errors.csq.coords$id,
    errors.csq.rectangle_type$id,
    errors.rect.check$id,
    fdi.coords.on.land$id,
    fdi.csq.on.land$id
  )
)

cols <- names(fdi)
# fwrite(fdi.csq,'fdi.csq.table.h.csv')
# fwrite(fdi,'fdi.table.h.rbind.csv')
fdi.no.csq <- fdi[!id%in%fdi.csq$id,]
fdi <- NULL;gc()
fdi <- rbind(fdi.csq[,!c("type","valid")],fdi.no.csq)
fdi.csq <- NULL;
fdi.no.csq <- NULL;gc()

fdi <- fdi[, valid := "Y"]
fdi <- fdi[id %in% errors.ids, valid := "N"]

errors.csq.coords <- NULL;                
errors.csq.rectangle_type<- NULL;
errors.lat.lon.bounds<- NULL;
errors.no.lat.lon.no.csq<- NULL;
errors.one.coord<- NULL;
errors.rect.check<- NULL;
errors.rect.only<- NULL;
fdi.coords<- NULL;
fdi.coords.on.land<- NULL;
fdi.csq<- NULL;
fdi.csq.on.land<- NULL;
fdi.csq.coords.on.land<- NULL;
fdi.csq.coords<- NULL;
fdi.rect.lat.lon<- NULL;
fdi.csq.rect.lat.lon<- NULL;
fdi.no.csq<- NULL;
na.rectangle_type<- NULL;
na.rectangle_type.na.c_square<- NULL;
points.on.land<- NULL;

gc()
# I need to accept FRA point on land after a discussion with Sebastien
fdi[country=='FRA'&rectangle_lon==2.75&rectangle_lat==43.25,]
fdi[country=='FRA'&rectangle_lon==2.75&rectangle_lat==43.25,valid:='Y']
fdi[country=='FRA'&rectangle_lon==2.75&rectangle_lat==43.25,]

setwd(dataF)
# List of the gears
gearsFDI       <- unique(fdi$gear_type)
# List of the mesh size
meshSize       <- unique(fdi$mesh_size_range)
# table(fdi$gear_type,fdi$mesh_size_range)
# Defining the trawl macro area aggregation
trawl          <- as.list(c("OTB", "PTB", "OTM", "PTM", "OTT"))
fdi[gear_type %in%  trawl, gear_typeN := "TRAWL",] 
fdi[!gear_type %in% trawl, gear_typeN := gear_type, ]
# unique mesh size
TRAWLMs         <- fdi[gear_typeN == "TRAWL", ]
TRAWLMsu        <- TRAWLMs[, unique(mesh_size_range), by = "gear_typeN"]
names(TRAWLMsu) <- c("gear", "meshsize")
msU             <- unique(TRAWLMsu$meshsize)
sort(msU)
# checking which country used the 90D105 mesh size range
mesh90d105      <- fdi[mesh_size_range == "90D105",]
unique(mesh90d105$country)

m100            <- as.list(c("100D110", "100D120", "100DXX","105D110", "110D120", "110DXX",  "120DXX", 
                             "100D400", "400DXX"))
l100            <- as.list(c("00D14","14D16" , "14D20","00D16", "16D20", "16D32", "20D40","40D50" , 
                             "32D70", "32D80","50D100" , "32D90", "40D45", "40D55", "45D50","50D100",
                             "50D65","55D60", "65D100", "60D65", "65D70", "70D100","70D80", "70S90", 
                             "80D100","65D70", "70D100","90D105", "80D100", "70D80","00S40", "40SXX", 
                             "70S90"))

fdi[gear_typeN == "TRAWL" & mesh_size_range == "NK",
    gear_typeN := "TRAWLNONE", ]

fdi[gear_typeN == "TRAWL" & mesh_size_range %in% m100,
    gear_typeN := "TRAWLM100", ]

fdi[gear_typeN == "TRAWL" & mesh_size_range %in% l100,
    gear_typeN := "TRAWLL100", ]

unique(fdi$gear_typeN)

tbbMSU                <- fdi[ gear_typeN == "TBB", unique(mesh_size_range),]
sort(tbbMSU)

m120                  <- as.list(c("120DXX", "100D400","400DXX"))

# The 100DXX mesh size range is presented in 8 lines. 
# Accordingly metier these lines could be assigned as TBBL120. 

l120                  <- as.list(c("00D14","14D16" , "14D20","00D16", "16D20", "16D32", "20D40","40D50" , 
                                   "32D70", "32D80","50D100" , "32D90", "40D45", "40D55", "45D50","50D100",
                                   "50D65","55D60", "65D100", "60D65", "65D70", "70D100","70D80", "70S90", 
                                   "80D100","65D70", "70D100","90D105", "80D100", "70D80","00S40", "40SXX", 
                                   "70S90", "105D110", "100D110", "110D120", "100DXX"))

fdi[gear_typeN == "TBB" & mesh_size_range == "NK", gear_typeN := "TBBNONE",]
fdi[gear_typeN == "TBB" & mesh_size_range %in% m120, gear_typeN := "TBBM120",]
fdi[gear_typeN == "TBB" & mesh_size_range %in% l120, gear_typeN := "TBBL120",]
gear_typeU             <- unique(fdi$gear_typeN)
sort(gear_typeU)

#########
#Maksims#
#########

#Define gear classes

seine   <- as.list(c("SDN", "SPR", "SSC", "SB", "SV"))
nets    <- as.list(c("GND", "GNS", "GNC", "GTR", "GTN"))
dredges <- as.list(c("DRB", "HMD", "DRH"))
hooks   <- as.list(c("LHM", "LHP", "LLD", "LLS", "LTL"))
snets   <- as.list(c("PS", "LA"))
traps   <- as.list(c("FPO", "FPN", "FYK"))

fdi[gear_typeN %in% seine, gear_typeN := "SEINE",]
fdi[gear_typeN %in% nets, gear_typeN := "NETS",]
fdi[gear_typeN %in% dredges, gear_typeN := "DREDGES",]
fdi[gear_typeN %in% hooks, gear_typeN := "HOOKS",]
fdi[gear_typeN %in% snets, gear_typeN := "sNETS",]
fdi[gear_typeN %in% traps, gear_typeN := "TRAPS",]

geartypeU             <- unique(fdi$gear_typeN)


gclasses <- as.list(c("DREDGES", "HOOKS", "NETS", "SEINE", "sNETS", "TBBL120",
                      "TBBM120", "TBBNONE", "TRAPS", "TRAWLL100", "TRAWLM100",
                      "TRAWLNONE"))

# if(i == "TABLE_I") svalue <- "fishing_days=sum(totwghtlandg)" else svalue <- "landings=sum(totwghtlandg)"

fdi <- fdi[gear_typeN %in% gclasses]

# This above is deadly long so I substitued with data.table syntax, and checked
# the output is the same.
categorical.fields.tableau <-
  c('country',
    'year',
    'quarter',
    'vessel_length',
    'fishing_tech',
    'gear_type',
    'mesh_size_range',
    'target_assemblage',
    'metier',
    'supra_region',
    'sub_region',
    'specon_tech',
    'deep',
    'species',
    'rectangle_type',
    "cscode" = 'c_square',
    'rectangle_lat',
    'rectangle_lon',
    'confidential',
    'valid'
  )
# I need to remove the confidential records for Portugal for Tableau
fdi[,totvallandg:=as.numeric(totvallandg),]

fdi.tableau <-
  fdi[, .(sum(totwghtlandg,na.rm = T),
          sum(totvallandg,na.rm = T)), by = categorical.fields.tableau]
fdi.tableau[,`:=`(totwghtlandg = V1,
                  totvallandg = V2,
                  V1 = NULL,
                  V2 = NULL)]
nrow(unique(fdi.tableau, by = c(
  categorical.fields.tableau, c('totwghtlandg',
                                'totvallandg')
)))

setwd(dataF)
save(fdi.tableau,file = paste("fdi_Tableau_", i, ".RData", sep=''))
rm(list=ls())
gc()
