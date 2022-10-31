#-------------------------------------------------------------------------------
#
# Script to clean, analyse and map the spatial effort and spatial landings
# datasets of the FDI EWG22-10 20220912 - 20220916
# Tor 3 team : Maciej, Maksims, Maurizio, Stefanos. Every 
# contribution is highlighted.
# Contact: maurizio.gibin@gmail.com
#
# Date: 2022-09-12 - 2022-09-16
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
#library(dplyr)
library(parallel)
# library(nngeo)

options(scipen = 999)
options(digits = 9)

#- Clear workspace
rm(list = ls())
#- Settings paths
#cDIR = '~/work/EWG-FDI-22-10'
cDIR = 'D:/EWG-FDI-22-10'

setwd(cDIR)
#- Settings paths
codePath         <-
  paste0(cDIR, "/scripts/")    # R scripts location
dataF            <- paste0(cDIR, "/data/")# data folder
csqF             <- paste0(cDIR, "/csquares/")
icesrF           <- paste0(cDIR, "/ices_rects/")
fshzn            <- paste0(cDIR, "/fishing_zones/")
outPath          <- paste0(cDIR, "/output/")   # output

setwd(csqF)
#load(file = "grids.RData")

csq_inside_russia_eez <- st_read('csquares_completely_inside_Russia_EEZ.shp')
st_geometry(csq_inside_russia_eez) <- NULL;
csq_inside_russia_eez <- unique(csq_inside_russia_eez$cscode)

setwd(outPath)
gc()
# FIXING THE FILTERING OF RUSSIAN EEZ CSQUARES IN LANDINGS CSVFILES ----
# EU28---------
setwd('./dissemination/old_format/EU28/csv/')
spatial_csv_files <- list.files(path = '.',pattern = glob2rx('*.csv'))
# Effort
effort <- fread(spatial_csv_files[1])
effort <- effort[!cscode %in%csq_inside_russia_eez,]
#nrow(unique(effort))
fwrite(effort,spatial_csv_files[1])
fwrite(effort,paste0('../../../new_format/EU28/',spatial_csv_files[1]))
effort_total_cscodes <- unique(effort$cscode)
# Landings
landings_total_cscodes <- unique(unlist( 
  lapply(spatial_csv_files[2:10],function(x){
    filname <- x
    print(paste('PROCESSING',filname))
    #print(getwd())
    csvf<- fread(x)
    csvf <- csvf[!cscode %in%csq_inside_russia_eez,]
    print(paste("SAVING",filname,'to',getwd(), '=START='))
    fwrite(csvf,filname)
    print(paste("SAVING",filname,'to','../../../new_format/EU28/', '=START='))
    fwrite(csvf,paste0('../../../new_format/EU28/',filname))
    
    #print(paste("SAVING",filname,'=END='))
    return( unique(csvf$cscode))
})))

effort_total_cscodes_sf <- st_sf(csq05[csq05$cscode %in% effort_total_cscodes,])
effort_total_cscodes_sf <- st_make_valid(effort_total_cscodes_sf)
st_write(effort_total_cscodes_sf,
         '../../../new_format/EU28/effort_csquares.shp',
         append=T,
         delete_dsn=T)
landings_total_cscodes_sf <- st_sf(csq05[csq05$cscode %in% landings_total_cscodes,])
landings_total_cscodes_sf <- st_make_valid(landings_total_cscodes_sf)
st_write(landings_total_cscodes_sf,
         '../../../new_format/EU28/landings_csquares.shp',
         append=T,
         delete_dsn=T)


setwd(outPath)
# EU27---------
setwd('./dissemination/old_format/EU27/csv/')
spatial_csv_files <- list.files(path = '.',pattern = glob2rx('*.csv'))
# Effort
effort <- fread(spatial_csv_files[1])
effort <- effort[!cscode %in%csq_inside_russia_eez,]
#nrow(unique(effort))
fwrite(effort,spatial_csv_files[1])
fwrite(effort,paste0('../../../new_format/EU27/',spatial_csv_files[1]))
effort_total_cscodes <- unique(effort$cscode)
# Landings
landings_total_cscodes <- unique(unlist( 
  lapply(spatial_csv_files[2:10],function(x){
    filname <- x
    print(paste('PROCESSING',filname))
    #print(getwd())
    csvf<- fread(x)
    csvf <- csvf[!cscode %in%csq_inside_russia_eez,]
    print(paste("SAVING",filname,'to',getwd(), '=START='))
    fwrite(csvf,filname)
    print(paste("SAVING",filname,'to','../../../new_format/EU27/', '=START='))
    fwrite(csvf,paste0('../../../new_format/EU27/',filname))
    
    #print(paste("SAVING",filname,'=END='))
    return( unique(csvf$cscode))
  })))

effort_total_cscodes_sf <- st_sf(csq05[csq05$cscode %in% effort_total_cscodes,])
effort_total_cscodes_sf <- st_make_valid(effort_total_cscodes_sf)
st_write(effort_total_cscodes_sf,
         '../../../new_format/EU27/effort_csquares.shp',
         append=T,
         delete_dsn=T)
landings_total_cscodes_sf <- st_sf(csq05[csq05$cscode %in% landings_total_cscodes,])
landings_total_cscodes_sf <- st_make_valid(landings_total_cscodes_sf)
st_write(landings_total_cscodes_sf,
         '../../../new_format/EU27/landings_csquares.shp',
         append=T,
         delete_dsn=T)

# FIXING THE FILTERING OF RUSSIAN EEZ CSQUARES IN LANDINGS SHAPEFILES ----
setwd(outPath)
# EU28----
setwd('./dissemination/old_format/EU28/shp/')
spatial_shp_files <- list.files(path = '.',pattern = glob2rx('*.shp'))
# Effort
effort <- st_read(spatial_shp_files[1])
effort <- effort[!effort$cscode %in%csq_inside_russia_eez,]
#nrow(unique(effort))
st_write(effort,spatial_shp_files[1],append = F)
effort <- NULL;gc()
#st_write(effort,paste0('../../../new_format/EU28/',spatial_shp_files[1]))
#effort_total_cscodes <- unique(effort$cscode)
# Landings
landings <- 
  lapply(spatial_shp_files[8:10],function(x){
    filname <- x
    print(paste('PROCESSING',filname))
    #print(getwd())
    shpf<- st_read(x)
    shpf <- shpf[!shpf$cscode %in%csq_inside_russia_eez,]
    print(nrow(shpf))
    print(paste("SAVING",filname,'to',getwd(), '=START='))
    st_write(shpf,filname,append=F)
    #print(paste("SAVING",filname,'to','../../../new_format/EU28/', '=START='))
    #fwrite(csvf,paste0('../../../new_format/EU28/',filname))
    
    #print(paste("SAVING",filname,'=END='))
    #return( unique(csvf$cscode))
  })
setwd(outPath)
# EU27----
setwd('./dissemination/old_format/EU27/shp/')
spatial_shp_files <- list.files(path = '.',pattern = glob2rx('*.shp'))
# Effort
effort <- st_read(spatial_shp_files[1])
effort <- effort[!effort$cscode %in%csq_inside_russia_eez,]
#nrow(unique(effort))
st_write(effort,spatial_shp_files[1],append = F)
effort <- NULL;gc()
#st_write(effort,paste0('../../../new_format/EU27/',spatial_shp_files[1]))
#effort_total_cscodes <- unique(effort$cscode)
# Landings
#landings <- 
  lapply(spatial_shp_files[7:10],function(x){
    filname <- x
    print(paste('PROCESSING',filname))
    #print(getwd())
    shpf<- st_read(x)
    shpf <- shpf[!shpf$cscode %in%csq_inside_russia_eez,]
    print(nrow(shpf))
    print(paste("SAVING",filname,'to',getwd(), '=START='))
    st_write(shpf,filname,append=F)
    #print(paste("SAVING",filname,'to','../../../new_format/EU27/', '=START='))
    #fwrite(csvf,paste0('../../../new_format/EU27/',filname))
    
    #print(paste("SAVING",filname,'=END='))
    #return( unique(csvf$cscode))
  })
