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
#Maksims#
#########
library(data.table)
library(sf)
library(ggplot2)
library(dplyr)

#- Clear workspace
rm(list=ls())
# cDir <- setwd("~Work=-/FDI2018/")

cDIR = '~/work/EWG-FDI-21-12'
setwd(cDIR)
#- Settings paths
codePath         <- paste0(cDIR, "/scripts/")    # R scripts location
dataF            <- paste0(cDIR, "/data/")# data folder
csqF             <- paste0(cDIR, "/csquares/")
icesrF           <- paste0(cDIR, "/ices_rects/")
fshzn            <- paste0(cDIR, "/fishing_zones/")
outPath          <- paste0(cDIR, "/output/")   # output

#########
#Maksims#
#########
setwd(codePath)
source('CSquareToLonLat.R')
world <- map_data('world')

setwd(fshzn)
fz_limits <- read.csv("fishing_zones_limits.csv", sep=',')
setwd(outPath)


# EFFORT MAPS -----


load(file = "spatial_effort.RData")

# Renaming the variable
result$fishing_days <- result$totfishdays
result$totfishdays        <- NULL

result_sf <- result
result_sf <- as.data.table(result_sf)

cc <- result_sf[, CSquare2LonLat(result_sf$cscode, 0.5)]
result_sf <- cbind(result_sf, cc)
result_sf <- result_sf[, .(lat = SI_LATI, lon = SI_LONG), by = .(country,year,quarter,gear_typeN,specon_tech,sub_region,
                                                                 fishing_zone,icesname,confidential,cscode, fishing_days)]


#Group and sum fishing days per fishing zone, gear type, specon_lo
result_sum <- result_sf[, .(fishing_days=sum(fishing_days), value=log(sum(fishing_days))), 
                        by = .(fishing_zone, gear_typeN, specon_tech, year, confidential, 
                               cscode, lon, lat)]

#Plotting effort
setwd(outPath)

#Plot all fishing areas
for(i in fz_limits$id){
  xmin <- fz_limits$lonMin[i]
  xmax <- fz_limits$lonMax[i]
  ymin <- fz_limits$latMin[i]
  ymax <- fz_limits$latMax[i]
  pl <- ggplot(if(fz_limits$fishing_zone[i]=="Earth"){
    result_plot <- result_sum[, .(value=log(sum(fishing_days))), by = .(year, lon, lat)]} 
    else result_plot <- result_sum[fishing_zone==fz_limits$fishing_zone[i]])+ 
    theme_bw() +
    geom_tile(aes(lon, lat, fill = value)) +
    coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
    geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
    scale_fill_continuous(low = "yellow", high = "red") +
    labs(x = NULL, y = NULL, fill = "Logarithm of \nFishing days")
    d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6), 
                                                axis.text = element_text(size = 6),
                                                legend.title = element_text(size = 7),
                                                legend.position="bottom",
                                                # legend.key.size = unit(0.3, "cm"),
                                                legend.text = element_text(size = 7))
  fname <- paste(getwd(),"/effort/areas/", gsub(' ','_',fz_limits$fishing_zone[i]), "_log_of_fishing_days.png", sep = "")
  ggsave(filename=fname, plot=d1)
}
# d1

#Plot specons
result_lo <- result_sf %>%
  group_by(year, specon_tech, confidential, lon, lat) %>% 
  summarise(fishing_days=sum(fishing_days)) %>%
  mutate(value=log(fishing_days))

#result_lo <- result_sf[., .(fishing_days=sum(fishing_days), value=log(sum(fishing_days))), by = .(year, specon_tech, lon, lat)]

specons <- unique(result_lo$specon_tech)
specons <- specons[!is.na(specons)]
for(i in specons){
  r <- result_lo %>% filter(specon_tech==i)
  #r <- result_lo[specon_tech==i]
  xmin <- min(r$lon)
  xmax <- max(r$lon)
  ymin <- min(r$lat)
  ymax <- max(r$lat)
  pl <- ggplot(r)  +
    theme_bw() +
    geom_tile(aes(lon, lat, fill = value)) +
    coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
    geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
    scale_fill_continuous(low = "yellow", high = "red") +
    labs(x = NULL, y = NULL, fill = "Logarithm of \nFishing days")
    d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6), 
                                                   axis.text = element_text(size = 6),
                                                   legend.title = element_text(size = 7),
                                                   legend.position="bottom",
                                                   # legend.key.size = unit(0.3, "cm"),
                                                   legend.text = element_text(size = 7))
  fname <- paste(getwd(),"/effort/specons/", i, "_log_of_fishing_days.png", sep = "")
  ggsave(filename=fname, plot=d1)
}

lstpolys <- lapply(1:nrow(fz_limits), function(x) {
  dt <- data.table(
    lon = c(
      fz_limits[x,]$lonMin,
      fz_limits[x,]$lonMin,
      fz_limits[x,]$lonMax,
      fz_limits[x,]$lonMax,
      fz_limits[x,]$lonMin
    ),
    lat = c(
      fz_limits[x,]$latMin,
      fz_limits[x,]$latMax,
      fz_limits[x,]$latMax,
      fz_limits[x,]$latMin,
      fz_limits[x,]$latMin
    ),
    group = x,
    order = 1:5
  )
  # poly <- st_polygon(list(as.matrix(dt)))
})
lstpolysdf <- rbindlist(lstpolys)

#Plot errors
for(i in fz_limits$id){
  if(fz_limits$fishing_zone[i]=="Earth") next
  else r <- result_sum %>% filter(fishing_zone==fz_limits$fishing_zone[i])
  #r <- result_sum[fishing_zone==fz_limits$fishing_zone[i]]
  xmin <- min(r$lon)
  xmax <- max(r$lon)
  ymin <- min(r$lat)
  ymax <- max(r$lat)
  pl <- ggplot(r)  +
    theme_bw() +
    geom_tile(aes(lon, lat, fill = value)) +
    coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
    # xlim(xmin,xmax)+
    # ylim(ymin, ymax)+
    geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
    geom_polygon(data = lstpolysdf[lstpolysdf$group ==i], aes(lon, lat, group=group), fill = NA, colour = 'black')+
    scale_fill_continuous(low = "yellow", high = "red") +
    labs(x = NULL, y = NULL, fill = "Logarithm of \nFishing days")
  d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6), 
                                                   axis.text = element_text(size = 6),
                                                   legend.title = element_text(size = 7),
                                                   legend.position="bottom",
                                                   # legend.key.size = unit(0.3, "cm"),
                                                   legend.text = element_text(size = 7))
  fname <- paste(getwd(),"/effort/errors/", gsub(' ','_',fz_limits$fishing_zone[i]), "_errors_log_of_fishing_days.png", sep = "")
  ggsave(filename=fname, plot=d1)
}

#Plot gear types
result_gr <- result_sf %>%
  group_by(gear_typeN, year, confidential, lon, lat) %>% 
  summarise(fishing_days=sum(fishing_days)) %>%
  mutate(value=log(fishing_days))
#result_gr <- result_sf[., .(fishing_days=sum(fishing_days), value=log(sum(fishing_days))), by = .(gear_typeN, lon, lat)]
gr <- unique(result_gr$gear_typeN)
for(i in gr){
  r <- result_gr %>% filter(gear_typeN==i)
  #r <- result_gr[gear_typeN==i]
  xmin <- min(r$lon)
  xmax <- max(r$lon)
  ymin <- min(r$lat)
  ymax <- max(r$lat)
  pl <- ggplot(r)  +
    theme_bw() +
    geom_tile(aes(lon, lat, fill = value)) +
    coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
    geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
    scale_fill_continuous(low = "yellow", high = "red") +
    labs(x = NULL, y = NULL, fill = "Logarithm of \nFishing days")
  d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6), 
                                                   axis.text = element_text(size = 6),
                                                   legend.title = element_text(size = 7),
                                                   legend.position="bottom",
                                                   # legend.key.size = unit(0.3, "cm"),
                                                   legend.text = element_text(size = 7))
  fname <- paste(getwd(),"/effort/gears/", i, "_log_of_fishing_days.png", sep = "")
  ggsave(filename=fname, plot=d1)
}

# LANDINGS MAPS ----
setwd(outPath)

load(file = "spatial_landings.RData")

result_sf <- result
result_sf <- as.data.table(result_sf)

result_sf[totwghtlandg<=1, totwghtlandg := totwghtlandg+1]

cc <- result_sf[, CSquare2LonLat(result_sf$cscode, 0.5)]
result_sf <- cbind(result_sf, cc)
result_sf <- result_sf[, .(lat = SI_LATI, lon = SI_LONG), by = .(country,year,quarter,gear_typeN,specon_tech,sub_region,
                                                                 fishing_zone,icesname,confidential,cscode, totwghtlandg)]

#Group and sum fishing days per fishing zone, gear type, specon_lo
result_sum <- result_sf[, .(totwghtlandg=sum(totwghtlandg,na.rm = T), value=log(sum(totwghtlandg))), 
                        by = .(fishing_zone, gear_typeN, specon_tech, year, confidential, 
                               cscode, lon, lat)]


setwd(outPath)
#Plot all fishing areas

for(i in fz_limits$id){
  xmin <- fz_limits$lonMin[i]
  xmax <- fz_limits$lonMax[i]
  ymin <- fz_limits$latMin[i]
  ymax <- fz_limits$latMax[i]
  pl <- ggplot(if(fz_limits$fishing_zone[i]=="Earth"){
    result_plot <- result_sum[, .(value=log(sum(totwghtlandg,na.rm=T))), by = .(year, lon, lat)]} 
    else result_plot <- result_sum[fishing_zone==fz_limits$fishing_zone[i]])+ 
    theme_bw() +
    geom_tile(aes(lon, lat, fill = value)) +
    coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
    geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
    scale_fill_continuous(low = "yellow", high = "red",na.value = NA) +
    labs(x = NULL, y = NULL, fill = "Logarithm of \nLandings")
  d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6), 
                                                   axis.text = element_text(size = 6),
                                                   legend.title = element_text(size = 7),
                                                   legend.position= "bottom",
                                                   # legend.key.size = unit(0.3, "cm"),
                                                   legend.text = element_text(size = 7))
  fname <- paste(getwd(),"/landings/areas/", gsub(' ','_',fz_limits$fishing_zone[i]), "_log_of_landings.png", sep = "")
  ggsave(filename=fname, plot=d1)
}


#Plot specons
result_lo <- result_sf %>%
  group_by(year, specon_tech, confidential, lon, lat) %>% 
  summarise(totwghtlandg=sum(totwghtlandg)) %>%
  mutate(value=log(totwghtlandg))

specons <- unique(result_lo$specon_tech)
specons <- unique(result_lo$specon_tech)
specons <- specons[!is.na(specons)]

for(i in specons){
  r <- result_lo %>% filter(specon_tech==i)
  #r <- result_lo[specon_tech==i]
  xmin <- min(r$lon)
  xmax <- max(r$lon)
  ymin <- min(r$lat)
  ymax <- max(r$lat)
  pl <- ggplot(r)  +
    theme_bw() +
    geom_tile(aes(lon, lat, fill = value)) +
    coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
    geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
    scale_fill_continuous(low = "yellow", high = "red") +
    labs(x = NULL, y = NULL, fill = "Logarithm of \nLandings")
  d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6), 
                                                   axis.text = element_text(size = 6),
                                                   legend.title = element_text(size = 7),
                                                   legend.position= "bottom",
                                                   # legend.key.size = unit(0.3, "cm"),                                                   legend.key.size = unit(0.3, "cm"),
                                                   legend.text = element_text(size = 7))
  fname <- paste(getwd(),"/landings/specons/", i, "_log_of_landings.png", sep = "")
  ggsave(filename=fname, plot=d1)
}

lstpolys <- lapply(1:nrow(fz_limits), function(x) {
  dt <- data.table(
    lon = c(
      fz_limits[x,]$lonMin,
      fz_limits[x,]$lonMin,
      fz_limits[x,]$lonMax,
      fz_limits[x,]$lonMax,
      fz_limits[x,]$lonMin
    ),
    lat = c(
      fz_limits[x,]$latMin,
      fz_limits[x,]$latMax,
      fz_limits[x,]$latMax,
      fz_limits[x,]$latMin,
      fz_limits[x,]$latMin
    ),
    group = x,
    order = 1:5
  )
  # poly <- st_polygon(list(as.matrix(dt)))
})
lstpolysdf <- rbindlist(lstpolys)


#Plot errors
for(i in fz_limits$id){
  if(fz_limits$fishing_zone[i]=="Earth") next
  else r <- result_sum %>% filter(fishing_zone==fz_limits$fishing_zone[i])
  #r <- result_sum[fishing_zone==fz_limits$fishing_zone[i]]
  xmin <- min(r$lon)
  xmax <- max(r$lon)
  ymin <- min(r$lat)
  ymax <- max(r$lat)
  pl <- ggplot(r)  +
    theme_bw() +
    geom_tile(aes(lon, lat, fill = value)) +
    coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
    geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
    geom_polygon(data = lstpolysdf[lstpolysdf$group ==i], aes(lon, lat, group=group), fill = NA, colour = 'black')+
    scale_fill_continuous(low = "yellow", high = "red") +
    labs(x = NULL, y = NULL, fill = "Logarithm of \nLandings")
  d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6), 
                                                   axis.text = element_text(size = 6),
                                                   legend.title = element_text(size = 7),
                                                   legend.position= "bottom",
                                                   # legend.key.size = unit(0.3, "cm"),                                                   legend.key.size = unit(0.3, "cm"),
                                                   legend.text = element_text(size = 7))
  fname <- paste(getwd(),"/landings/errors/", gsub(' ','_',fz_limits$fishing_zone[i]), "_errors_log_of_landings.png", sep = "")
  ggsave(filename=fname, plot=d1)
}

#Plot gear types
result_gr <- result_sf %>%
  group_by(gear_typeN, year, confidential, lon, lat) %>% 
  summarise(totwghtlandg=sum(totwghtlandg)) %>%
  mutate(value=log(totwghtlandg))
gr <- unique(result_gr$gear_typeN)
for(i in gr){
  r <- result_gr %>% filter(gear_typeN==i)
  #r <- result_gr[gear_typeN==i]
  xmin <- min(r$lon)
  xmax <- max(r$lon)
  ymin <- min(r$lat)
  ymax <- max(r$lat)
  pl <- ggplot(r)  +
    theme_bw() +
    geom_tile(aes(lon, lat, fill = value)) +
    coord_quickmap(xlim = c(xmin,xmax), ylim = c(ymin, ymax))+
    geom_polygon(data=world, aes(long, lat, group=group), fill = "#909191")+
    scale_fill_continuous(low = "yellow", high = "red") +
    labs(x = NULL, y = NULL, fill = "Logarithm of \nLandings")
  d1 <- pl + facet_wrap( ~ year, nrow = 2) + theme(strip.text.x = element_text(size = 6), 
                                                   axis.text = element_text(size = 6),
                                                   legend.title = element_text(size = 7),
                                                   legend.position= "bottom",
                                                   # legend.key.size = unit(0.3, "cm"),                                                   legend.key.size = unit(0.3, "cm"),
                                                   legend.text = element_text(size = 7))
  fname <- paste(getwd(),"/landings/gears/", i, "_log_of_landings.png", sep = "")
  ggsave(filename=fname, plot=d1)
}

