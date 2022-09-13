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
library(parallel)
# library(nngeo)

options(scipen = 999)
options(digits = 9)

#- Clear workspace
rm(list = ls())
#- Settings paths
cDIR = '~/work/EWG-FDI-21-12'
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
load(file = "grids.RData")
# FDI DATA ----
setwd(dataF)

####################
#Maciej 19.09.2019 #
####################
load(file = "fdi_Tableau_Table.I.RData")

# Please use data.table! In the landings script I achieve the same result
# using DT in less time. I left this but I suggest to use DT in the future
# especially when the number of rows ia considerable, like Table H.
setwd(fshzn)
#Loading the file with subregions assigned to fishing zones
fishing_zones           <-
  fread("fishing_zones_2021.csv", stringsAsFactors = F)
setwd(dataF)
#Assign fishing zones to the fdi.tableau data
fdi.tableau <- left_join(fdi.tableau, fishing_zones, by = "sub_region")
fdi.tableau <- data.table(fdi.tableau)
sort(unique(fdi.tableau$sub_region))
sort(unique(fdi.tableau$supra_region))

# #Remove rows with sub_region = NK and remove BSAs
# fdi.tableau<-fdi.tableau[!sub_region %in% c("NK","BSA")]
# Check if all rows have a fishing zone assigned
unique(fdi.tableau[is.na(fishing_zone), .(sub_region)])
unique(fdi.tableau[sub_region == 'NK', ])

#Remove  incorrect data
nrow(fdi.tableau[sub_region == 'NK', ])
# Shall we remove this? Not in the dissemination in my opinion?
fdi.tableau <- fdi.tableau[sub_region != 'NK', ] 
fdi.tableau <- fdi.tableau[valid == "Y"]

unique(fdi.tableau$rectangle_type)
fdi.tableau[is.na(rectangle_type), rectangle_type := "05*05"]
fdi.tableau.rectangle.na.csq <- fdi.tableau[is.na(rectangle_type)]


#Create id for each lon/lat combination in the fdi.tableau data
fdi.tableau <- mutate(fdi.tableau,
                      rect_id = paste(
                        as.character(rectangle_lon),
                        as.character(rectangle_lat),
                        sep = '/'
                      ))
#Create id containing lon and lat of the centroid for each ICES rectangle
icesr <-
  mutate(icesr, rect_id = paste(as.character(ices_x), as.character(ices_y), sep = '/'))

#Join fdi.tableau data with ices rectangles dataset
fdi.tableau <- left_join(fdi.tableau, icesr, by = "rect_id")
fdi.tableau <- select(fdi.tableau, country:icesname)

#Keep the the data with the ICES rectangles assigned in a separate dataset
fdi.tableau.ices <- filter(fdi.tableau, !is.na(icesname))

sum(fdi.tableau.ices$totfishdays)

#Join the fdi.tableau.ices dataset with c-squares dataset. Warning! Fishing days will be doubled.
fdi.tableau.ices <- left_join(fdi.tableau.ices, csq05, by = "icesname")
#Divide fishing days by 2 (each ICES rectangle has 2 c-squares)
fdi.tableau.ices <- mutate(fdi.tableau.ices, totfishdays = totfishdays /
                             2)
#Check if the total fishing days remained the same
sum(fdi.tableau.ices$totfishdays)
fdi.tableau.ices <- select(fdi.tableau.ices, country:icesname, cscode)

#Keep the the data with the ICES rectangles NOT assigned in a separate dataset
fdi.tableau.not.ices      <- filter(fdi.tableau, is.na(icesname))
fdi.tableau.not.ices.05.1 <-
  filter(fdi.tableau.not.ices, rectangle_type == "05*1")
fdi.tableau.not.ices.t    <-
  filter(fdi.tableau.not.ices, rectangle_type != "05*1")

#Handle 0.5x1 rectangles outside ICES area
#Create the ids containing lon and lat of the centre left and centre right of the csquare
sum(fdi.tableau.not.ices.05.1$totfishdays)
csq05 <- mutate(csq05,
                rect_id = paste(as.character(w_csq), as.character(csq_y), sep =
                                  '/'))#rect_id = centre/left
fdi.tableau.not.ices.05.1.left <-
  left_join(fdi.tableau.not.ices.05.1, csq05, by = "rect_id") %>%
  mutate(totfishdays = totfishdays / 2)

csq05 <- mutate(csq05,
                rect_id = paste(as.character(e_csq), as.character(csq_y), sep =
                                  '/'))#rect_id = centre/right
fdi.tableau.not.ices.05.1.right <-
  left_join(fdi.tableau.not.ices.05.1, csq05, by = "rect_id") %>%
  mutate(totfishdays = totfishdays / 2)
fdi.tableau.not.ices.05.1 <-
  rbind(fdi.tableau.not.ices.05.1.left,
        fdi.tableau.not.ices.05.1.right)
fdi.tableau.not.ices.05.1 <-
  select(fdi.tableau.not.ices.05.1,
         country:icesname.x,
         icesname.y,
         cscode)
fdi.tableau.not.ices.05.1$icesname.x <- NULL
fdi.tableau.not.ices.05.1 <-
  rename(fdi.tableau.not.ices.05.1, icesname = icesname.y)
sum(fdi.tableau.not.ices.05.1$totfishdays)

print(paste0(
  "fdi.tableau.not.ices.05.1 number of rows without cscode: ",
  nrow(filter(
    fdi.tableau.not.ices.05.1, is.na(cscode)
  ))
))
temp <- filter(fdi.tableau.not.ices.05.1, is.na(cscode)) %>%
  select(country:icesname)
fdi.tableau.not.ices.05.1 <-
  filter(fdi.tableau.not.ices.05.1, !is.na(cscode))

# Check what rectangle types are present in the rest of the data
fdi.tableau.not.ices %>%
  filter(rectangle_type != "05*1") %>%
  group_by(rectangle_type) %>%
  summarise(n = n())

# The rest of the data has 05*05, 1*1 and 5*5 rectangle_type

# Handle 05*05 rectangles
fdi.tableau.csq.05.05 <-
  filter(fdi.tableau.not.ices, rectangle_type == "05*05") %>%
  rename(csq_c_id = rect_id)

# Create the id containing lon and lat of the centroid for each c-square
csq05 <- mutate(csq05,
                csq_c_id = paste(as.character(csq_x), as.character(csq_y), sep =
                                   '/'))


# Join the fdi.tableau.csq.c dataset with c-squares dataset.
fdi.tableau.csq.05.05 <-
  left_join(fdi.tableau.csq.05.05, csq05, by = "csq_c_id")
fdi.tableau.csq.05.05 <-
  select(fdi.tableau.csq.05.05, country:icesname.x,
         icesname.y, cscode)
fdi.tableau.csq.05.05$icesname.x <- NULL
fdi.tableau.csq.05.05 <-
  fdi.tableau.csq.05.05 %>% rename(icesname = icesname.y)
# Check if there are any rows without csquare assigned
print(paste0(
  "fdi.tableau.csq.05.05 number of rows without cscode: ",
  nrow(filter(fdi.tableau.csq.05.05, is.na(cscode)))
))

# Handle 1*1 rectangles
fdi.tableau.csq.1.1 <-
  filter(fdi.tableau.not.ices, rectangle_type == "1*1") %>%
  rename(csq_c_id = rect_id)

csquares.1.1 <-
  select(fdi.tableau.csq.1.1, csq_c_id, rectangle_lon, rectangle_lat) %>%
  mutate(key = 1) %>%
  distinct()
grid.for.1.1 <- data.frame(
  lon_diff = c(0, 0.5, 0.5, 0),
  lat_diff = c(0, 0, 0.5, 0.5),
  key = c(1)
)
csquares.1.1 <- inner_join(csquares.1.1, grid.for.1.1, by = "key") %>%
  mutate(bl_lon = rectangle_lon - lon_diff,
         bl_lat = rectangle_lat - lat_diff) %>%
  select(csq_c_id, bl_lon, bl_lat)

sum(fdi.tableau.csq.1.1$totfishdays)
fdi.tableau.csq.1.1 <- fdi.tableau.csq.1.1 %>%
  inner_join(csquares.1.1, by = "csq_c_id") %>%
  mutate(
    totfishdays = totfishdays / 4,
    rect_id = paste(as.character(bl_lon), as.character(bl_lat), sep =
                      '/')
  )
sum(fdi.tableau.csq.1.1$totfishdays)

# create bottom-left id of csquare
csq05 <- mutate(csq05,
                rect_id = paste(as.character(w_csq), as.character(s_csq), sep =
                                  '/'))
fdi.tableau.csq.1.1 <-
  left_join(fdi.tableau.csq.1.1, csq05, by = "rect_id")
fdi.tableau.csq.1.1 <-
  select(fdi.tableau.csq.1.1,
         country:fishing_zone,
         rect_id,
         icesname.x,
         cscode)
print(paste0(
  "fdi.tableau.csq.1.1 number of rows without cscode: ",
  nrow(filter(fdi.tableau.csq.1.1, is.na(cscode)))
))

# Handle 5*5 rectangles
fdi.tableau.csq.5.5 <-
  filter(fdi.tableau.not.ices, rectangle_type == "5*5") %>%
  rename(csq_c_id = rect_id)

csquares.5.5 <-
  select(fdi.tableau.csq.5.5, csq_c_id, rectangle_lon, rectangle_lat) %>%
  mutate(key = 1) %>%
  distinct()

grid.for.5.5 <-
  inner_join(data.frame(
    lon_diff = seq(-2, by = 0.5, length.out = 10),
    key = 1
  ),
  data.frame(
    lat_diff = seq(-2, by = 0.5, length.out = 10),
    key = 1
  ),
  by = "key")

csquares.5.5 <- inner_join(csquares.5.5, grid.for.5.5, by = "key") %>%
  mutate(bl_lon = rectangle_lon - lon_diff,
         bl_lat = rectangle_lat - lat_diff) %>%
  select(csq_c_id, bl_lon, bl_lat)

sum(fdi.tableau.csq.5.5$totfishdays)
fdi.tableau.csq.5.5 <- fdi.tableau.csq.5.5 %>%
  inner_join(csquares.5.5, by = "csq_c_id") %>%
  mutate(
    totfishdays = totfishdays / 100,
    rect_id = paste(as.character(bl_lon), as.character(bl_lat), sep =
                      '/')
  )
sum(fdi.tableau.csq.5.5$totfishdays)

# create bottom-left id of csquare
csq05 <- mutate(csq05,
                rect_id = paste(as.character(w_csq), as.character(s_csq), sep =
                                  '/'))
fdi.tableau.csq.5.5 <-
  left_join(fdi.tableau.csq.5.5, csq05, by = "rect_id")
fdi.tableau.csq.5.5 <-
  select(fdi.tableau.csq.5.5,
         country:fishing_zone,
         rect_id,
         icesname.x,
         cscode)
print(paste0(
  "fdi.tableau.csq.5.5 number of rows without cscode: ",
  nrow(filter(fdi.tableau.csq.5.5, is.na(cscode)))
))

fdi.tableau.ices <- rename(fdi.tableau.ices, geo_id = rect_id)
fdi.tableau.not.ices.05.1 <-
  rename(fdi.tableau.not.ices.05.1, geo_id = rect_id)
fdi.tableau.csq.05.05 <- rename(fdi.tableau.csq.05.05, geo_id = csq_c_id)
fdi.tableau.csq.1.1 <-
  rename(fdi.tableau.csq.1.1, geo_id = rect_id, icesname = icesname.x)
fdi.tableau.csq.5.5 <-
  rename(fdi.tableau.csq.5.5, geo_id = rect_id, icesname = icesname.x)

result <- fdi.tableau.ices %>%
  rbind(fdi.tableau.csq.05.05) %>%
  rbind(fdi.tableau.not.ices.05.1) %>%
  rbind(fdi.tableau.csq.1.1) %>%
  rbind(fdi.tableau.csq.5.5) %>%
  as.data.frame()

print(paste0("number of rows without cscode: ", nrow(filter(
  result, is.na(cscode)
))))
result <- as.data.table(result)
print(paste0(
  "Is total effort correct? : ",
  round(sum(result$totfishdays),0) == round(sum(fdi.tableau$totfishdays),0)
))

csquares.1.1 <- NULL;gc()
csquares.5.5 <- NULL;gc()
fdi.tableau <- NULL;gc()
fdi.tableau.csq.05.05 <- NULL;gc()
fdi.tableau.csq.1.1 <- NULL;gc()
fdi.tableau.csq.5.5 <- NULL;gc()
fdi.tableau.ices <- NULL;gc()
fdi.tableau.not.ices <- NULL;gc()
fdi.tableau.not.ices.05.1 <- NULL;gc()
fdi.tableau.not.ices.05.1.left <- NULL;gc()
fdi.tableau.not.ices.05.1.right <- NULL;gc()
fdi.tableau.not.ices.t <- NULL;gc()
fdi.tableau.rectangle.na.csq <- NULL;gc()
grid.for.1.1 <- NULL;gc()
grid.for.5.5 <- NULL;gc()
temp <- NULL;gc()


# Confidentiality and last errors removal
country_by_csquare <- result[,.('country'=unique(country)),by=.(cscode)]
country_by_csquare <- country_by_csquare[,nrows:=nrow(.SD),by=.(cscode)]

# NLD asked to omit the confidential records for those csquares where there is
# only the Dutch fleet
NLD_csquares <- country_by_csquare[country=='NLD'&nrows==1,]$cscode
# Now I will remove the confidential records for those c squares where there is only
# the dutch fleet
result <- result[!(cscode %in% NLD_csquares & country=='NLD'& confidential=='Y'),]
# PRT
result <- result[!(country == 'PRT' & confidential == 'Y' & supra_region =='OFR'), ]
# ITA
#result <- result[!(country == 'ITA' & year=='2020'), ]
# Removing the csqaures completely inside the Russian EEZ
setwd(csqF)
csq_inside_russia_eez <- st_read('csquares_completely_inside_Russia_EEZ.shp')
st_geometry(csq_inside_russia_eez) <- NULL;
csq_inside_russia_eez <- unique(csq_inside_russia_eez$cscode)
setwd(dataF)
result <- result[!cscode %in%csq_inside_russia_eez,]

# Creating the two versions:
# resultEU28 with UK 
# resultEU27 without UK 
names(result)
resultEU28 <- result
resultEU27 <- result[!(country %in% c('ENG','SCO','GBG','GBJ','IOM','NIR')),]
result <- NULL;gc()

fields.to.keep <-
  c(
    "year",
    "quarter",
    "vessel_length",
    "fishing_tech",
    "gear_type",
    "mesh_size_range",
    "target_assemblage",
    "metier",
    "supra_region",
    "sub_region",
    "specon_tech",
    "deep",
    "rectangle_type",
    "rectangle_lon",
    "rectangle_lat",
    "totfishdays",
    "icesname",
    "cscode"
  )

resultEU28 <- resultEU28[, fields.to.keep, with = FALSE]
resultEU28.cs <- resultEU28[, .(sum(totfishdays, na.rm = T),
                        unique(rectangle_lon),
                        unique(rectangle_lat)),
                    by = c(
                      "year",
                      "quarter",
                      "vessel_length",
                      "fishing_tech",
                      "gear_type",
                      "mesh_size_range",
                      "target_assemblage",
                      "metier",
                      "supra_region",
                      "sub_region",
                      "specon_tech",
                      "deep",
                      "rectangle_type",
                      "icesname",
                      "cscode"
                    )]

setnames(resultEU28.cs,
         c('V1', 'V2', 'V3') ,
         c('totfishdays', "rectangle_lon", "rectangle_lat"))

#resultEU28.cs <- resultEU28.cs[totfishdays > 0, ]

csq05Coast <- NULL;
csq05grid  <- NULL;
csq05Land  <- NULL;
csq05Sea   <- NULL;
csq05pts   <- NULL;
resultEU28DT   <- as.data.table(resultEU28.cs)

# Counts the number of vessels by csquare
setwd(outPath)

fwrite(resultEU28DT, 'spatial_effort_tableau_pts_EU28.csv')
resultEU28_sf <- left_join(resultEU28.cs, csq05[, c("cscode", "geometry")], by = 'cscode')
resultEU28_sf$rectangle_lat <- NULL;
resultEU28_sf$rectangle_lon <- NULL;
resultEU28_sf <- st_sf(
  resultEU28_sf,
  sf_column_name = 'geometry',
  crs = 4326)
st_write(
  resultEU28_sf,
  'spatial_effort_tableau_EU28.shp',
  delete_dsn = T
)
resultEU28 <- NULL;
resultEU28DT <- NULL;
resultEU28.cs <- NULL;
resultEU28_sf <- NULL;gc()
gc()
# Creating the shapefiles for the EU27
resultEU27 <- resultEU27[, fields.to.keep, with = FALSE]
resultEU27.cs <- resultEU27[, .(sum(totfishdays, na.rm = T),
                        unique(rectangle_lon),
                        unique(rectangle_lat)),
                    by = c(
                      "year",
                      "quarter",
                      "vessel_length",
                      "fishing_tech",
                      "gear_type",
                      "mesh_size_range",
                      "target_assemblage",
                      "metier",
                      "supra_region",
                      "sub_region",
                      "specon_tech",
                      "deep",
                      "rectangle_type",
                      "icesname",
                      "cscode"
                    )]

setnames(resultEU27.cs,
         c('V1', 'V2', 'V3') ,
         c('totfishdays', "rectangle_lon", "rectangle_lat"))

#resultEU27.cs <- resultEU27.cs[totfishdays > 0, ]

csq05Coast <- NULL;
csq05grid  <- NULL;
csq05Land  <- NULL;
csq05Sea   <- NULL;
csq05pts   <- NULL;
resultEU27DT   <- as.data.table(resultEU27.cs)

# Counts the number of vessels by csquare
setwd(outPath)

fwrite(resultEU27DT, 'spatial_effort_tableau_pts_EU27.csv')
resultEU27_sf <- left_join(resultEU27.cs, csq05[, c("cscode", "geometry")], by = 'cscode')
resultEU27_sf$rectangle_lat <- NULL;
resultEU27_sf$rectangle_lon <- NULL;
resultEU27_sf <- st_sf(
  resultEU27_sf,
  sf_column_name = 'geometry',
  crs = 4326)
st_write(
  resultEU27_sf,
  'spatial_effort_tableau_EU27.shp',
  delete_dsn = T
)


# Part to fix the wrong sub_region codes by joining with the official DCF FAO dataset
# csvF      <- fread("spatial_effort_tableau_pts.csv")
# dcf.fao <- st_read('../dcf/fdi_fao_areas_sta_gsa.shp',
#                    stringsAsFactors = F,
#                    crs = 4326)
# csvF[, id := .I, ]
# gc()
# csvF.rest <- csvF
# csvF      <- csvF[, .(id, rectangle_lon, rectangle_lat)]
# chunk     <- ceiling(nrow(csvF) / (detectCores() - 1))
# nrowSplit <- nrow(csvF)
# repSplit  <- rep(1:(detectCores() - 1), each = chunk)[1:nrowSplit]
# ## Pre-split the data into m/n chunks
# dataSplit1b <- split(csvF, repSplit)
# repSplit    <- NULL
# nrowSplit   <- NULL
# gc()
# # Function for the mc split and PnP ----
# pNp <- function(pts) {
#   # pts <- dataSplit1b[[1]]
#   lbfleetMapsSog1          <-
#     st_as_sf(pts,
#              coords = c('rectangle_lon', 'rectangle_lat'),
#              crs = 4326)
#   lbfleetMapsSog1          <-
#     st_join(lbfleetMapsSog1, dcf.fao[, ("F_CODE")], join = st_intersects)
#   gc()
#   lbfleetMapsSog1$geometry <- NULL
#   lbfleetMapsSog1          <- as.data.table(lbfleetMapsSog1)
#   names(lbfleetMapsSog1)   <- tolower(names(lbfleetMapsSog1))
#   return(lbfleetMapsSog1)
# }
# ## Approach 1b
# res1b <-
#   mclapply(dataSplit1b,
#            pNp,
#            mc.cores = (detectCores() - 1),
#            mc.preschedule = TRUE)
# dataSplit1b <- NULL
# gc()
# out   <- rbindlist(res1b)
# res1b <- NULL
# out   <- out[!duplicated(id),]
# gc()
# csvF <- merge(out, csvF.rest, by = 'id')
# out <- NULL
# csvF.rest <- NULL
# 
# csvF[, f_code_s := gsub(" ", "", tolower(gsub('\\.', '', f_code)))]
# csvF[, sub_region_s := tolower(gsub('\\.', '', sub_region))]
# csvF[, sub_region_s := ifelse(sub_region_s == 'bsa', 27, sub_region_s)]
# csvF[, sub_region_s := ifelse(sub_region_s %like% 'gsa*',
#                               sub_region_s,
#                               substr(sub_region_s, 1, 2))]
# csvF[, f_code_s := ifelse(f_code_s %like% 'gsa*', f_code_s,
#                           substr(f_code_s, 1, 2))]
# csvF[, check := ifelse(f_code_s == sub_region_s, 0, 1)]
# 
# nrow(csvF[check == 1 | is.na(check)])
# 
# csvF <- csvF[check != 1, ]
# csvF[, `:=`(
#   id = NULL,
#   f_code = NULL,
#   f_code_s = NULL,
#   sub_region_s = NULL,
#   rectangle_lon = NULL,
#   rectangle_lat = NULL,
#   check = NULL
# )]
# length(unique(csvF$cscode))
# nrow(csvF[cscode == '', ])
# nrow(csvF[is.na(cscode), ])
# 
# csvF <- left_join(csvF, csq05[, c("cscode", "geometry")], by = 'cscode')
# 
# st_write(st_sf(
#   csvF,
#   sf_column_name = 'geometry',
#   crs = 4326),
#   'spatial_effort_tableau.shp'
# )
