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
library(ggplot2)

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

setwd(dataF)
# EFFORT MAPS -----
#tableI <- fread('table_i_total_valid_and_not.csv')
#tableI <- fread('')
load(file="fdi_Table.I.RData")
#Loading the file with subregions assigned to fishing zones
setwd(fshzn)
fishing_zones           <- fread("fishing_zones_2021.csv", stringsAsFactors = F)
setwd(dataF)
#Assign fishing zones to the fdi data
fdi <-merge(fdi,fishing_zones,by="sub_region",all.x=T)
fdi<-data.table(fdi)

#Remove rows with sub_region = NK and remove BSAs
fdi<-fdi[!sub_region %in% c("NK","BSA")]
#Check if all rows have a fishing zone assigned
unique(fdi[is.na(fishing_zone),.(sub_region)])
unique(fdi$year)

fdi[,fishing_zone_year:=paste0(fishing_zone,'\n',as.character(year))]
library(esquisse)
#esquisser()
# Renaming the variable
# Number of records
ggplot(fdi[!is.na(fishing_zone)]) +
  aes(x = gear_typeN, fill = confidential) +
  geom_bar(position = "fill",na.rm = T) +
  scale_fill_hue(direction = -1) +
  theme_light() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(fishing_zone_year)) +
  facet_grid(year~fishing_zone) +
  theme(axis.text.x=element_text(size=6,angle = 90, hjust = 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  xlab('Gear Type') +
  ylab('Percentage of records by year') + 
  guides(fill=guide_legend(title="Confidential")) +
  scale_fill_manual(
    values = c(Y = "#F8766D",
               N = "#00C19F"),
    labels = c("Yes", "No"))

  ggsave(filename = '../output/Table_I_Confidential_Records.png',
         plot=last_plot(),
         width=2000,height=2800,
         scale= 1.2,
         unit = 'px',
         dpi = 300)
esquisser()

# Total total Records
  ggplot(fdi[!is.na(fishing_zone)]) +
    aes(x=fishing_zone, fill = confidential) +
    geom_bar(position = "fill",na.rm = T) +
    scale_fill_hue(direction = -1) +
    theme_light() +
    theme(legend.position = "bottom") +
    #facet_wrap(vars(fishing_zone_year)) +
    facet_wrap(vars(year)) +
    theme(axis.text.x=element_text(size=8,angle = 90, hjust = 1,
                                   vjust=0.5
                                   )) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    xlab('Gear Type') +
    ylab('Percentage of records by year') + 
    guides(fill=guide_legend(title="Confidential")) +
    scale_fill_manual(
      values = c(Y = "#F8766D",
                 N = "#00C19F"),
      labels = c("Yes", "No"))
  
  ggsave(filename = '../output/Table_I_Confidential_Records_Total.png',
         plot=last_plot(),
         width=2000,height=2800,
         scale= 1.2,
         unit = 'px',
         dpi = 300)
  
# Number of c squares
ggplot(fdi[!is.na(fishing_zone)&!is.na(c_square)]) +
  aes(x = gear_typeN, fill = confidential) +
  geom_bar(position = "fill",na.rm = T) +
  scale_fill_hue(direction = -1) +
  theme_light() +
  theme(legend.position = "bottom") +
  scale_y_log10()+
  facet_grid(year~fishing_zone) +
  theme(axis.text.x=element_text(size=6, angle = 90, hjust = 0)) +
  xlab('Gear Type') +
  ylab('Precentage of c-squares by year') + 
  guides(fill=guide_legend(title="Confidential")) +
  scale_fill_manual(
    values = c(Y = "#F8766D",
               N = "#00C19F"),
    labels = c("Yes", "No")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


ggsave(filename = '../output/Table_I_Confidential_C-Squares.png',
       plot=last_plot(),
       width=2000,height=2800,
       scale= 1.2,
       unit = 'px',
       dpi = 300)
# Total Effort
ggplot(fdi[!is.na(fishing_zone)]) +
  aes(x = gear_typeN, y=totfishdays,fill = confidential) +
  geom_bar(stat = 'sum',position = "fill",na.rm = T,show.legend = c(size=F)) +
  scale_fill_hue(direction = -1) +
  theme_light() +
  theme(legend.position = "bottom") +
  facet_grid(year~fishing_zone) +
  theme(axis.text.x=element_text(size=6,angle = 90, hjust = 0)) +
  xlab('Gear Type') +
  ylab('Percentage of the total effort by year') + 
  guides(fill=guide_legend(title="Confidential")) +
  scale_fill_manual(
    values = c(Y = "#F8766D",
               N = "#00C19F"),
    labels = c("Yes", "No")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

  ggsave(filename = '../output/Table_I_Confidential_Total_Effort.png',
       plot=last_plot(),
       width=2000,height=2800,
       scale= 1.2,
       unit = 'px',
       dpi = 300) 

###########
# TABLE H #
###########   
  
load(file="fdi_Table.H.RData")
#Loading the file with subregions assigned to fishing zones
setwd(fshzn)
fishing_zones           <- fread("fishing_zones_2021.csv",stringsAsFactors = F)
setwd(dataF)
#Assign fishing zones to the fdi data
fdi <-merge(fdi,fishing_zones,by="sub_region",all.x=T)
fdi<-data.table(fdi)

#Remove rows with sub_region = NK and remove BSAs
fdi<-fdi[!sub_region %in% c("NK","BSA")]
#Check if all rows have a fishing zone assigned
unique(fdi[is.na(fishing_zone),.(sub_region)])
unique(fdi$year)

#fdi[,fishing_zone_year:=paste0(fishing_zone,'\n',as.character(year))]
library(esquisse)
# esquisser()
fdi[confidential=='V',]$confidential <- 'N'
fdi[confidential=='A',]$confidential <- 'Y'

unique(fdi$confidential)

# Total total Records
ggplot(fdi[!is.na(fishing_zone)]) +
  aes(x=fishing_zone, fill = confidential) +
  geom_bar(position = "fill",na.rm = T) +
  scale_fill_hue(direction = -1) +
  theme_light() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(fishing_zone_year)) +
  facet_wrap(vars(year)) +
  theme(axis.text.x=element_text(size=8,angle = 90, hjust = 1,
                                 vjust=0.5
  )) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  xlab('Gear Type') +
  ylab('Percentage of records by year') + 
  guides(fill=guide_legend(title="Confidential")) +
  scale_fill_manual(
    values = c(Y = "#F8766D",
               N = "#00C19F"),
    labels = c("Yes", "No"))

ggsave(filename = '../output/Table_H_Confidential_Records_Total.png',
       plot=last_plot(),
       width=2000,height=2800,
       scale= 1.2,
       unit = 'px',
       dpi = 300)


# Renaming the variable
# Number of records
ggplot(fdi[!is.na(fishing_zone)]) +
  aes(x = gear_typeN, fill = confidential) +
  geom_bar(position = "fill",na.rm = T) +
  scale_fill_hue(direction = -1) +
  theme_light() +
  theme(legend.position = "bottom") +
  #facet_wrap(vars(fishing_zone_year)) +
  facet_grid(year~fishing_zone) +
  theme(axis.text.x=element_text(size=6,angle = 90, hjust = 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c(Y = "#F8766D",
               N = "#00C19F"),
    labels = c("Yes", "No"))+
  xlab('Gear Type') +
  ylab('Percentage of records by year') + 
  guides(fill=guide_legend(title="Confidential"))

ggsave(filename = '../output/Table_H_Confidential_Records.png',
       plot=last_plot(),
       width=2000,height=2800,
       scale= 1.2,
       unit = 'px',
       dpi = 300)

# Number of c squares
ggplot(fdi[!is.na(fishing_zone)&!is.na(c_square)]) +
  aes(x = gear_typeN, fill = confidential) +
  geom_bar(position = "fill",na.rm = T) +
  scale_fill_hue(direction = -1) +
  theme_light() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #scale_y_log10()+
  facet_grid(year~fishing_zone) +
  theme(axis.text.x=element_text(size=6, angle = 90, hjust = 0)) +
  xlab('Gear Type') +
  ylab('Percentage of c-squares by year') + 
  guides(fill=guide_legend(title="Confidential")) +
  scale_fill_manual(
    values = c(Y = "#F8766D",
               N = "#00C19F"),
    labels = c("Yes", "No"))

ggsave(filename = '../output/Table_H_Confidential_C-Squares.png',
       plot=last_plot(),
       width=2000,height=2800,
       scale= 1.2,
       unit = 'px',
       dpi = 300)

# Total Landings
ggplot(fdi[!is.na(fishing_zone)]) +
  aes(x = gear_typeN, y=totwghtlandg,fill = confidential) +
  geom_bar(stat = 'sum',position = "fill",na.rm = T,show.legend = c(size=F)) +
  scale_fill_hue(direction = -1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#  scale_y_log10()+
  theme_light() +
  theme(legend.position = "bottom") +
  #scale_y_continuous() +
  facet_grid(year~fishing_zone) +
  theme(axis.text.x=element_text(size=6,angle = 90, hjust = 0)) +
  xlab('Gear Type') +
  ylab('Percentage of the total landings weight by year') + 
  guides(fill=guide_legend(title="Confidential"))+
  scale_fill_manual(
    values = c(Y = "#F8766D",
               N = "#00C19F"),
    labels = c("Yes", "No"))

ggsave(filename = '../output/Table_H_Confidential_Total_Landings.png',
       plot=last_plot(),
       width=2000,height=2800,
       scale= 1.2,
       unit = 'px',
       dpi = 300) 
