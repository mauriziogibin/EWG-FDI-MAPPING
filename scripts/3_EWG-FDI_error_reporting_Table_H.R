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
library(xlsx)

options(scipen = 999)
options(digits = 9)

#- Clear workspace
rm(list=ls())

cDIR = '~/work/EWG-FDI-21-12'
setwd(cDIR)
#- Settings paths
codePath         <- paste0(cDIR, "/scripts/")    # R scripts location
dataF            <- paste0(cDIR, "/data/")# data folder
csqF             <- paste0(cDIR, "/csquares/")
icesrF           <- paste0(cDIR, "/ices_rects/")
outPath          <- paste0(cDIR, "/output/")   # output

# FDI DATA ----
setwd(outPath)
# 
# We need to select the nuber of fields to keep and also made a recap on the number of rows and landings/effort we loose.
# TABLE I Reporting ----
setwd(paste0(outPath,'landings/'))
tH <- list.files(path = '.', pattern = glob2rx("*Table.H*.csv"))
sort(tH)
tH <- tH[!tH %like% '*gearNOT*']
sort(tH)
# We need to select the nuber of fHelds to keep and also made a recap on the number of rows and landHngs/effort we loose.
# TABLE H ReportHng ----
setwd(paste0(outPath,'landings/'))
table.H.errors <- lapply(tH,fread)
names(table.H.errors) <- tH
names(table.H.errors)
table.H.errors <- 
  lapply(1:length(table.H.errors),function(x){
    if (x!=12&&x!=13){  
      n <- 6
      countrylbl <- gsub('.csv','',substr(names(table.H.errors[x]), nchar(names(table.H.errors[x]))-n, nchar(names(table.H.errors[x]))))
      DT <- rbindlist(table.H.errors[x])
      DT[,country:=countrylbl]
      return(DT)}
    else{DT <- rbindlist(table.H.errors[x])
    return(DT)}
  })

table.H.errors[12]
names(table.H.errors) <- tH
names(table.H.errors)[c(-12,-13)] <-   gsub('.{8}$','',names(table.H.errors)[c(-12,-13)])
names(table.H.errors)[c(12,13)] <-   gsub('.csv$','',names(table.H.errors)[c(12,13)])
table.H.errors = table.H.errors[order(names(table.H.errors))]

names(table.H.errors)
newnames <- c(names(table.H.errors[1]),
              names(table.H.errors[2:11]),
              names(table.H.errors[12]),
              names(table.H.errors[13]),
              names(table.H.errors[14:19]),
              names(table.H.errors[20]))
table.H.errors <- list( rbindlist(table.H.errors[1]),
                        rbindlist(table.H.errors[2:11]),
                        rbindlist(table.H.errors[12]),
                        rbindlist(table.H.errors[13]),
                        rbindlist(table.H.errors[14:19]),
                        rbindlist(table.H.errors[20]))
names(table.H.errors) <- unique(newnames)
names(table.H.errors)
# The third table is the  errors in unit weight that is already summarised,
# while the others are not.
errors.unit.weight <- table.H.errors[[3]]
table.H.errors <- table.H.errors[-3]
errors.unit.weight[,`:=` (totwghtlandg = round(sum(totwghtlandg,na.rm = T),0),
                           totvallandg = round(sum(totvallandg,na.rm = T),0)
                          #,nrows = .N)
                          ),
                    by = .(country, year)]

#errors.unit.weight[,totwghtlandg:=round(totwghtlandg,0)]
names(errors.unit.weight) <- c("Country", "Year", "Total Landings Weight","Total Landings Value")

wb<-createWorkbook(type="xlsx")

CellStyle(wb, dataFormat=NULL, alignment=NULL,
          border=NULL, fill=NULL, font=NULL)
TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=14, 
                                   color = "000000" , isBold=TRUE)
SUB_TITLE_STYLE <- CellStyle(wb) + 
  Font(wb,  heightInPoints=12,
       isItalic=TRUE, isBold=FALSE)
# Styles for the data table row/column names
TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
  Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
  Border(color="black", position=c("TOP", "BOTTOM"), 
         pen=c("BORDER_THIN", "BORDER_THICK")) 


# errors.total <- fread('fdi_TABLE_H_errors.csv')
# errors.total <- errors.total[,.(country,year,totwghtlandg,totvallandg)]
# errors.total <- errors.total[, list(totwghtlandg = round(sum(totwghtlandg,na.rm = T),0),
#                                     totvallandg = round(sum(totvallandg,na.rm = T),0),
#                                     nrows = nrow(.SD)),
#                              by = .(country, year)]
# errors.total <- setorder(errors.total,country)
# names(errors.total) <- c("Country", "Year", "Total Landings Weight",
#                          "Total Landings Value","Number of rows")

# Create a new sheet in the workbook
#++++++++++++++++++++++++++++++++++++
# sheet <- createSheet(wb, sheetName = "Table H Errors")
# 
# xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
#   rows <-createRow(sheet,rowIndex=rowIndex)
#   sheetTitle <-createCell(rows, colIndex=1)
#   setCellValue(sheetTitle[[1,1]], title)
#   setCellStyle(sheetTitle[[1,1]], titleStyle)
# }
# 
# # Add title
# xlsx.addTitle(sheet, rowIndex=1, title="Table H Errors",
#               titleStyle = TITLE_STYLE)
# # Add sub title
# xlsx.addTitle(sheet, rowIndex=3, 
#               title="Recap on the number of rows in Table H with errors",
#               titleStyle = SUB_TITLE_STYLE)
# # Add a table into a worksheet
# #++++++++++++++++++++++++++++++++++++
# addDataFrame(errors.total, sheet, startRow=5, startColumn=1,
#              row.names = F,
#              colnamesStyle = TABLE_COLNAMES_STYLE,
#              rownamesStyle = TABLE_ROWNAMES_STYLE)
# # Change column width
# setColumnWidth(sheet, colIndex=c(1:ncol(errors.total)), colWidth=16)

# Create a new sheet in the workbook
#++++++++++++++++++++++++++++++++++++
sheet <- createSheet(wb, sheetName = "Table H Recap Table to check Unit Weight and Price")

xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}

# Add title
xlsx.addTitle(sheet, rowIndex=1, title="Table H Recap Table to check Unit Weight and Price",
              titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet, rowIndex=3, 
              title="Summary Table for the Landings and Value",
              titleStyle = SUB_TITLE_STYLE)
# Add a table into a worksheet
#++++++++++++++++++++++++++++++++++++
addDataFrame(errors.unit.weight, sheet, startRow=5, startColumn=1,
             row.names = F,
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
# Change column width
setColumnWidth(sheet, colIndex=c(1:ncol(errors.unit.weight)), colWidth=16)
# Add a plot into a worksheet
# Create a new sheet in the workbook
#++++++++++++++++++++++++++++++++++++
missing.subregion <- table.H.errors[[3]]
table.H.errors <- table.H.errors[-3]
missing.subregion <-
  missing.subregion[, list(totwghtlandg = round(sum(totwghtlandg,na.rm = T),0),
                           totvallandg = round(sum(totvallandg,na.rm = T),0),
                            nrows =nrow(.SD)),
                     by = .(country, year)]

names(missing.subregion) <- c("Country", "Year", "Total Landings Weight",
                              "Total Landings Value","Number of rows")

sheet <- createSheet(wb, sheetName = "Table H Missing Subregion")

xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}

# Add title
xlsx.addTitle(sheet, rowIndex=1, title="Table H Missing Subregion",
              titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet, rowIndex=3, 
              title="Recap on the number of rows in Table H with missing Subregion",
              titleStyle = SUB_TITLE_STYLE)
# Add a table into a worksheet
#++++++++++++++++++++++++++++++++++++
addDataFrame(missing.subregion, sheet, startRow=5, startColumn=1,
             row.names = F,
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
# Change column width
setColumnWidth(sheet, colIndex=c(1:ncol(missing.subregion)), colWidth=16)
# Add a plot into a worksheet

#
# The missing subregion table does not have a total of fishing effort or landing indicated
# For the errors table we consider country number of rows affected by the issue and then total effort lost.

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# table <- table.I.errors[1]

addTables <- function(table){
  names(table) <- gsub('\\.',' ',names(table))
  names(table) <- gsub(' csv','',names(table))
  names(table) <- simpleCap(names(table))
  sheetName    <- names(table) 
  table <- rbindlist(table)
  table[,totwghtlandg:=round(totwghtlandg,0)]
  table[,totvallandg:=round(totwghtlandg,0)]
  names(table) <-  c("Country", "Year", "Total Landings Weight","Total Landings Value", "Number of rows")
  sheet <- createSheet(wb, sheetName = sheetName)
  xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
    rows <-createRow(sheet,rowIndex=rowIndex)
    sheetTitle <-createCell(rows, colIndex=1)
    setCellValue(sheetTitle[[1,1]], title)
    setCellStyle(sheetTitle[[1,1]], titleStyle)
  }
  # Add title
  xlsx.addTitle(sheet, rowIndex=1, title=sheetName,
                titleStyle = TITLE_STYLE)
  # Add sub title
  xlsx.addTitle(sheet, rowIndex=3, 
                title="Recap on the number of rows and landings by country and year",
                titleStyle = SUB_TITLE_STYLE)
  # Add a table into a worksheet
  #++++++++++++++++++++++++++++++++++++
  addDataFrame(table, sheet, startRow=5, startColumn=1,
               row.names = F,
               colnamesStyle = TABLE_COLNAMES_STYLE,
               rownamesStyle = TABLE_ROWNAMES_STYLE)
  # Change column width
  setColumnWidth(sheet, colIndex=c(1:ncol(table)), colWidth=16)
}

# table.H.errors <- Map(cbind,table.H.errors,valid ="N")
table.H.errors <- lapply(table.H.errors,function(x){return(x[,.(country,year,totwghtlandg,totvallandg)])})
table.H.tables <- lapply(table.H.errors,function(x){return(x <- x[,list(totwghtlandg = round(sum(totwghtlandg,na.rm = T),0),totvallandg = round(sum(as.numeric(totvallandg),na.rm = T),0),                                                                      nrows = nrow(.SD)),
                                                                  by = .(country,year)])})

lapply(1:length(table.H.tables),function(x){addTables(table.H.tables[x])})

saveWorkbook(wb, "Table.H.checks.Tor.3.3.xlsx")

table.h.total <- fread('../../data/table_h_total_valid_and_not.csv')
nrow(table.h.total[valid=='N',])
# Number of non valid rows
nrow(table.h.total[valid=='N',])
# Percentage
round((nrow(table.h.total[valid=='N',])/
         nrow(table.h.total)*100),2)