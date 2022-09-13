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
tI <- list.files(path = 'effort', pattern = glob2rx("*Table.I*.csv"))
sort(tI)
tI <- tI[!tI %like% '*gearNOT*']
sort(tI)
# We need to select the nuber of fields to keep and also made a recap on the number of rows and landings/effort we loose.
# TABLE I Reporting ----
setwd(paste0(outPath,'effort/'))
table.I.errors <- lapply(tI,fread)
names(table.I.errors) <- tI
table.I.errors <- 
  lapply(1:length(table.I.errors),function(x){
    if (x!=11){  
    n <- 6
    countrylbl <- gsub('.csv','',substr(names(table.I.errors[x]), nchar(names(table.I.errors[x]))-n, nchar(names(table.I.errors[x]))))
    DT <- rbindlist(table.I.errors[x])
    DT[,country:=countrylbl]
    return(DT)}
    else{NULL}
})

names(table.I.errors) <- tI
names(table.I.errors)[-11] <-   gsub('.{8}$','',names(table.I.errors)[-11])
names(table.I.errors)[11] <-   gsub('.csv$','',names(table.I.errors)[11])
table.I.errors = table.I.errors[order(names(table.I.errors))]

names(table.I.errors)
newnames <- c(names(table.I.errors[1]),
              names(table.I.errors[2]),
              names(table.I.errors[3]),
              names(table.I.errors[11]),
              names(table.I.errors[12]),
              names(table.I.errors[18]))
table.I.errors <- list( rbindlist(table.I.errors[1]),
                        rbindlist(table.I.errors[2]),
                        rbindlist(table.I.errors[3:10]),
                        rbindlist(table.I.errors[11]),
                        rbindlist(table.I.errors[12:17]),
                        rbindlist(table.I.errors[18]))
names(table.I.errors) <- newnames
# The first table is the missing subregion one that is already summarised,
# while the others are not.
missing.subregion <- table.I.errors[[4]]
table.I.errors <- table.I.errors[-4]
missing.subregion <-
  missing.subregion[, list(totfishdays = sum(totfishdays),
                           nrows = sum(nrows)),
                    by = .(country, year)]
table.I.errors <- Map(cbind,table.I.errors,valid ="N")
table.I.errors <- lapply(table.I.errors,function(x){return(x[,.(country,year,totfishdays)])})
table.I.tables <- lapply(table.I.errors,function(x){return(x <- x[,list(totfishdays = sum(totfishdays),
                                                                        nrows = nrow(.SD)),
                                                               by = .(country,year)])})
missing.subregion[,totfishdays:=round(totfishdays,0)]
names(missing.subregion) <- c("Country", "Year", "Fishing days", "Number of rows")

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

# errors.total <- fread('fdi_TABLE_I_errors.csv')
# errors.total <- errors.total[,.(country,year,totfishdays)]
# errors.total <- errors.total[, list(totfishdays = sum(totfishdays),
#                                     nrows = nrow(.SD)),
#                              by = .(country, year)]
# errors.total[,totfishdays := round(totfishdays,0)]
# errors.total <- setorder(errors.total,country)
# names(errors.total) <- c("Country", "Year", "Fishing days", "Number of rows")
# 
# # Create a new sheet in the workbook
# #++++++++++++++++++++++++++++++++++++
# sheet <- createSheet(wb, sheetName = "Table I Errors")
# 
# xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
#   rows <-createRow(sheet,rowIndex=rowIndex)
#   sheetTitle <-createCell(rows, colIndex=1)
#   setCellValue(sheetTitle[[1,1]], title)
#   setCellStyle(sheetTitle[[1,1]], titleStyle)
# }
# 
# # Add title
# xlsx.addTitle(sheet, rowIndex=1, title="Table I Errors",
#               titleStyle = TITLE_STYLE)
# # Add sub title
# xlsx.addTitle(sheet, rowIndex=3, 
#               title="Recap on the total number of rows in Table I with errors",
#               titleStyle = SUB_TITLE_STYLE)
# Add a table into a worksheet
#++++++++++++++++++++++++++++++++++++
# addDataFrame(errors.total, sheet, startRow=5, startColumn=1,
#              row.names = F,
#              colnamesStyle = TABLE_COLNAMES_STYLE,
#              rownamesStyle = TABLE_ROWNAMES_STYLE)
# # Change column width
# setColumnWidth(sheet, colIndex=c(1:ncol(errors.total)), colWidth=16)

# Create a new sheet in the workbook
#++++++++++++++++++++++++++++++++++++
sheet <- createSheet(wb, sheetName = "Table I Missing Subregion")

xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}

# Add title
xlsx.addTitle(sheet, rowIndex=1, title="Table I Missing Subregion",
              titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet, rowIndex=3, 
              title="Recap on the number of rows in Table I with unknown Subregion",
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
  table[,totfishdays:=round(totfishdays,0)]
  names(table) <-  c("Country", "Year", "Fishing days", "Number of rows")
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
                title="Recap on the number of rows and fishing days by country and year",
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

lapply(1:length(table.I.tables),function(x){addTables(table.I.tables[x])})

saveWorkbook(wb, "Table.I.checks.Tor.3.3.xlsx")
  
table.i.total <- fread('../../data/table_i_total_valid_and_not.csv')
# Number of non valid rows
nrow(table.i.total[valid=='N',])
# Percentage
round((nrow(table.i.total[valid=='N',])/
  nrow(table.i.total)*100),2)
