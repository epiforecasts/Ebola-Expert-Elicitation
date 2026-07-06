# run from the repository root

# packages
require(dplyr)
require(RColorBrewer)
require(leaflet)
require(ggplot2)
require(mapview)
require(raster)
require(htmltools)
library(sp)
require(sf)
require(rgeos)
require(htmlwidgets)
library(spdep)
library(tidyverse)


### get data

source("R/data_prep.r")

# keep the original health_zone column name for the cleaning steps below
evd_cases = read_case_data(rename_hz = FALSE)

### clean evd_cases
# correct classes
evd_cases<-as.data.frame(evd_cases)
evd_cases$health_zone<-toupper(as.character(evd_cases$health_zone))
evd_cases<-evd_cases%>%mutate_all(as.character)
evd_cases$total_cases<-as.numeric(evd_cases$total_cases)
# only keep records with 1 or more cases
evd_cases<-evd_cases%>%filter(total_cases>0)
# add a row column in case any problems later
evd_cases$rowN<-1:nrow(evd_cases)
# resolve those where the cases database and the spatial databases don't match
evd_cases$health_zone<-gsub("KAYINA", "KAYNA",evd_cases$health_zone)
evd_cases$health_zone<-as.character(evd_cases$health_zone)
evd_cases$health_zone<-gsub("MANGURUJIPA", "MANGUREDJIPA",evd_cases$health_zone)
evd_cases$health_zone<-as.character(evd_cases$health_zone)
evd_cases$health_zone<-gsub("N/A", NA ,evd_cases$health_zone)
evd_cases$health_zone<-as.character(evd_cases$health_zone)
evd_cases$health_zone<-gsub("RWAMPARA \\(BUNIA)", "RWAMPARA" ,evd_cases$health_zone)


# remove cases where health zone not specified
evd_cases<-evd_cases[!is.na(evd_cases$health_zone),] #remove HZ with no name that has 1 case
# check if any NAs
table(is.na(evd_cases$health_zone))
table(is.na(evd_cases$total_cases))

## output particular data on EVD cases for plot 
# include only cases in the last 2 weeks of data
evd_cases$report_date<-as.Date(evd_cases$report_date)

novquestions_cases<-evd_cases%>%
  filter(report_date>"2019-10-05" & report_date<="2019-10-19")%>%
  group_by(country, province, health_zone)%>%summarise(cases_2w=max(na.omit(total_cases))-min(na.omit(total_cases)))%>%
  filter(cases_2w>0)

decquestions_cases<-evd_cases%>%
  filter(report_date>"2019-10-26" & report_date<="2019-11-09")%>%
  group_by(country, province, health_zone)%>%summarise(cases_2w=max(na.omit(total_cases))-min(na.omit(total_cases)))%>%
  filter(cases_2w>0)

janquestions_cases<-evd_cases%>%
  filter(report_date>"2019-12-01" & report_date<="2019-12-15")%>%
  group_by(country, province, health_zone)%>%summarise(cases_2w=max(na.omit(total_cases))-min(na.omit(total_cases)))%>%
  filter(cases_2w>0)

febquestions_cases<-evd_cases%>%
  filter(report_date>"2019-12-29" & report_date<="2020-01-12")%>%
  group_by(country, province, health_zone)%>%summarise(cases_2w=max(na.omit(total_cases))-min(na.omit(total_cases)))%>%
  filter(cases_2w>0)

marquestions_cases<-evd_cases%>%
  filter(report_date>"2020-01-28" & report_date<="2020-02-11")%>%
  group_by(country, province, health_zone)%>%summarise(cases_2w=max(na.omit(total_cases))-min(na.omit(total_cases)))%>%
  filter(cases_2w>0)

marquestions_cases
  
