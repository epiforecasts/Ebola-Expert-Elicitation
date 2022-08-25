
library(dplyr)
library(rgdal)
library(sf)
library(stringr)
library(raster)
library(data.table)
library(dplyr)

fix_nan <- function(x) {

  x[is.na(x)] <- 0
  x

}

fix_nan1 <- function(x) {

  x[is.na(x)] <- 1
  x

}

extract_totcase_data <- function(subarea = FALSE, case_type='confirmed_cases')

  {
  
  #DRC_all = st_read("data/shapes/ZS_MSF/COD_htbnd_lvl1_ZS_a_msf181114.shp", stringsAsFactors = FALSE) 

  DRC2 = st_read("data/shapes/Fixed_2/DRC_FIXED.shp", stringsAsFactors = FALSE)    # shapefiles


  #DRC2$ADM2_NAME <- str_to_title(DRC2$ADM2_NAME)
  POPDATA = read.csv('data/population/population_perHZ.csv', stringsAsFactors = FALSE)               # population data
  POPDATA$ADM2_NAME[POPDATA$ADM2_NAME == "Ganga"] = "Ganga Dingila"
  POPDATA$ADM2_NAME[POPDATA$ADM2_NAME == "Kimbi Lulenge"] = "Kimbi-Lulenge"
  DRC2_drc = DRC2
  colnames(DRC2_drc)[colnames(DRC2_drc)=="name"] <- "ADM2_NAME"                                         # sort out column names
  DRC2_drc = left_join(DRC2_drc, POPDATA, by = "ADM2_NAME")

  if (subarea != FALSE) {

    DRC2_drc = DRC2_drc[DRC2_drc$ADM1_NAME %in% subarea, ]                                              # filter data by province

  }

  headers =  read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrr9DRaC2fXzPdmOxLW-egSYtxmEp_RKoYGggt-zOKYXSx4RjPsM4EO19H7OJVX1esTtIoFvlKFWcn/pub?gid=1564028913&single=true&output=csv", header = F, nrows = 1, as.is = TRUE)
  DRCDATA = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrr9DRaC2fXzPdmOxLW-egSYtxmEp_RKoYGggt-zOKYXSx4RjPsM4EO19H7OJVX1esTtIoFvlKFWcn/pub?gid=1564028913&single=true&output=csv", skip = 2, header = F)
  colnames(DRCDATA)= headers
  colnames(DRCDATA)[colnames(DRCDATA)=="health_zone"] <- "ADM2_NAME"

  DRCDATA[is.na(DRCDATA)] <- 0                                                                          # set NAN values to 0

  DT = data.table::data.table(DRCDATA)

  DT$ADM2_NAME = as.character(DT$ADM2_NAME)                                                             # clean HZ names
  DT$ADM2_NAME[DT$ADM2_NAME == "Kayina"] = "Kayna"
  DT$ADM2_NAME[DT$ADM2_NAME == "Mangurujipa"] = "Manguredjipa"
  DT$ADM2_NAME[DT$ADM2_NAME == "Nyankunde"] = "Nyakunde"
  DT$ADM2_NAME[DT$ADM2_NAME == "Rwampara (Bunia)"] = "Rwampara"

  DT_hz = DT[,max(total_cases), by=ADM2_NAME]                                                       # calculate total cases to date by HZ
  DF_hz = as.data.frame.matrix(DT_hz)
  DRC2_cases = left_join(DRC2_drc, DF_hz, by = "ADM2_NAME")
  DRC2_cases = DRC2_cases[ !(DRC2_cases$ADM2_NAME == "Lubunga" & DRC2_cases$ADM1_NAME == "Kasa\x95-Central") & !(DRC2_cases$ADM2_NAME=="Bili" & DRC2_cases$ADM1_NAME=="Nord-Ubangi"),]
  DRC2_cases

}

construct_time_series_mat <- function(DRC2_drc = extract_totcase_data())

  {
  headers =  read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrr9DRaC2fXzPdmOxLW-egSYtxmEp_RKoYGggt-zOKYXSx4RjPsM4EO19H7OJVX1esTtIoFvlKFWcn/pub?gid=1564028913&single=true&output=csv", header = F, nrows = 1, as.is = TRUE)
  DRCDATA = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrr9DRaC2fXzPdmOxLW-egSYtxmEp_RKoYGggt-zOKYXSx4RjPsM4EO19H7OJVX1esTtIoFvlKFWcn/pub?gid=1564028913&single=true&output=csv", skip = 2, header = F)
  colnames(DRCDATA)= headers
  colnames(DRCDATA)[colnames(DRCDATA)=="health_zone"] <- "ADM2_NAME"

  DRCDATA[is.na(DRCDATA)] <- 0
  DRCDATA$ADM2_NAME = as.character(DRCDATA$ADM2_NAME)
  DT = data.table::data.table(DRCDATA)
  DT$ADM2_NAME = as.character(DT$ADM2_NAME)
  DT$ADM2_NAME[DT$ADM2_NAME == "Kayina"] = "Kayna"
  DT$ADM2_NAME[DT$ADM2_NAME == "Mangurujipa"] = "Manguredjipa"
  DT$ADM2_NAME[DT$ADM2_NAME == "Nyankunde"] = "Nyakunde"
  DT$ADM2_NAME[DT$ADM2_NAME == "Rwampara (Bunia)"] = "Rwampara"

  DT_hz = DT[,max(total_cases), by=ADM2_NAME]
  DF_hz = as.data.frame.matrix(DT_hz)


  DRCDATA$ADM2_NAME = as.character(DRCDATA$ADM2_NAME)

  DRCDATAT = DT

  DRCDATAT$report_date = as.Date(DRCDATAT$report_date, "%Y-%m-%d" )

  casesbydate = c()
  areas = c()
  
  DRCDATAT = DRCDATAT[order(report_date),]
  DRCDATAT[, neg_confirmed_cases := confirmed_cases - lag(confirmed_cases, 1), by = ADM2_NAME]
  
  DRCDATAT[, new_confirmed_cases := confirmed_cases - lag(confirmed_cases, 1), by = ADM2_NAME]
  DRCDATAT[new_confirmed_cases != 0, new_confirmed_cases := fix_nan(pmin(lag(new_confirmed_cases, 1),0)) + new_confirmed_cases, by = ADM2_NAME]
  DRCDATAT[new_confirmed_cases < 0, new_confirmed_cases := 0, by = ADM2_NAME]
  
  hznameframe = dplyr::select(DRC2_drc, "ADM2_NAME", OBJECTID)                        # create dummy array with all the HZs
  hznameframe = data.table::data.table(hznameframe)
  hznameframe = hznameframe[(is.na(hznameframe$ADM2_NAME) == FALSE)]
  
  DRCDATATfull = left_join(hznameframe[order(ADM2_NAME), -c('geometry', 'OBJECTID')], DRCDATAT, by=c('ADM2_NAME'))
  
  diff_cases = dcast(DRCDATATfull[ADM2_NAME %in% unique(DRC2_drc$ADM2_NAME), c('ADM2_NAME', 'report_date', 'new_confirmed_cases')], formula =  ADM2_NAME ~ report_date, value.var = 'new_confirmed_cases')
  
  diff_cases = fix_nan(diff_cases[,-'NA'])
  
  casesbydate = dcast(DRCDATATfull[ADM2_NAME %in% unique(DRC2_drc$ADM2_NAME), c('ADM2_NAME', 'report_date', 'confirmed_cases')], formula =  ADM2_NAME ~ report_date, value.var = 'confirmed_cases')
  casesbydate = fix_nan(casesbydate[,,-'NA'])
  
  dates = sort(unique(DRCDATATfull$report_date))   
            
  list(casebydate = casesbydate[, -'ADM2_NAME'], diff_cases=diff_cases[, -'ADM2_NAME'], dates=dates)
#  # Create matrix of cumulative incidence
#  for (date in sort(unique(DRCDATAT$report_date) ))
#  {
#
#    tempframe = left_join(hznameframe, dplyr::select(DRCDATAT[DRCDATAT$report_date==date], "total_cases", "ADM2_NAME"), by ="ADM2_NAME", name=character(date))
#    casesbydate = append(casesbydate, c(as.vector(fix_nan(data.table::setorder(tempframe, "ADM2_NAME")$total_cases))))
#    areas = append(areas, tempframe$ADM2_NAME)
#
#
#  }
#  
#  casesbydate = matrix(casesbydate, ncol=length(unique(DRCDATAT$report_date)))        # naje ut a matrix
#  casesbydate[is.na(casesbydate)] = 0.0
#
#  casesbydate_pre = casesbydate[,c(1,1:(ncol(casesbydate)-1))]                        # shifted array to find daily incidence
#
#  casesbydate_pre[,1] = 0
#
#  diff_cases = casesbydate - casesbydate_pre                                          # calculate daily incidence
#
#
#  diff_cases[diff_cases < 0] = 0                                                      # make negative values 0 (not sure how these arise in the data - not sure how to find out)
#
#                                # return cumulative and daily incidence
#  dates_in_data = sort(unique(DRCDATA$report_date))
#  datecol = data.table::as.data.table(c(sapply(seq(as.Date(head(dates_in_data,n=1)), as.Date(tail(dates_in_data,n=1)), by="days"), as.character)))
#  colnames(datecol) = c('report_date')
#
#
#  diff_cases_dt = t(as.data.table(diff_cases))
#  colnames(diff_cases_dt) = DRC2_drc$ADM2_NAME
#  diff_cases_dt = as.data.table(diff_cases_dt)
#  diff_cases_dt$report_date = sort(unique(DRCDATA$report_date))
#  diff_cases_dt = merge(datecol, diff_cases_dt, on='report_date', all=TRUE)
#  diff_cases_dt = fix_nan(diff_cases_dt)
#  diff_cases_dt = diff_cases_dt[, colSums(diff_cases_dt != 0) > 0, with = FALSE]
#  write.csv(diff_cases_dt, paste( Sys.Date(), "_casesbyhz.csv", sep=""))
#
#  list(casebydate = casesbydate, diff_cases=diff_cases, dates=as.Date(data.frame(datecol)[,], "%Y-%m-%d"))
#
}




construct_time_series_mat_old <- function(DRC2_drc = extract_totcase_data())
  
{
  headers =  read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrr9DRaC2fXzPdmOxLW-egSYtxmEp_RKoYGggt-zOKYXSx4RjPsM4EO19H7OJVX1esTtIoFvlKFWcn/pub?gid=1564028913&single=true&output=csv", header = F, nrows = 1, as.is = TRUE)
  DRCDATA = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSrr9DRaC2fXzPdmOxLW-egSYtxmEp_RKoYGggt-zOKYXSx4RjPsM4EO19H7OJVX1esTtIoFvlKFWcn/pub?gid=1564028913&single=true&output=csv", skip = 2, header = F)
  colnames(DRCDATA)= headers
  colnames(DRCDATA)[colnames(DRCDATA)=="health_zone"] <- "ADM2_NAME"
  
  DRCDATA[is.na(DRCDATA)] <- 0
  DRCDATA$ADM2_NAME = as.character(DRCDATA$ADM2_NAME)
  DT = data.table::data.table(DRCDATA)
  DT$ADM2_NAME = as.character(DT$ADM2_NAME)
  DT$ADM2_NAME[DT$ADM2_NAME == "Kayina"] = "Kayna"
  DT$ADM2_NAME[DT$ADM2_NAME == "Mangurujipa"] = "Manguredjipa"
  DT$ADM2_NAME[DT$ADM2_NAME == "Nyankunde"] = "Nyakunde"
  DT$ADM2_NAME[DT$ADM2_NAME == "Rwampara (Bunia)"] = "Rwampara"
  
  DT_hz = DT[,max(total_cases), by=ADM2_NAME]
  DF_hz = as.data.frame.matrix(DT_hz)
  
  
  DRCDATA$ADM2_NAME = as.character(DRCDATA$ADM2_NAME)
  
  DRCDATAT = DT
  
  DRCDATAT$report_date = as.Date(DRCDATAT$report_date, "%Y-%m-%d" )
  
  casesbydate = c()
  areas = c()
  
  hznameframe = dplyr::select(DRC2_drc, "ADM2_NAME", OBJECTID)                        # create dummy array with all the HZs
  
  hznameframe = data.table::data.table(hznameframe)
  
  hznameframe = hznameframe[(is.na(hznameframe$ADM2_NAME) == FALSE)]
  
  # Create matrix of cumulative incidence
  for (date in sort(unique(DRCDATAT$report_date) ))
  {
    
    tempframe = left_join(hznameframe, dplyr::select(DRCDATAT[DRCDATAT$report_date==date], "total_cases", "ADM2_NAME"), by ="ADM2_NAME", name=character(date))
    casesbydate = append(casesbydate, c(as.vector(fix_nan(data.table::setorder(tempframe, "ADM2_NAME")$total_cases))))
    areas = append(areas, tempframe$ADM2_NAME)
    
    
  }
  casesbydate = matrix(casesbydate, ncol=length(unique(DRCDATAT$report_date)))        # naje ut a matrix
  casesbydate[is.na(casesbydate)] = 0.0
  
  casesbydate_pre = casesbydate[,c(1,1:(ncol(casesbydate)-1))]                        # shifted array to find daily incidence
  
  casesbydate_pre[,1] = 0
  
  diff_cases = casesbydate - casesbydate_pre                                          # calculate daily incidence
  
  
  diff_cases[diff_cases < 0] = 0                                                      # make negative values 0 (not sure how these arise in the data - not sure how to find out)
  
  # return cumulative and daily incidence
  dates_in_data = sort(unique(DRCDATA$report_date))
  datecol = data.table::as.data.table(c(sapply(seq(as.Date(head(dates_in_data,n=1)), as.Date(tail(dates_in_data,n=1)), by="days"), as.character)))
  colnames(datecol) = c('report_date')
  
  
  diff_cases_dt = t(as.data.table(diff_cases))
  colnames(diff_cases_dt) = DRC2_drc$ADM2_NAME
  diff_cases_dt = as.data.table(diff_cases_dt)
  diff_cases_dt$report_date = sort(unique(DRCDATA$report_date))
  diff_cases_dt = merge(datecol, diff_cases_dt, on='report_date', all=TRUE)
  diff_cases_dt = fix_nan(diff_cases_dt)
  diff_cases_dt = diff_cases_dt[, colSums(diff_cases_dt != 0) > 0, with = FALSE]
  write.csv(diff_cases_dt, paste( Sys.Date(), "_casesbyhz.csv", sep=""))
  
  list(casebydate = casesbydate, diff_cases=diff_cases, dates=as.Date(data.frame(datecol)[,], "%Y-%m-%d"))
  
}

find_weighted_centroids <- function(shapes, raster_obj){
  cent_ys = c()
  cent_xs = c()
  for (hz in shapes$ADM2_NAME) {


    cropped_HZ = crop(raster_obj, extent(DRC2_cases[DRC2_cases$ADM2_NAME == hz,]))
    masked_HZ = mask(cropped_HZ, DRC2_cases[DRC2_cases$ADM2_NAME == hz,])
    masked_points = fix_nan1(rasterToPoints(masked_HZ))
    y = weighted.mean(masked_points[,2], masked_points[,3])
    x = weighted.mean(masked_points[,1], masked_points[,3])

    if (is.na(y)){
    coord = st_coordinates(st_centroid(DRC2_cases[DRC2_cases$ADM2_NAME == hz,]))
    x = coord[1]
    y = coord[2]
    }
    cent_ys = append(cent_ys, y)
    cent_xs = append(cent_xs, x)


  }
  shapes$cent_xs = cent_xs
  shapes$cent_ys = cent_ys

  centroids = st_as_sf(data.table(shapes), coords=c('cent_xs', y='cent_ys'), crs=4326)



  map = leaflet() %>%
    addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group="Basemap") %>%
    addRasterImage(fix_inf(log10(masked)), colors=viridis::viridis(100), opacity=0.5, group="population") %>%


    addPolygons(data=shapes, fill=F, color='red', fillOpacity = 0.0) %>%

    addCircleMarkers(data=centroids, color='k', fillOpacity = 0.5, radius=10)



    centroids


}

