require(EpiCastR)
require(tidyverse)
require(DescTools)
source("R/data_prep.r")

sf::sf_use_s2(FALSE)
DRC2_cases = extract_totcase_data(subarea=c("Nord-Kivu", "Ituri", "Tshopo", "Maniema", "Sud-Kivu", "Haut-Uele", "Bas-Uele"))

timeseries = construct_time_series_mat_old(DRC2_cases)

DRC2_cases = DRC2_cases[order(DRC2_cases[['ADM2_NAME']]),]

timeseries_mat = timeseries$diff_cases

timeseries_mat = cbind(timeseries_mat, matrix(rep(rep(0, dim(timeseries_mat)[1]), 30), nrow=dim(timeseries_mat)[1]))
start_date = timeseries$dates[1]

thresholds = c(2, 6, 10, 20)
scores = data.frame()
scores_adj = data.frame()

month = 'November_2019'

expert_results = fread(paste0('../Expert-elicitation/Outputs/results_', month,'.csv'))

expert_results[, date := lubridate::dmy(date)]

forcast_to_date = lubridate::ceiling_date(lubridate::my(month), 'month') - 1 
nominal_forecast_date = lubridate::floor_date(lubridate::my(month), 'month')

forecast_to_day = as.numeric(forcast_to_date - start_date)
forecast_from_day = as.numeric(nominal_forecast_date - start_date)

expert_dates = c(nominal_forecast_date, sort(unique(expert_results$date)))



for (d in 1:length(unique(expert_dates))){ #
  
  ellicitation_date = unique(expert_dates)[d]
  
  print(ellicitation_date)
  
  buffer = as.numeric(nominal_forecast_date - ellicitation_date)
  day_of_forecast = as.numeric(ellicitation_date - start_date)
  time_horizons = c(as.numeric(forcast_to_date - ellicitation_date))
  
  
  
  forecasts = make_forecast(timeseries_mat = timeseries$diff_cases, shapes = DRC2_cases,day_of_forecast = day_of_forecast, fit_meth = 'nuts',
                            fit_over = 60,timestep = 1, period_and_lag = c(5,7), interaction = c(1), chains=5, cores=5, iter = 600, warmup = 100, buffer=buffer, timehorizons = time_horizons, thresholds = thresholds)
  
  
  forecasts_adj = make_forecast(timeseries_mat = timeseries$diff_cases, shapes = DRC2_cases,day_of_forecast = day_of_forecast, fit_meth = 'nuts',
                                fit_over = 60,timestep = 1, period_and_lag = c(5,7), interaction = c(4), chains=5, cores=5, iter = 600, warmup = 100, buffer=buffer, timehorizons = time_horizons, thresholds = thresholds)
  
  
  plain_risks = forecasts$risks %>% dplyr::select(c(c("PROVINCE", "ADM2_NAME", "casestodate"), colnames(forecasts$risks)[grepl("risk", colnames(forecasts$risks))] ))
  colnames(plain_risks) = c( 'PROVINCE',     'ADM2_NAME', 'casestodate' , sapply(colnames(plain_risks[, colnames(forecasts_adj$risks)[grepl("risk", colnames(forecasts_adj$risks))]]), function(X){str_replace_all(X, as.character(time_horizons[1]), 'TH')}))
  adjac_risks = forecasts_adj$risks %>% dplyr::select(c(c("PROVINCE", "ADM2_NAME", "casestodate"), colnames(forecasts_adj$risks)[grepl("risk", colnames(forecasts_adj$risks))] ))
  colnames(adjac_risks) = c( 'PROVINCE',     'ADM2_NAME', 'casestodate' , sapply(colnames(adjac_risks[, colnames(forecasts_adj$risks)[grepl("risk", colnames(forecasts_adj$risks))]]), function(X){str_replace_all(X, as.character(time_horizons[1]), 'TH')}))
  
  plain_risks = data.frame(plain_risks)[,!colnames(plain_risks) %in% c('geometry')]
  adjac_risks = data.frame(adjac_risks)[,!colnames(adjac_risks) %in% c('geometry')]
  
  scores = rbind(scores, unlist(forecasts$scores))
  scores_adj = rbind(scores_adj, unlist(forecasts_adj$scores))
  
  
  write.csv(data.table(plain_risks), paste0("forecasts/new/ebola_risks_", month, '_', as.character(ellicitation_date) ,".csv"))
  write.csv(data.table(adjac_risks), paste0("forecasts/new/ebola_risks_adj_", month,'_', as.character(ellicitation_date) ,".csv"))
  
  saveRDS(forecasts, paste0('forecasts/new/forecast_gravity_fit_', month, '_', as.character(ellicitation_date) ,".rds"))
  saveRDS(forecasts_adj, paste0('forecasts/new/forecast_adjacency_fit_', month, '_', as.character(ellicitation_date) ,".rds"))
}
