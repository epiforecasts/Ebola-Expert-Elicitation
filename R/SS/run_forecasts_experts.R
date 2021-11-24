

require(EpiCastR)
require(tidyverse)
require(DescTools)
source("R/data_prep.r")

sf::sf_use_s2(FALSE)
DRC2_cases = extract_totcase_data(subarea=c("Nord-Kivu", "Ituri", "Tshopo", "Maniema", "Sud-Kivu", "Haut-Uele", "Bas-Uele"))

setwd("~/Documents/Work/Analyses/Ebola_DRC/")
timeseries = construct_time_series_mat(DRC2_cases)

DRC2_cases = DRC2_cases[order(DRC2_cases[['ADM2_NAME']]),]

timeseries_mat = timeseries$diff_cases

timeseries_mat = cbind(timeseries_mat, matrix(rep(rep(0, dim(timeseries_mat)[1]), 30), nrow=dim(timeseries_mat)[1]))
start_date = timeseries$dates[1]


thresholds = c(2, 6, 10, 20)
scores = data.frame()
scores_adj = data.frame()

month = 'March_2020'

expert_results = fread(paste0('../Expert-elicitation/Outputs/results_', month,'.csv'))

expert_results[, date := lubridate::dmy(date)]

forcast_to_date = lubridate::ceiling_date(lubridate::my(month), 'month') - 1 
nominal_forecast_date = lubridate::floor_date(lubridate::my(month), 'month')

forecast_to_day = as.numeric(forcast_to_date - start_date)
forecast_from_day = as.numeric(nominal_forecast_date - start_date)

expert_dates = sort(unique(expert_results$date))



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
  
  
  write.csv(data.table(plain_risks), paste0("forecasts/ebola_risks_", month, '_', as.character(ellicitation_date) ,".csv"))
  write.csv(data.table(adjac_risks), paste0("forecasts/ebola_risks_adj_", month,'_', as.character(ellicitation_date) ,".csv"))
  
}

colnames(scores) = sapply(thresholds, FUN = function(X){paste0('score_', X)})
colnames(scores_adj) = sapply(thresholds, FUN = function(X){paste0('score_', X)})
write.csv(scores, paste0("~/Documents/work/Analyses/ExpertEllicitaion/scores/ebola_scores_", as.character(ellicitation_date) ,".csv"))
write.csv(scores_adj, paste0("~/Documents/work/Analyses/ExpertEllicitaion/scores/ebola_scores_adj_", as.character(ellicitation_date) ,".csv"))



log_score = function(risk_values,wherecaseswere){
  log_probs = wherecaseswere * log(abs(risk_values-1e-10)) + ((1 - wherecaseswere ) * log(1 - (abs(risk_values-1e-10))))
  mean_log_probs = mean(log_probs)
  mean_log_probs
  



experts_december = read.csv(paste0('../../Analyses/Expert-elicitation/Outputs/results_', month, '_cm.csv'))
experts_december %>% filter(p_cm == ">=2", HZ == 'BENI')

experts_december['date'] = lubridate::dmy(experts_december['expert.date'])



expert_scores = data.frame()

experts =  sort(unique(experts_december$expert))
for (thresh in thresholds){
  
  long_thresh = paste0(">=", as.character(thresh))
  
  cases_in_dec = 1. * (rowSums(timeseries_mat[,forecast_from_day :forecast_to_day]) > (thresh - 1))
  
  DRC2_cases['cases_in_dec'] = cases_in_dec
  
  DRC2_cases_df = as.data.frame(DRC2_cases)
  
  actual_cases = DRC2_cases_df[,c("ADM2_NAME", "cases_in_dec")] %>% filter(ADM2_NAME %in% str_to_title(sort(unique(experts_december[,'HZ']))))
  

  for (e in experts){
    expert_date = (expert_results %>% filter(expert == e))$date[1]
    
    for (hz in sort(unique(experts_december[,'HZ']))){
      
      score = BrierScore(
        as.double(experts_december %>% filter(p_cm == long_thresh, expert==e, HZ==hz) %>% dplyr::select("p_cm_val")),
        as.double(actual_cases %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select("cases_in_dec")))
      
      score_log = log_score(
        as.double(experts_december %>% filter(p_cm ==  long_thresh, expert==e, HZ==hz) %>% dplyr::select("p_cm_val")),
        as.double(actual_cases %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select("cases_in_dec")))
      
      prob = as.double(experts_december %>% filter(p_cm ==  long_thresh, expert==e, HZ==hz) %>% dplyr::select("p_cm_val"))
      cases = as.double(actual_cases %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select("cases_in_dec"))
      
      expert_scores = rbind(expert_scores,
                            data.frame(e, hz, prob, cases, score, score_log,  long_thresh, type='expert', expert_date, forecast_to_day))
      
    }
    ebola_risks = read.csv(paste0("../../Analyses/ExpertEllicitaion/forecasts/ebola_risks_", month,"_" , as.character(expert_date), ".csv"))
    ebola_risks_adj = read.csv(paste0("../../Analyses/ExpertEllicitaion/forecasts/ebola_risks_adj_", month,"_" , as.character(expert_date), ".csv"))
    
    ebola_risks %>% filter(ADM2_NAME %in% str_to_title(sort(unique(experts_december[,'HZ'])))) %>% dplyr::select(paste0('risk_TH_', thresh))
    
    for (hz in sort(unique(experts_december[,'HZ']))){
      
      score = BrierScore(
        as.double(ebola_risks %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select(paste0('risk_TH_', thresh))),
        as.double(actual_cases %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select("cases_in_dec")))
      score_log = log_score(
        as.double(ebola_risks %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select(paste0('risk_TH_', thresh))),
        as.double(actual_cases %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select("cases_in_dec")))
      
      prob = as.double(ebola_risks %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select(paste0('risk_TH_', thresh)))
      cases = as.double(actual_cases %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select("cases_in_dec"))
      expert_scores = rbind(expert_scores,
                            data.frame(e = paste0(e), hz=hz, prob=prob, cases=cases, score, score_log, long_thresh, type='model', expert_date, forecast_to_day))
    }

  }
  score = BrierScore(
    sapply(experts_december %>% filter(p_cm == long_thresh, expert==e) %>% dplyr::select("p_cm_val"), as.double),
    sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))
  score_log = log_score(
    sapply(experts_december %>% filter(p_cm == long_thresh, expert==e) %>% dplyr::select("p_cm_val"), as.double),
    sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))
  expert_scores = rbind(expert_scores,
                        data.frame(e, hz='All', prob=0, cases=0, score, score_log, long_thresh, type='all', expert_date, forecast_to_day))
  
  score = BrierScore(
    sapply(experts_december %>% filter(p_cm == long_thresh, expert==e) %>% dplyr::select("p_cm_val"), as.double),
    sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))
  score_log = log_score(
    sapply(experts_december %>% filter(p_cm == long_thresh, expert==e) %>% dplyr::select("p_cm_val"), as.double),
    sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))
  expert_scores = rbind(expert_scores,
                        data.frame(e, hz='All', prob=0, cases=0, score, score_log, long_thresh, type='all', expert_date, forecast_to_day))
}




ggplot(data.table(expert_scores)[type != 'all']) + 
  geom_point(aes(x=expert_date, y=prob, color=type)) + 
  geom_point(aes(x=expert_date, y=cases), color='black', alpha=0.2) +
  facet_grid(long_thresh ~ hz) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(data.table(expert_scores)[type != 'all']) + 
  geom_point(aes(x=expert_date, y=score, color=type)) + 
  facet_grid(long_thresh ~ hz)+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(data.table(expert_scores)[type != 'all']) + 
  geom_point(aes(x=expert_date, y=score_log, color=type)) + 
  facet_grid(long_thresh ~ hz) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# =============



score = BrierScore(
  sapply(ebola_risks %>% filter(ADM2_NAME %in% str_to_title(sort(unique(experts_december[,'HZ'])))) %>% dplyr::select(paste0('risk_TH_', thresh)), as.double),
  sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))
score_log = log_score(
  sapply(ebola_risks %>% filter(ADM2_NAME %in% str_to_title(sort(unique(experts_december[,'HZ'])))) %>% dplyr::select(paste0('risk_TH_', thresh)), as.double),
  sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))
expert_scores = rbind(expert_scores,
                      data.frame(e='model', hz='All', prob='NULL', cases='NULL', score, score_log))

for (hz in sort(unique(experts_december[,'HZ']))){
  print(hz)
  
  score = BrierScore(
    as.double(ebola_risks_adj %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select(risk_39_2)),
    as.double(actual_cases %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select("cases_in_dec")))
  score_log = log_score(
    as.double(ebola_risks_adj %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select(risk_39_2)),
    as.double(actual_cases %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select("cases_in_dec")))
  expert_scores = rbind(expert_scores,
                        data.frame(e = 'model_adj', hz=hz, prob='NULL', cases='NULL', score, score_log))
}

score = BrierScore(
  sapply(ebola_risks_adj %>% filter(ADM2_NAME %in% str_to_title(sort(unique(experts_december[,'HZ'])))) %>% dplyr::select(risk_39_2), as.double),
  sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))
score_log = log_score(
  sapply(ebola_risks_adj %>% filter(ADM2_NAME %in% str_to_title(sort(unique(experts_december[,'HZ'])))) %>% dplyr::select(risk_39_2), as.double),
  sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))
expert_scores = rbind(expert_scores,
                      data.frame(e='model_adj', hz='All', prob='NULL', cases='NULL', score, score_log))

for (hz in sort(unique(experts_december[,'HZ']))){
  
  score = BrierScore(
    mean(sapply(experts_december %>% filter(p_cm == ">=2", HZ==hz) %>% dplyr::select("p_cm_val"), as.double)),
    as.double(actual_cases %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select("cases_in_dec")))
  score_log = log_score(
    mean(sapply(experts_december %>% filter(p_cm == ">=2", HZ==hz) %>% dplyr::select("p_cm_val"), as.double)),
    as.double(actual_cases %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select("cases_in_dec")))
  prob = mean(sapply(experts_december %>% filter(p_cm == ">=2", HZ==hz) %>% dplyr::select("p_cm_val"), as.double))
  cases = as.double(actual_cases %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select("cases_in_dec"))
  
  expert_scores = rbind(expert_scores,
                        data.frame(e='mean', hz, prob, cases, score, score_log))
  
}

mean_pred = sapply(experts_december %>% filter(p_cm == ">=2") %>% group_by(HZ) %>% summarise_at(vars(p_cm_val), funs(mean(., na.rm=TRUE))) %>% dplyr::select("p_cm_val"), as.double)
score = BrierScore(mean_pred, sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))
score_log = log_score(mean_pred, sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))

expert_scores = rbind(expert_scores,
                      data.frame(e="mean", hz='All', prob='NULL', cases='NULL', score, score_log))

