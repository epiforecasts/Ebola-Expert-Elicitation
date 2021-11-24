require(EpiCastR)
require(tidyverse)
require(DescTools)

source("R/data_prep.r")

experts_path = '../Expert-elicitation'


DRC2_cases = extract_totcase_data(subarea=c("Nord-Kivu", "Ituri", "Tshopo", "Maniema", "Sud-Kivu", "Haut-Uele", "Bas-Uele"))

timeseries = construct_time_series_mat(DRC2_cases)

DRC2_cases = DRC2_cases[order(DRC2_cases[['ADM2_NAME']]),]

timeseries_mat = timeseries$diff_cases
timeseries_mat = cbind(timeseries_mat, matrix(rep(rep(0, dim(timeseries_mat)[1]), 30), nrow=dim(timeseries_mat)[1]))


start_date = timeseries$dates[1]


thresholds = c(2, 6, 10, 20)
scores = data.frame()
scores_adj = data.frame()
expert_scores = data.frame()

months = c('December_2019', 'January_2020', 'February_2020')

for (month in months){
  
  expert_results = fread(paste0(experts_path, '/Outputs/results_', month,'.csv'))
  
  expert_results[, date := lubridate::dmy(date)]
  
  forcast_to_date = lubridate::ceiling_date(lubridate::my(month), 'month') - 1 
  nominal_forecast_date = lubridate::floor_date(lubridate::my(month), 'month')
  
  forecast_to_day = as.numeric(forcast_to_date - start_date)
  forecast_from_day = as.numeric(nominal_forecast_date - start_date)
  
  expert_dates = sort(unique(expert_results$date))
  
  log_score = function(risk_values,wherecaseswere){
    log_probs = wherecaseswere * log(abs(risk_values-1e-10)) + ((1 - wherecaseswere ) * log(1 - (abs(risk_values-1e-10))))
    mean_log_probs = mean(log_probs)
    mean_log_probs
    
  }
  
  
  experts_december = read.csv(paste0(experts_path, '/Outputs/results_', month, '_cm.csv'))
  experts_december %>% filter(p_cm == ">=2", HZ == 'BENI')
  
  experts_december['date'] = lubridate::dmy(experts_december$date)
  
  
  
  
  
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
                              data.frame(e, hz, prob, cases, score, score_log,  long_thresh, type='expert', expert_date, forcast_to_date))
        
      }
      ebola_risks = read.csv(paste0("forecasts/ebola_risks_", month,"_" , as.character(expert_date), ".csv"))
      ebola_risks_adj = read.csv(paste0("forecasts/ebola_risks_adj_", month,"_" , as.character(expert_date), ".csv"))
      
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
                              data.frame(e, hz=hz, prob=prob, cases=cases, score, score_log, long_thresh, type='model', expert_date, forcast_to_date))
      }
      
      # Collective score of all experts over each HZ
      for (hz in sort(unique(experts_december[,'HZ']))){
        probs = as.vector(unlist(experts_december %>% filter(p_cm == long_thresh, HZ==hz) %>% dplyr::select("p_cm_val")))
        bincases = rep(as.double(actual_cases %>% filter(ADM2_NAME == str_to_title(hz)) %>% dplyr::select("cases_in_dec")), length(probs))
        score = BrierScore(probs,bincases)
        score_log = log_score(probs, bincases)
        
        expert_scores = rbind(expert_scores,
                              data.frame(e='all', hz, prob=mean(probs), cases=bincases[1], score, score_log,  long_thresh, type='expert', expert_date = nominal_forecast_date, forcast_to_date))
        
      }
      
      
      
    
    score = BrierScore(
      sapply(experts_december %>% filter(p_cm == long_thresh, expert==e) %>% dplyr::select("p_cm_val"), as.double),
      sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))
    score_log = log_score(
      sapply(experts_december %>% filter(p_cm == long_thresh, expert==e) %>% dplyr::select("p_cm_val"), as.double),
      sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))
    print(e)
    expert_scores = rbind(expert_scores,
                          data.frame(e, hz='All', prob=0, cases=0, score, score_log, long_thresh, type='all', expert_date, forcast_to_date))
    
    score = BrierScore(
      sapply(experts_december %>% filter(p_cm == long_thresh, expert==e) %>% dplyr::select("p_cm_val"), as.double),
      sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))
    score_log = log_score(
      sapply(experts_december %>% filter(p_cm == long_thresh, expert==e) %>% dplyr::select("p_cm_val"), as.double),
      sapply(actual_cases %>% dplyr::select("cases_in_dec"), as.double))
    expert_scores = rbind(expert_scores,
                          data.frame(e, hz='All', prob=0, cases=0, score, score_log, long_thresh, type='all', expert_date, forcast_to_date))
  }
  
  }
  
}



ggplot(data.table(expert_scores)[(type != 'all') & (forcast_to_date == '2020-02-29')]) + 
  geom_point(aes(x=expert_date, y=prob, color=type)) + 
  geom_point(aes(x=expert_date, y=cases), color='black', alpha=0.2) +
  facet_grid(long_thresh ~ hz) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(data.table(expert_scores)[(type != 'all') & (forcast_to_date == '2019-12-31')]) + 
  geom_point(aes(x=expert_date, y=score, color=type)) + 
  facet_grid(long_thresh ~ hz)+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(data.table(expert_scores)[type != 'all']) + 
  geom_point(aes(x=expert_date, y=score_log, color=type)) + 
  facet_grid(long_thresh ~ hz) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

