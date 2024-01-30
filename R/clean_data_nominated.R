library(data.table)
library(lubridate)
library(ggplot2)
library(DescTools)
library(stringr)


log_score = function(risk_values,wherecaseswere){
  log_probs = wherecaseswere * log(abs(risk_values-1e-10)) + ((1 - wherecaseswere ) * log(1 - (abs(risk_values-1e-10))))
  mean_log_probs = mean(log_probs)
  mean_log_probs
}



source("R/data_prep.r")

experts_path = '../Expert-elicitation'

# Get the case data for whole outbreak 
DRC2_cases = extract_totcase_data(subarea=c("Nord-Kivu", "Ituri", "Tshopo", "Maniema", "Sud-Kivu", "Haut-Uele", "Bas-Uele"))

#set order as alphabetical
DRC2_cases = DRC2_cases[order(DRC2_cases[['ADM2_NAME']]),]

# convert the data to incidence matrix [time x HZ]
timeseries = construct_time_series_mat(DRC2_cases)
timeseries_mat = timeseries$diff_cases
timeseries_mat = cbind(timeseries_mat, matrix(rep(rep(0, dim(timeseries_mat)[1]), 30), nrow=dim(timeseries_mat)[1]))

# find the startdate of the outbreak (for epiday calculations)
start_date = timeseries$dates[1]

# set thresholds and months of interest
thresholds = c(2, 6, 10, 20)
months = c('December_2019', 'January_2020', 'February_2020', 'March_2020')

# set container for results


get_clean_flair_data = function(no_case_p = 0, yes_case_p = 0.05){
  expert_model_data_all = data.table()
  
  
  
  # load data by month and arrange
  for (m in months){
    # set dates of forecast horizon and begining of month and find corresponding epi days
    forcast_to_date = lubridate::ceiling_date(lubridate::my(m), 'month') - 1 
    nominal_forecast_date = lubridate::floor_date(lubridate::my(m), 'month')
    forecast_to_day = as.numeric(forcast_to_date - start_date)
    forecast_from_day = as.numeric(nominal_forecast_date - start_date)
    
    # load expert ellicitaion data 
    experts_data = fread(paste0(experts_path, '/Outputs/results_', m, '_additional_cm.csv'))
    experts_data_surveyed = fread(paste0(experts_path, '/Outputs/results_', m, '_cm.csv'))
    experts_data_surveyed[, month:=m]
    
    experts_data[experts_data$HZ == 'NYANKUNDE','HZ'] = 'NYAKUNDE'
    experts_data_surveyed[experts_data_surveyed$HZ == 'NYANKUNDE','HZ'] = 'NYAKUNDE'
    
    experts_data[experts_data$HZ == 'MAKISO_KISANGANI','HZ'] = 'MAKISO-KISANGANI'
    experts_data_surveyed[experts_data_surveyed$HZ == 'MAKISO_KISANGANI','HZ'] = 'MAKISO-KISANGANI'
    
    #set appropriate column values 
    experts_data[, expert_date := lubridate::dmy(date)]  
    experts_data[, horizon_end_date := forcast_to_date]
    experts_data[, horizon_start_date := nominal_forecast_date]
    experts_data[, delay:=horizon_start_date - expert_date]
    experts_data[, total_horizon := horizon_end_date - expert_date]
    experts_data[, month := m]
    experts_data[, type := 'expert']
    
    # drop unwanted columns
    experts_data = experts_data[, -c('date', 'expert.date', 'V1')]
    
    # --- set up actual case data for scoring (1 or 0 for each threshold)
    # set container for actual case data
    actual_cases_allthreshs = data.table()
    
    # find true outcome for each threshold
    for (thresh in thresholds){
      
      # set the identifier for the threshold
      long_thresh = paste0(">=", as.character(thresh))
      
      # calculate birary outcome for each HZ
      cases_in_dec = 1. * (rowSums(timeseries_mat[,forecast_from_day :forecast_to_day]) > (thresh - 1))
      
      # add to case data for HZ names to link
      DRC2_cases['reported_cases'] = cases_in_dec
      DRC2_cases_df = as.data.frame(DRC2_cases)
      DRC2_cases_df['ADM2_NAME'] = str_to_upper(unlist(DRC2_cases_df['ADM2_NAME']))
      
      
      
      # filter out as a table of HZ and outcome for HZs of interest
      actual_cases = DRC2_cases_df[,c("ADM2_NAME", "reported_cases")]
      
      # set appropriate collums for merge with expert data
      actual_cases['p_cm'] = paste0('>=', thresh)
      colnames(actual_cases) = c("HZ", "reported_cases", 'p_cm')
      
      # add outcomes for threshold to overall outcome container
      actual_cases_allthreshs = rbind(actual_cases_allthreshs, actual_cases)
    }
    
    
   
    
    # calculate risks of for the forecast at nominal forecast date 
    ebola_risks = fread(paste0("forecasts/ebola_risks_", m,"_" , as.character(nominal_forecast_date), ".csv"))
    ebola_risks_adj = fread(paste0("forecasts/ebola_risks_adj_", m,"_" , as.character(nominal_forecast_date), ".csv"))
    
    
    ebola_risks = ebola_risks[, c('ADM2_NAME', 'risk_TH_2', 'risk_TH_6', 'risk_TH_10', 'risk_TH_20')]
    colnames(ebola_risks) = c('HZ', ">=2",  ">=6", ">=10", ">=20")
    
    ebola_risks_adj = ebola_risks_adj[, c('ADM2_NAME', 'risk_TH_2', 'risk_TH_6', 'risk_TH_10', 'risk_TH_20')]
    colnames(ebola_risks_adj) = c('HZ', ">=2",  ">=6", ">=10", ">=20")
  
    ebola_risks[, HZ := str_to_upper(HZ)]
    ebola_risks_adj[, HZ := str_to_upper(HZ)]
  
    # convert outcomes to long format for concatination
    ebola_risks_long = melt(ebola_risks, id.vars = c('HZ'), measure.vars = c(">=2",  ">=6", ">=10", ">=20"), variable.name = 'p_cm', value.name = 'p_cm_val' )
    ebola_risks_adj_long = melt(ebola_risks_adj, id.vars = c('HZ'), measure.vars = c(">=2",  ">=6", ">=10", ">=20"), variable.name = 'p_cm', value.name = 'p_cm_val' )
    
    # pull appropriate values from expert data for concatination
    ebola_risks_long[, expert := 100]
    ebola_risks_long[, expert_date := nominal_forecast_date]
    ebola_risks_long[, horizon_start_date := nominal_forecast_date]
    ebola_risks_long[, horizon_end_date := e_data$horizon_end_date[1]]
    ebola_risks_long[, delay := nominal_forecast_date-nominal_forecast_date]
    ebola_risks_long[, total_horizon := forcast_to_date - nominal_forecast_date]
    ebola_risks_long[, month := m]
    ebola_risks_long[, type := 'model_nfd']
    
    ebola_risks_adj_long[, expert := 200]
    ebola_risks_adj_long[, expert_date := nominal_forecast_date]
    ebola_risks_adj_long[, horizon_start_date := e_data$horizon_start_date[1]]
    ebola_risks_adj_long[, horizon_end_date := e_data$horizon_end_date[1]]
    ebola_risks_adj_long[, delay := nominal_forecast_date-nominal_forecast_date]
    ebola_risks_adj_long[, total_horizon := forcast_to_date - nominal_forecast_date]
    ebola_risks_adj_long[, month := m]
    ebola_risks_adj_long[, type := 'model_nfd']
    
    
    
  
    # merge model and expert data with actual case outcomes for scoring
    #model_data   = left_join(model_data, actual_cases_allthreshs, by=c('HZ', 'p_cm'))
    ebola_risks_adj_long = left_join(ebola_risks_adj_long, actual_cases_allthreshs, by=c('HZ', 'p_cm'))
    ebola_risks_long = left_join(ebola_risks_long, actual_cases_allthreshs, by=c('HZ', 'p_cm'))
    experts_data = merge(experts_data, actual_cases_allthreshs, by=c('HZ', 'p_cm'))
    
    
    
    expert_nonrep_data = copy(ebola_risks_long[type=='model_nfd',])
    expert_nonrep_data[, p_cm_val := NA]
    dim(expert_nonrep_data[type == 'expert',])
    
    
    expert_nonrep_data[is.na(p_cm_val) & reported_cases == 1, ':='(p_cm_val=yes_case_p, type='expert')]
    expert_nonrep_data[is.na(p_cm_val) & reported_cases == 0, ':='(p_cm_val=no_case_p, type='expert')]
    
    all_expert_non_rep_data = data.table()
    
    for (e in sort(unique(experts_data_surveyed[month==m,]$expert))){
      all_expert_non_rep_data = rbind(all_expert_non_rep_data, expert_nonrep_data[, expert:=e])
      }
    
    expert_data_expanded = rbind(experts_data, all_expert_non_rep_data[!experts_data[month==m], on = c('HZ', 'expert'),])
    
    
    #expert_data_expanded = experts_data
    #dim(expert_data_expanded)
    
  
    
    
    
    # add the model and expert data for month to the overall results container
    HZ_surveyed = unique(experts_data_surveyed$HZ)
    expert_model_data_all = rbind(expert_model_data_all, expert_data_expanded[!(HZ %in% HZ_surveyed),])
    #expert_model_data_all = rbind(expert_model_data_all, model_data[!(HZ %in% HZ_surveyed),])
    expert_model_data_all = rbind(expert_model_data_all, ebola_risks_long[!(HZ %in% HZ_surveyed),])
    expert_model_data_all = rbind(expert_model_data_all, ebola_risks_adj_long[!(HZ %in% HZ_surveyed),])
  }
  expert_model_data_all[, score_bri := mapply(FUN=function(x, y){BrierScore(x, y)}, x=p_cm_val, y=reported_cases)]
  expert_model_data_all[, score_log := mapply(FUN=function(x, y){log_score(x, y)}, x=p_cm_val, y=reported_cases)]
  
  expert_model_data_all[p_cm == '>=2']
}

expert_model_data_all_upper = get_clean_flair_data()
expert_model_data_all_lower = get_clean_flair_data(no_case_p = 0.05, yes_case_p = 0.0)
expert_model_data_all_upper = get_clean_flair_data(no_case_p = 0.05)
expert_model_data_all_lower = get_clean_flair_data(yes_case_p = 0)


# using the model or expert risk and actual case outcome score each indevidual forecast using briar and log score


write.csv(expert_model_data_all_upper, 'outputs/indevidual_results_with_scores_adj_additional_uu.csv') 
write.csv(expert_model_data_all_lower, 'outputs/indevidual_results_with_scores_adj_additional_ll.csv') 
write.csv(expert_model_data_all_uppermid, 'outputs/indevidual_results_with_scores_adj_additional_um.csv') 
write.csv(expert_model_data_all_lowermid, 'outputs/indevidual_results_with_scores_adj_additional_lm.csv') 

expert_model_data_all[, expert := as.character(expert)]

ggplot(expert_model_data_all[p_cm == '>=2']) + 
  geom_point(aes(x=total_horizon, y=score_bri, color=type, shape=type)) + 
  facet_grid(HZ~month)+
  scale_color_discrete()




experts_data[expert==13]


