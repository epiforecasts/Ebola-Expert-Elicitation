library(data.table)
library(lubridate)
library(ggplot2)
library(DescTools)

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
months = c('November_2019', 'December_2019', 'January_2020', 'February_2020', 'March_2020')

# set container for results
all_cases = data.table()



# load data by month and arrange
for (month in months){
  # set dates of forecast horizon and begining of month and find corresponding epi days
  forcast_to_date = lubridate::ceiling_date(lubridate::my(month), 'month') - 1 
  nominal_forecast_date = lubridate::floor_date(lubridate::my(month), 'month')
  forecast_to_day = as.numeric(forcast_to_date - start_date)
  forecast_from_day = as.numeric(nominal_forecast_date - start_date)
  
  # load expert ellicitaion data 
  experts_data = fread(paste0(experts_path, '/Outputs/results_', month, '_cm.csv'))
  
  experts_data_full = fread(paste0(experts_path, '/Outputs/results_', month, '.csv'))
  
  experts_info = unique(experts_data_full[, c('expert', 'institution', 'last.in.field', 'experience.ide')])
  
  experts_data[experts_data$HZ == 'NYANKUNDE','HZ'] = 'NYAKUNDE'
  
  #set appropriate column values 
  experts_data[,expert_date := lubridate::dmy(date)]  
  experts_data[,horizon_end_date := forcast_to_date]
  experts_data[,horizon_start_date := nominal_forecast_date]
  experts_data[,delay:=horizon_start_date - expert_date]
  experts_data[,total_horizon := horizon_end_date - expert_date]
  experts_data[,month := month]
  experts_data[,type := 'expert']
  
  # drop unwanted columns
  experts_data = experts_data[, -c('date', 'expert.date', 'V1')]
  
  cases_in_dec= 1. * (rowSums(timeseries_mat[,forecast_from_day :forecast_to_day]))
  
  # --- set up actual case data for scoring (1 or 0 for each threshold)
  # set container for actual case data
  
  DRC2_cases_df['number_of_cases'] = cases_in_dec
  
  actual_cases = data.table(DRC2_cases_df[,c("ADM2_NAME", "number_of_cases")] %>% filter(ADM2_NAME %in% unlist(unique(experts_data[,'HZ']))))
  actual_cases[, m:=month]
  all_cases = rbind(all_cases, actual_cases)
  
}
  
  