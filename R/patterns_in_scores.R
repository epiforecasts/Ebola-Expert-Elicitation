library(data.table)
library(lubridate)
library(viridis)
library(hrbrthemes)



expert_results = fread(paste0(experts_path, '/Outputs/results_', month,'.csv'))
results_data = fread('outputs/indevidual_results_with_scores.csv')

ggplot(results_data[p_cm == '>=2']) + 
  geom_point(aes(x=total_horizon, y=score_bri, color=type, shape=type)) + 
  facet_grid(HZ~month)+
  scale_color_discrete()



DRC2_cases = extract_totcase_data(subarea=c("Nord-Kivu", "Ituri", "Tshopo", "Maniema", "Sud-Kivu", "Haut-Uele", "Bas-Uele"))
population_data = data.table(DRC2_cases[c('ADM2_NAME', 'Shape_Area', 'totpop2019')])
population_data[, HZ := str_to_upper(ADM2_NAME)]

results_covar = merge(results_data, population_data[,-'ADM2_NAME'], by='HZ')
results_covar[HZ=='BENI', Shape_Area := 0.0001374846]


results_covar[, population_density := totpop2019 / Shape_Area]

results_covar[, p_cm := factor(p_cm, levels=c('>=2', '>=6', '>=10', '>=20'))]
results_covar[, expert := factor(expert, levels=c("1", "2", "3", "4", "5",  "6", "7", "8", "9",  "10", "11", "12", "13", "14", "15", "ensemble_just_experts", "ensemble_with_model",    "model"))]
results_covar[, month := factor(month, levels=c("November_2019", "December_2019", "January_2020", "February_2020", "March_2020"))]
results_covar[, rc_str := as.character(reported_cases)]

results_covar[, popband := cut(results_covar$population_density, breaks=c(0, 5e5, 1e7, Inf))]
results_covar[, delayband := cut(delay, breaks = c(0,5,10,15,Inf), right=FALSE)]

ggplot(results_covar[type != 'model_nfd']) + 
  geom_histogram(aes(x=score_bri, group=type, fill=type), position='dodge', bins=10) + 
  facet_grid(p_cm~popband, scales = "free")+
  theme_minimal()

ggplot() + 
  geom_histogram(data=results_covar[type != 'model_nfd'], aes(y = score_bri, group=type, fill=type),  position='dodge', bins=10) + 
  facet_grid(p_cm ~ rc_str, scales = "free")+
  theme_minimal()

ggplot() + 
  geom_histogram(data=results_covar[type != 'model_nfd'], aes(y = score_bri, group=type, fill=type),  position='dodge', bins=10) + 
  facet_grid(p_cm ~ delayband, scales = "free")+
  theme_minimal()

results_covar[type != 'model_nfd', rankp := frankv(p_cm_val, order=-1, ties.method = 'min'), by=list(type, expert, p_cm, month)]

results_covar[type != 'model_nfd' & delayband == '[0,5)' & p_cm == '>=2' & type=='expert']


results_covar[type != 'model_nfd' & p_cm=='>=2' & month=='December_2019' & expert == 1 & type=='expert']



?rank
