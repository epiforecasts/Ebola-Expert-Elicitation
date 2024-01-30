library(data.table)
library(lubridate)
library(ggplot2)
library(DescTools)
library(viridis)

results_data = fread('outputs/indevidual_results_with_scores_adj_additional.csv')
results_data_surveyed = fread('outputs/indevidual_results_with_scores_adj.csv')

# Get the case data for whole outbreak 
DRC2_cases = extract_totcase_data(subarea=c("Nord-Kivu", "Ituri", "Tshopo", "Maniema", "Sud-Kivu", "Haut-Uele", "Bas-Uele"))
DRC2_cases = DRC2_cases[order(DRC2_cases[['ADM2_NAME']]),]

timeseries = construct_time_series_mat(DRC2_cases)
timeseries_mat = timeseries$diff_cases

DRC2_cases['cases_to_date'] = rowSums(timeseries_mat)

DRC2_cases_simp =  st_simplify(DRC2_cases, preserveTopology = FALSE, dTolerance = 0.025)



case_table = timeseries_mat
names = head(timeseries$dates, dim(timeseries_mat)[2])
colnames(case_table) = as.character(names)

all_hz_cases = colSums(case_table)
all_hz_cases_dt = cbind(data.table(ADM2_NAME ='All',  totpop2019 = 0), t(data.table(all_hz_cases)))



DRC2_cases_final_ep = cbind(data.table(DRC2_cases[,c('ADM2_NAME', "totpop2019")]), case_table)[,-c('geometry')]
colnames(all_hz_cases_dt) = colnames(DRC2_cases_final_ep)
DRC2_cases_final_ep = rbind(DRC2_cases_final_ep, all_hz_cases_dt)

DRC2_cases_final_ep_long =melt(DRC2_cases_final_ep, measure.vars = as.character(names),
                               variable.name = "date", value.name = "cases")
DRC2_cases_final_ep_long[, date := lubridate::ymd(date)]

selection_cut_offs = data.table(
  month = c('nov', 'dec', 'jan', 'feb', 'march'), 
  start = lubridate::ymd(c("2019-10-05", "2019-10-26", "2019-12-01", "2019-12-29", "2020-01-28")), 
  end   = lubridate::ymd(c("2019-10-19", "2019-11-09", "2019-12-15", "2020-01-12", "2020-02-11"))
)

for (m in selection_cut_offs$month){
  print(month)
  DRC2_cases_final_ep_long[date > selection_cut_offs[month == m, ]$start & date <= selection_cut_offs[month == m, ]$end, selection_period := m]
  
}

DRC2_cases_final_ep_long[!is.na(selection_period), total_cases := sum(cases), by = c('selection_period', 'ADM2_NAME')]

case_summary_selection = unique(DRC2_cases_final_ep_long[!is.na(selection_period), -c('cases')])

for (m in unique(selection_cut_offs$month)){
  print(c(m, unique(case_summary_selection[total_cases>0 & selection_period==m & (ADM2_NAME != 'All'),]$ADM2_NAME)))
}

DRC2_cases_final_ep_long[, month := lubridate::month(date, label=TRUE)]
DRC2_cases_final_ep_long[, month := factor(month, levels=c('Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar'))]




HZs_included_rule = list()
HZs_included_actual = list()
months = c('November_2019', 'December_2019', 'January_2020', 'February_2020', 'March_2020')
for (mo in months){ 
  
  #month_before = lubridate::floor_date(lubridate::my(mo)-1, 'month')-30
  
  DRC2_cases_final_ep_long[,total_cases:=0]
  
  #first_interview = min(results_data_surveyed[month == mo,]$expert_date)
  first_interview = lubridate::ymd(20191109)
  cases_from = lubridate::ymd(20191026)
  #cases_from = first_interview -14
  #cases_from = month_before
  print(c(cases_from, first_interview))
  
  DRC2_cases_final_ep_long[date>cases_from & date<=first_interview, total_cases := sum(cases), by=c('ADM2_NAME')]
  HZs_included_rule[[mo]] = unique(DRC2_cases_final_ep_long[total_cases > 0,]$ADM2_NAME)
  HZs_included_actual[[mo]] = unique(results_data_surveyed[month == mo,]$HZ)
}

results_data[, surveyed := 0]
results_data_surveyed[, surveyed := 1]

results_full = rbind(results_data, results_data_surveyed)

for (mo in months){ 
  
  month_before_beginning = lubridate::floor_date(lubridate::my(mo)-1, 'month')
  month_before_end= lubridate::floor_date(lubridate::my(mo), 'month')-1
  DRC2_cases_final_ep_long[,total_cases:=0]
  
  DRC2_cases_final_ep_long[date>=month_before_beginning & date<month_before_end, total_cases := sum(cases), by=c('ADM2_NAME')]
  HZs_with_cases = unique(DRC2_cases_final_ep_long[total_cases > 1,]$ADM2_NAME)
  
  results_full[month == mo & HZ %in% str_to_upper(HZs_with_cases), cases_month_before:= 1]
  results_full[month == mo & !(HZ %in% str_to_upper(HZs_with_cases)), cases_month_before:= 0]
}

introductions = unique(results_full[p_cm == '>=2', c('month', 'HZ', 'reported_cases', 'surveyed', 'cases_month_before')])
introductions[, ADM2_NAME := str_to_title(HZ)]
introductions[, month := factor(month, levels=c("November_2019","December_2019", "January_2020", "February_2020", "March_2020"))]

DRC2_cases_simp =  st_simplify(DRC2_cases, preserveTopology = FALSE, dTolerance = 0.025)

DRC2_intros_monthly_sf = st_sf(merge(introductions, DRC2_cases_simp[,c('ADM2_NAME', 'geometry')],by=c('ADM2_NAME')))


case_maps = ggplot()+
  geom_sf(data=DRC2_cases_simp, fill='white', color='light grey')+
  geom_sf(data=DRC2_intros_monthly_sf[DRC2_intros_monthly_sf$reported_cases==1 & DRC2_intros_monthly_sf$cases_month_before==1,], fill='black')+
  geom_sf(data=DRC2_intros_monthly_sf[DRC2_intros_monthly_sf$reported_cases==1 & DRC2_intros_monthly_sf$cases_month_before==0,], fill='red')+
  geom_sf(data=DRC2_intros_monthly_sf[DRC2_intros_monthly_sf$reported_cases==0 & DRC2_intros_monthly_sf$cases_month_before==1,], fill='green')+
  facet_wrap(~month, nrow=1)+
  xlim(c(27.5, 31))+
  ylim(c(-2, 3))+
  scale_fill_binned(name='Cases', type='viridis', option='magma' )+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title  = element_text(size=14),
        legend.position="right") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5))+
  ggtitle('C')


unique(results_full[reported_cases==1 & cases_month_before==0]$HZ)
