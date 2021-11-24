library(data.table)
library(lubridate)
library(sf)
library(patchwork)

results_data = fread('outputs/indevidual_results_with_scores.csv')


# Get the case data for whole outbreak 
DRC2_cases = extract_totcase_data(subarea=c("Nord-Kivu", "Ituri", "Tshopo", "Maniema", "Sud-Kivu", "Haut-Uele", "Bas-Uele"))
DRC2_cases = DRC2_cases[order(DRC2_cases[['ADM2_NAME']]),]

timeseries = construct_time_series_mat(DRC2_cases)
timeseries_mat = timeseries$diff_cases

DRC2_cases['cases_to_date'] = rowSums(timeseries_mat)
 
p_provinces = ggplot(DRC2_cases) + 
  geom_sf(aes(fill=PROVINCE))


p_cases = ggplot() + 
  geom_sf(data = DRC2_cases[DRC2_cases$cases_to_date == 0,], fill='white', color='grey', alpha=0.5) +
  geom_sf(data = DRC2_cases[DRC2_cases$cases_to_date > 0,], aes(fill=cases_to_date), color='grey')+
  scale_fill_viridis(name='Total cases') + 
  theme_minimal()


HZs = str_to_title(unique(results_data$HZ))

p_quest = ggplot()+ 
  geom_sf(data = DRC2_cases[!(DRC2_cases$ADM2_NAME %in% HZs),], fill='white', color='light gray')+ 
  geom_sf(data = DRC2_cases[DRC2_cases$ADM2_NAME %in% HZs,], aes(fill=ADM2_NAME, color=ADM2_NAME))+
  xlim(c(27.5, 31))+
  ylim(c(-2, 3))+
  theme_minimal()

p_provinces + p_cases + p_quest


results_data[,c('month', 'for')]

