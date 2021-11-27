library(data.table)
library(lubridate)
library(sf)
library(patchwork)
library(ggplot2)
library(ggnewscale)
library(viridis)


results_data = fread('outputs/indevidual_results_with_scores.csv')
sf::sf_use_s2(FALSE)


library(rgeoboundaries)
DRC_boundary <- geoboundaries(c("Democratic Republic of the Congo", "Uganda", "Rwanda", "Central African Republic", "South Sudan", "Burundi", "United Republic of Tanzania"))


DRC_boundary_simp = st_simplify(DRC_boundary, preserveTopology = FALSE, dTolerance = 0.025)

ggplot(data = DRC_boundary_simp) +
  geom_sf()

# Get the case data for whole outbreak 
DRC2_cases = extract_totcase_data(subarea=c("Nord-Kivu", "Ituri", "Tshopo", "Maniema", "Sud-Kivu", "Haut-Uele", "Bas-Uele"))
DRC2_cases = DRC2_cases[order(DRC2_cases[['ADM2_NAME']]),]

timeseries = construct_time_series_mat(DRC2_cases)
timeseries_mat = timeseries$diff_cases

DRC2_cases['cases_to_date'] = rowSums(timeseries_mat)

DRC2_cases_simp =  st_simplify(DRC2_cases, preserveTopology = FALSE, dTolerance = 0.025)

 
p_provinces = ggplot() + 
  geom_sf(data = DRC_boundary_simp, aes(fill=shapeName), size=0, alpha=0.8) +
  scale_fill_brewer(name='Country', palette="Greys")+
  new_scale("fill") +
  geom_sf(data=DRC2_cases_simp, aes(fill=ADM1_NAME), color='lightgray', size=0.01) + 
  scale_fill_discrete(name='Province') +
  geom_rect(aes(xmin = 27.5, xmax = 31, ymin = -2, ymax = 3), fill='transparent', color='red', size=1) +
  xlim(c(23, 32))+
  ylim(c(-5, 5))+
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title  = element_text(size=20))+
  ggtitle('A')



p_cases = ggplot() + 
  geom_sf(data = DRC_boundary_simp, aes(fill=shapeName), size=0, alpha=0.8) +
  scale_fill_brewer(name='Country', palette="Greys", guide='none')+
  new_scale("fill") +
  geom_sf(data = DRC2_cases[DRC2_cases_simp$cases_to_date == 0,], fill='white', color='grey', alpha=0.5, size=0.01) +
  geom_sf(data = DRC2_cases[DRC2_cases_simp$cases_to_date > 0,], aes(fill=cases_to_date), color='grey', size=0.01)+
  xlim(c(27.5, 31))+
  ylim(c(-2, 3))+
  scale_fill_viridis(name='Total case count') + 
  theme_minimal()+
  ggtitle('B')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title  = element_text(size=20))


HZs = str_to_title(unique(results_data$HZ))

p_quest = ggplot()+ 
  geom_sf(data = DRC_boundary_simp, aes(fill=shapeName), size=0, alpha=0.8) +
  scale_fill_brewer(name='Country', palette="Greys", guide='none')+
  new_scale("fill") +
  geom_sf(data = DRC2_cases[!(DRC2_cases_simp$ADM2_NAME %in% HZs),], fill='white', color='light gray', size=0.01)+ 
  geom_sf(data = DRC2_cases[DRC2_cases_simp$ADM2_NAME %in% HZs,], aes(fill=ADM2_NAME, color=ADM2_NAME))+
  xlim(c(27.5, 31))+
  ylim(c(-2, 3))+
  scale_fill_discrete(name='Health Zone')+
  scale_colour_discrete(name='Health Zone')+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title  = element_text(size=20))+
  ggtitle('C')

info_maps = p_provinces + p_cases + p_quest

ggsave('plots/info_maps_figure_countries.pdf', info_maps, width = 15, height=7)


results_data[,c('month', 'for')]

aggregate(DRC2_cases, by='ADM1_Name')
