library(data.table)
library(lubridate)
library(sf)
library(patchwork)
library(ggplot2)
library(ggnewscale)
library(viridis)

source('r/data_prep.r')
results_data = fread('outputs/indevidual_results_with_scores.csv')
#sf::sf_use_s2(FALSE)


library(rgeoboundaries)
DRC_boundary <- geoboundaries(c("Democratic Republic of the Congo", "Uganda", "Rwanda", "Central African Republic", "South Sudan", "Burundi", "United Republic of Tanzania"))

saveRDS(DRC_boundary, file = 'data/shapes/DRC_boundary.rds')

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
  geom_sf(data=DRC2_cases_simp, aes(fill=ADM1_NAME), color='lightgray', size=0.1) + 
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
        plot.title  = element_text(size=14))+
  ggtitle('A')



p_cases = ggplot() + 
  geom_sf(data = DRC_boundary_simp, aes(fill=shapeName), size=0, alpha=0.8) +
  scale_fill_brewer(name='Country', palette="Greys", guide='none')+
  new_scale("fill") +
  geom_sf(data = DRC2_cases[DRC2_cases_simp$cases_to_date == 0,], fill='white', color='grey', alpha=0.5, size=0.1) +
  geom_sf(data = DRC2_cases[DRC2_cases_simp$cases_to_date > 0,], aes(fill=cases_to_date), color='grey', size=0.1)+
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
        plot.title  = element_text(size=14))


HZs = str_to_title(unique(results_data$HZ))

p_quest = ggplot()+ 
  geom_sf(data = DRC_boundary_simp, aes(fill=shapeName), size=0, alpha=0.8) +
  scale_fill_brewer(name='Country', palette="Greys", guide='none')+
  new_scale("fill") +
  geom_sf(data = DRC2_cases_simp[!(DRC2_cases_simp$ADM2_NAME %in% HZs),], fill='white', color='light gray', size=0.1)+ 
  geom_sf(data = DRC2_cases_simp[DRC2_cases_simp$ADM2_NAME %in% HZs,], aes(fill=ADM2_NAME, color=ADM2_NAME))+
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
        plot.title  = element_text(size=14))+
  ggtitle('B')

info_maps = p_cases+ p_quest+ p_provinces   

ggsave('plots/info_maps_figure_countries.pdf', info_maps, width = 17, height=7)


results_data[,c('month', 'for')]

aggregate(DRC2_cases, by='ADM1_Name')



start_date_period = lubridate::floor_date(lubridate::my('October_2019'), 'month')
start_date_data = timeseries$dates[1]

start_day_period = as.numeric(start_date_period - start_date_data)

dim(timeseries_mat)

dim(tail(t(timeseries_mat), -(start_day_period-1)))

t(tail(t(timeseries_mat), -(start_day_period-1)))

case_table = data.table(t(tail(t(timeseries_mat), -(start_day_period-1))))
names = seq(start_date_period,start_date_period + dim(case_table)[2]-1, by='days')
colnames(case_table) = as.character(names)

all_hz_cases = colSums(case_table)
all_hz_cases_dt = cbind(data.table(ADM2_NAME ='All',  totpop2019 = 0), t(data.table(all_hz_cases)))



DRC2_cases_final_ep = cbind(data.table(DRC2_cases[,c('ADM2_NAME', "totpop2019")]), case_table)[,-c('geometry')]
colnames(all_hz_cases_dt) = colnames(DRC2_cases_final_ep)
DRC2_cases_final_ep = rbind(DRC2_cases_final_ep, all_hz_cases_dt)

DRC2_cases_final_ep_long =melt(DRC2_cases_final_ep, measure.vars = as.character(names),
             variable.name = "date", value.name = "cases")

DRC2_cases_final_ep_long[, date := lubridate::ymd(date)]
DRC2_cases_final_ep_long[, month := lubridate::month(date, label=TRUE)]
DRC2_cases_final_ep_long[, month := factor(month, levels=c('Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar'))]

case_ts = ggplot(DRC2_cases_final_ep_long[ADM2_NAME %in% c(HZs) & cases>0,]) + 
  geom_point(aes(x=date, y=cases, color=month))+ 
  geom_point(data=DRC2_cases_final_ep_long[ADM2_NAME == 'All' & cases>0,], aes(x=date, y=cases, color=month), shape=21)+ 
  facet_wrap(~ADM2_NAME, ncol=3, strip.position = 'top')+
  scale_color_manual(values=c("#000000", "#0a3a8c", "#4192bf", "#009E73", "#D55E00"), name='')+
  theme_minimal()+
  xlab('')+
  theme(
        legend.position="right",
        legend.box="horizontal",
        plot.title  = element_text(size=14),)+
  ggtitle('B')



DRC2_cases_final_ep_long[, monthlycases := sum(cases), by=c('ADM2_NAME', 'month')]
DRC2_cases_monthly = unique(DRC2_cases_final_ep_long[,c('ADM2_NAME', 'month', 'monthlycases')])




DRC2_cases_monthly_sf = st_sf(merge(DRC2_cases_monthly, DRC2_cases_simp[,c('ADM2_NAME', 'geometry')], by=c('ADM2_NAME')))


case_maps = ggplot()+
  geom_sf(data = DRC_boundary_simp, aes(fill=shapeName), size=0, alpha=0.8) +
  scale_fill_brewer(name='Country', palette="Greys", guide='none')+
  new_scale("fill") +
  geom_sf(data = DRC2_cases_simp, fill='white', color='light gray', size=0.01)+
  geom_sf(data=DRC2_cases_monthly_sf[(DRC2_cases_monthly_sf$monthlycases>0),], aes(fill=monthlycases), size=0)+
  geom_sf(data=DRC2_cases_simp[DRC2_cases_simp$cases_to_date > 0,], color='red', fill='transparent', size=0.3)+
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
 

case_maps + case_ts + plot_layout(widths = c(2, 3))
  
  
names_full = seq(start_date_data, lubridate::ymd(20200331), by='days')
case_table_full = data.table(timeseries_mat)
colnames(case_table_full) = as.character(head(timeseries$dates, dim(timeseries_mat)[2]))


all_cases = data.table(
  cases = colSums(case_table_full), 
  dates = head(timeseries$dates, dim(timeseries_mat)[2]))

all_cases[, in_period := dates >= start_date_period]
all_cases[, month := lubridate::month(dates, label=TRUE)]
all_cases[, month := factor(month, levels=c('Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar'))]

all_case_plot = ggplot() + 
  geom_point(data=all_cases[in_period==FALSE & cases>0], aes(x=dates, y=cases), color='gray') + 
  geom_point(data=all_cases[in_period==TRUE & cases>0], aes(x=dates, y=cases, color=month))+
  scale_color_manual(values=c("#000000", "#0a3a8c", "#4192bf", "#009E73", "#D55E00"))+
  ylim(c(0,30))+
  xlab('')+
  theme_minimal()+ 
  theme(
    legend.position = 'none', 
    plot.title  = element_text(size=14),
  )+ 
  ggtitle('A')

case_history_plot  = all_case_plot / (case_ts/case_maps + plot_layout(heights = c(3,2))) + plot_layout(heights=c(1,8))

fig_1 = (all_case_plot) / (case_ts/(p_cases + case_maps + plot_layout(widths = c(1,5))) + plot_layout(heights = c(3,2))) + plot_layout(heights=c(1,8))

fig_1a = (all_case_plot + p_cases + plot_layout(widths =c(6,2))) / case_maps + plot_layout(heights = c(3,3))


ggsave(fig_1a, 'plots/fig1.pdf', fig_1a, width = 12, height=10)


