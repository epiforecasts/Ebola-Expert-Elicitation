library(data.table)
library(lubridate)
library(viridis)
library(ggbeeswarm)
library(wesanderson)


results_data = fread('outputs/indevidual_results_with_scores_adj.csv')


results_data[, predicted_hazard_rate := sum(p_cm_val), by = c('p_cm', 'month', 'expert', 'type')]
results_data[, actual_hazard_rate := sum(reported_cases), by = c('p_cm', 'month', 'expert', 'type')]


results_data[, predicted_hazard_rate_all := sum(p_cm_val), by = c('p_cm' , 'expert', 'type')]
results_data[, actual_hazard_rate_all := sum(reported_cases), by = c('p_cm', 'expert', 'type')]

results_data[, hazard_rate_gap := predicted_hazard_rate - actual_hazard_rate]
results_data[, hazard_rate_gap_all := predicted_hazard_rate_all - actual_hazard_rate_all]

results_data[, expert:=as.character(expert)]
results_data[expert=='100', expert:='gravity']
results_data[expert=='200', expert:='adjacency']
results_data[, p_cm := factor(p_cm, levels=c('>=2', '>=6', '>=10', '>=20'))]
results_data[, expert := factor(expert, levels=c("1", "2", "3", "4", "5",  "6", "7", "8", "9",  "10", "11", "12", "13", "14", "15",   "gravity", "adjacency", "ensemble_just_experts", "ensemble_just_models",  "ensemble_with_model"))]

results_data[, month := factor(month, levels=c("November_2019", "December_2019", "January_2020", "February_2020", "March_2020"))]

month.labs <- c('November', 'December', 'January', 'February', 'March')
names(month.labs) <- c('November_2019', 'December_2019', 'January_2020', 'February_2020', 'March_2020')



ggplot(results_data[!(type %in% c('model' , 'model_adj'))]) +
  geom_hline(yintercept = 0)+
  geom_segment( aes(x=expert, xend=expert, y=0, yend=hazard_rate_gap)) +
  geom_point(data= results_data[type=='expert'], aes(x=expert, y=hazard_rate_gap, fill=expert, color=expert), size=4, alpha=0.7, shape=21, stroke=0.5) + 
  geom_point(data= results_data[type=='model_nfd'], aes(x=expert, y=hazard_rate_gap, fill=expert, shape=expert), fill='orange', color='black', size=4, alpha=0.7, stroke=0.5) + 
  facet_grid(p_cm ~ month, scales = 'free_x', labeller=labeller( month = month.labs)) + 
  #ylim(c(-6,6))+ 
  ylab('Hazard gap')+ 
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, size=10), 
        axis.text.y = element_text(size=10), 
        legend.text = element_text())




hr_month = 
  ggplot(results_data[!(type %in% c('model' , 'model_adj'))]) +
  geom_hline(yintercept = 0)+
  geom_segment( aes(x=p_cm, xend=p_cm, y=0, yend=hazard_rate_gap)) +
  geom_point(data= results_data[type=='expert'], aes(x=p_cm, y=hazard_rate_gap, fill=expert, color=expert), size=4, alpha=0.7, shape=21, stroke=0.5) + 
  geom_point(data= results_data[type=='model_nfd'], aes(x=p_cm, y=hazard_rate_gap, fill=expert, shape=expert), fill='orange', color='black', size=4, alpha=0.7, stroke=0.5) + 
  facet_grid(month ~ expert, scales = 'free_x', labeller=labeller( month = month.labs)) + 
  #ylim(c(-5,5))+ 
  ylab('Hazard gap')+ 
  xlab('')+
  theme_minimal()+
  scale_shape_discrete(name='Model')+
  scale_color_discrete(name='Expert')+
  scale_fill_discrete(name='Expert')+
  theme(axis.text.x = element_text(angle=90, size=10), 
        axis.text.y = element_text(size=10), 
        legend.text = element_text())+
ggtitle('Monthly bias')


hr_all = 
  ggplot(results_data[!(type %in% c('model' , 'model_adj'))]) +
  geom_hline(yintercept = 0)+
  geom_segment( aes(x=p_cm, xend=p_cm, y=0, yend=hazard_rate_gap_all)) +
  geom_point(data= results_data[type=='expert'], aes(x=p_cm, y=hazard_rate_gap_all, fill=expert, color=expert), size=4, alpha=0.7, shape=22, stroke=0.5) + 
  geom_point(data= results_data[type=='model_nfd'], aes(x=p_cm, y=hazard_rate_gap_all, fill=expert, shape=expert), fill='orange', color='black', size=4, alpha=0.7, stroke=0.5) + 
  facet_wrap(~expert, scales = 'free_x', nrow=1) + 
  #ylim(c(-5,5))+ 
  ylab('Hazard gap')+ 
  xlab('Case threshold')+
  ggtitle('Overall bias')+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90, size=10), 
        axis.text.y = element_text(size=10), 
        legend.text = element_text(), 
        legend.position = 'none')

hr_full = (hr_month/hr_all) + plot_layout(heights = c(5, 1))


ggsave('plots/bias.pdf',hr_full, width=15, height=8)
