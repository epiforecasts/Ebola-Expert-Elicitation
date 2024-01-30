library(ggplot2) 
library(data.table)
library(ggdist)
library(patchwork)
library(ggnewscale)

results_data = fread('outputs/indevidual_results_with_scores_adj.csv')

months = c('December_2019', 'January_2020', 'February_2020', 'March_2020')
experts_data_surveyed = data.table()
for(m in months){
  experts_data_surveyed = rbind(experts_data_surveyed, fread(paste0(experts_path, '/Outputs/results_', m, '_cm.csv'))[,month:=m])
}

experts_data_surveyed[experts_data_surveyed$HZ == 'MAKISO_KISANGANI','HZ'] = 'MAKISO-KISANGANI'
experts_data_surveyed[experts_data_surveyed$HZ == 'NYANKUNDE','HZ'] = 'NYAKUNDE'


surveyedHZ = stringr::str_to_title(unique(experts_data_surveyed$HZ))


results_data = data.table(results_data)
results_data[,`:=`(reported_cases = as.character(reported_cases), 
                   p_cm = factor(p_cm, levels = c('>=2', '>=6', '>=10', '>=20')),
                   month = factor(gsub('_', ' ', month), levels = c('November 2019','December 2019', 'January 2020', 'February 2020', 'March 2020')),
                   HZ = stringr::str_to_title(HZ), 
                   expert = as.character(expert)
                   )]

expert_dists = 
  ggplot() + 
  geom_boxplot(data=results_data[month != 'November 2019' &type=='expert' & (HZ %in% surveyedHZ), ], 
               aes(x=p_cm, y=p_cm_val, color=reported_cases), stroke=1)+
  stat_dots(data = results_data[month != 'November 2019' &type=='expert' & (HZ %in% surveyedHZ), ], 
            aes(x=p_cm, y=p_cm_val, side='both'), color='darkgray', size=1, shape=10, alpha=0.7)+
  scale_color_discrete(name='Experts: Threshold met?', labels=c('No', 'Yes'))+ 
  
  #new_scale_color()+
  geom_point(data= results_data[month != 'November 2019' &type=='model_nfd' & (HZ %in% surveyedHZ),],
             aes(x=p_cm, y=p_cm_val, shape=expert), color='grey', size=2, stroke=1, alpha=0.7)+
  
  scale_shape_manual(name='Model', labels=c('Gravity', 'Adjacency'), values = c(0, 5))+ 
  
  facet_grid(month~HZ)+
  
  theme_minimal()+
  ylab('Probability')+
  xlab('')+
  scale_y_continuous(breaks=c(0,0.5,1))+
  guides(color=guide_legend(nrow=1,byrow=TRUE))+
  theme(
    axis.text.x = element_text(angle=90), 
    legend.position = 'top', 
    text = element_text(size=9)
  )

ggsave('plots/expert_dists.pdf', width=12, height=6)
ggsave('plots/expert_dists.png', width=20, height=12, units='cm')  


(expert_dists + ggtitle('A')) / (exp_rank_plot + ggtitle('B'))  + plot_layout(heights=c(8, 2))

ggsave('plots/expert_dists_ranks.png', width=20, height=20, units='cm')  

restab = results_data[month != 'November 2019' &type=='expert' & (HZ %in% surveyedHZ), c('HZ', 'month', 'expert', 'p_cm', 'p_cm_val')] |> 
  dcast('HZ     +   month  + expert ~ p_cm', value.var = 'p_cm_val')

write.csv(restab, 'outputs/restab.csv')

results_data[month != 'November 2019' & type=='expert' & (HZ %in% surveyedHZ), ]|> 
  ggplot() + 
  geom_violin(aes(x=expert, y=score_bri, color=expert, group=expert))+
  stat_dots(aes(x=expert, y=score_bri, side='both'), color='gray', size=1, shape=10, alpha=0.4)+
  facet_wrap(~p_cm, ncol=1)
  

