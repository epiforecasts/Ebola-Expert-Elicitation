library(data.table)
library(lubridate)
library(ggplot2)
library(DescTools)
library(viridis)
library(ggbump)
library(cowplot)

results_data = fread('outputs/indevidual_results_with_scores.csv')

# Ensemble forecasts


# mean ensemble with just experts
results_data[, mean_expert := mean(p_cm_val), by = c('HZ', 'type', 'p_cm', 'month')]

# number of experts for ensemble weighting
results_data[, num_experts := .N, by = c('HZ', 'type', 'p_cm', 'month')]

#calculate weighting of each expert (and model) such that model counts for 50%
results_data[type != 'model',  weighting := (1./num_experts)/sum(1./num_experts), by = c('HZ', 'p_cm', 'month')]
# check weights sum to 1
results_data[type != 'model',  weight_sense := sum(weighting), by = c('HZ', 'p_cm', 'month')]
# calculate weighted mean
results_data[type != 'model',  mean_exp_mod  := sum(p_cm_val * weighting), by = c('HZ', 'p_cm', 'month')]

# output tables of ensembles 
mean_expert_data = unique(results_data[type == 'expert', c('HZ', 'p_cm', 'type', 'mean_expert', 'reported_cases', 'month')])
mean_exp_mod_data = unique(results_data[type == 'expert', c('HZ', 'p_cm', 'mean_exp_mod', 'reported_cases', 'month')])

# homogenise
mean_exp_mod_data[, type := 'ensemble']
mean_exp_mod_data[, expert:='ensemble_with_model']
mean_exp_mod_data[, risk_value := mean_exp_mod]

mean_expert_data[, type := 'ensemble']
mean_expert_data[, expert:='ensemble_just_experts']
mean_expert_data[, risk_value := mean_expert]



# compile ensembles frame and score
ensembles = rbind(mean_exp_mod_data[, -c('mean_exp_mod')], mean_expert_data[,-c('mean_expert')])

ensembles[, score_bri := mapply(FUN=function(x, y){BrierScore(x, y)}, x=risk_value, y=reported_cases)]
ensembles[, score_log := mapply(FUN=function(x, y){log_score(x, y)}, x=risk_value, y=reported_cases)]

# format plain results in same format for plotting
slim_results = results_data[, c('HZ', 'p_cm', 'type', 'p_cm_val', 'reported_cases', 'month', 'score_bri', 'score_log', 'expert')]
slim_results[, risk_value := p_cm_val]

# combine plain and ensemble results
all_results = rbind(ensembles, slim_results[, -'p_cm_val'] )
# change model at nominal forecast date 'expert' to 'model'
all_results[expert==0, expert:='model']


# calculate combined scores by HZ (over all months) and by month (over all HZ) for each expert, model and ensemble
all_results[, score_bri_hz := BrierScore(risk_value, reported_cases), by=c('HZ', 'p_cm', 'type', 'expert') ]
all_results[, score_bri_mnth := BrierScore(risk_value, reported_cases), by=c('month', 'p_cm', 'type', 'expert') ]

# set strings to factors for plotting orders
all_results[, p_cm := factor(p_cm, levels=c('>=2', '>=6', '>=10', '>=20'))]
all_results[, expert := factor(expert, levels=c("1", "2", "3", "4", "5",  "6", "7", "8", "9",  "10", "11", "12", "13", "14", "15", "ensemble_just_experts", "ensemble_with_model",    "model"))]
all_results[, month := factor(month, levels=c("November_2019","December_2019", "January_2020", "February_2020", "March_2020"))]

# extract tables for each HZ and month wise forecast scoring
scores_by_hz = unique(all_results[,c('HZ', 'p_cm', 'type', 'expert', 'score_bri_hz')])[type != 'model']
scores_by_mnth = unique(all_results[,c('month', 'p_cm', 'type', 'expert', 'score_bri_mnth')])[type != 'model']


# Create plots

sbhz_plot = 
  ggplot() + 
  geom_violin(data = scores_by_hz[type=='expert'], aes(x=1, y=score_bri_hz), color='white', alpha=0.8, fill='turquoise')+ 
  geom_point(data=scores_by_hz[type=='expert'], aes(x = 1, y=score_bri_hz, color=expert), alpha=0.5, position = position_dodge(width = 0.5), size=3) + 
  geom_point(data=scores_by_hz[type=='ensemble'],aes(x = 3, y=score_bri_hz, shape=expert), position = position_dodge(width = 0.5), color='orange', size=3) + 
  geom_point(data=scores_by_hz[type=='model_nfd'],aes(x = 2, y=score_bri_hz), position = position_dodge(width = 1.0), color='red', shape=20, size=3) + 
  facet_grid(HZ~p_cm, as.table = F) + 
  scale_x_discrete(limits=c(1,2,3), labels=c('experts', 'model', 'ensembles'), name = 'Forecast type')+
  ylab('Briar Score')+ 
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))

ggsave('plots/score_by_hz.pdf', sbhz_plot, width=10, height=15)


sbmo_plot = 
  ggplot() + 
  geom_violin(data = scores_by_mnth[type=='expert'], aes(x=1, y=score_bri_mnth), color='white', alpha=0.8, fill='turquoise')+ 
  geom_point(data=scores_by_mnth[type=='expert'], aes(x = 1, y=score_bri_mnth, color=expert), alpha=0.5, position = position_dodge(width = 0.5), size=3) + 
  geom_point(data=scores_by_mnth[type=='ensemble'],aes(x = 3, y=score_bri_mnth, shape=expert), position = position_dodge(width = 0.5), color='orange', size=3) + 
  geom_point(data=scores_by_mnth[type=='model_nfd'],aes(x = 2, y=score_bri_mnth), position = position_dodge(width = 1.0), color='red', shape=20, size=3) + 
  facet_grid(month~p_cm,labeller=labeller( month = month.labs)) + 
  scale_x_discrete(limits=c(1,2,3), labels=c('experts', 'model', 'ensembles'), name = 'Forecast type')+
  ylab('Briar Score')+ 
  theme_minimal()+
  theme(axis.text.x = element_text(angle=90))

ggsave('plots/score_by_mo.pdf', sbmo_plot, width=10, height=10)


all_results[,rankp := frankv(risk_value, order=-1, ties.method = 'first'), by=list(type, expert, p_cm, month)]
all_results[, HZ := str_to_title(HZ)]
November_HZ = all_results[month=='November_2019'& p_cm == '>=2' & expert == 'ensemble_just_experts']$HZ[order(all_results[month=='November_2019' & p_cm == '>=2' & expert == 'ensemble_just_experts']$rankp, decreasing = TRUE)]

levs = c( unique(all_results[!(HZ%in%November_HZ)]$HZ), November_HZ)

all_results[, HZ := factor(
  levels=levs, 
  x=all_results$HZ)]

ggplot(all_results[!(type %in% c('model', 'expert'))][order(reported_cases)]) + 
  geom_tile(aes(y=HZ, x=expert, fill=rankp))+
  geom_point(aes(y=HZ, x=expert, alpha=reported_cases)) +
  scale_fill_viridis(direction=-1) + 
  scale_alpha_continuous(breaks=c(0,1), labels=c(0,1))+
  scale_x_discrete(labels=c('experts', 'exp + mod', 'model'), name="Forecast type")+
  facet_grid(p_cm~month) + 
  theme_minimal()

colors = c('transparent', 'black')

ggplot(all_results[!(type %in% c('model', 'expert'))]) + 
  geom_tile(aes(y=round(rankp), x=expert, fill=HZ))+
  scale_fill_discrete(type = 'pastel')+
  geom_tile(aes(y=round(rankp), x=expert, size=reported_cases), color='black', fill='transparent')+
  scale_size(breaks=c(0,1), range=c(0,2))+
  scale_color_continuous(colors)+
  geom_text(aes(y=round(rankp), x=expert, label=HZ))+
  #geom_point(aes(y=round(rankp), x=expert, alpha=reported_cases)) +
  scale_alpha_continuous(breaks=c(0,1), labels=c(0,1))+
  scale_x_discrete(labels=c('experts', 'exp + mod', 'model'), name="Forecast type")+
  facet_grid(p_cm~month) + 
  theme_minimal()


ggplot(all_results[!(type %in% c('model', 'expert'))][order(reported_cases)]) + 
  geom_tile(aes(y=HZ, x=month, fill=rankp))+
  geom_point(aes(y=HZ, x=month, alpha=reported_cases)) +
  scale_fill_viridis(direction=-1) + 
  scale_alpha_continuous(breaks=c(0,1), labels=c(0,1))+
  #scale_x_discrete(labels=c('experts', 'exp + mod', 'model'), name="Forecast type")+
  facet_grid(p_cm~expert) + 
  theme_minimal()

df = all_results[!(type %in% c('model', 'expert'))]

all_results[expert == 'ensemble_just_experts','e':=1]
all_results[expert == 'ensemble_with_model','e' := 2]
all_results[expert == 'model','e' := 3]


month.labs <- c('November', 'December', 'January', 'February', 'March')
names(month.labs) <- c('November_2019', 'December_2019', 'January_2020', 'February_2020', 'March_2020')


rank_plot = 
  ggplot(all_results[!(type %in% c('model', 'expert'))], aes(x=e, y=rankp, color = HZ)) +
  geom_point(aes(alpha=reported_cases),size = 3) +
  geom_bump(aes(alpha=reported_cases), size = 1, smooth = 8)  +
  scale_alpha_continuous(range = c(0.3,1), breaks=c(0,1), labels=c('No','Yes'), limits=c(0.,1.0), name='Threshold met')+
  scale_color_discrete(name='Health Zone')+
  #geom_text(data = df_short %>% filter(e == min(e)),
  #          aes(x = e - .1, label = HZ), size = 5, hjust = 1) +
  #geom_text(data = df_short %>% filter(e == max(e)),
  #          aes(x = e + .1, label = HZ), size = 5, hjust = 0) +
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(), 
        axis.text.x = element_text(angle=90, size=10), 
        axis.text.y = element_text(size=10), 
        legend.text = element_text()) +
  labs(y = "Rank",
       x = NULL) +
  scale_y_reverse(breaks = scales::breaks_width(-1, offset=-1))+ 
  facet_grid(month ~ p_cm, scales = 'free_y', space='free', shrink = FALSE, labeller=labeller( month = month.labs))+
  scale_x_continuous(breaks=c(1,2,3), labels = c('experts', 'experts + model', 'model')) +
  coord_cartesian(clip = "off")

ggsave('plots/rankplot.pdf', width=8, height=8)


exp_results = scores_by_mnth[type =='expert']
exp_results[, exp_num := as.numeric(expert)]

exp_results[month=='November_2019', month_num := 1]
exp_results[month=='December_2019', month_num := 2]
exp_results[month=='January_2020', month_num := 3]
exp_results[month=='February_2020', month_num := 4]
exp_results[month=='March_2020', month_num := 5]


exp_results[,ranke := frankv(score_bri_mnth, order=-1, ties.method = 'first'), by=list(p_cm, month)]


exp_score_plot = 
  ggplot(exp_results[ month_num>1], aes(x=month_num, y=score_bri_mnth, color=expert)) +
  geom_point(size = 3) +
  geom_bump(size = 1, smooth = 8)  +
  scale_color_discrete(name='Expert')+
  #geom_text(data = df_short %>% filter(e == min(e)),
  #          aes(x = e - .1, label = HZ), size = 5, hjust = 1) +
  #geom_text(data = df_short %>% filter(e == max(e)),
  #          aes(x = e + .1, label = HZ), size = 5, hjust = 0) +
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(), 
        axis.text.x = element_text(angle=90, size=10), 
        axis.text.y = element_text(size=10), 
        legend.text = element_text()) +
  labs(y = "Rank",
       x = NULL) +
  facet_wrap(~p_cm, shrink = FALSE, ncol=4)+
  scale_x_continuous(breaks=c(2,3, 4, 5), labels = c('Dec', 'Jan', 'Feb', 'Mar')) +
  coord_cartesian(clip = "off")

exp_rank_plot = 
  ggplot(exp_results[ month_num>1], aes(x=month_num, y=ranke, color=expert)) +
  geom_point(size = 3) +
  geom_bump(size = 1, smooth = 8)  +
  scale_color_discrete(name='Expert')+
  #geom_text(data = df_short %>% filter(e == min(e)),
  #          aes(x = e - .1, label = HZ), size = 5, hjust = 1) +
  #geom_text(data = df_short %>% filter(e == max(e)),
  #          aes(x = e + .1, label = HZ), size = 5, hjust = 0) +
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(), 
        axis.text.x = element_text(angle=90, size=10), 
        axis.text.y = element_text(size=10), 
        legend.text = element_text()) +
  scale_y_reverse(breaks = scales::breaks_width(-1, offset=-1))+ 
  labs(y = "Rank",
       x = NULL) +
  facet_wrap(~p_cm, shrink = FALSE, ncol=1)+
  scale_x_continuous(breaks=c(2,3, 4, 5), labels = c('Dec', 'Jan', 'Feb', 'Mar')) +
  coord_cartesian(clip = "off")


ggsave('plots/rankplot.pdf', width=8, height=8)
