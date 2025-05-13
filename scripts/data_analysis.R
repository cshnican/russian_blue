library(tidyverse)
library(ggplot2)
library(brms)
library(gridExtra)
library(grid)
library(bayestestR)
library(zoo)
library(tidybayes)
library(ggrepel)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

task1_eng <- read.csv('../data/english_data.csv') %>% 
  select(-is_monolingual, -trial_index) %>%
  mutate(language='English')
task1_rus <- read.csv('../data/russian_data.csv') %>% 
  select(-is_monolingual, -trial_index) %>%
  mutate(language='Russian')

task2_eng <- read.csv('../data/english_data_forced_choice.csv') %>% 
  select(-is_monolingual, -trial_index) %>%
  mutate(language='English')
  
task2_rus <- read.csv('../data/russian_data_forced_choice.csv') %>% 
  select(-is_monolingual, -trial_index) %>%
  mutate(language='Russian')

naming_eng <- read.csv('../data/english_edge_naming.csv') %>%
  select(-is_monolingual, -trial_index) %>%
  mutate(language='English')

naming_rus <- read.csv('../data/russian_edge_naming.csv') %>%
  select(-is_monolingual, -trial_index) %>%
  mutate(language='Russian')


task1 <- rbind(task1_eng, task1_rus) %>% mutate(subject_id_colour_target = paste(subject_id, colour, target, sep = '_'),
                                                subject_id_colour_distractor = paste(subject_id, colour, distractor, sep = '_'))

naming_data <- rbind(naming_eng, naming_rus)

task2_summary_eng <- task2_eng %>% 
  group_by(subject_id, colour, num, language) %>% 
  summarise(p_dark = mean(is_dark)) %>% ungroup() %>%
  group_by(subject_id, colour, language) %>% 
  mutate(p_dark_roll = rollapply(p_dark, width = 3, FUN = function(x) mean(x, na.rm = TRUE),
                                by = 1, by.column = TRUE, 
                                partial = TRUE, fill = NA, align = 'center')) %>% ungroup()

task2_summary_rus <- task2_rus %>% 
  group_by(subject_id, colour, num, language) %>% 
  summarise(p_dark = mean(is_dark)) %>% ungroup() %>%
  group_by(subject_id, colour, language) %>% 
  mutate(p_dark_roll = rollapply(p_dark, width = 3, FUN = function(x) mean(x, na.rm = TRUE),
                                by = 1, by.column = TRUE, 
                                partial = TRUE, fill = NA, align = 'center')) %>% ungroup()

task2_summary <- rbind(task2_summary_eng, task2_summary_rus)


task2_target <- task2_summary %>% mutate(subject_id_colour_target = paste(subject_id, colour, num, sep = '_'),
                                         p_dark_target = p_dark_roll) %>% select(subject_id_colour_target, p_dark_target)
task2_distractor <- task2_summary %>% mutate(subject_id_colour_distractor = paste(subject_id, colour, num, sep = '_'),
                                             p_dark_distractor = p_dark_roll) %>% select(subject_id_colour_distractor, p_dark_distractor)

task2_boundary <- task2_summary %>% mutate(subject_id_colour = paste(subject_id, colour, sep = '_')) %>% 
  group_by(subject_id, colour, language) %>% filter(p_dark_roll >= 0.50) %>%
  slice_min(order_by = num, n = 1) %>% mutate(bdy = num, p_dark_bound = p_dark_roll) %>% select(-num, -p_dark, -p_dark_roll) %>% ungroup()


task1_plot <- task1 %>% left_join(task2_distractor, by = 'subject_id_colour_distractor') %>%
  left_join(task2_target, by = 'subject_id_colour_target') %>% 
  left_join(task2_boundary, by = c('subject_id', 'colour', 'language')) %>% 
  select(-subject_id_colour_target, -subject_id_colour_distractor) %>% rowwise() %>% 
  mutate(category = case_when(
    p_dark_distractor <= 0.50 & p_dark_target <= 0.50 ~ 'within light',
    p_dark_distractor >= 0.50 & p_dark_target >= 0.50 ~ 'within dark',
    TRUE ~ 'between'
  ),
  target = as.numeric(target),
  distractor = as.numeric(distractor),
  category_bin = case_when(
    category %in% c("within dark", "within light") ~ "within",
    category == 'between' ~ 'between'
  ),
  dist_to_boundary = abs((target + distractor)/2 - bdy)) %>% rowwise() %>% 
  mutate(item = paste0(min(target, distractor), '_', max(target, distractor))) %>% ungroup()

# here we use Winawer et al.(2007)'s exclusion criteria
task1_plot_good <- task1_plot %>% filter( # we only analyze blues here
  rt <= 3000, #  and trials taking less than 3s
  accuracy == 1, # exclude incorrect trials
) %>% 
  group_by(subject_id_colour) %>% mutate(ntrials_included = n()) %>% ungroup() %>%
  mutate(percentage_trials_included = ntrials_included / 72) %>% # calculate the percentage of trials included
  filter(dist_to_boundary <= 9,  # only analyze 9 color pairs closest to the boundary
         percentage_trials_included >= 0.75) # only include participants with >75% of the trials included

# number of participants included:
task1_plot_good %>% group_by(language) %>% summarize(n = unique(subject_id) %>% length())

included_participants <- task1_plot_good %>% pull(subject_id) %>% unique()

# filter_naming_data
naming_data_good <- naming_data %>% filter(subject_id %in% included_participants) %>%
  rowwise() %>%
  mutate(resp_fixed = tolower(resp_fixed),
         resp_fixed = str_trim(resp_fixed),
         resp_fixed = ifelse(resp_fixed == 'желтый', 'жёлтый', resp_fixed),
         num = case_when(num == 1 ~ 'light', 
                         num == 20 ~ 'dark',
                         TRUE ~ NA),
         color_code = case_when(
           colour == 'blues' & num == 'light' ~ '#ADD8E6',
           colour == 'blues' & num == 'dark' ~ '#00008B',
           colour == 'browns' & num == 'light' ~ '#c4a484',
           colour == 'browns' & num == 'dark' ~ '#5C4033',
           colour == 'reds' & num == 'light' ~ '#FFC0CB',
           colour == 'reds' & num == 'dark' ~ '#FF0000',
           colour == 'yellows' & num == 'light' ~ '#FFFF00',
           colour == 'yellows' & num == 'dark' ~ '#FFA500',
         ))

naming_data_summary <- naming_data_good %>%
  group_by(language, colour, num, color_code, resp_fixed) %>%
  summarize(count = n(), .groups = 'drop_last') %>%
  group_by(language, colour, num, color_code) %>%
  mutate(total = sum(count),
         p = count / total) %>%
  # Collapse small proportions
  mutate(resp_fixed = ifelse(count < 2, "other labels", resp_fixed)) %>%
  # Regroup to combine "Other" counts
  group_by(language, colour, num, color_code, resp_fixed) %>%
  summarize(count = sum(count), .groups = 'drop_last') %>%
  group_by(language, colour, num, color_code) %>%
  mutate(total = sum(count),
         p = count / total) %>%
  arrange(language, num, colour, color_code, -p) %>%
  mutate(group_id = paste(language, num, colour, sep = '_'),
         disp = paste(resp_fixed, '(', round(100 * p, 2), '%)'))

unique_groups <- naming_data_summary$group_id %>% unique()

for (g in unique_groups){
  Cairo::CairoPNG(paste0('../figs/naming_data_', g, '.png'), height=600, width=600, family='Arial')
  print(
  ggplot(naming_data_summary %>% filter(group_id == g), aes(x="", y=count)) +
    #facet_wrap(language ~ num + colour, scales='free', ncol = 4) +
    geom_bar(stat='identity', width=1, fill = naming_data_summary %>% filter(group_id == g) %>% pull(color_code), 
             color='black', 
             alpha= 0.4*naming_data_summary %>% filter(group_id == g) %>% pull(p) + 0.6) +
    geom_text_repel(
      aes(label = disp, y = cumsum(count) - count / 2),
      direction = "y",
      box.padding = 0.2,
      segment.color = 'grey50',
      show.legend = FALSE,
      size=7,
      nudge_x= 1,
      nudge_y=-1,
      max.overlaps = 12
    ) +
    coord_polar("y", start=0) +
    theme_void() 
  )
  dev.off()
}




# visualization
ggplot(task2_summary %>% filter(subject_id %in% included_participants),
       aes(x = num, y = p_dark, group = colour, color = colour)) +
  facet_wrap(~language) +
  stat_summary(fun=mean, geom='line') +
  stat_summary(fun.data = mean_cl_boot, geom='errorbar', alpha=0.3) +
  theme_bw(25) +
  xlab('Stimulus number in each spectrum') +
  ylab('Average probability of labelling a stimulus as dark') +
  scale_color_manual(values=c('blue', 'brown', 'red', '#F6BE00')) +
  theme(legend.key.size = unit(1, 'cm'),
        panel.border = element_blank(),
        strip.background = element_rect(fill='white', color = 'gray'),
        legend.position = 'bottom',
        legend.title = element_blank())


blues<-ggplot(task1_plot_good %>% filter(colour=='blues') %>% 
                mutate(category = factor(category, levels = c('within light', 'between', 'within dark'))), 
              aes(x = language, y = rt, fill = category)) +
  stat_summary(geom = 'col', fun = mean, position = 'dodge') +
  stat_summary(geom = 'errorbar', fun.data = 'mean_cl_boot', position = 'dodge') +
  theme_bw(25) +
  scale_fill_manual(values = c('lightblue', 'gray50', 'darkblue')) +
  xlab('') +
  ylab('') +
  coord_cartesian(ylim=c(500,1500)) +
  theme(legend.key.size = unit(1, 'cm'),
        panel.border = element_blank(),
        strip.background = element_rect(fill='white', color = 'gray'),
        legend.position = 'bottom',
        legend.title = element_blank())

reds<-ggplot(task1_plot_good %>% filter(colour=='reds') %>% 
               mutate(category = factor(category, levels = c('within light', 'between', 'within dark'))), 
             aes(x = language, y = rt, fill = category)) +
  stat_summary(geom = 'col', fun = mean, position = 'dodge') +
  stat_summary(geom = 'errorbar', fun.data = 'mean_cl_boot', position = 'dodge') +
  theme_bw(25) +
  scale_fill_manual(values = c('pink', 'gray50', 'red')) +
  xlab('') +
  ylab('') +
  coord_cartesian(ylim=c(500,1500)) +
  theme(legend.key.size = unit(1, 'cm'),
        panel.border = element_blank(),
        strip.background = element_rect(fill='white', color = 'gray'),
        legend.position = 'bottom',
        legend.title = element_blank())

yellows<-ggplot(task1_plot_good %>% filter(colour=='yellows') %>% 
                  mutate(category = factor(category, levels = c('within light', 'between', 'within dark'))), 
                aes(x = language, y = rt, fill = category)) +
  stat_summary(geom = 'col', fun = mean, position = 'dodge') +
  stat_summary(geom = 'errorbar', fun.data = 'mean_cl_boot', position = 'dodge') +
  theme_bw(25) +
  scale_fill_manual(values = c('yellow', 'gray50', 'orange')) +
  xlab('') +
  ylab('') +
  coord_cartesian(ylim=c(500,1500)) +
  theme(legend.key.size = unit(1, 'cm'),
        panel.border = element_blank(),
        strip.background = element_rect(fill='white', color = 'gray'),
        legend.position = 'bottom',
        legend.title = element_blank())

browns <-ggplot(task1_plot_good %>% filter(colour=='browns') %>% 
                  mutate(category = factor(category, levels = c('within light', 'between', 'within dark'))), 
                aes(x = language, y = rt, fill = category)) +
  stat_summary(geom = 'col', fun = mean, position = 'dodge') +
  stat_summary(geom = 'errorbar', fun.data = 'mean_cl_boot', position = 'dodge') +
  theme_bw(25) +
  scale_fill_manual(values = c('#C4A484', 'gray50', '#5C4033')) +
  xlab('') +
  ylab('') +
  coord_cartesian(ylim=c(500,1500)) +
  theme(legend.key.size = unit(1, 'cm'),
        panel.border = element_blank(),
        strip.background = element_rect(fill='white', color = 'gray'),
        legend.position = 'bottom',
        legend.title = element_blank())

pdf('../figs/figure3.pdf', height=10, width=15)
grid.arrange(blues, reds, yellows, browns, nrow=2, 
             left=textGrob("Mean reaction time (ms)", rot=90, gp=gpar(fontsize=25,font=8)))
dev.off()


set.seed(20221114)
m1 <- brm(rt ~ category * language + (1 | subject_id) + (1 | item), 
          data = task1_plot_good %>%
            filter(category %in% c("within light", "between"), colour == 'blues'), 
          family = gaussian(), 
          warmup=2000, 
          iter=10000, 
          thin = 10)
summary(m1)
rope(m1)

# Regression Coefficients:
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            1239.70     26.17  1186.07  1289.66 1.00     2466     3035
# categorywithinlight                   -94.21     18.57  -130.48   -56.91 1.00     3121     3155
# languageRussian                       -22.85     44.35  -107.02    61.85 1.00     2524     2872
# categorywithinlight:languageRussian    40.32     30.76   -19.93    98.72 1.00     3122     3158

# Parameter                           | inside ROPE
# -------------------------------------------------
# Intercept                           |      0.00 %
# categorywithinlight                 |      0.00 %
# languageRussian                     |     66.97 %
# categorywithinlight:languageRussian |     58.52 %


set.seed(20221114)
m2 <- brm(rt ~ category * language + (1  | subject_id) + (1 | item), 
          data = task1_plot_good %>%
            filter(category %in% c("within dark", "between"), colour == 'blues'), 
          family = gaussian(), 
          warmup=2000, 
          iter=10000, 
          thin = 10)
summary(m2)
rope(m2)

# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                           1262.91     34.48  1195.50  1329.94 1.00     2964     2932
# categorywithindark                    30.74     25.71   -18.76    79.77 1.00     3037     3040
# languageRussian                      -13.56     52.93  -116.79    92.61 1.00     2913     2937
# categorywithindark:languageRussian     0.09     36.34   -70.27    71.43 1.00     2973     2848

# Parameter                          | inside ROPE
# ------------------------------------------------
# Intercept                          |      0.00 %
# categorywithindark                 |     81.51 %
# languageRussian                    |     70.30 %
# categorywithindark:languageRussian |     89.77 %

set.seed(20221114)
m3 <- brm(rt ~ category * language + (1  | subject_id) + (1 | item), 
          data = task1_plot_good %>%
            filter(category %in% c("within light", "between"), colour == 'reds'),
          family = gaussian(), 
          warmup=2000, 
          iter=10000, 
          thin = 10)
summary(m3)
rope(m3)

# Regression Coefficients:
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            1255.21     35.60  1186.25  1327.93 1.00     3170     3043
# categorywithinlight                   -69.79     27.60  -123.51   -14.83 1.00     3069     3086
# languageRussian                       -33.71     62.88  -160.26    88.86 1.00     3091     2906
# categorywithinlight:languageRussian    13.62     42.70   -68.99    97.04 1.00     3101     3296

# Parameter                           | inside ROPE
# -------------------------------------------------
# Intercept                           |      0.00 %
# categorywithinlight                 |     18.09 %
# languageRussian                     |     50.59 %
# categorywithinlight:languageRussian |     74.38 %

set.seed(20221114)
m4 <- brm(rt ~ category * language + (1  | subject_id) + (1 | item), 
          data = task1_plot_good %>%
            filter(category %in% c("within dark", "between"), colour == 'reds'),
          family = gaussian(), 
          warmup=2000, 
          iter=10000, 
          thin = 10)
summary(m4)
rope(m4)

# Regression Coefficients:
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                           1319.69     53.74  1213.33  1428.27 1.00     2845     2849
# categorywithindark                    67.40     37.23    -3.94   143.22 1.00     2971     3034
# languageRussian                      -46.09     73.60  -190.55    99.22 1.00     3045     3032
# categorywithindark:languageRussian   -34.25     51.69  -134.90    66.11 1.00     3124     2963

# Parameter                          | inside ROPE
# ------------------------------------------------
# Intercept                          |      0.00 %
# categorywithindark                 |     34.87 %
# languageRussian                    |     48.91 %
# categorywithindark:languageRussian |     64.44 %

set.seed(20221114)
m5 <- brm(rt ~ category * language + (1  | subject_id) + (1 | item), 
          data = task1_plot_good %>%
            filter(category %in% c("within light", "between"), colour == 'yellows'), 
          family = gaussian(), 
          warmup=2000, 
          iter=10000, 
          thin = 10)
summary(m5)
rope(m5)

# Regression Coefficients:
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            1218.53     31.86  1158.15  1279.91 1.00     3041     3054
# categorywithinlight                     9.18     21.01   -31.79    49.22 1.00     3363     3170
# languageRussian                        16.23     58.57   -99.48   133.53 1.00     2824     3072
# categorywithinlight:languageRussian    -7.53     36.53   -76.77    62.84 1.00     3209     3172

# Parameter                           | inside ROPE
# -------------------------------------------------
# Intercept                           |      0.00 %
# categorywithinlight                 |     99.80 %
# languageRussian                     |     62.43 %
# categorywithinlight:languageRussian |     84.28 %

set.seed(20221114)
m6 <- brm(rt ~ category * language + (1  | subject_id) + (1 | item), 
          data = task1_plot_good %>%
            filter(category %in% c("within dark", "between"), colour == 'yellows'), 
          family = gaussian(), 
          warmup=2000, 
          iter=10000, 
          thin = 10)
summary(m6)
rope(m6)

# Regression Coefficients:
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                           1216.24     32.90  1151.39  1280.75 1.00     2967     3040
# categorywithindark                    -7.60     22.68   -53.07    36.88 1.00     3135     3053
# languageRussian                       22.78     59.00   -93.36   136.79 1.00     2919     3128
# categorywithindark:languageRussian   -34.05     38.53  -110.76    42.33 1.00     3194     3056

# Parameter                          | inside ROPE
# ------------------------------------------------
# Intercept                          |      0.00 %
# categorywithindark                 |     98.91 %
# languageRussian                    |     59.67 %
# categorywithindark:languageRussian |     66.94 %

set.seed(20221114)
m7 <- brm(rt ~ category * language + (1  | subject_id) + (1 | item), 
          data = task1_plot_good %>%
            filter(category %in% c("within light", "between"), colour == 'browns'), 
          family = gaussian(), 
          warmup=2000, 
          iter=10000, 
          thin = 10)
summary(m7)
rope(m7)

# Regression Coefficients:
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                            1124.39     29.15  1065.15  1180.62 1.00     2993     2975
# categorywithinlight                     9.54     20.03   -30.40    48.80 1.00     3149     2790
# languageRussian                        -1.62     45.71   -91.35    89.27 1.00     2986     3025
# categorywithinlight:languageRussian    -0.56     29.79   -59.10    56.92 1.00     3300     3198

# Parameter                           | inside ROPE
# -------------------------------------------------
# Intercept                           |      0.00 %
# categorywithinlight                 |     98.26 %
# languageRussian                     |     70.16 %
# categorywithinlight:languageRussian |     90.76 %


set.seed(20221114)
m8 <- brm(rt ~ category * language + (1  | subject_id) + (1 | item), 
          data = task1_plot_good %>%
            filter(category %in% c("within dark", "between"), colour == 'browns'),
          family = gaussian(), 
          warmup=2000, 
          iter=10000, 
          thin = 10)
summary(m8)
rope(m8)

# Regression Coefficients:
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                           1143.17     31.08  1082.45  1203.60 1.00     3223     3215
# categorywithindark                    28.60     21.78   -13.98    71.68 1.00     3198     3285
# languageRussian                       11.09     48.34   -83.35   106.60 1.00     3054     3152
# categorywithindark:languageRussian   -33.85     32.50   -97.98    28.29 1.00     3071     3174

# Parameter                          | inside ROPE
# ------------------------------------------------
# Intercept                          |      0.00 %
# categorywithindark                 |     82.50 %
# languageRussian                    |     69.74 %
# categorywithindark:languageRussian |     67.37 %

p1 <- m1 %>% spread_draws(c(b_categorywithinlight, b_languageRussian, `b_categorywithinlight:languageRussian`)) %>%
  mutate(spectrum = 'blues') %>%
  rbind(m3 %>% spread_draws(c(b_categorywithinlight, b_languageRussian, `b_categorywithinlight:languageRussian`)) %>% 
          mutate(spectrum = 'reds')) %>%
  rbind(m5 %>% spread_draws(c(b_categorywithinlight, b_languageRussian, `b_categorywithinlight:languageRussian`)) %>%
          mutate(spectrum='yellows')) %>%
  rbind(m7 %>% spread_draws(c(b_categorywithinlight, b_languageRussian, `b_categorywithinlight:languageRussian`)) %>%
          mutate(spectrum='browns')) %>%
  mutate(spectrum = factor(spectrum, levels=c('browns', 'yellows', 'reds', 'blues'))) %>%
  ggplot(aes(x=`b_categorywithinlight:languageRussian`, y=spectrum, fill=spectrum)) +
  stat_halfeye() +
  scale_fill_manual(values=c('#C4A484', 'yellow', 'pink', 'lightblue')) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  theme_bw(18) +
  xlim(-400, 400) +
  theme(legend.key.size = unit(1, 'cm'),
        panel.border = element_blank(),
        strip.background = element_rect(fill='white', color = 'gray'),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  xlab('Interaction between category and language (ms)') +
  ggtitle('Among the "between" category and the "within light" category in each spectrum')

p2 <- m2 %>% spread_draws(c(b_categorywithindark, b_languageRussian, `b_categorywithindark:languageRussian`)) %>%
  mutate(spectrum = 'blues') %>%
  rbind(m4 %>% spread_draws(c(b_categorywithindark, b_languageRussian, `b_categorywithindark:languageRussian`)) %>% 
          mutate(spectrum = 'reds')) %>%
  rbind(m6 %>% spread_draws(c(b_categorywithindark, b_languageRussian, `b_categorywithindark:languageRussian`)) %>%
          mutate(spectrum='yellows')) %>%
  rbind(m8 %>% spread_draws(c(b_categorywithindark, b_languageRussian, `b_categorywithindark:languageRussian`)) %>%
          mutate(spectrum='browns')) %>%
  mutate(spectrum = factor(spectrum, levels=c('browns', 'yellows', 'reds', 'blues'))) %>%
  ggplot(aes(x=`b_categorywithindark:languageRussian`, y=spectrum, fill=spectrum)) +
  stat_halfeye(alpha=0.8) +
  scale_fill_manual(values=c('#5C4033', 'orange', 'red', 'darkblue')) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  theme_bw(18) +
  xlim(-400, 400) +
  theme(legend.key.size = unit(1, 'cm'),
        panel.border = element_blank(),
        strip.background = element_rect(fill='white', color = 'gray'),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  xlab('Interaction between category and language (ms)') +
  ggtitle('Among the "between" category and the "within dark" category in each spectrum')

pdf('../figs/figure4.pdf', height=10, width=15)  
grid.arrange(p1, p2, nrow=2)
dev.off()
