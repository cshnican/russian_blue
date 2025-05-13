library(tidyverse)
library(ggplot2)
library(brms)
library(gridExtra)
library(grid)
library(bayestestR)
library(zoo)
library(tidybayes)

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

task1 <- rbind(task1_eng, task1_rus) %>% mutate(subject_id_colour_target = paste(subject_id, colour, target, sep = '_'),
                                                subject_id_colour_distractor = paste(subject_id, colour, distractor, sep = '_'))

task1 %>% group_by(language) %>% summarize(n = unique(subject_id) %>% length())

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
         percentage_trials_included >= 0.50) # only include participants with >50% of the trials included




# number of participants included:
task1_plot_good %>% group_by(language) %>% summarize(n = unique(subject_id) %>% length())

included_participants <- task1_plot_good %>% pull(subject_id) %>% unique()

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

pdf('../figs/figure3_supp.pdf', height=10, width=15)
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
# Intercept                            1233.59     23.07  1188.13  1277.64 1.00     3214     3019
# categorywithinlight                   -63.59     15.94   -93.56   -31.89 1.00     3320     3173
# languageRussian                       -50.94     39.16  -127.91    24.99 1.00     3350     3104
# categorywithinlight:languageRussian    35.73     25.69   -14.29    84.67 1.00     3338     3020

# Parameter                           | inside ROPE
# -------------------------------------------------
# Intercept                           |      0.00 %
# categorywithinlight                 |     15.79 %
# languageRussian                     |     47.99 %
# categorywithinlight:languageRussian |     69.47 %


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
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                           1252.28     29.88  1193.84  1310.38 1.00     3112     3021
# categorywithindark                    16.57     21.05   -24.37    57.72 1.00     3332     3142
# languageRussian                      -42.72     45.15  -129.23    48.29 1.00     2880     3039
# categorywithindark:languageRussian    10.61     29.38   -48.13    70.86 1.00     3149     2860

# Parameter                          | inside ROPE
# ------------------------------------------------
# Intercept                          |      0.00 %
# categorywithindark                 |     98.49 %
# languageRussian                    |     59.64 %
# categorywithindark:languageRussian |     94.70 %

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
# Intercept                            1249.01     24.86  1199.31  1298.29 1.00     2876     3091
# categorywithinlight                   -41.70     16.53   -73.19    -9.31 1.00     3132     2927
# languageRussian                       -17.95     41.85   -97.83    64.16 1.00     2913     2960
# categorywithinlight:languageRussian    -0.89     27.15   -53.30    53.57 1.00     3346     2923

# Parameter                           | inside ROPE
# -------------------------------------------------
# Intercept                           |      0.00 %
# categorywithinlight                 |     69.24 %
# languageRussian                     |     76.28 %
# categorywithinlight:languageRussian |     98.09 %

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
# Intercept                           1287.57     36.98  1218.17  1360.57 1.00     2783     3023
# categorywithindark                    55.03     22.04    12.58    98.42 1.00     3234     3213
# languageRussian                      -20.83     49.48  -117.66    78.01 1.00     2997     3013
# categorywithindark:languageRussian   -17.36     31.30   -80.10    41.33 1.00     3082     3242

# Parameter                          | inside ROPE
# ------------------------------------------------
# Intercept                          |      0.00 %
# categorywithindark                 |     51.91 %
# languageRussian                    |     73.98 %
# categorywithindark:languageRussian |     90.76 %

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
# Intercept                            1245.10     26.55  1192.97  1298.74 1.00     2639     3060
# categorywithinlight                    32.53     18.42    -2.78    68.79 1.00     3228     3132
# languageRussian                       -23.62     45.39  -112.19    66.62 1.00     2487     2969
# categorywithinlight:languageRussian     9.52     29.19   -50.04    66.23 1.00     3286     3257

# Parameter                           | inside ROPE
# -------------------------------------------------
# Intercept                           |      0.00 %
# categorywithinlight                 |     87.70 %
# languageRussian                     |     72.17 %
# categorywithinlight:languageRussian |     95.07 %

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
# Intercept                           1215.32     23.96  1165.97  1261.62 1.00     2745     2918
# categorywithindark                   -15.51     16.93   -49.37    16.86 1.00     3111     3128
# languageRussian                      -11.87     42.85   -93.92    72.32 1.00     3028     3176
# categorywithindark:languageRussian    -8.18     28.97   -63.52    49.91 1.00     2766     2482

# Parameter                          | inside ROPE
# ------------------------------------------------
# Intercept                          |      0.00 %
# categorywithindark                 |    100.00 %
# languageRussian                    |     78.85 %
# categorywithindark:languageRussian |     95.69 %

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
# Intercept                            1144.41     22.68  1099.98  1187.94 1.00     2966     2913
# categorywithinlight                    -3.33     16.03   -33.73    28.66 1.00     3247     3130
# languageRussian                       -30.09     38.23  -105.69    44.64 1.00     2857     2713
# categorywithinlight:languageRussian    21.97     25.03   -26.97    71.34 1.00     3227     2993

# Parameter                           | inside ROPE
# -------------------------------------------------
# Intercept                           |      0.00 %
# categorywithinlight                 |    100.00 %
# languageRussian                     |     68.59 %
# categorywithinlight:languageRussian |     86.18 %


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
# Intercept                           1151.15     26.13  1100.17  1201.78 1.00     3004     3087
# categorywithindark                    30.22     17.91    -5.18    63.81 1.00     3421     3243
# languageRussian                      -24.36     44.38  -112.40    63.40 1.00     2753     2978
# categorywithindark:languageRussian   -10.04     27.67   -63.74    44.91 1.00     3118     3259

# Parameter                          | inside ROPE
# ------------------------------------------------
# Intercept                          |      0.00 %
# categorywithindark                 |     89.08 %
# languageRussian                    |     72.24 %
# categorywithindark:languageRussian |     95.10 %

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

pdf('../figs/figure4_supp.pdf', height=10, width=15)  
grid.arrange(p1, p2, nrow=2)
dev.off()
