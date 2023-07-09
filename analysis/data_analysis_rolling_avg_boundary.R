setwd("~/Documents/BCS/TedLab/Russian_color/jspsych_experiments")
library(tidyverse)
library(ggplot2)
library(brms)
library(gridExtra)
library(grid)
library(bayestestR)
library(zoo)
library(tidybayes)


task1_eng <- read.csv('data_english.csv') %>% select(-is.transition, -trial_index, -X)
task1_rus <- read.csv('data_russian.csv') %>% select(-is_monolingual, -is.transition, -trial_index, -X)

task2_eng <- read.csv('forced_choice_data_english.csv') %>% select(-X)
task2_rus <- read.csv('forced_choice_data_russian.csv') %>% select(-c(trial_index, is_monolingual, X))

task1 <- rbind(task1_eng, task1_rus) %>% mutate(subject_id_colour_target = paste(subject_id, colour, target, sep = '_'),
                                                subject_id_colour_distractor = paste(subject_id, colour, distractor, sep = '_'))

task2_summary_eng <- task2_eng %>% group_by(subject_id, colour, num, language) %>% summarise(p_dark = mean(is_dark)) %>% ungroup() %>%
  group_by(subject_id, colour, language) %>% mutate(p_dark_roll = rollapply(p_dark, width = 3, FUN = function(x) mean(x, na.rm = TRUE),
                                                                    by = 1, by.column = TRUE, 
                                                                    partial = TRUE, fill = NA, align = 'center')) %>% ungroup()

task2_summary_rus <- task2_rus %>% group_by(subject_id, colour, num, language) %>% summarise(p_dark = mean(is_dark)) %>% ungroup() %>%
  group_by(subject_id, colour, language) %>% mutate(p_dark_roll = rollapply(p_dark, width = 3, FUN = function(x) mean(x, na.rm = TRUE),
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

p <- grid.arrange(blues, reds, yellows, browns, nrow=2, left=textGrob("Mean reaction time (ms)", rot=90, gp=gpar(fontsize=25,font=8)))


task1_plot_good %>% group_by(language) %>% summarise(n = length(unique(subject_id)))

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
  theme_bw(25) +
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
  theme_bw(25) +
  xlim(-400, 400) +
  theme(legend.key.size = unit(1, 'cm'),
        panel.border = element_blank(),
        strip.background = element_rect(fill='white', color = 'gray'),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  xlab('Interaction between category and language (ms)') +
  ggtitle('Among the "between" category and the "within dark" category in each spectrum')

fig <- grid.arrange(p1, p2, nrow=2)




