
d_good %>% group_by(language) %>% summarise(n = length(unique(subject_id)))


yes <- raw %>% right_join(color_boundary, by = c("subject_id", 'colour', 'language'))

ggplot(yes, aes(x = num, y = is_dark, color = colour)) +
  facet_wrap(~subject_id) +
  stat_summary(geom = 'line', fun = 'mean') +
  geom_point(aes(x = boundary_left, y = p.x, color = colour), shape = 1) +
  geom_point(aes(x = boundary_right, y = p.y, color = colour), shape = 2)


ggplot(d_good %>% mutate(category = factor(category, levels = c("within_light", "between", "within_dark")),
                         colour_category = paste0(colour, "_", category),
                         colour_category = factor(colour_category, levels = c('blues_within_light', 'blues_between', 'blues_within_dark',
                                                                              'browns_within_light', 'browns_between', 'browns_within_dark',
                                                                              'reds_within_light', 'reds_between', 'reds_within_dark',
                                                                              'yellows_within_light', 'yellows_between', 'yellows_within_dark'))), 
       aes(x = language, y = accuracy, fill = colour_category))+
  facet_wrap(~colour) +
  stat_summary(geom = 'col', fun = mean, position = 'dodge') +
  stat_summary(geom = 'errorbar', fun.data = 'mean_cl_boot', position = 'dodge') +
  theme_classic(25) +
  scale_fill_manual(values = hexs) +
  theme(legend.position = 'none') +
  ylab('Accuracy')
