library(tidyverse)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("analysis_tools.R")

english_dir <- '../data/english_data/'
filenames_english <- list.files(english_dir, pattern = "*.csv")

russian_dir <- '../data/russian_data/'
filenames_russian <- list.files(russian_dir, pattern = "*.csv")

myfiles_english <- lapply(paste0(english_dir, filenames_english), read.csv)
myfiles_russian <- lapply(paste0(russian_dir, filenames_russian), read.csv)

data_english <- lapply(myfiles_english, process_data)
data_russian <- lapply(myfiles_russian, process_data)

# get results by task
# Task 1a: color discrimination
# Task 3: color labeling
task1a_en <- do.call(rbind, lapply(data_english, get_task1a))
task1a_ru <- do.call(rbind, lapply(data_russian, get_task1a))

task2_en <- do.call(rbind, lapply(data_english, get_task2))
task2_ru <- do.call(rbind, lapply(data_russian, get_task2))

task3_en <- do.call(rbind, lapply(data_english, get_task3))
task3_ru <- do.call(rbind, lapply(data_russian, get_task3))

# get accuracy in color discrimination
accuracy_en <- task1a_en %>%
  group_by(subject_id) %>%
  summarise(avg.rt = mean(rt),
            n = n(),
            avg.accuracy = mean(accuracy))

accuracy_ru <- task1a_ru %>%
  group_by(subject_id) %>%
  summarise(avg.rt = mean(rt),
            n = n(),
            avg.accuracy = mean(accuracy))

# skip this part - it will be filtered later
participant_list_en <- accuracy_en %>%
  #filter(avg.accuracy >= 0.75) %>%
  pull(subject_id)

participant_list_ru <- accuracy_ru %>%
  #filter(avg.accuracy >= 0.75) %>%
  pull(subject_id)

# filter
task1a_en_good <- task1a_en %>% filter(subject_id %in% participant_list_en)
task1a_ru_good <- task1a_ru %>% filter(subject_id %in% participant_list_ru)

task2_en_good <- task2_en %>% filter(subject_id %in% participant_list_en)
task2_ru_good <- task2_ru %>% filter(subject_id %in% participant_list_ru)

task3_en_good <- task3_en %>% filter(subject_id %in% participant_list_en)
task3_ru_good <- task3_ru %>% filter(subject_id %in% participant_list_ru)


# save data
write.csv(task1a_en_good, '../data/english_data.csv', row.names = FALSE)
write.csv(task1a_ru_good, '../data/russian_data.csv', row.names = FALSE)

write.csv(task2_en_good, '../data/english_edge_naming.csv', row.names = FALSE)
write.csv(task2_ru_good, '../data/russian_edge_naming.csv', row.names = FALSE)

write.csv(task3_en_good, '../data/english_data_forced_choice.csv', row.names = FALSE)
write.csv(task3_ru_good, '../data/russian_data_forced_choice.csv', row.names = FALSE)
