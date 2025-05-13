# data analysis
library(ggplot2)
library(tidyr)
library(tidyverse)
library(purrr)
library(stringr)
library(lme4)
library(lmerTest)

process_data <- function(d1){
  subj <- d1$response[1] %>% str_sub(16,-3) # change this to Regex if possible
  is_monolingual <- ifelse(d1$response[5] == 0, 'no', 
                      ifelse(d1$response[5] == 1, 'yes', 'NA')) # monolingually raised?
  # get task responses
  task1a <- d1[d1$task == 'task1a', c(1,2,5,15,17,18,19)]
  # task1b <- d1[d1$task == 'task1b',c(1,2,15,17,18,19)]
  task2 <- d1[d1$task == 'task2',c(1,2,5,18,19)]
  task3 <- d1[d1$task == 'task3',c(1,2,5,17,18,19,20,21)]
  
  # task1a
  task1a <- task1a %>% mutate(accuracy = ifelse(response == ans, 1, 0),
                              is_monolingual = rep(is_monolingual, nrow(task1a)),
                              subject_id = rep(subj,nrow(task1a)),
                              mod = str_sub(str_extract(stim, "(\\d+)"),1,1),
                              target = rep(0, nrow(task1a)),
                              distractor = rep(0, nrow(task1a)))
  for (i in 1:nrow(task1a)){
    task1a$target[i] <- get_target_stim(task1a$mod[i], task1a$num[i])
    task1a$distractor[i] <- get_distractor_stim(task1a$mod[i], task1a$num[i])
  }
  
  task1a <- task1a[,c("subject_id", "trial_index", "is_monolingual", "colour", "target", "distractor", "accuracy", "rt")]
  
  ### TODO: the num here is NOT the stimulus number!!
  ### TODO: convert this to TARGET stimulus
  
  # task1b 
  # task1b <- task1b %>% mutate(accuracy = ifelse(response == ans, 1, 0),
  #                             subject_id = rep(subj,nrow(task1b)),
  #                             mod = str_sub(str_extract(stim, "(\\d+)"),1,1),
  #                             target = rep(0, nrow(task1b)),
  #                             distractor = rep(0, nrow(task1b)))
  # 
  # for (i in 1:nrow(task1b)){
  #   task1b$target[i] <- get_target_stim(task1b$mod[i], task1b$num[i])
  #   task1b$distractor[i] <- get_distractor_stim(task1b$mod[i], task1b$num[i])
  # }
  # 
  # task1b <- task1b[,c("subject_id", "colour", "target", "distractor", "accuracy", "rt")]
  
  # task2
  task2 <- task2 %>% mutate(resp = str_sub(response, 8, -3),
                            is_monolingual = rep(is_monolingual, nrow(task2)),
                            subject_id = rep(subj, nrow(task2)))
  task2 <- task2[, c("subject_id", "trial_index", "is_monolingual", "colour", "num", "resp", "rt")]
  
  # task3
  task3 <- task3 %>% mutate(subject_id = rep(subj, nrow(task3)),
                            is_monolingual = rep(is_monolingual, nrow(task3)),
                            choices = rep("", nrow(task3)),
                            chosen = rep("", nrow(task3)),
                            is_dark = rep(NA, nrow(task3)),)
                            # num = as.numeric(str_sub(gsub("[^[:digit:].]", "\\1", stim),3,4)))

  
  for (i in 1:nrow(task3)){
    task3$choices[i] <- get_choices(task3$response[i])
    task3$chosen[i] <- eval(parse(text = paste0("task3$", task3$choices[i], "[",i,"]")))
    task3$is_dark[i] <- get_darkness(task3$chosen[i])
  }
  
  task3 <- task3[, c("subject_id", "trial_index", "colour", "is_monolingual", "num", "is_dark", "rt")]
  # processed_data <- list(task1a = task1a, task1b = task1b, task2 = task2, task3 = task3)
  processed_data <- list(task1a = task1a, task2 = task2, task3 = task3)
  
  return(processed_data)

}

get_choices <- function(key){
  if (key == 'arrowleft'){
    return('label1')
  }
  if (key == 'arrowright'){
    return('label2')
  }
}  

get_darkness <- function(label){
  if (label == 'Голубой' | label == 'Желтый' | label == 'Розовый' | label == 'Светло коричневый' | 
      label == 'lightblue' | label == 'yellow' | label == 'pink' | label == 'lightbrown'){
    return(0)
  }
  if (label == 'Синий' | label == 'Оранжевый' | label == 'Красный' | label == 'Tемно коричневый' |
      label == 'darkblue' | label == 'orange' | label == 'red' | label == 'darkbrown'){
    return(1)
  }
}  


get_target_stim <- function(mod, num){
  if (mod == 1 | mod == 4){
    return(num)
  }
  if (mod == 2 | mod == 3){
    return(num + 2)
  }
}

get_distractor_stim <- function(mod, num){
  if (mod == 1 | mod == 4){
    return(num + 2)
  }
  if (mod == 2 | mod == 3){
    return(num)
  }
}

get_task1a <- function(list){
  return(list$task1a)
}

get_task1b <- function(list){
  return(list$task1b)
}

get_task2 <- function(list){
  return(list$task2)
}

get_task3 <- function(list){
  return(list$task3)
}

### determining color boundary
# criterion:
# [p(dark) for the ith stimulus - 0.5] * [p(dark) for the (i+2)th chip - 0.5] <= 0 

is_transition <- function(task3_summary_participant, subject_id, color, target, distractor){
  p_target <- mean(task3_summary_participant$avg.dark[(task3_summary_participant$subject_id == subject_id) &
                                                       (task3_summary_participant$num == target) &
                                                       (task3_summary_participant$colour == color)])
  p_distractor <- mean(task3_summary_participant$avg.dark[(task3_summary_participant$subject_id == subject_id) &
                                                       (task3_summary_participant$num == distractor) &
                                                       (task3_summary_participant$colour == color)])
  if ((p_target - 0.5) * (p_distractor - 0.5) <= 0){
    return(TRUE)
  }
  if ((p_target - 0.5) * (p_distractor - 0.5) > 0){
    return(FALSE)
  }
  
}

### mixed effect model

me <- function(task, color_input){
  m <- lmer(rt ~ is.transition + (1 + is.transition | subject_id) + (1 | target), data = task %>% filter(colour == color_input))
  return(m)
}
