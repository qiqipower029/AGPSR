#-------------------------------------------------Actigraph function---------------------------------------------#
#----------------------------------------------------Jieqi Tu----------------------------------------------------#





#----------------------------------------------------Dependencies------------------------------------------------#
require(tidyverse)
require(SummarizedActigraphy)
require(activityCounts)
require(lubridate)
require(openxlsx)
#----------------------------------------------------------------------------------------------------------------#

#--------------------------------------------------Argument Description------------------------------------------#
# data_directory: a string of working directory (should be a gt3x file)
# valid_day_criteria: minimum hours to be considered as a valid day (default: 10)
# wake_bout: the number of consecutive minutes with positive activity counts to define wake time (default: 1)
# sleep_bout: the number of consecutive minutes with positive activity counts to define wake time (default: 1)
# non_wear_chunk_min: the number of inactive minutes to be considered as non-wear time (default: 45)
# start_hour: the starting hour of a day (default: "04:00:00")
# participant_id: a string of participant ID (default: "P2E30001")
# days_include: the number of days to be included in this study (default: 21)
#----------------------------------------------------------------------------------------------------------------#

#---------------------------------------------------gt3x function------------------------------------------------#
gt3x_function = function(data_directory, valid_day_criteria = 10, wake_bout = 1, sleep_bout = 1, non_wear_chunk_min = 45, start_hour = "04:00:00", participant_id = "P2E30001", days_include = 21) {
  
  
  #--------------------------Read in the raw dataset-----------------------------#
  x = read_actigraphy(paste0(data_directory))
  
  #-----------------------------Data formatting----------------------------------#
  # Summarize the data (minute level)
  daily = summarize_daily_actigraphy(x = x,calculate_ac = T, unit = "1 min")
  
  # Separate date and time into different columns
  daily$date = as.Date(daily$time)
  daily$time_GMT = format(as.POSIXct(daily$time), format = "%H:%M:%S")
  
  #---------------------------Activity Classification----------------------------#
  
  # Create a new variable to classify activity at minute level
  daily$activity = 
    ifelse(daily$AC <= 100, "Sedentary",
           ifelse(daily$AC <= 929, "Low light",
                  ifelse(daily$AC <= 1952, "High light",
                         ifelse(daily$AC <= 3299, "Low moderate",
                                ifelse(daily$AC <= 5724, "High moderate",
                                       "Vigorous")))))
  
  # Only keep time, AC and activity classification to the output file
  daily_ac_minute = 
    daily %>% select(time, date, time_GMT, AC, activity)
  
  # Save the activity classification result data
  write.csv(daily_ac_minute, file = paste0(participant_id, "_acticity_minute.csv"))
  

  
  #---------------------------Wake/Sleep time detection--------------------------#
  
  # Ignore time from midnight to the pre-specified start hour (default: 4:00:00)
  daily = 
    daily %>% 
    filter(time_GMT >= start_hour)
  
  # Detect wake-up time and bed time on each day
  date = c()
  wake = c()
  sleep = c()
  non_wear = c()
  
  for (j in as.character(unique(daily$date))) {
    sub.daily = daily %>% filter(date == j)
    print(j)
    date = c(date, j) 
    
    # Wake-up time
    for (k in 1:nrow(sub.daily)) {
      
      if(sub.daily[k,"AC"] > 0) {
        # Check if the AC for the pre-specified bout mins > 0
        bout_check = numeric(wake_bout-1)
        for (l in 1:wake_bout-1) {
          if(sub.daily[(k+l), "AC"] > 0) {
            bout_check[l] = 1
          }
        }
        if(sum(bout_check) == wake_bout-1) {
          waketime = sub.daily$time_GMT[[k]]
          wake = c(wake, waketime)
          break
        }
      }
      if(k == nrow(sub.daily)) {
        waketime = NA
        wake = c(wake, waketime)
      }
    }
    
    # Sleep time
    for (k in nrow(sub.daily):1) {
      if(sub.daily[k,"AC"]>0) {
        bout_check = numeric(sleep_bout-1)
        for (m in 1:sleep_bout-1) {
          if(sub.daily[(k-m), "AC"] > 0) {
            bout_check[m] = 1
          }
        }
        if(sum(bout_check) == sleep_bout-1) {
          sleeptime = sub.daily$time_GMT[[k]]
          sleep = c(sleep, sleeptime)
          break
        } 
      }
      if(k == 1) {
        sleeptime = NA
        sleep = c(sleep, sleeptime)
      }
    }
    
    # Detect big chunks of non-wear times (starting from wake time)
    daily_waking = 
      sub.daily %>% 
      filter(time_GMT >= waketime & time_GMT <= sleeptime)
    
    
    if(nrow(daily_waking) > valid_day_criteria*60) {
      # Extract wake time (unit: hour)
      wake_hour = format(strptime(waketime, '%H:%M:%S'),'%H') 
      # Extract sleep time (unit: hour)
      sleep_hour = format(strptime(sleeptime, '%H:%M:%S'),'%H')
      # Create the time checkpoints
      checkpoints_start = strptime(paste0(wake_hour, ":00:00"), '%H:%M:%S') + minutes(30)
      checkpoints_end = strptime(paste0(sleep_hour, ":00:00"), '%H:%M:%S') - minutes(30)
    } else {
      checkpoints_start = 0
      checkpoints_end = 0
    }
    
    if(checkpoints_end > (checkpoints_start+minutes(non_wear_chunk_min))) {
      time_checkpoints = format(seq.POSIXt(from = checkpoints_start, to = checkpoints_end, by = paste(non_wear_chunk_min, "min")), format = "%H:%M:%S")
    } else {
      time_checkpoints = NA
    }
    
    nonwear_hour = 0 # non-wear time initialization (mins)
    if(length(time_checkpoints)*3/4 >= valid_day_criteria) {
      for (k in 1:(length(time_checkpoints)-1)) {
        timewindow = daily_waking %>% 
          filter(time_GMT >= time_checkpoints[[k]] & time_GMT < time_checkpoints[[k+1]])
        ac.sum = sum(timewindow$AC)
        if(ac.sum == 0) {
          nonwear_hour = nonwear_hour + non_wear_chunk_min/60
        }
      }
    }
    non_wear = c(non_wear, nonwear_hour)
    
  }
  
  # Combine and summarize the results
  result = cbind(date, wake, sleep, non_wear) %>% as.data.frame()
  result$sleep_minus_wake = round(difftime(strptime(result$sleep, format = "%H:%M:%S"),strptime(result$wake, format = "%H:%M:%S"), units = "hours"),2)
  result$wear_time  = round(difftime(strptime(result$sleep, format = "%H:%M:%S"),strptime(result$wake, format = "%H:%M:%S"), units = "hours"),2) - non_wear
  
  # Inclusion and Exclusion of days
  inclusion = c()
  inclusion[1] = "Exclude"
  if (nrow(result) > days_include) {
    for (i in 2:days_include) {
      if(result$wear_time[i] >= valid_day_criteria) {
        inclusion[i] = "Include"
      } else {
        inclusion[i] = "Exclude"
      }
    }
    for (i in (days_include + 1):nrow(result)) {
      inclusion[i] = "Exclude"
    }
  } else {
    for (i in 2:nrow(result)) {
      if(result$wear_time[i] >= valid_day_criteria) {
        inclusion[i] = "Include"
      } else {
        inclusion[i] = "Exclude"
      }
    }
  }
  result$inclusion = inclusion
  write.csv(result, paste0(participant_id, "_inclusion_decision.csv"))
  
}
#----------------------------------------------------------------------------------------------------------------#


#---------------------------------------------------Example------------------------------------------------------#

data_directory = "/Users/jieqi/Library/CloudStorage/Box-Box/E3 study/Analysis/0920/ActiGraph Data/P2E30001.gt3x"
setwd("/Users/jieqi/Library/CloudStorage/Box-Box/E3 study/Analysis/2023/0330") # Can be changed to your own working directory
gt3x_function(data_directory = data_directory, participant_id = "P2E30001")
#----------------------------------------------------------------------------------------------------------------#