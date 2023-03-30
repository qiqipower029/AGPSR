#-------------------------------------------------data_merge function--------------------------------------------#
#------------------------------------------------------Jieqi Tu--------------------------------------------------#


#----------------------------------------------------Dependencies------------------------------------------------#
require(tidyverse)
#----------------------------------------------------------------------------------------------------------------#


#--------------------------------------------------Argument Description------------------------------------------#
# acti_data: a csv file output by gt3x function with activity classification
# gps_data: a csv file output by GPSimpute function
# participant_id: a string of participant ID (default: "P2E30001")
#----------------------------------------------------------------------------------------------------------------#


#--------------------------------------------------Data_merge functio--------------------------------------------#

data_merge = function(acti_data, gps_data, participant_id = "P2E30001") {
  
  #-----------------------------Data formatting----------------------------------#
  gps_data$`UTC TIME` = paste(gps_data$`UTC.DATE`, gps_data$`UTC.TIME`)
  gps_data$`UTC TIME` = ifelse(gps_data$`UTC TIME` == "NA NA", NA, 
                               ifelse(gps_data$`UTC TIME` == "NA", NA, gps_data$`UTC TIME`))
  gps_data$`UTC TIME` = format(as.POSIXct(gps_data$`UTC TIME`), format = '%Y-%m-%d %H:%M')
  acti_data$time = format(as.POSIXct(acti_data$time), format = '%Y-%m-%d %H:%M')
  
  # Only keep time, activity counts and activity categories in activity dataset
  acti_data = 
    acti_data %>% 
    select(time, AC, activity)
  
  new_data = full_join(x = acti_data, y = gps_data, by = c("time" = "UTC TIME"))
  write.csv(new_data, file = paste0(participant_id, "_merged.csv"))
}

#----------------------------------------------------------------------------------------------------------------#


#---------------------------------------------------Example------------------------------------------------------#
wd = "/Users/jieqi/Library/CloudStorage/Box-Box/E3 study/Analysis/2023/0330" 
setwd(wd) # It can be changed to your own working directory
gps_data = read.csv("./P2E30001_cleaned.csv")
acti_data = read.csv("./P2E30001_acticity_minute.csv")
data_merge(acti_data, gps_data, participant_id = "P2E30001")
#----------------------------------------------------------------------------------------------------------------#