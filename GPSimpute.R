#-------------------------------------------------GPSimpute function---------------------------------------------#


#----------------------------------------------------Dependencies------------------------------------------------#
require(tidyverse)
require(maps)
require(tools)
#----------------------------------------------------------------------------------------------------------------#

#--------------------------------------------------Argument Description------------------------------------------#
# data_directory: a string of working directory (should be a .csv file)
# high.speed: Speed higher than this value (km/h) will be dropped (default: 130)
# height.change: One-min height change higher than this value (m) will be dropped (default: 1000)
# impute.gap.size: Size (mins) of the gaps to impute (should be less than 30 mins) (default: 5)
# counties: A string vector of county names to be investigated (default: "illinois,cook", "illinois,dekalb", "illinois,du page","illinois,grundy",
#                                                                        "illinois,kane", "illinois,kendall", "illinois,lake", "illinois,mchenry", 
#                                                                        "illinois,will", "indiana,jasper", "indiana,lake", "indiana,newton", 
#                                                                        "indiana,porter", "wisconsin,kenosha")
#----------------------------------------------------------------------------------------------------------------#





#-------------------------------------------------GPSimpute function---------------------------------------------#
GPSimpute = function(data_directory, high.speed = 130, height.change = 1000, impute.gap.size = 5, 
                     counties = c("illinois,cook", "illinois,dekalb", "illinois,du page","illinois,grundy", 
                                  "illinois,kane", "illinois,kendall", "illinois,lake", "illinois,mchenry", 
                                  "illinois,will", "indiana,jasper", "indiana,lake", "indiana,newton", 
                                  "indiana,porter", "wisconsin,kenosha")) {
  
  #--------------------------Read in the raw dataset-----------------------------#
  rawdata = read.csv(paste0(data_directory))
  
  #---------------------------------Tidy data------------------------------------#
  rownum = nrow(rawdata)
  rawdata$speed_mod = as.numeric(unlist(strsplit(rawdata$SPEED," "))[(1:rownum)*3-1])
  
  # Locate values that are at too high of a speed
  hispeed = which(rawdata$speed_mod > high.speed)
  
  # For the height variable, create the change in height 
  rawdata$height_mod = as.numeric(unlist(strsplit(rawdata$HEIGHT," "))[(1:rownum)*3-1])
  
  # Make the value for the last change in height equal to zero. 
  rawdata$height_change = abs(rawdata$height_mod[1:rownum] - rawdata$height_mod[c(2:rownum,rownum)])
  
  # Locate values that are too much of a height change
  height = which(rawdata$height_change > height.change)
  
  #------------------------------Identify counties--------------------------------#
  
  # Returns the county for each lat/long
  counties_list = map.where("county",-rawdata$LONGITUDE,rawdata$LATITUDE)
  
  # Get list of legit counties
  inmsa = c()
  NAs = which(is.na(counties_list))
  for (i in counties) {
    countydata = which(counties_list == i)
    inmsa = c(inmsa, countydata)
  }
  notinmsa = setdiff(1:length(counties_list),inmsa)
  
  # List of values to drop/fix/clean
  other_cts = names(table(counties_list[notinmsa]))
  list_counties = c();
  for (i in 1:length(other_cts)){
    list_counties = paste(list_counties,other_cts[i],sep = ", ")
  }
  
  #---------------------------------GPS Imputation---------------------------------#
  
  # Make a list of all the rows to drop
  drops = c(height,hispeed,notinmsa)
  cleandata = rawdata[-drops,]
  
  # Compute hour
  # Compute minute
  # Compute running minute
  temp = strsplit(cleandata$LOCAL.TIME,":")
  cleandata$hour = as.numeric(unlist(temp)[(1:dim(cleandata)[1])*3-2])
  cleandata$minute = as.numeric(unlist(temp)[(1:dim(cleandata)[1])*3-1])
  
  # Running minute per day, starting at 12AM
  # Minutes since 12AM, 0 to 1440
  cleandata$runningminute = cleandata$hour*60+cleandata$minute;
  
  # Look for gaps in running minutes
  # Gap of 1 minute is expected
  cleandata$gap = cleandata$runningminute[c(2:length(cleandata$runningminute),cleandata$runningminute[length(cleandata$runningminute)])]-cleandata$runningminute[1:length(cleandata$runningminute)]
  
  # Gaps of 1439 are breaks over the start of the day
  cleandata$gap[which(cleandata$gap == -1439)] = 1;
  
  # Negative gaps are breaks of more than 1 day
  # Gaps of 0 are OK because I'm rounding to the nearest minute, and sometimes multiple measures/second
  
  daygap = length(which(cleandata$gap < 0))
  veryshortgaps = length(intersect(which(cleandata$gap <= impute.gap.size),which(cleandata$gap > 1)));
  shortgaps = length(intersect(which(cleandata$gap <= 30),which(cleandata$gap > impute.gap.size)));
  longgaps = length(which(cleandata$gap > 30));
  
  
  
  # Do the imputation on gaps that are very short##
  imputes = intersect(which(cleandata$gap <= impute.gap.size),which(cleandata$gap > 1))
  
  # STEP 1. First create a dataset with NAs for each minute of an imputable gap
  # Create the start of the imputed dataset
  imputeddata = cleandata[1:imputes[1],]
  
  # For each imputable gap insert a set of NAs - one for each minute
  for(i in 1:length(imputes)){
    
    # Create the rows to insert into the dataset
    temp = matrix(NA,nrow = cleandata$gap[imputes[i]],ncol = dim(cleandata)[2])
    colnames(temp) = colnames(imputeddata)
    imputeddata = as.data.frame(rbind(imputeddata,temp))
    
    # Copy over the part of the dataset up until the next gap
    if(i < length(imputes)){
      imputeddata = as.data.frame(rbind(imputeddata,cleandata[c((imputes[i]+1):imputes[i+1]),]))
    }
    # Copy over the part of the dataset up until the end of the dataset
    if(i == length(imputes)){
      imputeddata = as.data.frame(rbind(imputeddata,cleandata[c((imputes[i]+1):dim(cleandata)[1]),]))
    }
    
  }
  
  # STEP 2. Now impute the gaps
  imputesnew = intersect(which(imputeddata$gap <= impute.gap.size),which(imputeddata$gap > 1))
  
  #For each gap to impute
  for(i in 1:length(imputesnew)){
    
    gap = imputeddata$gap[imputesnew[i]];
    
    # Change in latitude across the gap
    latchg = cleandata$LATITUDE[imputesnew[i]+gap+1] - cleandata$LATITUDE[imputesnew[i]];
    
    ##Change in longitude across the gap;
    longchg = cleandata$LONGITUDE[imputesnew[i]+gap+1] - cleandata$LONGITUDE[imputesnew[i]];
    
    ##For each minute in the imputable gap##
    for (j in 1:gap){
      imputeddata$LATITUDE[imputesnew[i]+j] = cleandata$LATITUDE[imputesnew[i]] + latchg/j;
      imputeddata$LONGITUDE[imputesnew[i]+j] = cleandata$LONGITUDE[imputesnew[i]] + longchg/j;
      imputeddata$runningminute[imputesnew[i]+j] = cleandata$runningminute[imputesnew[i]] + j;
    }
  }
  
  output = rbind(
    paste0("Number of input values: ",dim(rawdata)[1]),
    paste0("Number of too fast (> ", high.speed, " km/h): ",length(hispeed)),
    paste0("Number of change height (> ",  height.change, " m/min): ",length(height)),
    paste0("Number of missing county data: ",length(NAs)),
    paste0("Number of non MSA values (including NA): ",length(notinmsa)),
    paste0("State, County's excluded: ",list_counties),
    paste0("Gaps more than 1 day: ",daygap),
    paste0("Gaps 30 min to 1 day: ",longgaps),
    paste0("Gaps ", impute.gap.size+1, "-30 min: ", shortgaps),
    paste0("Imputed gaps 2-", impute.gap.size, " min: ", veryshortgaps),
    paste0("Dataset after clean/impute: ", dim(imputeddata)[1])
  )
  
  # Save imputed dataset
  rawfile.name = basename(data_directory)
  newfile.name = tools::file_path_sans_ext(rawfile.name)
  write.csv(imputeddata, file = paste0(newfile.name,"_cleaned.csv"))
  
  return(output)
}
#----------------------------------------------------------------------------------------------------------------#


#---------------------------------------------------Example------------------------------------------------------#

wd = "C:/Users/tjqja/Box/E3 study/Analysis/2023/0314/" # Can be changed to your working directory
setwd(wd)
data_directory = paste0(wd, "/data files/P2E30021.csv")
GPSimpute(data_directory)
