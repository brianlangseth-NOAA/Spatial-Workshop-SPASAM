library(tidyr)

####
#User directories
####

if(Sys.getenv("USERNAME") == "Brian.Langseth") {
  #data_loc is where you have your YFT data stored (have to clone repo from github)
  #master_loc is where the base level OM and EM reside
  #mod_loc is where you want to set up your new EM run
  
  data_loc <- "C:\\Users\\Brian.Langseth\\Desktop\\Spatial-Assessment-Modeling-Workshop\\data"
  master_loc <- "C:\\Users\\Brian.Langseth\\Desktop\\test\\sel F set to OM\\Opearting_Model"
  mod_loc <- "C:\\Users\\Brian.Langseth\\Desktop\\Spatial-Workshop-SPASAM"
}


######################################################
#Read in data from github
######################################################

#One area - can adjust for other datasets
load(file.path(data_loc,'YFT_SRD_1A_4_v2.Rdata'))
dat <- dat_1A_4


####
#Access model files
####

#CURRENTLY WE HAVE TO PICK THE .REP FILE FROM A RUN SET UP WITH THE CORRECT FORMAT (AGE, FLEET, AREA, etc.) 
#THIS WAS FROM A RUN WITH JON's TIM_OM_all.dat
#ENTER THE YFT DATA INTO THAT THE OM.rep FILE
#BEST TO START FRESH AND FULLY AUTOMATE FILE GENERATION BUT NOT THERE AT THE MOMENT
om_rep <- readLines(file.path(master_loc,"TIM_OM.rep"),n=-1)


#Set up folder for new model run
mod_name <- "YFT_1area"

if(!dir.exists(file.path(mod_loc, mod_name))){
  dir.create(file.path(mod_loc, mod_name))
  dir.create(file.path(mod_loc, mod_name, "Operating_Model"))
  print("Directories created")
}


####
#Function to ensure new data length equals current data length
#If equal, outputs TRUE
#If not equal, outputs number of lines to ammed to OM.rep file
####
#TO DO (FOR LATER): add element to append automatically if doesnt equal
check_entry = function(loc, loc_end, new_entry){
  entries <- grep("#", om_rep)
  
  length_data <- length(om_rep[(loc+1):loc_end]) #length of old data
  length_entry <- length(new_entry) #length of new data
  
  #If the lines being added are same as lines already there, replace old lines with new lines new data
  if(length_data==length_entry) { 
    return(TRUE)
  }else{ 
    #If there are more lines being added than the lines already there, append new lines to make up the difference and then replace
    length_element <- (entries[which(entries %in% loc)+1]-1)-loc
    add <- length_entry - length_element
    return(add)
  }
}



#####################################################
#Data Munging!!!!
#####################################################



####
#Catches
####

#Catch - OBS_yield_fleet
table(dat$catch$seas)#check to ensure seas is the same DECISION - assumed seas 1 is start of year
loc <- grep("#OBS_yield_fleet$", om_rep)
new_val <- tidyr::unite(
  round(dat$catch[1:dat$Nfleet], 3), #DECISION - round to 3 decimals
  sep = " ", 
  col = "new_val")
om_rep[(loc + 1):(loc + dat$endyr)] <- new_val$new_val

#Catch SE - OBS_yield_fleet_se_EM
loc <- grep("#OBS_yield_fleet_se_EM", om_rep)
new_val <- rep(paste(dat$se_log_catch, collapse = " "),dat$endyr)
om_rep[(loc + 1):(loc + dat$endyr)] <- new_val


####
#Comps
####

#Length Comp - #OBS_catch_prop
#For TIM, the comp data is of age comps by year
#For YFT data, the comp data is of length comps by year

#Convert lengths to ages


##TO DO Need to complete - cannot wrap head around how to do this--------  

##Get length bins - initial work on this. Likely need to replace
data_lbins <- as.numeric(substr(colnames(dat$lencomp)[grep("^l", colnames(dat$lencomp))],2,100))
#diff <- data_lbins[-1]-data_lbins[-length(data_lbins)] #Confirm differences are the same
#data_lbins_mids <- data_lbins + unique(diff)/2  #DECISION - Assume length in bins is at midpoint (including plus group)

#TO DO Need to complete - cannot wrap head around how to do this

rlnorm(100,log(biol_dat$L[2]),sqrt(log(1+cv_l^2)))

#End length comp conversion section -------------------------------------


#Sample size of comps - #OBS_catch_prop_N_EM
loc <- grep("#OBS_catch_prop_N_EM", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr, ncol = dat$Nfleet) #Set up for all years and fleets
tmp_val[dat$lencomp$Yr, dat$lencomp$FltSvy] <- dat$lencomp$Nsamp #Assign for just the years and fleets in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr)] <- new_val


####
#Survey
####

#Survey index - #OBS_survey_fleet_bio
loc <- grep("#OBS_survey_fleet_bio$", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr, ncol = dat$Nsurveys) #Set up for all years and surveys
tmp_val[as.numeric(levels(dat$CPUE$year)),dat$Nsurveys] <- dat$CPUE$cpu #Assign for just the years and surveys in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr)] <- new_val

#Survey index SE - #OBS_survey_fleet_bio_se_EM
loc <- grep("#OBS_survey_fleet_bio_se_EM", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr, ncol = dat$Nsurveys) #Set up for all years and surveys
tmp_val[as.numeric(levels(dat$CPUE$year)),dat$Nsurveys] <- dat$CPUE$cv #Assign for just the years and surveys in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr)] <- new_val
#DECISION - not using precise se (not using CV to SE conversion)

#Survey Comps - #OBS_catch_prop
#DECISION - Believe these to be the same as the fleet comps. Can set survey select to that of catch

#Sample size of comps - #OBS_survey_prop_N_EM
#DECISION - Believe these to be the same as the fleet comps. Can set survey select to that of catch. Right now copying from catch
loc <- grep("#OBS_survey_prop_N_EM", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr, ncol = dat$Nsurvey) #Set up for all years and surveys
tmp_val[dat$lencomp[dat$lencomp$FltSvy==3,]$Yr, dat$Nsurvey] <- dat$lencomp[dat$lencomp$FltSvy==3,]$Nsamp #Assign for just the years and fleets in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr)] <- new_val


####
#Tagging
####

#Number of years with tag releases - #nyrs_release
loc <- grep("#nyrs_release", om_rep)
om_rep[(loc + 1)] <- length(unique(dat$tag_releases$yr)) #NOT equal to dat$N_tag_groups

#Years of tag releases - #years_of_tag_releases
loc <- grep("#years_of_tag_releases", om_rep)
tmp_val <- unique(dat$tag_releases$yr)
om_rep[(loc + 1)] <- paste(tmp_val, collapse = " ")

#DECISION - need to define lifespan for tags (assume it equals to max_periods)
loc <- grep("#max_life_tags", om_rep)
om_rep[(loc + 1)] <- dat$max_periods

#DECISION - age of full selection (kept currently at 8)

#DECISION - tag report rate (assume to be 1)
loc <- grep("#input_report_rate_EM", om_rep)
om_rep[(loc + 1)] <- 1

#Tags released - #ntags
#YFT data already adjusts for initial tagging mortality
#Entry numbers dont align so adding check
loc <- grep("#ntags$", om_rep)
tmp_val <- matrix(0, nrow = length(unique(dat$tag_releases$yr)), ncol = dat$Nages) #Set up for all years and surveys
tmp_val[as.numeric(factor(dat$tag_releases$yr)), dat$tag_releases$age] <- dat$tag_releases$nrel #Assign for just the years and fleets in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
#Check that number of entries dont match up. Append new lines before replacing
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
om_rep <- append(om_rep, new_val[1:add_lines], after = loc)
om_rep[(loc + 1):(loc + length(new_val))] <- new_val

#Total tags - #ntags_total
#DECISION set equal to number of tags (ntags) rounded up
loc <- grep("#ntags_total", om_rep)
new_val <- paste(ceiling(rowSums(tmp_val)), collapse = " ") #from ntags script
om_rep[(loc + 1)] <- new_val

#Tag sample size - #tag_N_EM
#DECISION NEED TO DECIDE WHAT TO USE - SET ARBITRARILY TO 200
#Entry numbers dont align so adding check
loc <- grep("#tag_N_EM", om_rep)
tmp_val <- matrix(0, nrow = length(unique(dat$tag_releases$yr)), ncol = dat$Nages) #Set up for all years and ages
tmp_val[as.numeric(factor(dat$tag_releases$yr)), dat$tag_releases$age] <- 200 #Assign for just the years and ages in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
#Check that number of entries dont match up. Append new lines before replacing
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
om_rep <- append(om_rep, new_val[1:add_lines], after = loc)
om_rep[(loc + 1):(loc + length(new_val))] <- new_val

#Tag recaptures - #OBS_tag_prop_final
#Dont know how to enter recaptures (i.e. unsure of format within TIM)



##------------------------Done Munging--------------------------------------------##

####
#Run YFT model
####


#TO DO: add running scripts here or work with SIM_TIM_editing.R





####
#TO DO OR CONFIRM THAT WE DONT NEED TO DO
####

#UNITS OF CATCH - do we use in TIM anywhere. YRT are in number
#CPUE - how to enter blanks for years without data. If we enter 0 does it read as such or as empty
#WHAT TIM DATA SET TO USE - Fill in EM.dat file or OM.dat file
#SURVEY DATA - I dont see survey comps nor survey sample size in YFT data
#SURVEY COMPS - In YFT I believe these are the same as the fleet comps. Thus either reuse survey comps or exclude
#and fix survey selectivity to be the same as the fleet selectivity. Currently, Ive reused. 
#SURVEY TIMING - I dont understand why dat$surveytiming = 0.5 (as is fleetinfo1 for llcpue) yet CPUE is for season 1. What is the timing
#TAGGING in general. YFT tagging set up is much less details than TIM. Currently missing effN, tag mortality, report rate, lifespan of tag, etc
