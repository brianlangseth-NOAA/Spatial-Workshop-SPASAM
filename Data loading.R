library(tidyr)

####
#User directories
####

if(Sys.getenv("USERNAME") == "Brian.Langseth") {
  #data_loc is where you have your YFT data stored (have to clone repo from github)
  #master_loc is where the base level OM and EM reside
  #code_loc is where your code scripts reside (the github repo)
  #mod_loc is where you want to set up your new EM run
  #EM_loc is the folder you want to copy your .tpl from
  
  #data_loc <- "C:\\Users\\Brian.Langseth\\Desktop\\Spatial-Assessment-Modeling-Workshop\\data\\Datasets_current_UseThese"
  #data_loc <- "C:\\Users\\Brian.Langseth\\Desktop\\Spatial-Assessment-Modeling-Workshop\\data\\Datasets_old_DoNotUse"
  data_loc <- "C:\\Users\\Brian.Langseth\\Desktop\\Spatial-Assessment-Modeling-Workshop\\data"
  master_loc <- "C:\\Users\\Brian.Langseth\\Desktop\\Spatial-Workshop-SPASAM\\Operating_Model"
  code_loc <- "C:\\Users\\Brian.Langseth\\Desktop\\Spatial-Workshop-SPASAM"
  mod_loc <- "C:\\Users\\Brian.Langseth\\Desktop\\test"
  tpl_loc <- "C:\\Users\\Brian.Langseth\\Desktop\\Spatial-Workshop-SPASAM\\Shortened105_estSel_Rdevs"
}
if(Sys.getenv("USERNAME") == "jonathan.deroba") {
  #data_loc is where you have your YFT data stored (have to clone repo from github)
  #master_loc is where the base level OM and EM reside
  #mod_loc is where you want to set up your new EM run
  
  #data_loc <- "C:\\Spatial_SPASAM_2021_Sim\\Spatial-Assessment-Modeling-Workshop\\data\\Datasets_current_UseThese"
  #data_loc <- "C:\\Spatial_SPASAM_2021_Sim\\Spatial-Assessment-Modeling-Workshop\\data\\Datasets_old_DoNotUse"
  data_loc <- "C:\\Spatial_SPASAM_2021_Sim\\Spatial-Assessment-Modeling-Workshop\\data"
  master_loc <- "C:\\Spatial_SPASAM_2021_Sim\\Spatial-Workshop-SPASAM-main\\Operating_Model"
  code_loc <- "C:\\Spatial_SPASAM_2021_Sim\\Spatial-Workshop-SPASAM-main"
  mod_loc <- "C:\\Spatial_SPASAM_2021_Sim\\Spatial-Workshop-SPASAM-main"
  tpl_loc <- "C:\\Spatial_SPASAM_2021_Sim\\Spatial-Workshop-SPASAM-main\\Shortened105_estSel_Rdevs"
}

#FOR OTHER USERS, CAN ENTER LOCATIONS HERE ONCE


######################################################
#Read in data from cloned github repository
#Munge the data and then run the script
#DECISION - using "Shortened105_estSel_Rdevs" tpl, if want a specific one need to adjust
######################################################

#One area - can adjust for other datasets
load(file.path(data_loc,'YFT_SRD_1A_4.RData'))
dat <- dat_1A_4
bdat <- biol_dat 
mod_name <- "YFT_1area"
om_rep <- mungeData(mod_name, reduce = NULL, run = FALSE)


#One area with 100 runs - ESS_05 is the base
load(file.path(data_loc,'YFT_SRD_1A_4.RData'))
bdat <- biol_dat #this is only available in the single dataset
load(file.path(data_loc,'YFT_1area_observations_1_100_ESS_05.RData'))
for(i in 1:100){
  dat <- get(paste0("dat_1A_",i))
  mod_name <- paste0("YFT_1area_100sets",i)
  om_rep <- mungeData(mod_name, reduce = 105, run = FALSE)
  cat(paste0("\n Data set",i),"\n ")
}


########################################################################################
#-------FUNCTION TO GET YFT DATA INTO THE PROPER FORMAT FROM THE ORIGINAL OM FILE------#
########################################################################################
#mod_name: the name of the model (name of the .dat, .tpl, and .exe folders)
#reduce: whether years of data should be removed, if so then enter the number of years
#run: whether to run the EM

mungeData <- function(mod_name, reduce = NULL, run = FALSE){


####################
#Set up OM.rep file
####################

####
#Set up folder for new model run
####

mod_name <- mod_name

if(!dir.exists(file.path(mod_loc, mod_name))){
  dir.create(file.path(mod_loc, mod_name))
  dir.create(file.path(mod_loc, mod_name, "Estimation_Model"))
  dir.create(file.path(mod_loc, mod_name, "Operating_Model"))
  print("Directories created")
}


####
#Run the OM to get the .rep file - Only need to do this once (will run if no files currently are in OM folder)
####
setwd(file.path(mod_loc, mod_name, "Operating_Model"))
if(length(list.files())==0){
  file.copy(from = file.path(master_loc, "TIM_OM.exe"), to=getwd()) #Will return FALSE if files already exist
  file.copy(from = file.path(master_loc, "TIM_OM.tpl"), to=getwd()) #Will return FALSE if files already exist
  file.copy(from = file.path(mod_loc, "Panmictic", "Operating_Model", "TIM_OM_all.dat"), to = "TIM_OM.dat") #Will return FALSE if files already exist
  invisible(shell(paste0("TIM_OM.exe"," -nox -nohess"), wait=T))
  file.remove(list.files()[-grep(".rep|.tpl|.exe|.dat", list.files())]) #remove extra files
  print("OM run completed")
}

#Access the .rep file to then enter the YFT data
om_rep <- readLines("TIM_OM.rep",n=-1)


####
#Function to ensure new data length equals current data length
#If equal, outputs TRUE
#If not equal, outputs number of lines to ammend to OM.rep file
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



####################
##Data Munging!!!!
####################

#The following element may be able to be removed after updating SIM_TIM.R (see issue 17)

#Add switch for catch in numbers after the switch for SSB_type
loc <- grep("#SSB_type", om_rep)
om_rep <- append(om_rep, c("#catch_num_switch",1), after = (loc+1))


####
#Catches
####

#Catch - OBS_yield_fleet
loc <- grep("#OBS_yield_fleet$", om_rep)
new_val <- tidyr::unite(
  round(dat$catch[,1:7], 3), #DECISION - round to 3 decimals
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
#So Create age-length key taken from Jon's ALK.R and modified
#DECISION - dat$lbin_vector final bin is 198, whereas dat$lencomp final bin is 200. Assume it should be 200
#DECISION - set end of final lbin to be 205 cm (as opposed to ongoing) Thus assume the catches in bin 200 go up to 205
#DECISION - assume lognormal error for distribution of lengths around an age
#DECISION - round pdf probability to 2 sig digits (which truncates the possible ages with probability)
#DECISION - not using precise se (not using CV to SE conversion)
lengths=seq(10,205,by=5)
Latage=c(22,
         35.2865,
         41.384,
         45.7348,
         49.904,
         53.8991,
         57.7273,
         64.2198,
         74.721,
         85.5489,
         96.3811,
         105.259,
         112.534,
         118.496,
         123.383,
         127.387,
         130.669,
         133.359,
         135.563,
         137.369,
         138.85,
         140.063,
         141.058,
         141.872,
         142.54,
         143.088,
         143.536,
         144.168)
#Function to calculate probability in a given length interval
getprob=function(L1=NULL,L2=NULL,meanL=NULL,sd=NULL){
  #prob=pnorm(L2,mean=meanL,sd=sd)-pnorm(L1,mean=meanL,sd=sd)
  prob=plnorm(L2,meanlog=meanL,sdlog=sd)-plnorm(L1,meanlog=meanL,sdlog=sd)
  return(prob)
}
#matrix to hold alk
alk=matrix(NA,nrow=length(Latage),ncol=(length(lengths)-1),dimnames = list(paste0("a",seq(1:length(Latage))),paste0("l",(lengths[1:(length(lengths)-1)]))))
for(a in 1:length(Latage)){ #loop over 28 ages and the length bins and calculate probility for each bin
  for(l in 1:(length(lengths)-1)){
    alk[a,l]=getprob(L1=lengths[l],L2=lengths[l+1],meanL=log(Latage[a]),sd=0.1)
  }
}
alk=round(alk,digits=2)
noentry = which(colSums(alk)==0) #determine which lengths dont have any entries
alk[1,noentry[1]]=1 #set the first column to be 1 for the first age 1
alk[dat$Nages,noentry[-1]]=1 #set the last columns to be 1 for the last age
alk=t(t(alk)/colSums(alk)) #divided by column sums to produce P(A|L); i.e. column sums among ages = 1
#Convert length comps to age comps
#DECISION - assume OBS_catch_prop is blocked in each year for all fleets (year 1 for all fleets, then year 2 for all fleets, etc.)
agecomp <- as.matrix(dat$lencomp[7:ncol(dat$lencomp)]) %*% t(alk)
agecomp_prop <- agecomp/rowSums(agecomp)
agecomp_yr <- cbind("Yr" = dat$lencomp$Yr, "FltSvy" = dat$lencomp$FltSvy, agecomp_prop)
#Put into om_rep file
loc <- grep("#OBS_catch_prop$", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr*dat$Nfleet, ncol = dat$Nages) #Set up for all years and fleets
tmp_val[(dat$Nfleet * (agecomp_yr[,"Yr"] - 1) + agecomp_yr[,"FltSvy"]),] <- agecomp_yr[,-c(1,2)] #Assign for just the years and fleets in YFT data. Each year for all fleets. 
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr*dat$Nfleet)] <- new_val

#Sample size of comps - #OBS_catch_prop_N_EM
loc <- grep("#OBS_catch_prop_N_EM", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr, ncol = dat$Nfleet) #Set up for all years and fleets
tmp_val[cbind(dat$lencomp$Yr, dat$lencomp$FltSvy)] <- dat$lencomp$Nsamp #Assign for just the years and fleets in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr)] <- new_val


####
#Survey
####

#Survey index - #OBS_survey_fleet_bio
loc <- grep("#OBS_survey_fleet_bio$", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr, ncol = dat$Nsurveys) #Set up for all years and surveys
tmp_val[cbind(as.numeric(levels(dat$CPUE$year)),dat$Nsurveys)] <- dat$CPUE$cpu #Assign for just the years and surveys in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr)] <- new_val

#True survey index - #true_survey_fleet_bio
#This isn't used when diagnostic_switch is 0 (our default) but still change here
loc <- grep("#true_survey_fleet_bio$", om_rep)
om_rep[(loc + 1):(loc + dat$endyr)] <- new_val

#Survey index SE - #OBS_survey_fleet_bio_se_EM
loc <- grep("#OBS_survey_fleet_bio_se_EM", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr, ncol = dat$Nsurveys) #Set up for all years and surveys
tmp_val[cbind(as.numeric(levels(dat$CPUE$year)),dat$Nsurveys)] <- dat$CPUE$cv #Assign for just the years and surveys in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr)] <- new_val
#DECISION - not using precise se (not using CV to SE conversion)

#Survey Comps - #OBS_survey_prop set to #OBS_catch_prop for fleet 3
#DECISION - copying from proportions in fleet 3 catch for only years that overlap with cpue years. 
#Believe these to be the same as the fleet comps. Can set survey select to that of catch
loc <- grep("#OBS_survey_prop$", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr*dat$Nsurveys, ncol = dat$Nages) #Set up for all years and surveys
#years of comp data for fleet 3 that overlap with years of CPUE data
yrs_comp <- agecomp_yr[agecomp_yr[,"FltSvy"]==3,"Yr"][agecomp_yr[agecomp_yr[,"FltSvy"]==3,"Yr"] %in% dat$CPUE$year] 
tmp_val[yrs_comp,] <- agecomp_yr[agecomp_yr[,"FltSvy"] == 3 & agecomp_yr[,"Yr"]%in%yrs_comp, -c(1:2)]
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr)] <- new_val

#Sample size of comps - #OBS_survey_prop_N_EM
#DECISION - copying from proportions in fleet 3 catch for only years that overlap with cpue years.  
#Believe these to be the same as the fleet comps. Can set survey select to that of catch.
#DECISION - What is NCPUEObs though?
loc <- grep("#OBS_survey_prop_N_EM", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr, ncol = dat$Nsurveys) #Set up for all years and surveys
tmp_val[yrs_comp,] <- dat$lencomp[dat$lencomp$FltSvy==3 & dat$lencomp$Yr%in%yrs_comp,]$Nsamp #Assign for just the years and fleets in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr)] <- new_val


####
#Rec index
####

#Rec index - #OBS_rec_index_BM
#DECISION - set to zero
loc <- grep("#OBS_rec_index_BM", om_rep)
tmp_val <- rep(0, dat$endyr)
new_val <- paste(tmp_val, collapse = " ")
om_rep[(loc + 1)] <- new_val


####
#Tagging
####

#Have tag switch on - #do_tag
loc <- grep("#do_tag$", om_rep)
om_rep[(loc + 1)] <- 1

#Number of years with tag releases - #nyrs_release
loc <- grep("#nyrs_release", om_rep)
om_rep[(loc + 1)] <- length(unique(dat$tag_releases$yr)) #NOT equal to dat$N_tag_groups (which is number tag release events, not years)

#Years of tag releases - #years_of_tag_releases
loc <- grep("#years_of_tag_releases", om_rep)
tmp_val <- unique(dat$tag_releases$yr)
om_rep[(loc + 1)] <- paste(tmp_val, collapse = " ")

#Lifespan of tags
#DECISION - Set to fixed value (originally assumed it equals to maximum difference in year recapture from year released within data)
#Originally had as max_periods but that is SS-speak and is the periods after which tags go into the accumulator
#age. See issue #6: https://github.com/aaronmberger-nwfsc/Spatial-Assessment-Modeling-Workshop/issues/6
full_recap_info <- merge(dat$tag_releases, dat$tag_recaps, by = "tg") #combine tag release and recapture information by tag (tg)
full_recap_info$yr_diff <- full_recap_info$yr.y - full_recap_info$yr.x
table(full_recap_info$yr_diff)
maxlife <- 18 #max(full_recap_info$yr_diff) #16 is 90% quantile, 18 is 92%, 21 is 95%
loc <- grep("#max_life_tags", om_rep)
om_rep[(loc + 1)] <- maxlife

#DECISION - age of full selection (kept currently at 8)

#Tag report rate - #...report_rate...
#DECISION - fix at true value from OM (but reset EM to be the same)
#but dont have likelihood penalty (wt_B_pen = 0) so dont need report_rate_ave or report_rate_sigma
#Use based on analyst guidance document (https://aaronmberger-nwfsc.github.io/Spatial-Assessment-Modeling-Workshop/articles/Analyst_guidance.html)
loc <- grep("#report_rate_switch", om_rep)
om_rep[(loc + 1)] <- 0 #stay at 0
loc <- grep("#input_report_rate_EM", om_rep)
om_rep[(loc + 1)] <- 0.9
loc <- grep("#report_rate_TRUE", om_rep)
new_val <- rep(0.9, length(unique(dat$tag_releases$yr)))
#Check if number of entries dont match up. Append new lines before replacing
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
if(!is.logical(add_lines)) {
  om_rep <- append(om_rep, new_val[1:add_lines], after = loc)
}
om_rep[(loc + 1):(loc + length(new_val))] <- new_val
#DECISION - there is no tag retention or tag loss in the TIM yet. It is in YFT

#Tags released - #ntags
#YFT data already adjusts for initial tagging mortality
#DECISION - assume tag releases of age 20 fish are actually age 20 fish. In reality, age 20 is a plus group for tagging
loc <- grep("#ntags$", om_rep)
tmp_val <- matrix(0, nrow = length(unique(dat$tag_releases$yr)), ncol = dat$Nages) #Set up for all years and surveys
tmp_val[cbind(as.numeric(factor(dat$tag_releases$yr)), dat$tag_releases$age)] <- dat$tag_releases$nrel #Assign for just the years and fleets in YFT data
tags_yr_age <- tmp_val #save for later
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
#Check if number of entries dont match up. Append new lines before replacing
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
if(!is.logical(add_lines)) {
  om_rep <- append(om_rep, new_val[1:add_lines], after = loc)
}
om_rep[(loc + 1):(loc + length(new_val))] <- new_val

#Total tags - #ntags_total
#DECISION - set equal to number of tags (ntags)
loc <- grep("#ntags_total", om_rep)
new_val <- paste(rowSums(tags_yr_age), collapse = " ") #from ntags script
om_rep[(loc + 1)] <- new_val

#Tag sample size - #tag_N_EM
#DECISION - SET ARBITRARILY TO 200
#Could set to actual sample size but may swamp likelihood
loc <- grep("#tag_N_EM", om_rep)
tmp_val <- matrix(0, nrow = length(unique(dat$tag_releases$yr)), ncol = dat$Nages) #Set up for all years and ages
tmp_val[cbind(as.numeric(factor(dat$tag_releases$yr)), dat$tag_releases$age)] <- 200 #Assign for just the years and ages in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
#Check if number of entries dont match up. Append new lines before replacing
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
if(!is.logical(add_lines)) {
  om_rep <- append(om_rep, new_val[1:add_lines], after = loc)
}
om_rep[(loc + 1):(loc + length(new_val))] <- new_val

#Tag recaptures - #OBS_tag_prop_final
#Final column (which represents the possible recpature years + 1) covers all tags that were not recovered
#Rows are entered as ages (28) for each release event (10) (so in blocks of 28 rows)
#DECISION - lifespan of tags decision is used here. If adjusted, these data are adjusted too
#DECISION - currently ignoring tag mixing rate (dat$mixing_latency_period), which is SS-speak. Counting
#all early recaps regardless of how soon recaptured
loc <- grep("#OBS_tag_prop_final$", om_rep)
#Set up dimensions for nt (number of years where recaptures are possible)
xy = nt = NULL
for(i in 1:length(unique(dat$tag_releases$yr))){ #Script from TM.tpl to set dimensions
  xx <- unique(dat$tag_releases$yr)[i]
  xy[i] <- min(maxlife,dat$endyr-xx+1)
  nt[i] <- xy[i]*dat$N_areas+1
}
#Set up array for tag recaptures by age (row), by year when recaptures possible (column), by release year (3rd-dimension)
#DECISION - ignore all recaptures made after the entered maxlife of tags 
if(length(unique(nt)) == 1){
  tmp_val <- array(0, dim = c(dat$Nages, max(nt), length(unique(dat$tag_releases$yr)))) #numbers
  tmp_val_prop <- tmp_val #proportions
}
#Add number of recaptures
tmp_recap_info <- full_recap_info[full_recap_info$yr_diff <= maxlife, ]
tmp_val[cbind(tmp_recap_info$age, tmp_recap_info$yr_diff, as.numeric(factor(tmp_recap_info$yr.x)))] <- tmp_recap_info$recaps #Assign for just the years and ages in YFT data
#Add in number of tags not captured (nt'th column) based on difference between total tags and recaptures at age
#across possible recapture periods for each release event
tmp_val[, max(nt), ] <- t(tags_yr_age) - apply(tmp_val, c(1,3), FUN = sum)
#Due to rounding (Aaron Berger's email on Jan 25, 2022) there may be small negative numbers, 
#DECISION - Where recaptures were higher than releases set number of recaptures (which are integers) to 
#equal the slightly lower number of tags released (which are doubles) and set non-captures to 0
tmp_val[16,c(2,max(nt)),1] <- c(tmp_val[16,2,1] + tmp_val[16,max(nt),1], 0)
#Change to proportions
#Sum number of tags for each age across possible recapture periods for each release event
for(i in 1:dim(tmp_val)[3]){
  tmp_prop <- tmp_val[,,i]/(apply(tmp_val, c(1,3), FUN = sum)[,i])
  tmp_val_prop[,,i][which(tmp_prop!=0)] <- tmp_prop[which(tmp_prop!=0)]
}
new_val <- apply(tmp_val_prop, c(1,3), FUN = paste, collapse = " ")
new_val <- c(new_val)
#Check if number of entries dont match up. Append new lines before replacing
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
if(!is.logical(add_lines)) {
  om_rep <- append(om_rep, new_val[1:add_lines], after = loc)
}
om_rep[(loc + 1):(loc + length(new_val))] <- new_val

#True tag recaptures - #TRUE_tag_prop
#Set to be the same as OBS tag recaptures
loc <- grep("#TRUE_tag_prop$", om_rep)
#Check if number of entries dont match up. Append new lines before replacing
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
if(!is.logical(add_lines)) {
  om_rep <- append(om_rep, new_val[1:add_lines], after = loc)
}
om_rep[(loc + 1):(loc + length(new_val))] <- new_val

#Tag recaptures no age - #OBS_tag_prop_final_no_age
loc <- grep("#OBS_tag_prop_final_no_age", om_rep)
tmp_val_noage <- apply(tmp_val, c(3,2), FUN = sum)
tmp_val_noage_prop <- tmp_val_noage/rowSums(tmp_val_noage)
new_val <- apply(tmp_val_noage_prop, 1, FUN = paste, collapse = " ")
#Check if number of entries dont match up. Append new lines before replacing
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
if(!is.logical(add_lines)) {
  om_rep <- append(om_rep, new_val[1:add_lines], after = loc)
}
om_rep[(loc + 1):(loc + length(new_val))] <- new_val

#True tag recaptures no age - #TRUE_tag_prop_no_age
#Set to be the same as OBS tag recaptures
loc <- grep("#TRUE_tag_prop_no_age", om_rep)
#Check if number of entries dont match up. Append new lines before replacing
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
if(!is.logical(add_lines)) {
  om_rep <- append(om_rep, new_val[1:add_lines], after = loc)
}
om_rep[(loc + 1):(loc + length(new_val))] <- new_val

#F scalar when tag mixing is incomplete - #F_tag_scalar
#DECISION - Copy the 7th release value into the 8th-10th
#Could resolve by fixing the OM.dat nyrs_release to 10 and rerunning
loc <- grep("#F_tag_scalar", om_rep)
tmp_val <- c(om_rep[loc+1], rep(0.73,3)) #add three more entries (TO DO: the 3 entries are HARD CODED, so possibly revise)
new_val <- paste(tmp_val, collapse = " ")
om_rep[(loc + 1)] <- new_val

#Residency rate of tagged fish in year of release - #T_tag_res
#DECISION - Copy the 7th release value into the 8th-10th
#Could resolve by fixing the OM.dat nyrs_release to 10 and rerunning
loc <- grep("#T_tag_res", om_rep)
tmp_val <- c(om_rep[loc+1], rep(0.73,3)) #add three more entries (TO DO: the 3 entries are HARD CODED, so possibly revise)
new_val <- paste(tmp_val, collapse = " ")
om_rep[(loc + 1)] <- new_val


##
#Selectivity
##

#The following two elements may be able to be removed after updating SIM_TIM.R (see issue 17)

#Update selectivity switches to differ by fleet
loc <- grep("#select_switch$", om_rep)
om_rep[loc+1] <- c("2 2 1 2 2 2 2")

#Update selectivity switches to differ by survey fleet
loc <- grep("#select_switch_survey", om_rep)
om_rep[loc+1] <- "1"

#Indicate whether there is a mirror fleet
new_val <- "3" #mirroing fleet 3. 0 is no mirroring
om_rep <- append(om_rep, rbind("#survey_mirror",c(new_val)), after = loc+1)

#If have mirror then turn off survey selectivity and set weight of survey age comp to zero
if(new_val != "0"){
  loc <- grep("#ph_sel_log_surv", om_rep)
  om_rep[loc+1] <- "-2"
  loc <- grep("#ph_sel_dubl_surv", om_rep)
  om_rep[loc+1] <- "-5"
  loc <- grep("#wt_srv_age", om_rep)
  om_rep[loc+1] <- "0"
}


##
#Biological data
##

#Steepness TRUE - #steep
loc <- grep("#steep$", om_rep)
om_rep[(loc + 1)] <- 0.8

#Steepness starting value - #steep_start
loc <- grep("#steep_start", om_rep)
om_rep[(loc + 1)] <- 0.8

#R average TRUE - #R_ave
loc <- grep("#R_ave", om_rep)
om_rep[(loc + 1)] <- bdat$B0/1000 #from OM description this IS R0

#R average start in ln space - #Rave_start
loc <- grep("#Rave_start", om_rep)
om_rep[(loc + 1)] <- log(bdat$B0/1000) #from OM description this IS R0

#R average mean in ln space for penalizing from - #Rave_mean
loc <- grep("#Rave_mean", om_rep)
om_rep[(loc + 1)] <- log(bdat$B0/1000) #from OM description this IS R0

#Variance in recruitment - #sigma_recruit_EM
#DECISION - sigma r of 0.6
loc <- grep("#sigma_recruit", om_rep)
om_rep[(loc + 1)] <- 0.6 

#Set recruitment deviations starting value to account for variability so that after bias correction values are 1
loc <- grep("#Rdevs_start", om_rep)
om_rep[(loc + 1)] <- 0.18 

#Weight at age - #input_weight
loc <- grep("#input_weight", om_rep)
tmp_val <- round(bdat$a*bdat$L^bdat$b, 3) 
om_rep[(loc + 1)] <- paste(tmp_val, collapse = " ") 

#Weight at age in catch - #input_catch_weight
loc <- grep("#input_catch_weight", om_rep)
tmp_val <- round(bdat$a*bdat$L^bdat$b, 3) 
om_rep[(loc + 1)] <- paste(tmp_val, collapse = " ") 

#Maturity at age - #maturity
loc <- grep("#maturity$", om_rep)
tmp_val <- bdat$Maturity
om_rep[(loc + 1)] <- paste(tmp_val, collapse = " ")

#Fecundity - #fecundity
#DECISION - keep as 1 for all ages

#Prop_fem - #prop_fem
#DECISION - keep as 0.5

#Mortality at age - #input_M_EM
#DECISION - Im also doing this for input_M_TRUE and input_M_EM
loc <- grep("#input_M", om_rep)
tmp_val <- bdat$M
om_rep[(loc + 1)] <- paste(tmp_val, collapse = " ")


##
#Switches for penalties and weighting
##

#Penalty for recruitment being different from Rave_mean
loc <- grep("#Rave_pen_switch", om_rep)
om_rep[(loc + 1)] <- 1
loc <- grep("#wt_Rave_pen", om_rep)
om_rep[(loc + 1)] <- 10

#Tagging weight to include tag likelihood 
loc <- grep("#wt_tag", om_rep)
om_rep[(loc + 1)] <- 0

#Initial abundance set up and penalty for initial value at age being different from mean_N
loc <- grep("#init_abund_switch", om_rep) #decaying from Rave
om_rep[(loc + 1)] <- 1
loc <- grep("#abund_pen_switch", om_rep) #keep off
om_rep[(loc + 1)] <- 0
loc <- grep("#wt_abund_pen", om_rep) #keep off
om_rep[(loc + 1)] <- 0.1


##
#Bounds
##

#Balance initial abundance bounds so that init_abund_devs in report become 1 if fixed (so they are set to their midpoint before exp)
loc <- grep("#lb_init_dist", om_rep)
om_rep[(loc + 1)] <- -10
loc <- grep("#ub_init_dist", om_rep)
om_rep[(loc + 1)] <- 10
loc <- grep("#lb_init_abund", om_rep)
om_rep[(loc + 1)] <- -10
loc <- grep("#ub_init_abund", om_rep)
om_rep[(loc + 1)] <- 10

#Catchability - reduce lower bound
loc <- grep("#lb_q", om_rep)
om_rep[(loc + 1)] <- -15

#Selectivity parameters
loc <- grep("lb_sel_beta", om_rep) #lower bounds for all fleets and surveys
om_rep[(loc + 1)] <- -5
loc <- grep("ub_sel_beta", om_rep) #uppers bounds for all fleets and surveys
om_rep[(loc + 1)] <- 3
loc <- grep("^#sel_beta1.*start$", om_rep) #start for fleet and survey
om_rep[(loc + 1)] <- 0
loc <- grep("^#sel_beta2.*start$", om_rep) #start for fleet and survey
om_rep[(loc + 1)] <- 2
loc <- grep("^#sel_beta3.*start$", om_rep) #start for fleet and survey
om_rep[(loc + 1)] <- -0.7
loc <- grep("^#sel_beta4.*start$", om_rep) #start for fleet and survey
om_rep[(loc + 1)] <- 1

##Done Munging##


####
#Save YFT model as .dat file, copy .tpl over from tpl_loc
####

setwd(file.path(mod_loc, mod_name, "Estimation_Model"))
writeLines(om_rep, paste0(mod_name,".dat"))
file.copy(from = file.path(tpl_loc, "YFT_1area.tpl"), to=file.path(getwd(), paste0(mod_name,".tpl"))) #Will return FALSE if files already exist
shell(paste0("admb ",mod_name)) #build .exe from the .tpl

####
#If want to remove years from the data
####

if(!is.null(reduce)) { #for removing years from the data
  source(file.path(code_loc,"Remove_data_years.R"))
  #Read in existing model .dat file
  new_dat <- remove_years(reduce, mod_name)
  writeLines(new_dat, file.path(getwd(), paste0(mod_name,".dat")))
  om_rep <- new_dat
  print(paste("First", reduce, "years of data removed"))
}


####
#Run model if desired - DECISION: currently running with nohess
####

if(run){
  #Run script - Ensure there is an .exe file. May need to create one manually and rename it
  system.time({
    invisible(shell(paste0(mod_name, ".exe"," -nohess -nox -ind"), wait=T))
    #invisible(shell(paste0(mod_name, ".exe"," -nox -ind"), wait=T))
  })
}

return(om_rep)

} #End of mungeData function



##########################################################################################
##--------- If want to remove years of data from a model already ran use the following ---------##
##########################################################################################

# source(file.path(code_loc,"Remove_data_years.R"))
# 
# #Read in existing model .dat file
# dir <- "C:\\Users\\Brian.Langseth\\Desktop\\test\\Newest tagging data\\explorations"
# setwd(file.path(dir, "20_no0catch"))
# new_dat <- remove_years(80)
# new_folder <- "21_no0catch_short80"
# writeLines(new_dat, file.path(dir, new_folder, paste0("YFT_1area.dat")))
# #Copy over .exe and .tpl files
# file.copy(from = file.path("YFT_1area.exe"), to=file.path(dir, new_folder, paste0(mod_name, ".exe"))) #Will return FALSE if files already exist
# file.copy(from = file.path("YFT_1area.tpl"), to=file.path(dir, new_folder, paste0(mod_name, ".tpl"))) #Will return FALSE if files already exist
# #Run script
# setwd(file.path(dir, new_folder))
# system.time({
#   invisible(shell(paste0(mod_name, ".exe"," -nohess -nox -ind"), wait=T))
# })



