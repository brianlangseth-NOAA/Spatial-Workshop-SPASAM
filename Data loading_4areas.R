library(tidyr)
library(tidyverse)
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
  #code_loc is where your code scripts reside (the github repo)
  #mod_loc is where you want to set up your new EM run
  #EM_loc is the folder you want to copy your .tpl from
  
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
#DECISION - using generalized one area tpl, if want a specific one need to adjust
######################################################

#Four areas - can adjust for other datasets
#OM is set up for 4 fleets so fleetcombo NEEDS to be TRUE
load(file.path(data_loc,'YFT_SRD_4A_4.RData'))
dat <- dat_4A_4
bdat <- biol_dat 
mod_name <- "YFT_4area_4fleets"
om_rep <- mungeData(mod_name, reduce = NULL, run = FALSE, fleetcombo = TRUE)

#Four areas - can adjust for other datasets
#OM is also set up for 7 fleets so here fleetcombo is FALSE
load(file.path(data_loc,'YFT_SRD_4A_4.RData'))
dat <- dat_4A_4
bdat <- biol_dat 
mod_name <- "YFT_4area_7fleets"
om_rep <- mungeData(mod_name, reduce = NULL, run = FALSE, fleetcombo = FALSE)

#Four areas - can adjust for other datasets
#OM is also set up for 7 fleets so here fleetcombo is FALSE
load(file.path(data_loc,'YFT_SRD_4A_4.RData'))
dat <- dat_4A_4
bdat <- biol_dat 
mod_name <- "YFT_4area_7fleets_105"
om_rep <- mungeData(mod_name, reduce = 105, run = FALSE, fleetcombo = FALSE)



########################################################################################
#-------FUNCTION TO GET YFT DATA INTO THE PROPER FORMAT FROM THE ORIGINAL OM FILE------#
########################################################################################
#mod_name: the name of the model (name of the .dat, .tpl, and .exe folders)
#reduce: whether years of data should be removed, if so then enter the number of years
#run: whether to run the EM
#fleetcombo: do you want combine some fleets (TRUE or FALSE); define fleet combos in newfleets list below

mungeData <- function(mod_name, reduce = NULL, run = FALSE, fleetcombo=FALSE){
  
###########
#if desired then combine data to reduce number of fleets
#The OM is set up either for four fleets or seven fleets, will need to edit if want something different
###########
if(fleetcombo == "TRUE") newfleets=list(c(1,2,7),3,c(4,6),5)
if(fleetcombo == "FALSE") newfleets=list(c(1),c(2),c(3),c(4),c(5),c(6),c(7))

fleetsname = c("fishing_gi","fishing_hd","fishing_ll","fishing_other","fishing_bb","fishing_ps","fishing_trol")
newfleets_names=lapply(newfleets, FUN = function(x) fleetsname[x])

#quick loop to sum catches etc.
for(f in 1:length(newfleets)){
  for(a in 1:dat$N_areas){
    if(f==1 & a==1){
      #sum total catches by desired fleet combos and areas
      newcatch=data.frame(rowSums(dat$catch[
        intersect(
          grep(paste(fleetsname[newfleets[f][[1]]],collapse="|"),colnames(dat$catch)),
          grep(a,colnames(dat$catch)))
      ]))
      colnames(newcatch)=paste(c(newfleets[f][[1]],"_",a),collapse="")
      
    } else {
      newcatch=cbind(newcatch,rowSums(dat$catch[
        intersect(
          grep(paste(fleetsname[newfleets[f][[1]]],collapse="|"),colnames(dat$catch)),
          grep(a,colnames(dat$catch)))
      ]))
      colnames(newcatch)[dat$N_areas*(f-1)+a]=paste(c(newfleets[f][[1]],"_",a),collapse="")
      
    } #end else
  } #end area for loop
} #end fleet for loop

#Aggregate by fleet and area, then rename fleets using combined fleet names
#Add fleet names and areas to lencomp dataframe for aggregating below
dat$lencomp$FltName = dat$fleetnames[dat$lencomp$FltSvy]
dat$lencomp$area = dat$fleetinfo$area[dat$lencomp$FltSvy]
#Add new fleet numbers based on the fleet names as based on aggregation defined with 'newfleets'
dat$lencomp$newfltsvy = unlist(lapply(gsub('.{2}$', '', dat$lencomp$FltName), 
                                      FUN = function(x) grep(x,newfleets_names)))
newlencomp <-
  dat$lencomp %>%
  group_by("FltSvyB"=newfltsvy,area,Yr) %>%
  summarise(across(l10:l200,sum),.groups = "keep")
newlencomp$FltSvy=unlist(lapply(newlencomp$FltSvyB,function(x) paste(newfleets[x][[1]],collapse="")))

#Add some columns to lencomp and change some to numeric to match original dat
newlencomp$FltSvy=as.numeric(newlencomp$FltSvy)
newlencomp$Seas=1
newlencomp$Gender=0
newlencomp$Part=0
newlencomp$Nsamp=5

#Add to newcatch to match original dat
newcatch=cbind(newcatch,year=dat$catch$year)
newcatch=cbind(newcatch,seas=dat$catch$seas)
newse_log_catch=dat$se_log_catch[1:length(newfleets)] #define catch se for new catch file

#Update variables 
sel_switch=paste(ifelse(newfleets[1:length(newfleets)]=="3",1,2),collapse=" ") #define selectivity by fleet
nfleets_EM=length(newfleets) #redefine number of fleets

#replace values in dat with combined fleet data
dat$catch=newcatch
dat$se_log_catch=newse_log_catch
dat$lencomp=data.frame(newlencomp)
dat$Nfleet=nfleets_EM #The data sets number of fleets to be across all areas. TIM needs in each area


  
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
  if(fleetcombo == TRUE) file.copy(from = file.path(mod_loc, "Spatial", "Operating_Model", "TIM_OM_all_4area_4fleet.dat"), to = "TIM_OM.dat") #Will return FALSE if files already exist
  if(fleetcombo == FALSE) file.copy(from = file.path(mod_loc, "Spatial", "Operating_Model", "TIM_OM_all_4area_7fleet.dat"), to = "TIM_OM.dat") #Will return FALSE if files already exist
  invisible(shell(paste0("TIM_OM.exe"," -nox -nohess"), wait=T))
  file.remove(list.files()[-grep(".rep|.tpl|.exe|.dat", list.files())]) #remove extra files
  writeLines(paste("\nOM run completed with", length(newfleets), "fleets"))
}

#Access the .rep file to then enter the YFT data
om_rep <- readLines("TIM_OM.rep",n=-1)


####
#Function to ensure new data length equals current data length
#If equal, outputs TRUE
#If not equal, outputs number of lines to ammend to OM.rep file
#Ammend can be positive or negative
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
#Areas
####

#Estimate movement if 4 area (dont do for move_switch_OM)
#For testing set to 0 for now
loc <- grep("#move_switch$", om_rep)
om_rep[loc+1] <- 0
#om_rep[loc+1] <- 1 # <<<<<<<<<<<<REVISIT ONCE DONE WITH TESTING

#Resest number of surveys: TIM requires number per area, data is number across areas
dat$Nsurveys = dat$Nsurveys/dat$N_areas

####
#Catches
####

#Catch - OBS_yield_fleet
#Model reads as all years (as rows) by fleets (as columns) for area 1, then repeated for area 2, etc.
#Append successive areas to the bottom of area 1 catch for each fleet
loc <- grep("#OBS_yield_fleet$", om_rep)
temp_val <- data.frame(mapply(c, dat$catch[,grep("_1",colnames(dat$catch))], 
                  dat$catch[,grep("_2",colnames(dat$catch))],
                  dat$catch[,grep("_3",colnames(dat$catch))],
                  dat$catch[,grep("_4",colnames(dat$catch))])) 
new_val <- tidyr::unite(
  round(temp_val, 3), #DECISION - round to 3 decimals
  sep = " ", 
  col = "new_val")
om_rep[(loc + 1):(loc + dat$endyr*dat$N_areas)] <- new_val$new_val

#Use the same for true catch
#This isn't used when diagnostic_switch is 0 (our default) but still change here
loc <- grep("#yield_fleet", om_rep)
om_rep[(loc + 1):(loc + dat$endyr*dat$N_areas)] <- new_val$new_val

#Catch SE - OBS_yield_fleet_se_EM
#DECISION - Switch catch SE from dat$se_log_catch (0.01) to 0.2
loc <- grep("#OBS_yield_fleet_se_EM", om_rep)
new_val <- rep(paste(rep(0.2,dat$Nfleet), collapse = " "),dat$endyr*dat$N_areas)
om_rep[(loc + 1):(loc + dat$endyr*dat$N_areas)] <- new_val


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
#Confirmed these are blocked by area (see closed issue #31)
#agecomp <- as.matrix(dat$lencomp[7:ncol(dat$lencomp)]) %*% t(alk)
agecomp <- as.matrix(dat$lencomp[which(colnames(dat$lencomp) %in% paste0("l",seq(10,200,by=5)))]) %*% t(alk)
agecomp_prop <- agecomp/rowSums(agecomp)
agecomp_yr <- cbind("Yr" = dat$lencomp$Yr, "FltSvy" = dat$lencomp$FltSvy, "FltSvyB" = dat$lencomp$FltSvyB, "area" = dat$lencomp$area, agecomp_prop)

#Put into om_rep file
loc <- grep("#OBS_catch_prop$", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr*dat$Nfleet*dat$N_areas, ncol = dat$Nages) #Set up for all years and fleets
tmp_val[(dat$Nfleet * (agecomp_yr[,"Yr"] - 1 + (agecomp_yr[,"area"] - 1) * dat$endyr) + agecomp_yr[,"FltSvyB"]),] <- agecomp_yr[,-which(colnames(agecomp_yr) %in% c("FltSvyB","Yr","FltSvy","area"))] #Assign for just the years and fleets in YFT data. Each year for all fleets. 
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
#Check if number of entries dont match up. Append new lines or remove some lines before replacing
#TO DO (for later): adjust add lines to automatically remove lines if need be
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
if(add_lines<0){
  om_rep <- om_rep[-((loc+1):(loc-1*add_lines))] #remove lines so element equals the length of entry being added
}
om_rep[(loc + 1):(loc + dat$endyr*dat$Nfleet*dat$N_areas)] <- new_val

#Sample size of comps - #OBS_catch_prop_N_EM
#DECISION - assume all years within area 1 are first, then all years in area 2, etc.
#DECISION - replace values in dat$lencomp$Nsamp (N = 5) with N = 15 and for fleet 5 with N = 25
loc <- grep("#OBS_catch_prop_N_EM", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr*dat$N_areas, ncol = dat$Nfleet) #Set up for all years and fleets by area
tmp_val[cbind(dat$endyr * (dat$lencomp$area-1) + dat$lencomp$Yr, dat$lencomp$FltSvyB)] <- 15 #Assign for just the years and fleets in YFT data, set to 15
tmp_val[,which(newfleets %in% 5)]  <- replace(tmp_val[,which(newfleets %in% 5)], tmp_val[,which(newfleets %in% 5)] == 15, 25) #For fleet 5 change to 25
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr*dat$N_areas)] <- new_val


####
#Survey
####

#make year in dat$CPUE numeric
dat$CPUE$year = as.numeric(dat$CPUE$year)+(min(as.numeric(levels(dat$CPUE$year)))-1)

#Survey index - #OBS_survey_fleet_bio'
#DECISION: This only works if there is one survey per area. Would need to recode if more than one survey per area
loc <- grep("#OBS_survey_fleet_bio$", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr*dat$N_areas, ncol = dat$Nsurveys) #Set up for all years and surveys
tmp_val[cbind(dat$endyr*(as.numeric(as.factor(dat$CPUE$index))-1) + dat$CPUE$year, dat$Nsurveys)] <- dat$CPUE$cpu #Assign for just the years and surveys in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr*dat$N_areas)] <- new_val

#True survey index - #true_survey_fleet_bio
#This isn't used when diagnostic_switch is 0 (our default) but still change here
loc <- grep("#true_survey_fleet_bio$", om_rep)
om_rep[(loc + 1):(loc + dat$endyr*dat$N_areas)] <- new_val

#Survey index SE - #OBS_survey_fleet_bio_se_EM
#DECISION: This only works if there is one survey per area. Would need to recode if more than one survey per area
loc <- grep("#OBS_survey_fleet_bio_se_EM", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr*dat$N_areas, ncol = dat$Nsurveys) #Set up for all years and surveys
tmp_val[cbind(dat$endyr*(as.numeric(as.factor(dat$CPUE$index))-1) + dat$CPUE$year, dat$Nsurveys)] <- dat$CPUE$cv #Assign for just the years and surveys in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr*dat$N_areas)] <- new_val
#DECISION - not using precise se (not using CV to SE conversion)

#Survey Comps - #OBS_survey_prop set to #OBS_catch_prop for fleet 3
#DECISION - copying from proportions in fleet 3 catch for only years that overlap with cpue years. 
#Believe these to be the same as the fleet comps. Can set survey select to that of catch
loc <- grep("#OBS_survey_prop$", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr*dat$Nsurveys*dat$N_areas, ncol = dat$Nages) #Set up for all years and surveys
#Comp data for fleet 3 that overlaps with years and areas of CPUE data.
#sub() function component extracts last value in fleetnames, which is the area
overlap_survey_comp <- agecomp_yr[grep("3",agecomp_yr[,"FltSvy"]),][apply(agecomp_yr[grep("3",agecomp_yr[,"FltSvy"]),c("Yr","area")], 1, FUN = paste, collapse ="") %in% apply(cbind(dat$CPUE$year,sub('.*(?=.$)', '', dat$fleetnames[as.numeric(dat$CPUE$index)],perl=T)),1,FUN=paste0,collapse=""),] 
tmp_val[dat$endyr*(overlap_survey_comp[,"area"] - 1) + overlap_survey_comp[,"Yr"],] <- overlap_survey_comp[,-which(colnames(agecomp_yr) %in% c("FltSvyB","Yr","FltSvy","area"))] 
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr*dat$Nsurveys*dat$N_areas)] <- new_val

#Set the same for true comp
#This isn't used when diagnostic_switch is 0 (our default) but still change here
loc <- grep("#survey_at_age_fleet_prop", om_rep)
om_rep[(loc + 1):(loc + dat$endyr*dat$Nsurveys*dat$N_areas)] <- new_val

#Sample size of comps - #OBS_survey_prop_N_EM
#DECISION - copying from proportions in fleet 3 catch for only years that overlap with cpue years.  
#Believe these to be the same as the fleet comps. Can set survey select to that of catch.
#DECISION - What is NCPUEObs though?
loc <- grep("#OBS_survey_prop_N_EM", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr*dat$N_areas, ncol = dat$Nsurveys) #Set up for all years and surveys
#Assign for just the years and fleets in YFT data
tmp_val[dat$endyr*(overlap_survey_comp[,"area"] - 1) + overlap_survey_comp[,"Yr"],] <- 
  dat$lencomp[apply(dat$lencomp[,c("Yr","FltSvyB","area")],1,FUN = paste, collapse = "") %in%
                apply(overlap_survey_comp[,c("Yr","FltSvyB","area")],1,FUN = paste, collapse = ""),]$Nsamp
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr*dat$N_areas)] <- new_val


####
#Rec index
####

#Rec index - #OBS_rec_index_BM
#DECISION - set to zero
loc <- grep("#OBS_rec_index_BM", om_rep)
tmp_val <- matrix(0, nrow = dat$N_areas, ncol = dat$endyr)
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$N_areas)] <- new_val


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
#DECISION - fix at true value from OM (but reset EM to be the same) i.e. Dont estimate it
#but dont have likelihood penalty (wt_B_pen = 0) so dont need report_rate_ave or report_rate_sigma
#Use based on analyst guidance document (https://aaronmberger-nwfsc.github.io/Spatial-Assessment-Modeling-Workshop/articles/Analyst_guidance.html)
loc <- grep("#report_rate_switch", om_rep)
om_rep[(loc + 1)] <- 0 #stay at 0
loc <- grep("#input_report_rate_EM", om_rep)
om_rep[(loc + 1)] <- paste(rep(0.9, dat$N_areas), collapse = " ")
loc <- grep("#report_rate_TRUE", om_rep)
tmp_val <- matrix(0.9, nrow = length(unique(dat$tag_releases$yr)), ncol = dat$N_areas)
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + length(new_val))] <- new_val
#DECISION - there is no tag retention or tag loss in the TIM yet. It is in YFT

#Tags released - #ntags
#YFT data already adjusts for initial tagging mortality
#DECISION - assume tag releases of age 20 fish are actually age 20 fish. In reality, age 20 is a plus group for tagging
loc <- grep("#ntags$", om_rep)
tmp_val <- matrix(0, nrow = length(unique(dat$tag_releases$yr))*dat$N_areas, ncol = dat$Nages) #Set up for all regions, years and surveys
tmp_val[cbind(dat$N_areas*(dat$tag_releases$reg - 1) + as.numeric(factor(dat$tag_releases$yr)), dat$tag_releases$age)] <- dat$tag_releases$nrel #Assign for just the years and fleets in YFT data
tags_yr_age <- tmp_val #save for later
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + length(new_val))] <- new_val

#Total tags - #ntags_total
#DECISION - set equal to number of tags (ntags)
loc <- grep("#ntags_total", om_rep)
tmp_val <- matrix(rowSums(tags_yr_age), nrow = dat$N_areas, ncol = length(unique(dat$tag_releases$yr)), byrow = T)
new_val <- paste(colSums(tmp_val), collapse = " ")
om_rep[(loc + 1)] <- new_val

#Tag sample size - #tag_N_EM
#DECISION - SET ARBITRARILY TO 200
#Could set to actual sample size but may swamp likelihood
loc <- grep("#tag_N_EM", om_rep)
tmp_val <- matrix(0, nrow = length(unique(dat$tag_releases$yr))*dat$N_areas, ncol = dat$Nages) #Set up for all regions, years and ages
tmp_val[cbind(dat$N_areas*(dat$tag_releases$reg - 1) + as.numeric(factor(dat$tag_releases$yr)), dat$tag_releases$age)] <- 200 #Assign for just the years and ages in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
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
#Set up array for tag recaptures by age (row), by year when recaptures possible (column), by release year by area (3rd-dimension)
#DECISION - ignore all recaptures made after the entered maxlife of tags 
if(length(unique(nt)) == 1){
  tmp_val <- array(0, dim = c(dat$Nages, max(nt), length(unique(dat$tag_releases$yr))*dat$N_areas)) #numbers
  tmp_val_prop <- tmp_val #proportions
}
#Add number of recaptures
tmp_recap_info <- full_recap_info[full_recap_info$yr_diff <= maxlife, ]
tmp_recap_info$recap.reg <- as.numeric(str_sub(rownames(dat$fleetinfo)[tmp_recap_info$fleet],-1)) #region of recap
tmp_val[cbind(tmp_recap_info$age, max(xy)*(tmp_recap_info$recap.reg - 1) + tmp_recap_info$yr_diff, dat$N_areas*(tmp_recap_info$reg - 1) + as.numeric(factor(tmp_recap_info$yr.x)))] <- tmp_recap_info$recaps #Assign for just the years, recap region, and ages in YFT data
#Add in number of tags not captured (nt'th column) based on difference between total tags and recaptures at age
#across possible recapture periods for each release event
tmp_val[, max(nt), ] <- t(tags_yr_age) - apply(tmp_val, c(1,3), FUN = sum)
#Due to rounding (Aaron Berger's email on Jan 25, 2022) there may be small negative numbers, 
#DECISION - Where recaptures were higher than releases set number of recaptures (which are integers) to 
#equal the slightly lower number of tags released (which are doubles) and set non-captures to 0
#The 0.9 age 16 fish caught in first year of release is less than...the one recaught in second year
  tags_yr_age[,which(tmp_val[, max(nt), ]<0)] 
  tmp_val[which(tmp_val[, max(nt), ]<0),,] 
tmp_val[16,c(2,max(nt)),1] <- c(tmp_val[16,2,1] + tmp_val[16,max(nt),1], 0)
#Change to proportions
#Sum number of tags for each age across possible recapture periods for each release event in each area
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
tmp_val_noage_prop[!is.finite(tmp_val_noage_prop)] = 0 #set nan's to zero
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
# loc <- grep("#F_tag_scalar", om_rep)
# tmp_val <- c(om_rep[loc+1], rep(0.73,3)) #add three more entries (TO DO: the 3 entries are HARD CODED, so possibly revise)
# new_val <- paste(tmp_val, collapse = " ")
# om_rep[(loc + 1)] <- new_val

#Residency rate of tagged fish in year of release - #T_tag_res
#DECISION - Copy the 7th release value into the 8th-10th
#Could resolve by fixing the OM.dat nyrs_release to 10 and rerunning
# loc <- grep("#T_tag_res", om_rep)
# tmp_val <- c(om_rep[loc+1], rep(0.73,3)) #add three more entries (TO DO: the 3 entries are HARD CODED, so possibly revise)
# new_val <- paste(tmp_val, collapse = " ")
# om_rep[(loc + 1)] <- new_val


####
#Selectivity and number of fleets
####

#Update selectivity switches to differ by fleet
loc <- grep("#select_switch$", om_rep)
om_rep[loc+1] <- sel_switch

#Update selectivity switches to differ by survey fleet
loc <- grep("#select_switch_survey", om_rep)
om_rep[loc+1] <- "1"

#Indicate whether there is a mirror fleet
new_val <- grep("3", newfleets) #mirroing original fleet 3, which is now fleet 2. 0 is no mirroring
om_rep <- append(om_rep, rbind("#survey_mirror",c(new_val)), after = loc+1)

#Update selectivity phases for logistic
#DECISION - selectivity phase is 2 for logistic
loc <- grep("#ph_sel_log", om_rep)
om_rep[loc+1] <- "2"

#Update selectivity phases for double normal
#DECISION - selectivity phase is 2 for double logistic
if(length(grep("2", sel_switch))>0){ #If any sel_switch is 2 (double logistic)
  loc <- grep("#ph_sel_dubl$", om_rep)
  om_rep[loc+1] <- "2"
}

#If have mirror then turn off survey selectivity and set weight of survey age comp to zero
if(new_val != "0"){
  loc <- grep("#ph_sel_log_surv", om_rep)
  om_rep[loc+1] <- "-2"
  loc <- grep("#ph_sel_dubl_surv", om_rep)
  om_rep[loc+1] <- "-5"
  loc <- grep("#wt_srv_age", om_rep)
  om_rep[loc+1] <- "0"
}


####
#Biological data
####

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
om_rep[(loc + 1):(loc + dat$N_areas)] <- paste(tmp_val, collapse = " ") 

#Weight at age in catch - #input_catch_weight
loc <- grep("#input_catch_weight", om_rep)
om_rep[(loc + 1):(loc + dat$N_areas)] <- paste(tmp_val, collapse = " ") 

#Maturity at age - #maturity
loc <- grep("#maturity$", om_rep)
tmp_val <- bdat$Maturity
om_rep[(loc + 1):(loc + dat$N_areas)] <- paste(tmp_val, collapse = " ")

#Fecundity - #fecundity
#DECISION - keep as 1 for all ages

#Prop_fem - #prop_fem
#DECISION - keep as 0.5

#Mortality at age - #input_M_EM
#DECISION - Im also doing this for input_M_TRUE and input_M_EM
loc <- grep("#input_M", om_rep)
tmp_val <- bdat$M
om_rep[(loc + 1)] <- paste(tmp_val, collapse = " ")


####
#Switches for penalties and weighting
####

#Penalty for recruitment being different from Rave_mean
loc <- grep("#Rave_pen_switch", om_rep)
om_rep[(loc + 1)] <- 1
loc <- grep("#wt_Rave_pen", om_rep)
om_rep[(loc + 1)] <- 10

#Tagging weight to include tag likelihood 
loc <- grep("#wt_tag", om_rep)
om_rep[(loc + 1)] <- 1

#Initial abundance set up and penalty for initial value at age being different from mean_N
#<<<<<<<<<<<<<<<<<<<REVISIT ONCE DONE WITH TESTING. Should turn on, especially if start at year 105 
loc <- grep("#init_abund_switch", om_rep) #decaying from Rave
om_rep[(loc + 1)] <- 1
loc <- grep("#ph_init_abund", om_rep) #turn off estimating init_abund because decay from Rave
om_rep[(loc + 1)] <- -1
loc <- grep("#abund_pen_switch", om_rep) #keep off
om_rep[(loc + 1)] <- 0
loc <- grep("#wt_abund_pen", om_rep) #keep off (because abund_pen_switch is off)
om_rep[(loc + 1)] <- 0.1


####
#Bounds
####

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
#DECISION - Based on exploring overall comp patterns
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
loc <- grep("^#sel_beta3_start", om_rep) #readjust start for fleet
om_rep[(loc + 1)] <- 0.006
loc <- grep("^#sel_beta4.*start$", om_rep) #start for fleet and survey
om_rep[(loc + 1)] <- 1
loc <- grep("^#sel_beta4_start", om_rep) #readjust start for fleet
om_rep[(loc + 1)] <- 0.1


####
#Phases
####

#DECISION - Set recruitment phase to 3 (selectivity is set in selectivity section)
loc <- grep("#ph_rec", om_rep)
om_rep[loc+1] <- "3"

#DECISION - Set F phase to 4
loc <- grep("#ph_F", om_rep)
om_rep[loc+1] <- "4"



##Done Munging##


####
#Save YFT model as .dat file, copy .tpl over
####

setwd(file.path(mod_loc, mod_name, "Estimation_Model"))
writeLines(om_rep, paste0(mod_name,".dat"))
file.copy(from = file.path(tpl_loc, "YFT_1area.tpl"), to=file.path(getwd(), paste0(mod_name,".tpl"))) #Will return FALSE if files already exist
shell(paste0("admb ",mod_name)) #build .exe from the .tpl


####
#If want to remove years from the data <---- NEED TO UPDATE THIS
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



