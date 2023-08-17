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
  master_loc <- "C:\\Users\\Brian.Langseth\\Desktop\\Spatial-Workshop-SPASAM\\Operating_Model_TOA"
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
  master_loc <- "C:\\Spatial_SPASAM_2021_Sim\\Spatial-Workshop-SPASAM-main\\Operating_Model_TOA"
  code_loc <- "C:\\Spatial_SPASAM_2021_Sim\\Spatial-Workshop-SPASAM-main"
  mod_loc <- "C:\\Spatial_SPASAM_2021_Sim\\Spatial-Workshop-SPASAM-main"
  tpl_loc <- "C:\\Spatial_SPASAM_2021_Sim\\Spatial-Workshop-SPASAM-main\\Shortened105_estSel_Rdevs"
}

#FOR OTHER USERS, CAN ENTER LOCATIONS HERE ONCE


######################################################
#Read in data from cloned github repository
#Munge the data and then run the script
#DECISION - using generalized tpl, if want a specific one need to adjust
######################################################

#One area - can adjust for other datasets
load(file.path(data_loc,'TOA_simulations_inputs_1region.RData'))
load(file.path(data_loc,'TOA.simulations.singlearea.external.RData'))
cdat <- realised.catches[[1]] #use dataset 1 for one dataset trial
cdat$year = as.numeric(cdat$year)
tdat <- realised.tags[[1]] #use dataset 1 for one dataset trial
tdat$year = as.numeric(tdat$year)
simdat <- sim[1:227] #each dataset is 227 entries. Use dataset 1 for one dataset trial
mod_name <- "TOA_1area"
om_rep <- mungeData(mod_name, reduce = NULL, run = FALSE, fleetcombo=FALSE)


########################################################################################
#-------FUNCTION TO GET YFT DATA INTO THE PROPER FORMAT FROM THE ORIGINAL OM FILE------#
########################################################################################
#mod_name: the name of the model (name of the .dat, .tpl, and .exe folders)
#reduce: whether years of data should be removed, if so then enter the number of years
#run: whether to run the EM
#fleetcombo: do you want combine some fleets (TRUE or FALSE); define fleet combos in newfleets list below

mungeData <- function(mod_name, reduce = NULL, run = FALSE,fleetcombo=FALSE){

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
  file.copy(from = file.path(master_loc, "TIM_OM_1area.dat"), to = "TIM_OM.dat") #Will return FALSE if files already exist
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

#Function to pull values in 'simdat' from separate variable names together
#name is the field desired within the simulation data (e.g CAA data)
var_to_data <- function(name){
  if(name == "CAA") var = c("obs","N") #field name where data are and error_value
  if(name == "cpue") var = c("obs","cv")
  if(name == "tag") var = c("obs")
  if(name == "") var = ""
  if(name == "") var = ""
  
  varnames <- names(simdat)[grep(name, simdat)]
  
  for(i in 1:length(varnames)){
    temp <- simdat[varnames[i]][[varnames[i]]] #dataset
    temp_dat = temp[[var[1]]] #take observations from dataset
    temp_dat$year <- as.numeric(temp$year) #assign year for the data. For tagging this is the recapture year
    
    #Assign error value for each line in the dataset for CAA and cpue data
    if(name %in% c("CAA","cpue")) {
      temp_dat$err <- as.numeric(temp$error_value[var[2]])
    }
    if(name == "tag"){ 
      #assign a new error matrix for each row in the data 
      temp_err <- temp$error_value
      temp_err$year <- as.numeric(temp$year)
      
      #assign tag year for each row in the data
      temp_dat$tagYear = as.numeric(substr(varnames[i],start = unlist(gregexpr("_",varnames[i]))[2]+1, stop = unlist(gregexpr("_",varnames[i]))[3]-1))
    }
    
    if(i==1) {
      df <- temp_dat
      if(name == "tag") df_err <- temp_err
    }else{
      df = rbind(df,temp_dat)
      if(name == "tag") df_err <- rbind(df_err,temp_err)
    }
  }
  
  #For CAA and CPUE only: Include all years 1995 to 2021 whether those years have data or not
  if(name %in% c("CAA","cpue")) df_full <- right_join(df,data.frame("year"=c(1995:2021)),by="year") %>% replace(is.na(.),0) %>% arrange(year)
  if(name == "CAA") names(df_full)[2:(length(names(df_full))-2)] <- temp$min_age:temp$max_age
  #For tag include column names as the ages for which tagging data is for
  if(name == "tag") names(df)[2:(length(names(df))-2)] <- temp$min_age:temp$max_age
  
  #For tagging only: Include the years with variables and include the error
  if(name %in% c("CAA","cpue")) return(df_full) 
  if(name %in% c("tag")) return(list("obs" = df, "err" = df_err))

}

####################
##Data Munging!!!!
####################

####
#Set up basic values like number of ages and number of years because these are not in dataset
####

Nflt = 1
Nsurv = 1
Nyear = 27
Nage = 30

#The following element may be able to be removed after updating SIM_TIM.R (see issue 17)

#Keep switch for catch in numbers after the switch for SSB_type so runs well with existing YFT EM
loc <- grep("#SSB_type", om_rep)
om_rep <- append(om_rep, c("#catch_num_switch",0), after = (loc+1))


####
#Catches
####

#Catch - OBS_yield_fleet
#Based on values in Table 2 of Mormede et al. 2014 these appear to be in KG. Need MTs so divide by 1000
loc <- grep("#OBS_yield_fleet$", om_rep)
new_val <- cdat %>% group_by(year) %>% summarize(sum = sum(catch)/1000) %>% 
  right_join(.,data.frame("year"=c(1995:2021)),by="year") %>% replace(is.na(.),0) %>% arrange(year)
om_rep[(loc + 1):(loc + Nyear)] <- round(new_val$sum,3) #DECISION - round to 3 decimals

#Catch SE - OBS_yield_fleet_se_EM
#DECISION - No observation error observed so fix to low value 0.05
loc <- grep("#OBS_yield_fleet_se_EM", om_rep)
new_val <- rep(paste(0.05, collapse = " "),Nyear)
om_rep[(loc + 1):(loc + Nyear)] <- new_val


####
#Comps
####

#Length Comp - #OBS_catch_prop
#For TIM, the comp data is of age comps by year
#For TOA data, the comp data is of age comps by region for each year

agecomp <- var_to_data("CAA")

#Put into om_rep file
loc <- grep("#OBS_catch_prop$", om_rep)
tmp_val <- matrix(0, nrow = Nyear*Nflt, ncol = Nage) #Set up for all years and fleets
tmp_val <- agecomp[,-which(colnames(agecomp) %in% c("year","region","err"))] #Assign for just the years and fleets in YFT data. Each year for all fleets. 
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
#Check if number of entries dont match up. Append new lines or remove some lines before replacing
#TO DO (for later): adjust add lines to automatically remove lines if need be
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
if(!add_lines){ #if not equal through warning
  stop(paste("Lines need to be added or removed for", om_rep[loc]))
}
om_rep[(loc + 1):(loc + Nyear*Nflt)] <- new_val

#Sample size of comps - #OBS_catch_prop_N_EM
#DECISION: Assume Na (from equation 3 in TOA description from github) is N in 'CAA-X-sim'$error_value
loc <- grep("#OBS_catch_prop_N_EM", om_rep)
tmp_val <- matrix(0, nrow = Nyear, ncol = Nflt) #Set up for all years and fleets
tmp_val <- round(agecomp$err,2)
new_val <- tmp_val
om_rep[(loc + 1):(loc + Nyear)] <- new_val


####
#Survey
####

surv <- var_to_data("cpue")

#Survey index - #OBS_survey_fleet_bio
loc <- grep("#OBS_survey_fleet_bio$", om_rep)
tmp_val <- matrix(0, nrow = Nyear, ncol = Nsurv) #Set up for all years and surveys
tmp_val <- round(surv$cpue,2)
new_val <- tmp_val
om_rep[(loc + 1):(loc + Nyear)] <- new_val

#True survey index - #true_survey_fleet_bio
#This isn't used when diagnostic_switch is 0 (our default) but still change here
loc <- grep("#true_survey_fleet_bio$", om_rep)
om_rep[(loc + 1):(loc + Nyear)] <- new_val

#Survey index SE - #OBS_survey_fleet_bio_se_EM
loc <- grep("#OBS_survey_fleet_bio_se_EM", om_rep)
tmp_val <- matrix(0, nrow = Nyear, ncol = Nsurv) #Set up for all years and surveys
tmp_val <- sqrt(log(1+surv$err^2))
new_val <- tmp_val
om_rep[(loc + 1):(loc + Nyear)] <- new_val

#Survey Comps - #OBS_survey_prop set to #OBS_catch_prop for fleet
#DECISION - copying from proportions in fleet catch for only years that overlap with cpue years. 
#Believe these to be the same as the fleet comps. Can set survey select to that of catch
loc <- grep("#OBS_survey_prop$", om_rep)
tmp_val <- matrix(0, nrow = Nyear*Nsurv, ncol = Nage) #Set up for all years and surveys
tmp_val <- agecomp[,-which(colnames(agecomp) %in% c("year","region","err"))] #Assign for just the years and fleets in YFT data. Each year for all fleets. 
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + Nyear)] <- new_val

#Sample size of comps - #OBS_survey_prop_N_EM
#DECISION - copying from proportions in fleet catch for only years that overlap with cpue years.  
#Believe these to be the same as the fleet comps. Can set survey select to that of catch.
#DECISION - Use same N as fleet prop
loc <- grep("#OBS_survey_prop_N_EM", om_rep)
tmp_val <- matrix(0, nrow = Nyear, ncol = Nflt) #Set up for all years and fleets
tmp_val <- round(agecomp$err,2)
new_val <- tmp_val
om_rep[(loc + 1):(loc + Nyear)] <- new_val

####
#Rec index
####

#Rec index - #OBS_rec_index_BM
#DECISION - set to zero
loc <- grep("#OBS_rec_index_BM", om_rep)
tmp_val <- rep(0, Nyear)
new_val <- paste(tmp_val, collapse = " ")
om_rep[(loc + 1)] <- new_val


####
#Tagging
####

recap <- var_to_data("tag")

#Have tag switch off - #do_tag
loc <- grep("#do_tag$", om_rep)
om_rep[(loc + 1)] <- 0

loc <- grep("#do_tag_mult$", om_rep)
om_rep[(loc + 1)] <- 0

#Number of years with tag releases - #nyrs_release
loc <- grep("#nyrs_release", om_rep)
om_rep[(loc + 1)] <- length(unique(tdat$year))

#Years of tag releases - #years_of_tag_releases
loc <- grep("#years_of_tag_releases", om_rep)
tmp_val <- sort(unique(tdat$year))-(1995-1) #year 1 is 1995
om_rep[(loc + 1)] <- paste(tmp_val, collapse = " ")

#Lifespan of tags
#DECISION - Assume to be 16 which is maximum value
#unique(data.frame(recap$obs)[,c("year")]-data.frame(recap$obs)[,c("tagYear")])
maxlife <- 16
loc <- grep("#max_life_tags", om_rep)
om_rep[(loc + 1)] <- maxlife

#DECISION - age of full selection (kept currently at 8)

#Tag report rate - #...report_rate...
#DECISION - fix at same values from YFT
#These assumptions are ignored based on analyst guidance document (https://aaronmberger-nwfsc.github.io/Spatial-Assessment-Modeling-Workshop/articles/OM_Description_TOA.html)
loc <- grep("#report_rate_switch", om_rep)
om_rep[(loc + 1)] <- 0 #stay at 0
loc <- grep("#input_report_rate_EM", om_rep)
om_rep[(loc + 1)] <- 0.9
loc <- grep("#report_rate_TRUE", om_rep)
new_val <- rep(0.9, length(unique(tdat_year$year)))
#Check if number of entries dont match up. Append new lines before replacing
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
if(!is.logical(add_lines)) {
  stop(paste("Lines need to be added or removed for", om_rep[loc]))
}
om_rep[(loc + 1):(loc + length(new_val))] <- new_val

#Tags released - #ntags
#Note: Hard coded here for ages 3 to 30
tdat_year <- tdat %>% group_by(year) %>% summarize(sum=sum(total)) %>% round(.,1)
tdat_yearAge <- tdat[,-which(names(tdat) %in% c("cell","total","region"))] %>% 
  group_by(year) %>% summarize_all(.funs = sum, na.rm=TRUE) %>% round(.,1)

loc <- grep("#ntags$", om_rep)
tmp_val <- matrix(0, nrow = length(unique(tdat$year)), ncol = Nage) #Set up for all years and surveys
tmp_val[,c(2:30)] <- as.matrix(tdat_yearAge[,-1]) #Assign for just the years and ages in TOA data
tags_yr_age <- tmp_val #save for later - has all ages
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
#Check if number of entries dont match up. Append new lines before replacing
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
if(!is.logical(add_lines)) {
  stop(paste("Lines need to be added or removed for", om_rep[loc]))
}
om_rep[(loc + 1):(loc + length(new_val))] <- new_val

#Total tags - #ntags_total
#DECISION - set equal to number of tags (ntags)
loc <- grep("#ntags_total", om_rep)
new_val <- paste(rowSums(tags_yr_age), collapse = " ") #from ntags script
om_rep[(loc + 1)] <- new_val


#<<<<<<<<<<<<CONTINUE HERE

#Tag sample size - #tag_N_EM
#Determine number of recaptures of each tagging event by multiplying proportion recaptured from each tag release by number of tags in that release
num_rel <- left_join(recap$obs[,"tagYear"],tdat_yearAge[,-2], by = c("tagYear" = "year"))
num_recap <- cbind(num_rel[,1], recap$obs[,-c(1,30,31)]*num_rel[,-1])
num_recap_yr <- 
#DECISION - SET ARBITRARILY TO number of recaptures plus 30 per 
#guidance on github page (https://aaronmberger-nwfsc.github.io/Spatial-Assessment-Modeling-Workshop/articles/OM_Description_TOA.html)
loc <- grep("#tag_N_EM", om_rep)
tmp_val <- matrix(0, nrow = length(unique(tdat_year$year)), ncol = Nage) #Set up for all years and ages
tmp_val[] <- 200 #Assign for just the years and ages in YFT data
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
#Selectivity and number of fleets
##

#The following two elements may be able to be removed after updating SIM_TIM.R (see issue 17)

#if nfleets_EM <7 then have to change #input_selectivity_EM 
if(nfleets_EM<7){
loc <- grep("#input_selectivity_EM", om_rep)
om_rep=om_rep[-((loc+((4*nfleets_EM)+1)):(loc+28))]

#loc<-grep("#selectivity_age", om_rep)
#om_rep[(loc+1):(loc+28)]=rep(paste(rep(1,nfleets_EM),collapse=" "),dat$Nages)
}

#update number of fleets
loc <- grep("#nfleets_EM", om_rep)
om_rep[loc+1] <- nfleets_EM

#Update selectivity switches to differ by fleet
loc <- grep("#select_switch$", om_rep)
om_rep[loc+1] <- sel_switch

#Update selectivity switches to differ by survey fleet
loc <- grep("#select_switch_survey", om_rep)
om_rep[loc+1] <- "1"

#Indicate whether there is a mirror fleet
new_val <- grep("3", newfleets) #mirroring original fleet 3, which is now fleet 2. 0 is no mirroring
om_rep <- append(om_rep, rbind("#survey_mirror",c(new_val)), after = loc+1)

#Update selectivity phases for logistic
#DECISION - selectivity phase is 2 to help with gradient and run-time; see issue #37 on github
loc <- grep("#ph_sel_log$", om_rep)
om_rep[loc+1] <- "2"

#Update selectivity phases for double normal
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
#DECISION - dont penalize recruitment being different from Rave_mean, to speed up runtime
loc <- grep("#Rave_pen_switch", om_rep)
om_rep[(loc + 1)] <- 0
loc <- grep("#wt_Rave_pen", om_rep)
om_rep[(loc + 1)] <- 0

#Tagging weight to include tag likelihood 
loc <- grep("#wt_tag", om_rep)
om_rep[(loc + 1)] <- 0

#Initial abundance set up and penalty for initial value at age being different from mean_N
#DECISION: estimate init_abund so switch to 0 and phase to 2
loc <- grep("#init_abund_switch", om_rep) 
om_rep[(loc + 1)] <- 0
loc <- grep("#abund_pen_switch", om_rep) #keep off
om_rep[(loc + 1)] <- 0
loc <- grep("#wt_abund_pen", om_rep) #keep off
om_rep[(loc + 1)] <- 0
loc <- grep("#ph_init_abund", om_rep)
om_rep[(loc + 1)] <- 2


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
om_rep[(loc + 1)] <- -20
loc <- grep("#q_start", om_rep)
om_rep[(loc + 1)] <- -14

#Selectivity parameters
loc <- grep("lb_sel_beta", om_rep) #lower bounds for all fleets and surveys
om_rep[(loc + 1)] <- -10
loc <- grep("ub_sel_beta", om_rep) #uppers bounds for all fleets and surveys
om_rep[(loc + 1)] <- 10
loc <- grep("^#sel_beta1.*start$", om_rep) #start for fleet and survey
om_rep[(loc + 1)] <- 0.2
loc <- grep("^#sel_beta2.*start$", om_rep) #start for fleet and survey
om_rep[(loc + 1)] <- 2
loc <- grep("^#sel_beta3.*start$", om_rep) #start for fleet and survey
om_rep[(loc + 1)] <- -2
loc <- grep("^#sel_beta4.*start$", om_rep) #start for fleet and survey
om_rep[(loc + 1)] <- -0.1

##Change F start
loc <- grep("#F_start", om_rep) 
om_rep[(loc + 1)] <- -5

#Fishing mortality
#DECISION - to speed up runtime and improve gradient; see issue #37 in github
loc <- grep("#lb_F$", om_rep)
om_rep[(loc + 1)] <- -15
loc <- grep("ub_F$", om_rep)
om_rep[(loc + 1)] <- 1.5


##
#Phases
##

#changes here help with gradient and run-time; see issue #37 on github

#DECISION - Set recruitment phase to 3 and F phase to 4 (selectivity is set in selectivity section)
loc <- grep("#ph_rec$", om_rep)
om_rep[loc+1] <- "3"
loc <- grep("#ph_F$", om_rep)
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



