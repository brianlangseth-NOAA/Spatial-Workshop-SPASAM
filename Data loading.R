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
bdat <- biol_dat 


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

#Create age-length key - taken from Jon's ALK.R and modified
#DECISION - dat$lbin_vector final bin is 198, whereas dat$lencomp final bin is 200. There
#isn't any catches in this last bin so ignoring for now
#DECISION - set end of final lbin to be 205 cm (as opposed to ongoing)
#DECISION - assume lognormal error for distribution of lengths around an age
#DECISION - round probability to 2 sig digits (which truncates the possible ages with probability)
#DECISION - not using precise se (not using CV to SE conversion)
lengths=seq(10,205,by=5) 
#Function to calculate probability in a given length interval
getprob=function(L1=NULL,L2=NULL,meanL=NULL,sd=NULL){
  #prob=pnorm(L2,mean=meanL,sd=sd)-pnorm(L1,mean=meanL,sd=sd)
  prob=plnorm(L2,meanlog=meanL,sdlog=sd)-plnorm(L1,meanlog=meanL,sdlog=sd)
  return(prob)
}
#matrix to hold alk
alk=matrix(NA,nrow=length(Latage),ncol=(length(lengths)-1),dimnames = list(paste0("a",seq(1:length(Latage))),paste0("l",(lengths[1:(length(lengths)-1)]))))
#loop over 28 ages and the length bins and calculate probility for each bin
for(a in 1:length(Latage)){
  for(l in 1:(length(lengths)-1)){
    alk[a,l]=getprob(L1=lengths[l],L2=lengths[l+1],meanL=log(Latage[a]),sd=0.1)
  }
}
alk=round(alk,digits=2)
noentry = which(colSums(alk)==0) #determine which lengths dont have any entries
alk[1,noentry[1]]=1 #set the first column to be 1 for the first age 1
alk[dat$Nages,noentry[-1]]=1 #set the last columns to be 1 for the last age
#divided by column sums to produce P(A|L); i.e. column sums among ages = 1
alk=t(t(alk)/colSums(alk))

#Convert length comps to age comps
agecomp <- as.matrix(dat$lencomp[7:ncol(dat$lencomp)]) %*% t(alk)
##########CONTINUE HERE



#DECISION - assume OBS_catch_prop is of blocked by year first for each fleet






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

#Survey index SE - #OBS_survey_fleet_bio_se_EM
loc <- grep("#OBS_survey_fleet_bio_se_EM", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr, ncol = dat$Nsurveys) #Set up for all years and surveys
tmp_val[cbind(as.numeric(levels(dat$CPUE$year)),dat$Nsurveys)] <- dat$CPUE$cv #Assign for just the years and surveys in YFT data
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
om_rep[(loc + 1):(loc + dat$endyr)] <- new_val
#DECISION - not using precise se (not using CV to SE conversion)

#Survey Comps - #OBS_survey_prop set to #OBS_catch_prop for fleet 7
#DECISION - Believe these to be the same as the fleet comps. Can set survey select to that of catch
#TO DO

#Sample size of comps - #OBS_survey_prop_N_EM
#DECISION - Believe these to be the same as the fleet comps. Can set survey select to that of catch. Right now copying from catch
#DECISION - What is NCPUEObs though?
loc <- grep("#OBS_survey_prop_N_EM", om_rep)
tmp_val <- matrix(0, nrow = dat$endyr, ncol = dat$Nsurvey) #Set up for all years and surveys
tmp_val[cbind(dat$lencomp[dat$lencomp$FltSvy==3,]$Yr, dat$Nsurvey)] <- dat$lencomp[dat$lencomp$FltSvy==3,]$Nsamp #Assign for just the years and fleets in YFT data
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

#Number of years with tag releases - #nyrs_release
loc <- grep("#nyrs_release", om_rep)
om_rep[(loc + 1)] <- length(unique(dat$tag_releases$yr)) #NOT equal to dat$N_tag_groups

#Years of tag releases - #years_of_tag_releases
loc <- grep("#years_of_tag_releases", om_rep)
tmp_val <- unique(dat$tag_releases$yr)
om_rep[(loc + 1)] <- paste(tmp_val, collapse = " ")

#Lifespan of tags
#DECISION - assume it equals to maximum difference in year recapture from year released within data
#Originally had as max_periods but that is SS-speak and is the periods after which tags go into the accumulator
#age. See issue #6: https://github.com/aaronmberger-nwfsc/Spatial-Assessment-Modeling-Workshop/issues/6
full_recap_info <- merge(dat$tag_releases, dat$tag_recaps, by = "tg") #combine tag release and recapture information by tag (tg)
full_recap_info$yr_diff <- full_recap_info$yr.y - full_recap_info$yr.x
loc <- grep("#max_life_tags", om_rep)
om_rep[(loc + 1)] <- max(full_recap_info$yr_diff)

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
om_rep <- append(om_rep, new_val[1:add_lines], after = loc)
om_rep[(loc + 1):(loc + length(new_val))] <- new_val
#DECISION - there is no tag retention or tag loss in the TIM yet. It is in YFT


#Tags released - #ntags
#YFT data already adjusts for initial tagging mortality
loc <- grep("#ntags$", om_rep)
tmp_val <- matrix(0, nrow = length(unique(dat$tag_releases$yr)), ncol = dat$Nages) #Set up for all years and surveys
tmp_val[cbind(as.numeric(factor(dat$tag_releases$yr)), dat$tag_releases$age)] <- dat$tag_releases$nrel #Assign for just the years and fleets in YFT data
tags_yr_age <- tmp_val #save for later
new_val <- apply(tmp_val, 1, FUN = paste, collapse = " ")
#Check if number of entries dont match up. Append new lines before replacing
entries <- grep("#", om_rep)
add_lines <- check_entry(loc, entries[which(entries %in% loc)+1]-1, new_val)
om_rep <- append(om_rep, new_val[1:add_lines], after = loc)
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
om_rep <- append(om_rep, new_val[1:add_lines], after = loc)
om_rep[(loc + 1):(loc + length(new_val))] <- new_val

#Tag recaptures - #OBS_tag_prop_final
#Final column (which represents the possible recpature years + 1) covers all tags that were not recovered
#Rows are entered as ages (28) for each release event (10) (so in blocks of 28 rows)
#DECISION - lifespan of tags decision is used here. If adjust, adjust here too
#DECISION - currently ignoring tag mixing rate (dat$mixing_latency_period), which is SS-speak. Counting
#all recaps regardless of time after release
loc <- grep("#OBS_tag_prop_final$", om_rep)
#Set up dimensions for nt (number of years where recaptures are possible)
xy = nt = NULL
for(i in 1:length(unique(dat$tag_releases$yr))){ #Script from TM.tpl to set dimensions
  xx <- unique(dat$tag_releases$yr)[i]
  xy[i] <- min(max(full_recap_info$yr_diff),dat$endyr-xx+1)
  nt[i] <- xy[i]*dat$N_areas+1
}
#Set up array for tag recaptures by age (row), by year when recaptures possible (column), by release year (3rd-dimension)
#TO DO: Confirm with new data that if nt's are different than will need to make a jagged array. Currently not done.
if(length(unique(nt)) == 1){  
  tmp_val <- array(0, dim = c(dat$Nages, max(nt), length(unique(dat$tag_releases$yr)))) #numbers
  tmp_val_prop <- tmp_val #proportions
}
#Add number of recaptures 
tmp_val[cbind(full_recap_info$age, full_recap_info$yr_diff, as.numeric(factor(full_recap_info$yr.x)))] <- full_recap_info$recaps #Assign for just the years and ages in YFT data
#Add in number of tags not captured (nt'th column) based on difference between total tags and recaptures at age
#across possible recapture periods for each release event
#TO DO: Ensure tagging data are fixed so that there are no negatives in this value
tmp_val[, 19, ] <- t(tags_yr_age) - apply(tmp_val, c(1,3), FUN = sum)
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
om_rep <- append(om_rep, new_val[1:add_lines], after = loc)
om_rep[(loc + 1):(loc + length(new_val))] <- new_val


##
#Selectivity
##



##
#Biological data
##

#Steepness - #steep
loc <- grep("#steep$", om_rep)
om_rep[(loc + 1)] <- 0.8

#R average - #R_ave
loc <- grep("#R_ave", om_rep)
om_rep[(loc + 1)] <- bdat$B0/1000 #from OM description this IS R0

#Variance in recruitment - #sigma_recruit_EM
#DECISION - sigma r of 0.6
loc <- grep("#sigma_recruit", om_rep)
om_rep[(loc + 1)] <- 0.6 

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



##------------------------Done Munging--------------------------------------------##

####
#Run YFT model
####


#TO DO: add running scripts here or work with SIM_TIM_editing.R





####
#TO DO OR CONFIRM THAT WE DONT NEED TO DO
####

#UNITS OF CATCH - do we use in TIM anywhere. YRT are in number. Units are in biomass. Need to adjust. 
#CPUE - how to enter blanks for years without data. If we enter 0 does it read as such or as empty
#WHAT TIM DATA SET TO USE - Fill in EM.dat file or OM.dat file
#SURVEY DATA - I dont see survey comps nor survey sample size in YFT data
#SURVEY COMPS - In YFT I believe these are the same as the fleet comps. Thus either reuse survey comps or exclude
#and fix survey selectivity to be the same as the fleet selectivity. Currently, Ive reused. 
