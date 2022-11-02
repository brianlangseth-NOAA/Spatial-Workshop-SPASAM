#Functions to remove no_yrs of data from the beginning of the time series
#Pulls from .dat file in current directory

remove_years <- function(no_yrs,mod_name){
  
  em_dat <- readLines(paste0(mod_name,".dat"),n=-1)
  
  #Number of fleets (for reference)
  loc <- grep("#nfleets_EM", em_dat)
  orig_flt <- as.numeric(em_dat[(loc+1)])
  
  #Number of OM fleets (for reference)
  loc <- grep("#nfleets_OM", em_dat)
  OM_flt <- as.numeric(em_dat[(loc+1)])
  
  #Number of ages (for reference)
  orig_ages <- as.numeric(em_dat[(2)])
  
  #Number of regions
  loc <- grep("#nregions_EM", em_dat)
  orig_reg <- as.numeric(em_dat[loc+1])
  
  #Number of years, and update to account for fewer years now
  loc <- grep("#nyrs$", em_dat)
  orig_yrs <- as.numeric(em_dat[(loc+1)])
  em_dat[(loc+1)] <- orig_yrs - no_yrs
  
  #OBS_rec_index_BM
  loc <- grep("#OBS_rec_index_BM", em_dat)
  em_dat[(loc+1):(loc+orig_reg)] <- shorten_line(em_dat, loc, no_yrs, orig_reg)
  
  #OBS_survey_fleet_bio
  loc <- grep("#OBS_survey_fleet_bio$", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #OBS_survey_fleet_bio_se_EM
  loc <- grep("#OBS_survey_fleet_bio_se_EM", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #OBS_survey_prop
  loc <- grep("#OBS_survey_prop$", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #OBS_survey_prop_N_EM
  loc <- grep("#OBS_survey_prop_N_EM", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #OBS_yield_fleet
  loc <- grep("#OBS_yield_fleet$", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #OBS_yield_fleet_se_EM
  loc <- grep("#OBS_yield_fleet_se_EM", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #OBS_catch_prop
  loc <- grep("#OBS_catch_prop$", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs*orig_flt, orig_yrs, orig_reg, prop = TRUE)
  #em_dat[(loc+1):(loc+((orig_yrs-no_yrs)*orig_flt))] <- shorten_vector(em_dat, loc, no_yrs*orig_flt, orig_yrs, prop = TRUE)
  
  #OBS_catch_prop_N_EM
  loc <- grep("#OBS_catch_prop_N_EM", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #years_of_tag_releases - Not affected by region
  loc <- grep("#years_of_tag_releases", em_dat)
  em_dat[(loc+1)] <- paste(as.numeric(unlist(strsplit(em_dat[loc+1], split = " "))) - no_yrs, collapse = " ")
  
  #age_full_selection
  loc <- grep("#age_full_selection", em_dat)
  em_dat[(loc+1):(loc+orig_reg)] <- shorten_line(em_dat, loc, no_yrs, orig_reg)
  
  #input_Rec_Prop_EM
  loc <- grep("#input_Rec_Prop_EM", em_dat)
  em_dat[(loc+1):(loc+orig_reg)] <- shorten_line(em_dat, loc, no_yrs, orig_reg)
  
  #rec_devs - Based on populations not regions so not affected by region
  loc <- grep("#rec_devs", em_dat)
  em_dat[(loc+1)] <- shorten_line(em_dat, loc, no_yrs, 1) #only one line
  
  #Rec_Prop (is of length years-1, but coded the same)
  loc <- grep("#Rec_Prop", em_dat)
  em_dat[(loc+1):(loc+orig_reg)] <- shorten_line(em_dat, loc, no_yrs, orig_reg)
  
  #recruits_BM
  loc <- grep("#recruits_BM", em_dat)
  em_dat[(loc+1):(loc+orig_reg)] <- shorten_line(em_dat, loc, no_yrs, orig_reg)
  
  #F
  loc <- grep("#F$", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #Fyear
  loc <- grep("#Fyear", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #biomass_AM
  loc <- grep("#biomass_AM", em_dat)
  em_dat[(loc+1):(loc+orig_reg)] <- shorten_line(em_dat, loc, no_yrs, orig_reg)
  
  #biomass_population - Based on populations not regions so not affected by region
  loc <- grep("#biomass_population", em_dat)
  em_dat[(loc+1)] <- shorten_line(em_dat, loc, no_yrs, 1) #only one line
  
  #catch_at_age_fleet_prop
  loc <- grep("#catch_at_age_fleet_prop", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs*OM_flt, orig_yrs, orig_reg, prop = TRUE)
  #em_dat[(loc+1):(loc+((orig_yrs-no_yrs)*orig_flt))] <- shorten_vector(em_dat, loc, no_yrs*orig_flt, orig_yrs, prop = TRUE)
  
  #yield_fleet
  loc <- grep("#yield_fleet", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #survey_at_age_fleet_prop
  loc <- grep("#survey_at_age_fleet_prop", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #true_survey_fleet_bio
  loc <- grep("#true_survey_fleet_bio", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #harvest_rate_region_bio
  loc <- grep("#harvest_rate_region_bio", em_dat)
  em_dat[(loc+1):(loc+orig_reg)] <- shorten_line(em_dat, loc, no_yrs, orig_reg)
  
  #depletion_region
  loc <- grep("#depletion_region", em_dat)
  em_dat[(loc+1):(loc+orig_reg)] <- shorten_line(em_dat, loc, no_yrs, orig_reg)
  
  #SSB_region
  loc <- grep("#SSB_region", em_dat)
  em_dat[(loc+1):(loc+orig_reg)] <- shorten_line(em_dat, loc, no_yrs, orig_reg)
  
  #Bratio_population - Based on populations not regions so not affected by region
  loc <- grep("#Bratio_population", em_dat)
  em_dat[(loc+1)] <- shorten_line(em_dat, loc, no_yrs, 1) #only one line
  
  #T_year
  loc <- grep("#T_year", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #T - length is years*ages*regions
  #Order is all ages for year 1, all ages for year 2, etc. Then blocked by region
  #No function for this so apply manually
  loc <- grep("#T$", em_dat)
  tmp <- em_dat[(loc+1):(loc+(orig_yrs*orig_ages*orig_reg))]
  em_dat <- em_dat[-c((loc+1):(loc+(orig_yrs*orig_ages*orig_reg)))] 
  tmp_shortened <- tmp[-c((1:(no_yrs*orig_ages)) + rep(orig_yrs*orig_ages*(0:(orig_reg-1)), each = no_yrs*orig_ages))] #remove no_yrs worth of data for each region
  em_dat <- append(em_dat, tmp_shortened, after = loc)
  
  #abund_frac_age_region
  loc <- grep("#abund_frac_age_region", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)
  #em_dat[(loc+1):(loc+(orig_yrs-no_yrs))] <- shorten_vector(em_dat, loc, no_yrs, orig_yrs)
  
  #abund_frac_year
  loc <- grep("#abund_frac_year", em_dat)
  em_dat[(loc+1):(loc+orig_reg)] <- shorten_line(em_dat, loc, no_yrs, orig_reg)
  
  return(em_dat)
  
}

#Entries with years all within one line, possibly repeated by region
shorten_line <- function(datfile, var_loc, n_yrs, n_reg) {
  
  unlisted <- strsplit(datfile[(var_loc+1):(var_loc+n_reg)], split = " ")
  #lapply(unlisted, FUN = function(x) unlist(strsplit(x, split = " ")))
  unlisted <- lapply(unlisted, FUN = function(x) if(x[1] == "") x[-1] else x) #remove the empty space in first element if data was indented
  shortened <- lapply(unlisted, FUN = function(x) paste(x[-c(1:n_yrs)], collapse = " "))
  return(unlist(shortened))
  
}  

#Entries with years on each line
shorten_vector <- function(datfile, var_loc, n_yrs, o_yrs, n_reg, prop = FALSE) {
  
  #Setup variables based on the datfile
  
  #Number of fleets (for reference)
  temp_loc <- grep("#nfleets_EM", datfile)
  o_flt <- as.numeric(datfile[(temp_loc+1)])
  
  #Temporarily remove data within element
  if(!prop) { #if not proportions length is yrs*regions
    unlisted <- datfile[(var_loc+1):(var_loc+o_yrs*n_reg)] 
    datfile <- datfile[-c((var_loc+1):(var_loc+o_yrs*n_reg))]
    shortened <- unlisted[-c((1:n_yrs) + rep(o_yrs*(0:(n_reg-1)), each = n_yrs))] #remove first n_yrs of data for each region
  }
  if(prop) { #if proportions length is yrs*flt*regions
    unlisted <- datfile[(var_loc+1):(var_loc+(o_yrs*o_flt*n_reg))] 
    datfile <- datfile[-c((var_loc+1):(var_loc+(o_yrs*o_flt*n_reg)))] 
    shortened <- unlisted[-c((1:n_yrs) + rep(o_yrs*o_flt*(0:(n_reg-1)), each = n_yrs))] #remove first n_yrs*Nfleet of data for each region
  }
  
  new_datfile <- append(datfile, shortened, after = var_loc) #append remaining years back into element
  
  return(new_datfile)
  
}
