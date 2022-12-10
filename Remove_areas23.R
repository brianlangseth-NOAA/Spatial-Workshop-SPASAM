#Functions to remove regions from the .dat file
#Pulls from .dat file in current directory
#
#Assigns catches in region 2 and 3 to region 1
#Would need to update the code if want to remove non-sequential regions
#Recaptures in region 2 and 3 are assumed to be not recaptured
#Excludes other data in region 2 and 3 altogether.

remove_areas23 <- function(mod_name, rm_regions){
  
  em_dat <- readLines(paste0(mod_name,".dat"),n=-1)
  
  #Number of fleets (for reference)
  loc <- grep("#nfleets_EM", em_dat)
  orig_flt <- as.numeric(em_dat[(loc+1)])
  
  #Number of OM fleets (for reference)
  loc <- grep("#nfleets_OM", em_dat)
  OM_flt <- as.numeric(em_dat[(loc+1)])
  
  #Number of ages (for reference)
  orig_ages <- as.numeric(em_dat[(2)])
  
  #Number of regions, and update to account for fewer regions now
  loc <- grep("#nregions_EM", em_dat)
  orig_reg <- as.numeric(em_dat[loc+1])
  em_dat[(loc+1)] <- orig_reg - length(rm_regions)
  
  loc <- grep("#nregions_OM", em_dat)
  em_dat[(loc+1)] <- orig_reg - length(rm_regions)
  
  #Number of years
  loc <- grep("#nyrs$", em_dat)
  orig_yrs <- as.numeric(em_dat[(loc+1)])
  
  
  #Starting to reduce data entries to specified regions
  
  #tsurvey_EM
  loc <- grep("#tsurvey_EM", em_dat)
  em_dat[(loc+1)] <- shorten_line(em_dat, loc, rm_regions)
  
  #input_weight
  loc <- grep("#input_weight", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep=1, rm_regions)
  
  #input_catch_weight
  loc <- grep("#input_catch_weight", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep=1, rm_regions)
  
  #fecundity
  loc <- grep("#fecundity", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep=1, rm_regions)
  
  #maturity
  loc <- grep("#maturity$", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep=1, rm_regions)
  
  #prop_fem
  loc <- grep("#prop_fem", em_dat)
  em_dat[(loc+1)] <- shorten_line(em_dat, loc, rm_regions)
  
  #OBS_rec_index_BM
  loc <- grep("#OBS_rec_index_BM", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep=1, rm_regions)
  
  #OBS_survey_fleet_bio
  loc <- grep("#OBS_survey_fleet_bio$", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = orig_yrs, rm_regions)
 
  #OBS_survey_fleet_bio_se_EM
  loc <- grep("#OBS_survey_fleet_bio_se_EM", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = orig_yrs, rm_regions)
  
  #OBS_survey_prop
  loc <- grep("#OBS_survey_prop$", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = orig_yrs, rm_regions)

  #OBS_survey_prop_N_EM
  loc <- grep("#OBS_survey_prop_N_EM", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = orig_yrs, rm_regions)

  #OBS_yield_fleet
  #Based on Figure 1 at https://aaronmberger-nwfsc.github.io/Spatial-Assessment-Modeling-Workshop/articles/OM_description_YFT.html
  #combine regions 1 and 2 together and regions 3 and 4 together
  loc <- grep("#OBS_yield_fleet$", em_dat)
  #Combine
  unlisted <- strsplit(em_dat[(loc+1):(loc+(151*orig_reg))], split = " ")
  unlisted <- lapply(unlisted, FUN = function(x) if(x[1] == "") x[-1] else x) #remove the empty space in first element if data was indented
  shortened <- unlisted[-(((rm_regions[1]-1)*151+1):(rm_regions[length(rm_regions)]*151))] #remove the entries from the regions desired
  removed <- unlisted[(((rm_regions[1]-1)*151+1):(rm_regions[length(rm_regions)]*151))] #keep the removed regions
  shortened.n <- lapply(shortened, FUN = as.numeric)
  removed.n <- lapply(removed, FUN = as.numeric)
  combined <- lapply(seq_along(shortened.n), FUN = function(x) unlist(shortened.n[x]) + unlist(removed.n[x]))
  new_combined <- unlist(lapply(combined, FUN = paste, collapse = " "))
  #Replace catches from all regions into combined catches for fewer regions
  temp_dat <- em_dat[-c((loc+1):(loc+(151*orig_reg)))] #temporarily remove repeated lines
  em_dat <- append(temp_dat, new_combined, after = loc) #append remaining years back into element

  #OBS_yield_fleet_se_EM
  loc <- grep("#OBS_yield_fleet_se_EM", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = orig_yrs, rm_regions)

  #OBS_catch_prop
  loc <- grep("#OBS_catch_prop$", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = orig_yrs*orig_flt, rm_regions)
  
  #OBS_catch_prop_N_EM
  loc <- grep("#OBS_catch_prop_N_EM", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = orig_yrs, rm_regions)

  #age_full_selection
  loc <- grep("#age_full_selection", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep=1, rm_regions)
  
  #input_report_rate_EM
  loc <- grep("#input_report_rate_EM", em_dat)
  em_dat[(loc+1)] <- shorten_line(em_dat, loc, rm_regions)
  
  #ntags
  loc <- grep("#ntags$", em_dat)
  nyr_tag_rel <- as.numeric(em_dat[grep("#nyrs_release", em_dat)+1])
  em_dat <- shorten_lines(em_dat, loc, rep = nyr_tag_rel, rm_regions)
  
  #tag_N_EM
  loc <- grep("#tag_N_EM", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = nyr_tag_rel, rm_regions)
  
  #input_T_EM 
  #<<<<<<<<<<<<<<<<<<<<<NEED TO DO<<<<<<<<<<<<<<<<
  loc <- grep("#input_T_EM", em_dat)

  #OBS_tag_prop_final
  #<<<<<<<<<<<<<<<<<<<<<NEED TO DO<<<<<<<<<<<<<<<<
  loc <- grep("#OBS_tag_prop_final$", em_dat)
  
  #OBS_tag_prop_final_no_age
  loc <- grep("#OBS_tag_prop_final_no_age$", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = nyr_tag_rel, rm_regions)
  
  #input_Rec_Prop_EM
  loc <- grep("#input_Rec_Prop_EM", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep=1, rm_regions)
  
  #input_selectivity_EM
  #These are entered over multiple lines (4 to be exact), so each region is repeated over 4*fleet lines
  loc <- grep("#input_selectivity_EM", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep=4*orig_flt, rm_regions)
  
  #input_survey_selectivity_EM
  loc <- grep("#input_survey_selectivity_EM", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep=orig_ages, rm_regions)
  
  #input_dist_init_abund
  loc <- grep("#input_dist_init_abund", em_dat)
  em_dat[(loc+1)] <- shorten_line(em_dat, loc, rm_regions)
  
  #init_abund_EM
  loc <- grep("#init_abund_EM", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = 1, rm_regions)

  #frac_natal_true
  #Has to sum to one so just entering here
  loc <- grep("#frac_natal_true", em_dat)
  em_dat[loc+1] <- paste(rep(1/length(rm_regions), length(rm_regions)), collapse = " ")
  
  #init_abund_TRUE
  loc <- grep("#init_abund_TRUE", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = 1, rm_regions)
  
  #q_survey
  loc <- grep("#q_survey", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = 1, rm_regions)
  
  #sel_beta1
  loc <- grep("#sel_beta1$", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = 1, rm_regions)
  
  #sel_beta2
  loc <- grep("#sel_beta2$", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = 1, rm_regions)
  
  #sel_beta3
  loc <- grep("#sel_beta3$", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = 1, rm_regions)
  
  #sel_beta4
  loc <- grep("#sel_beta4$", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = 1, rm_regions)
  
  #sel_beta1_survey
  loc <- grep("#sel_beta1_survey", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = 1, rm_regions)
  
  #sel_beta2_survey
  loc <- grep("#sel_beta2_survey", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = 1, rm_regions)
  
  #sel_beta3_survey
  loc <- grep("#sel_beta3_survey", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = 1, rm_regions)
  
  #sel_beta4_survey
  loc <- grep("#sel_beta4_survey", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = 1, rm_regions)
  
  #Rec_Prop
  loc <- grep("#Rec_Prop", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = 1, rm_regions)
  
  #recruits_BM
  loc <- grep("#recruits_BM", em_dat)
  em_dat <- shorten_lines(em_dat, loc, rep = 1, rm_regions)
  
  #>>>>>>>>>>>>>>> CONTINUE HERE
  
  #F
  loc <- grep("#F$", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)

  #Fyear
  loc <- grep("#Fyear", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)

  #biomass_AM
  loc <- grep("#biomass_AM", em_dat)
  em_dat[(loc+1):(loc+orig_reg)] <- shorten_line(em_dat, loc, no_yrs, orig_reg)
  
  #biomass_population - Based on populations not regions so not affected by region
  loc <- grep("#biomass_population", em_dat)
  em_dat[(loc+1)] <- shorten_line(em_dat, loc, no_yrs, 1) #only one line
  
  #catch_at_age_fleet_prop
  loc <- grep("#catch_at_age_fleet_prop", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs*OM_flt, orig_yrs, orig_reg, prop = TRUE)

  #yield_fleet
  loc <- grep("#yield_fleet", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)

  #survey_at_age_fleet_prop
  loc <- grep("#survey_at_age_fleet_prop", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)

  #true_survey_fleet_bio
  loc <- grep("#true_survey_fleet_bio", em_dat)
  em_dat <- shorten_vector(em_dat, loc, no_yrs, orig_yrs, orig_reg)

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

  #abund_frac_year
  loc <- grep("#abund_frac_year", em_dat)
  em_dat[(loc+1):(loc+orig_reg)] <- shorten_line(em_dat, loc, no_yrs, orig_reg)
  
  return(em_dat)
  
}

#Entries within one line of length regions
shorten_line <- function(datfile, var_loc, n_reg) {
  
  unlisted <- strsplit(datfile[(var_loc+1)], split = " ")
  unlisted <- lapply(unlisted, FUN = function(x) if(x[1] == "") x[-1] else x) #remove the empty space in first element if data was indented
  shortened <- unlist(unlisted)[-n_reg]
  return(paste(shortened, collapse = " "))
  
}  

#Entries within one line repeated rep times for each region 
#n_reg is coded as  2 to 3, functionality for regions not in sequence is not available
shorten_lines <- function(datfile, var_loc, rep, n_reg) {
  
  unlisted <- strsplit(datfile[(var_loc+1):(var_loc+(rep*orig_reg))], split = " ")
  unlisted <- lapply(unlisted, FUN = function(x) if(x[1] == "") x[-1] else x) #remove the empty space in first element if data was indented
  shortened <- unlisted[-(((n_reg[1]-1)*rep+1):(n_reg[length(n_reg)]*rep))] #remove the entries from the regions desired
  shortened <- unlist(lapply(shortened, FUN = function(x) paste(x, collapse = " ")))
  
  datfile <- datfile[-c((var_loc+1):(var_loc+(rep*orig_reg)))] #temporarily remove repeated lines
  
  new_datfile <- append(datfile, shortened, after = var_loc) #append remaining years back into element
  
  return(new_datfile)
  
}  


#Entries with years all within one line, possibly repeated by region
shorten_line_old <- function(datfile, var_loc, n_yrs, n_reg) {
  
  unlisted <- strsplit(datfile[(var_loc+1):(var_loc+n_reg)], split = " ")
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
