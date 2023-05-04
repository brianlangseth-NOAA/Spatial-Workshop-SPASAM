library(PBSadmb)
library(dplyr)
####
#User directories
####

if(Sys.getenv("USERNAME") == "Brian.Langseth") {
  code_loc <- "C:\\Users\\Brian.Langseth\\Desktop\\Spatial-Workshop-SPASAM"
}

if(Sys.getenv("USERNAME") == "jonathan.deroba") {
  #code_loc <- "C:\\Spatial_SPASAM_2021_Sim\\Spatial-Workshop-SPASAM-main_area\\Spatial-Workshop-SPASAM-main" #spatial
  code_loc <- "C:\\Spatial_SPASAM_2021_Sim\\Spatial-Workshop-SPASAM-main" #panmictic
}

##########################################################################
#Output model files from prior to workshop into format specified at:
#https://docs.google.com/document/d/1uE0DEi7V_xd8AD7kTMYePS_MWkoY_cTloo4uJff8DuQ/edit
##########################################################################

##
#Get output from model runs and output it in desired format
##


#Single area model
mod_name = "YFT_1area_7fleets_14" 
setwd(file.path(code_loc, mod_name, "Estimation_Model"))
em_rep<-readList(paste0(mod_name,".rep")) #read in .rep file

#code in the if false makes a wonky list of the 100 panmictic fits. New code makes list of lists with one element per data.
#Kept this just so we could check format of data as it was previously provided.
if(false){
#Read in the 100 single area models
for(i in 1:100){
  if(i==1){
  mod_name <- paste0("YFT_1area_normcomp_100sets",i)
  om_rep <- readList(file.path(code_loc, mod_name, "Estimation_Model", paste0(mod_name,".rep")))
  #assign(paste0("om_rep",i),om_rep)
  #cat(paste0("\n Assigned data set",i),"\n ")
  out1 <- summaryResults(om_rep, var_spatial = NULL)
  } else {
  mod_name <- paste0("YFT_1area_normcomp_100sets",i)
  om_rep <- readList(file.path(code_loc, mod_name, "Estimation_Model", paste0(mod_name,".rep")))
  tempout<-summaryResults(om_rep, var_spatial = NULL)
  for(e in 1:length(tempout)){
    if(length(tempout[[e]])==1){
    out1[[e]]=append(out1[[e]],tempout[[e]])
    }
    if(NCOL(tempout[[e]])==1 & NROW(tempout[[e]])>1){
      out1[[e]]=cbind(out1[[e]],tempout[[e]])
    }
    if(NCOL(tempout[[e]])>1 & NROW(tempout[[e]])>1){
      out1[[e]]=rbind(out1[[e]],tempout[[e]])
    }
  }
  }
}
} #end if false

out1=list()
for(i in 1:100){
  mod_name <- paste0("YFT_1area_normcomp_100sets",i)
  om_rep <- readList(file.path(code_loc, mod_name, "Estimation_Model", paste0(mod_name,".rep")))
  tempout<-summaryResults(var=om_rep, var_spatial = NULL)
  tempout$run=i
  out1[[i]]=tempout
}
#Save output as .RData file
save(out1, file = file.path(code_loc, "teamSPASAM_final_pan_newformat.RData"))

######################################
#Spatial model
mod_name = "YFT_2area_4fleets_F0.0001_18alk"
setwd(file.path(code_loc, mod_name, "Estimation_Model"))
em_rep_spatial<-readList(paste0(mod_name,".rep")) #read in .rep file

#Output these into desired format using the funtion below
#Currently this is set up for 1 simulation.
out1 <- summaryResults(var=NULL, var_spatial = em_rep_spatial)
#Save output as .RData file
save(out1, file = file.path(code_loc, "teamSPASAM_final_spatial_singledata.RData"))


#Read in the 100 multi-area models
out1=list()
for(i in 1:100){
    mod_name <- paste0("YFT_4area_100sets_18alk_",i)
    om_rep <- readList(file.path(code_loc, mod_name, "Estimation_Model", paste0(mod_name,".rep")))
    tempout<-summaryResults(var=NULL, var_spatial = om_rep)
    tempout$run=i
    out1[[i]]=tempout
  }

#Save output as .RData file
save(out1, file = file.path(code_loc, "teamSPASAM_wkshp_spatial_allruns.RData"))
########################################



##
#Function to get report file into desired format
##

summaryResults <- function(var=NULL, var_spatial=NULL){
  
  ########### Background info ############################################
  teamSPASAM_team	<- "SPASAM"                 # as character, team name
  teamSPASAM_status	<- "prelim"	              # as character, status of results (prelim, final)
  teamSPASAM_model_types <- "pan and spatial"	# as character, model results included (pan only, pan and spatial)
  teamSPASAM_notes <- NA		                  # as character, any notes regarding outputs
  teamSPASAM_ssb_units <- "weight"		        # as character, units of ssb (weight, # eggs)
  teamSPASAM_bio_units <- "metric tons"		    # as character, units of biomass (metric tons, pounds)
  teamSPASAM_recr_units	<- "1000s of fish"	  # as character, units recruitment or abundance (1000s of fish)
  ##CONFIRM: Unsure of recr_units
  
  teamSPASAM_F_units <- "instant apical F"	  # as character, units of fishing mort (instant apical F, harvest rate)
  ##CONFIRM: Unsure of apical - effectively its fully selected F
  
  teamSPASAM_catch_units <- "numbers 1000s"	  # as character, units of catch (numbers, weight)
  teamSPASAM_BRPs_calc	<- "yes"              # as character, biological reference points estimated? (yes, no)
  teamSPASAM_b0_calc <- "yes"	                # as character, B0 calculated? (yes, no)
  teamSPASAM_BRPs_type	<- "depletion"        # as character, other reference points that were calculated, list all that will be reported below (FMSY, %B0, depletion)
  ##CONFIRM: I write depletion because we have that as an output in the report
  if(!is.null(var)){
  ####### 1-area model results ##########################################
  teamSPASAM_pan_nsims <- 100		                # number of simulation runs provided for 1-area model
  ##TO DO: Need to update this to 100. Right now just working on 1
  
  teamSPASAM_pan_rds_num	<- 4                # the run corresponding to the representative data set (i.e., #4)
  teamSPASAM_pan_nyrs	<- var$nyrs          # number of timesteps for each 1-area model run
  teamSPASAM_pan_flts <- var$nfleets       # number of fleets for each 1-area model run
  if(var$nfleets == 7) {
    teamSPASAM_spat_flts_names	<- c("Gill", "Hand", "LL", "Other", "BB", "PS", "Trol")
  }
  if(var$nfleets == 4) {
    teamSPASAM_spat_flts_names	<-c("Gill-Hand_Trol", "LL", "Other-PS", "BB")
  }
                                           
  teamSPASAM_pan_b0	<- var$SSB_zero	          # unfished spawning biomass (1,nsims)
  teamSPASAM_pan_status_bio	<- last(var$Bratio_population) # terminal year stock status for biomass relative to B0 (1,nsims)
  ##CONFIRM: Should this be Bratio_population (which is SSB) or depletion_region (which is biomass)
  
  teamSPASAM_pan_R0	<- var$R_ave	            # unfished recruitment (1,nsims)
  teamSPASAM_pan_brp <- NA		# other biological reference points (1,nsims)
  ##CONFIRM: We have depletion but they have that above with status_bio. Hence why I have NA here. 
  
  teamSPASAM_pan_ssb <- var$SSB_region		    # spawning biomass (1,nyrs, 1, nsims)
  teamSPASAM_pan_ssb_CV	<- NA                 # uncertainty (coefficient of variation) for ssb (1,nyrs, 1, nsims)
  ##TO DO: Could we access this via the hessian? Normally we would run this 1000 times are get the simulated variance
  
  teamSPASAM_pan_bio <- var$biomass_population # biomass (1,nyrs, 1, nsims)
  teamSPASAM_pan_bio_CV <- NA		              # uncertainty (CV) for bio (1,nyrs, 1, nsims)
  ##TO DO: Same comment as above for ssb_cv
  
  teamSPASAM_pan_recr <- var$recruits_BM		  # recruitment (1,nyrs,1,nsims)
  teamSPASAM_pan_recr_CV <- NA	              # uncertainty (CV) for recruits (1,nyrs,1,nsims)
  ##TO DO: Same comment as above for ssb_cv
  
  teamSPASAM_pan_F <- t(var$F_year)		        # fishing mortality (1,nflts, 1, nyrs, 1, nsims)
  teamSPASAM_pan_F_CV	<- NA                 	# uncertainty (CV) for F (1,nflts, 1, nyrs, 1, nsims)
  ##TO DO: Same comment as above for ssb_cv
  
  teamSPASAM_pan_catch <- rowSums(var$OBS_yield_fleet) # catch (1,nyrs,1, nsims)
  
     } #close if is not null for one are var

  if(!is.null(var_spatial)){
  ####### Spatial model results ##########################################
  teamSPASAM_spat_nsims <- 100		                            # number of simulation runs provided for spatial model
  teamSPASAM_spat_rds_num	<- 4                              # the run corresponding to the representative data set (i.e., #4)
  teamSPASAM_spat_nyrs <- var_spatial$nyrs	                # number of timesteps for each spatial model run
  teamSPASAM_spat_nareas <- var_spatial$nregions	          # number of areas for each spatial model run
  teamSPASAM_spat_flts <- var_spatial$nfleets		            # number of fleets per area for each spatial model run (1, nareas)
  if(var_spatial$nfleets == 7) {
    teamSPASAM_spat_flts_names	<- c("Gill", "Hand", "LL", "Other", "BB", "PS", "Trol")
    }
    if(var_spatial$nfleets == 4) {
      teamSPASAM_spat_flts_names	<-c("Gill-Hand_Trol", "LL", "Other-PS", "BB")
    }
    # as character, fleet names (PS, Trol, BB, LL, Gill, Hand, etc.)
  teamSPASAM_spat_b0 <- var_spatial$SSB_zero		            # unfished spawning biomass (1,nsims)
  teamSPASAM_spat_status_bio <- last(var_spatial$Bratio_population)	# terminal year stock status for biomass relative to B0 (1,nsims)
  ##CONFIRM: Should this be Bratio_population (which is SSB) or depletion_region (which is biomass)
  
  teamSPASAM_spat_R0 <- var_spatial$R_ave		                # unfished recruitment (1,nsims)
  teamSPASAM_spat_brp	<- NA                                 # other biological reference points (1,nsims)
  ##CONFIRM: We have depletion but they have that above with status_bio. Hence why I have NA here. 
  
  teamSPASAM_spat_ssb	<- var_spatial$SSB_region   	        # spawning biomass (1, nareas, 1,nyrs, 1, nsims)
  teamSPASAM_spat_ssb_CV <- NA	# uncertainty (CV) for ssb (1, nareas, 1,nyrs, 1, nsims)
  ##TO DO: Could we access this via the hessian? Normally we would run this 1000 times are get the simulated variance
  
  teamSPASAM_spat_bio	<- var_spatial$biomass_AM	# biomass (1, nareas, 1,nyrs, 1, nsims)
  teamSPASAM_spat_bio_CV <- NA                    	        # uncertainty (CV) for biomass (1, nareas, 1,nyrs, 1, nsims)
  ##TO DO: Same comment as above for ssb_cv
  
  teamSPASAM_spat_recr <-	var_spatial$recruits_BM	# recruitment (1, nareas, 1,nyrs,1,nsims)
  teamSPASAM_spat_recr_CV <- NA                   	        # uncertainty (CV) for recruitment (1, nareas, 1,nyrs,1,nsims)
  ##TO DO: Same comment as above for ssb_cv
  
  teamSPASAM_spat_recr_apport	<- t(var_spatial$Rec_Prop)    # recruitment apportionment by area ( 1,nyrs,1,nsims, 1, nareas)
  teamSPASAM_spat_move <- 		                              # movement matrix (1,nyrs,1,nsims,1, nareas, 1, nareas)
    array(cbind(var_spatial$T_year[1:var_spatial$nyrs,], 
          var_spatial$T_year[(var_spatial$nyrs + 1):(2*var_spatial$nyrs),]), dim = c(var_spatial$nyrs, var_spatial$nregions, var_spatial$nregions)) 
  #For F need lots of extra steps
  #Var_spatial$F_year entered as 151 years for area 1 (as rows) for each fleet (as columns) followed by 151 years for area 2 (as the next set of rows)  
  temp_F_year <- cbind(rep(1:var_spatial$nyrs, var_spatial$nregions), rep(1:var_spatial$nregions, each = var_spatial$nyrs),var_spatial$F_year)
  #Reorder so these are ordered by the first year for each area, followed by the second year for each area, etc.
  F_year_reord <- temp_F_year[order(temp_F_year[,1]),] #order F_year based on year first (column 1), then region (column2)
  #Set up as 3D array with correct dimensions
  new_F_year <- array(unlist(split(data.frame(F_year_reord[,-c(1,2)]), rep(1:var_spatial$nyrs, each  = var_spatial$nregions))), c(var_spatial$nregions, var_spatial$nfleets, var_spatial$nyrs))
  teamSPASAM_spat_F <- new_F_year                        		# fishing mortality (1, nareas, 1,nflts, 1, nyrs, 1, nsims)
  teamSPASAM_spat_F_CV <- NA                    		        # uncertainty (CV) for F (1,areas, 1,nflts, 1, nyrs, 1, nsims)
  ##TO DO: Same comment as above for ssb_cv
  
  teamSPASAM_spat_catch	<-                                  # catch (1,nareas,1,nyrs,1, nsims)
    rbind(rowSums(var_spatial$OBS_yield_fleet[1:var_spatial$nyrs,]),
          rowSums(var_spatial$OBS_yield_fleet[(var_spatial$nyrs+1):(2*var_spatial$nyrs),]))
  } #close if is not null spatial var_spatial
  #Make a list of all variable values and name them with their variable
  output = lapply(ls()[grep("team",ls())], dynGet)
  names(output) = ls()[grep("team",ls())]
  
  return(output)
  
}
