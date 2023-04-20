#rm(list=(ls()))

###code to produce diagnostics for all one-area YFT runs, and plot distributions of estimated quantities
#Jon Deroba November 16, 2022
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
#####################################code from YFT_diagnostics
#load libraries
library(ggplot2)
library(ggforce)
library(reshape2)
library(dplyr)
library(matrixStats)
library(grid)
library(gridExtra)
library(PBSadmb)
library(gtable)
library(corrplot)

######################################################################
########### PLOT SETTINGS ############################################
######################################################################

##############################################
#plot set up 
##############################################
#set up the colors you want to use for TRUE and ESTIMATED
t.col="black" #true
e.col="blue"  #estimated

#set up the color that are wanted for MOVEMENT plot - used a color ramp
mycols=colorRampPalette(c("blue", "cyan","black"))

# Select Line width
line.wd=0.8

#select threshold correlation level for corrlation plot
cor.level = 0.8 #can change this in the make.plots function below


##############################################
#Residual values
##############################################

#set residual switch for different values plotted
resid.switch=2
# =1 straight residual (TRUE-ESTIMATED; not % of true)
# =2 Relative percent difference ((TRUE/ESTIMATED)/TRUE *100)




#########################################################
#########################################################
##### Ploting Code        ###############################
##### Look at the outputs ###############################
#########################################################
#########################################################

#plot and output function
#optional plot.comps takes a long time (~45 minutes), optional plot.yearly.abund takes ~1 minutes
make.plots<-function(direct=EM_direct, plot.comps = FALSE, plot.yearly.abund = FALSE){ #run diagnostics plotting
  
  #Read in model .rep
  out<-readList(paste(EM_direct,paste0(EM_name,".rep"),sep="\\")) #read in .rep file
  
  #use the pbsadmb package to get the cor and std files in
  
  cor.name<-paste0(EM_name,".cor")
  
  if (file.exists(cor.name)) {
    cor<-readRep(EM_name, suffix=(".cor"), global=FALSE)
  }
  
  
  std.name<-paste0(EM_name,".std")
  if (file.exists(std.name)) {
    std<-readRep(EM_name, suffix=".std", global=FALSE)
  }
  
  
  
  #pull info about the model
  na<-out$nages
  nyrs<-out$nyrs
  npops_OM<-out$npops_OM
  npops<-out$npops
  nreg_OM<-out$nregions_OM
  nreg<-out$nregions
  years<-seq(1:out$nyrs)
  ages<-seq(1:out$nages)
  fleets<-out$nfleets
  tag.age.switch<-out$fit_tag_age_switch
  
  #for running the meta pop example. Might need fixing if more complex
  if(npops_OM>1){
    nreg_OM=sum(nreg_OM)}
  if(npops>1){
    nreg=sum(nreg)}
  
  ###################################
  #Set my theme for ggplot plotting
  #################################
  
  diag_theme<-
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          legend.background = element_rect(fill="transparent"),
          panel.border = element_rect(colour = "black"))+
    theme(legend.title = element_blank())+
    theme(strip.text.x = element_text(size = 10, colour = "black", face="bold"))
  
  ######################################################################################
  
  ######################################################################################
  #Likelihoods in histagram
  
  #vector of values 
  likes<-data.frame(names=c("Catch","Survey","Fishery_age","Survey_age","Recruitment","Tags"),likes=c(out$catch_like,out$survey_like,out$fish_age_like,out$survey_age_like,out$rec_like,out$tag_like))
  
  # change the order
  likes$names <- factor(likes$names, as.character(likes$names))
  
  
  like.p <-ggplot(likes, aes(names, likes))+
    geom_bar(stat = "identity", col="grey60")+
    xlab(" ")+
    ylab("Values")+
    ggtitle("Likelihood Components")+
    diag_theme+
    theme(axis.text.x=element_text(angle=40,hjust=0.5,vjust=0.5),plot.margin = unit(c(0.5, 0.5, 2, 0.5),"lines"))
  
  ##############################
  #### RECRUITMENT PLOTS #######
  ##############################
  #rec total
  #Matching - might need to refine for additional mismatches
  if(nreg_OM==nreg){ 
    rec.total<-data.frame(Year=rep(years,nreg), Reg=rep(c(1:nreg),each=nyrs),Rec_Est = as.vector(t(out$recruits_BM))) #, Rec_True=as.vector(t(out$recruits_BM_TRUE)))
  }
  
  #spatial to panmictic
  if(nreg_OM>1 && nreg==1){ 
    #aggregating rec true  
    #Rec_True=colSums(out$recruits_BM_TRUE)
    
    rec.total<-data.frame(Year=rep(years,nreg), Reg=rep(c(1:nreg),each=nyrs), Rec_Est = as.vector(t(out$recruits_BM)))} #,Rec_True=Rec_True)}
  
  
  rec.total.plot<-melt(rec.total,id=c("Reg","Year"))
  rec.total.plot$Reg<-as.factor(rec.total.plot$Reg)
  
  rec1<-ggplot(rec.total.plot,aes(Year,value))+
    geom_line(aes(col = variable,linetype=variable), stat = "identity", lwd=line.wd)+
    facet_wrap(~Reg)+
    scale_color_manual(values = c(e.col,t.col),labels = c("Estimated","True"))+
    scale_linetype_manual(values=c(1,2),labels = c("Estimated","True"))+
    ylab("Recruitment")+
    diag_theme+
    theme(legend.position = c(1, 1), legend.justification = c(1,1))+
    ggtitle("Recruitment Total")
  
  #matching panmictic/metamictic or mismatch spatial to panmictic
  if(npops==1 && npops_OM==1){
    
    rec.devs<-data.frame(Year=years[-1],Rec_Dev_Est=out$rec_devs) #, Rec_Dev_True=out$rec_devs_TRUE[2:length(out$rec_devs_TRUE)])
    
    rec.devs.plot<-melt(rec.devs,id=c("Year"))
    
    rec2<-ggplot(rec.devs.plot,aes(Year,value))+
      geom_line(aes(col = variable,linetype=variable), stat = "identity", lwd=line.wd)+
      theme_bw()+
      #facet_wrap(~Reg)+
      #geom_hline(aes(yintercept=out$R_ave), col = e.col)+
      #geom_hline(aes(yintercept=out$R_ave_TRUE), col = t.col, lty=2)+
      scale_color_manual(values = c(e.col,t.col),labels = c("Estimated","True"))+
      scale_linetype_manual(values=c(1,2),labels = c("Estimated","True"))+
      ylab("Recruitment")+
      diag_theme+
      theme(legend.position = c(1, 1), legend.justification = c(1,1))+
      ggtitle("Recruitment Deviations")
    
  }
  
  #################################
  #### ABUNDANCE PLOTS ############
  #################################
  
  ####################
  #initial abundance
  
  #panmictic/metamictic matching  
  if(nreg_OM==nreg){ 
    init.abund<-data.frame(Age=rep(ages,nreg), Reg=rep(c(1:nreg),each=na),In_ab_Est = as.vector(t(out$Init_Abund))) #, In_ab_True=as.vector(t(out$Init_Abund_TRUE)))
  }
  #prepare for plot
  init.abund.plot<-melt(init.abund,id=c("Reg","Age"))
  init.abund.plot$Reg<-as.factor(init.abund.plot$Reg)
  
  #plot
  init.ab<-ggplot(init.abund.plot,aes(Age, value))+
    geom_line(aes(col = variable,linetype=variable), stat = "identity", lwd=line.wd)+
    facet_wrap(~Reg)+
    scale_color_manual(values = c(e.col,t.col),labels = c("Estimated","True"))+
    scale_linetype_manual(values=c(1,2),labels = c("Estimated","True"))+
    ylab("Abundance")+
    diag_theme+
    theme(legend.position = c(1, 1), legend.justification = c(1,1))+
    ggtitle("Initial Abundance")  
  
  
  #other years abundance - only applicable for one region
  abund.at.age.long <- data.frame("Year" = rep(years,na), "Age" = rep(ages, each = nyrs), stack(data.frame(out$abund_at_age_AM))[1])
  
  for(i in 1:ceiling(nyrs/10)){
    assign(paste0("yearly.ab",i),
           ggplot(abund.at.age.long, aes(Age, values))+
             geom_line(stat = "identity", lwd=line.wd)+
             facet_wrap_paginate(~Year, ncol = 1, nrow = 10, page = i, scale = "free_y")+
             ylab("Abundance")+
             diag_theme+
             ggtitle("Abundance at age over time"))
  }
  
  
  
  #################################
  # Total Biomass
  
  if(nreg_OM==nreg){ 
    bio.dat<-data.frame(Year=rep(years,nreg), Reg=rep(c(1:nreg),each=nyrs),Bio_est = as.vector(t(out$biomass_AM))) #, Bio_True=as.vector(t(out$biomass_AM_TRUE)))
  }
  
  bio.plot<-melt(bio.dat,id=c("Reg","Year"))
  bio.plot$Reg<-as.factor(bio.plot$Reg)
  
  bio.p<-ggplot(bio.plot,aes(Year,value))+
    geom_line(aes(col = variable,linetype=variable), stat = "identity", lwd=line.wd)+
    facet_wrap(~Reg)+
    scale_color_manual(values = c(e.col,t.col),labels = c("Predicted","True"))+
    scale_linetype_manual(values=c(1,2),labels = c("Predicted","True"))+
    ylab("Total Biomass")+
    diag_theme+
    theme(legend.position = c(1, 1), legend.justification = c(1,1))+
    ggtitle("Total Biomass")
  
  ###############################
  #SSB
  #matching
  if(nreg_OM==nreg){ 
    ssb.dat<-data.frame(Year=rep(years,nreg), Reg=rep(c(1:nreg),each=nyrs),SSB_est = as.vector(t(out$SSB_region)))#, SSB_True=as.vector(t(out$SSB_region_TRUE)))
  }
  ssb.plot<-melt(ssb.dat,id=c("Reg","Year"))
  ssb.plot$Reg<-as.factor(ssb.plot$Reg)
  
  ssb.p<-ggplot(ssb.plot,aes(Year,value))+
    geom_line(aes(col = variable,linetype=variable), stat = "identity", lwd=line.wd)+
    facet_wrap(~Reg)+
    scale_color_manual(values = c(e.col,t.col),labels = c("Predicted","True"))+
    scale_linetype_manual(values=c(1,2),labels = c("Predicted","True"))+
    ylab("SSB")+
    diag_theme+
    theme(legend.position = c(1, 1), legend.justification = c(1,1))+
    ggtitle("SSB")
  
  #####################
  #Fishery Selectivity
  
  #if(nreg_OM==nreg){ 
  #  f.select<-data.frame(Age=rep(ages,nreg), Reg=rep(c(1:nreg),each=na),Select_Est = as.vector(t(out$selectivity_age)))#, Select_T=as.vector(t(out$selectivity_age_TRUE)))
  #}
  f.select<-data.frame(Flt=rep(c(1:fleets),each=na),Age=rep(ages,fleets))
  f.select<-f.select[order(f.select$Age),]
  f.select=data.frame(f.select,Select_Est = as.vector(t(out$selectivity_age)))#, Select_T=as.vector(t(out$selectivity_age_TRUE)))
  f.select.plot<-melt(f.select,id=c("Flt","Age"))
  f.select.plot$Flt<-as.factor(f.select$Flt)
  
  f.select.p<-ggplot(f.select.plot,aes(Age, value))+
    geom_line(aes(col = variable,linetype=variable), stat = "identity", lwd=line.wd)+
    facet_wrap(~Flt)+
    scale_color_manual(values = c(e.col,t.col),labels = c("Estimated","True"))+
    scale_linetype_manual(values=c(1,2),labels = c("Estimated","True"))+
    ylab("Selectivity")+
    diag_theme+
    theme(legend.position = c(1, 0), legend.justification = c(1,0))+
    ggtitle("Fishery Selectivity")
  
  ########################
  #Survey Selectivity
  
  if(nreg_OM==nreg){ 
    s.select<-data.frame(Age=rep(ages,nreg), Reg=rep(c(1:nreg),each=na),Select_Est = as.vector(t(out$survey_selectivity_age)))#,Select_T=as.vector(t(out$survey_selectivity_age_TRUE)))
  }
  s.select.plot<-melt(s.select,id=c("Reg","Age"))
  s.select.plot$Reg<-as.factor(s.select$Reg)
  
  s.select.p<-ggplot(s.select.plot,aes(Age, value))+
    geom_line(aes(col = variable,linetype=variable), stat = "identity", lwd=line.wd)+
    facet_wrap(~Reg)+
    scale_color_manual(values = c(e.col,t.col),labels = c("Estimated","True"))+
    scale_linetype_manual(values=c(1,2),labels = c("Estimated","True"))+
    ylab("Selectivity")+
    diag_theme+
    theme(legend.position = c(1, 0), legend.justification = c(1,0))+
    ggtitle("Survey Selectivity")
  
  ############################################################
  ########## F by year #######################################
  ############################################################
  
  #combining true and estimated together
  #f.max<-rowMaxs(out$F)
  #f.max.t<-rowMaxs(out$F_TRUE)
  
  f.fleet=out$F_year
  #if(nreg_OM==nreg){ 
  # F.year<-data.frame(Year=rep(years,nreg), Reg=rep(c(1:nreg),each=nyrs),F_year=f.max)#, F_year_T=f.max.t)
  #}
  F.year<-data.frame(Flt=rep(c(1:fleets),each=nyrs),Year=rep(years,fleets))
  #F.year<-F.year[order(F.year$Year),]
  F.year=data.frame(F.year,F_Est = as.vector(f.fleet))#, Select_T=as.vector(t(out$selectivity_age_TRUE)))
  F.plot<-melt(F.year,id=c("Flt","Year"))
  F.plot$Flt<-as.factor(F.plot$Flt)
  
  #F.plot<-melt(F.year,id=c("Reg","Year"))
  #F.plot$Reg<-as.factor(F.plot$Reg)
  
  F.plot.p<-ggplot(F.plot,aes(Year,value))+
    geom_line(aes(col = variable,linetype=variable), stat = "identity", lwd=line.wd)+
    theme_bw()+
    facet_wrap(~Flt)+
    scale_color_manual(values = c(e.col,t.col),labels = c("Estimated","True"))+
    scale_linetype_manual(values=c(1,2),labels = c("Estimated","True"))+
    ylab("F")+
    diag_theme+
    theme(legend.position = c(1, 1), legend.justification = c(1,1))+
    ggtitle("Fully selected F by Year")
  
  ############################################################
  ########## Fits to data ####################################
  ############################################################
  
  ##############
  # Yield
  
  #need to fix this for fleets as areas 
  Y.year<-data.frame(Flt=rep(c(1:fleets),each=nyrs),Year=rep(years,fleets),Estimated=as.vector(out$yieldN_fleet),Observed=as.vector(out$OBS_yield_fleet))
  
  Y.year.plot<-melt(Y.year, id=c("Flt","Year"))
  #Fleet.a=rep(c(1:fleets),each=nyrs)
  #Fleet=rep(Fleet.a,times=nreg)
  #Y.year.plot$Fleet=Fleet
  #Y.year.plot$variable=gsub(paste0(".",'[0-9]'),'',Y.year.plot$variable)
  
  yield.p<-ggplot(Y.year.plot,aes(Year,value,shape=variable))+
    geom_line(aes(col = variable,linetype=variable), stat = "identity", lwd=line.wd)+
    geom_point(size=2, alpha = 0.5)+
    scale_shape_manual(values=c(NA,16),labels = c("Estimated","Observed"))+
    facet_wrap(~Flt)+
    scale_color_manual(values = c(e.col,t.col),labels = c("Estimated","Observed"))+
    scale_linetype_manual(values=c(1,0),labels = c("Estimated","Observed"))+
    ylab("Yield (N)")+
    diag_theme+
    theme(legend.position = c(1, 1), legend.justification = c(1,1))+
    ggtitle("Yield")
  
  yield.p.zoomIn<-ggplot(Y.year.plot,aes(Year,value,shape=variable))+
    geom_line(aes(col = variable,linetype=variable), stat = "identity", lwd=line.wd)+
    geom_point(size=2, alpha = 0.5)+
    scale_shape_manual(values=c(NA,16),labels = c("Estimated","Observed"))+
    facet_wrap(~Flt, scales = "free")+
    scale_color_manual(values = c(e.col,t.col),labels = c("Estimated","Observed"))+
    scale_linetype_manual(values=c(1,0),labels = c("Estimated","Observed"))+
    ylab("Yield (N)")+
    diag_theme+
    theme(legend.position = c(1, 1), legend.justification = c(1,1))+
    ggtitle("Yield Zoomed In")+
    coord_cartesian(ylim=c(0,7500))
  
  # calculate residuals
  
  #if(resid.switch==1){
  #  Y.year$resid<-(Y.year$Observed-Y.year$Estimated)}
  
  #if(resid.switch==2){
  #Y.year$resid<-((Y.year$Estimated-Y.year$Observed)/Y.year$Observed)*100 #JJD
  #Y.year[,paste0("resid.",seq(1:fleets))]=((Y.year[,paste0("Estimated.",seq(1:fleets))]-Y.year[,paste0("Observed.",seq(1:fleets))])/Y.year[,paste0("Observed.",seq(1:fleets))])*100
  Y.year$resid=(Y.year$Estimated-Y.year$Observed)/Y.year$Observed*100
  #}
  
  #y.resid.p<-melt(Y.year[,c(1,2,5)],id=c("Reg","Year")) #JJD
  #y.resid.p<-melt(Y.year[,c(1,2,(ncol(Y.year)-(fleets-1)):ncol(Y.year))], id=c("Reg","Year"))
  y.resid.p<-melt(Y.year,id=c("Flt","Year"))
  #y.resid.p$Fleet=Fleet #Fleet defined above for Y.year.plot
  #y.resid.p$variable=gsub(paste0(".",'[0-9]'),'',y.resid.p$variable)
  y.resid.p=y.resid.p[y.resid.p$variable=="resid",]
  #
  Y.resid.plot<-ggplot(y.resid.p,aes(Year,value))+
    geom_hline(aes(yintercept=0), col = "grey20", lty = 2)+
    geom_point(aes(color=value),size=2, alpha = 0.9, pch=16)+
    theme_bw()+
    scale_color_gradient2(low="red",mid="grey",high ="blue")+
    ylab("Difference (True-Estimated)")+
    facet_wrap(~Flt)+
    diag_theme+
    theme(legend.position = "none", legend.justification = c(1,1))+
    ggtitle("Yield Residuals")
  
  ################
  # Survey Index
  
  Survey.year<-data.frame(Year=rep(years,nreg), Reg=rep(c(1:nreg),each=nyrs), SI_Est=out$survey_fleet_bio,SI_Obs=out$OBS_survey_fleet_bio ) 
  
  Survey.year.plot<-melt(Survey.year, id=c("Reg","Year"))
  
  survey.p<-ggplot(Survey.year.plot,aes(Year,value,shape=variable))+
    geom_line(aes(col = variable,linetype=variable), stat = "identity", lwd=line.wd)+
    geom_point(size=2, alpha = 0.5)+
    scale_shape_manual(values=c(NA,16),labels = c("Estimated","Observed"))+
    facet_wrap(~Reg)+
    scale_color_manual(values = c(e.col,t.col),labels = c("Estimated","Observed"))+
    scale_linetype_manual(values=c(1,0),labels = c("Estimated","Observed"))+
    ylab("Survey Index")+
    diag_theme+
    theme(legend.position = c(1, 1), legend.justification = c(1,1))+
    ggtitle("Survey Biomass")
  
  
  #Calc resids
  #if(resid.switch==1){
  #  Survey.year$resid<-(Survey.year$SI_Obs-Survey.year$SI_Est)}
  
  #if(resid.switch==2){
  Survey.year$resid<-((Survey.year$SI_Est-Survey.year$SI_Obs)/Survey.year$SI_Obs)*100
  #}
  
  surv.resid.p<-melt(Survey.year[,c(1,2,5)],id=c("Reg","Year"))
  
  
  #
  Surv.resid.plot<-ggplot(surv.resid.p,aes(Year,value))+
    geom_hline(aes(yintercept=0), col = "grey20", lty = 2)+
    geom_point(aes(color=value),size=2, alpha = 0.9, pch=16)+
    theme_bw()+
    scale_color_gradient2(low="red",mid="grey",high ="blue")+
    ylab("Difference (True-Estimated)")+
    facet_wrap(~Reg)+
    diag_theme+
    theme(legend.position = "none", legend.justification = c(1,1))+
    ggtitle("Survey Index Residuals")
  
  ###################################################
  # Age Compositions
  ##################################################
  
  ####################
  #survey age comps
  
  survey.comps.resid<-data.frame(Year=rep(years,nreg), Reg=rep(c(1:nreg),each=nyrs))
  
  
  if(npops==1){
    survey.prop.resid<-data.frame((out$EST_survey_prop-out$OBS_survey_prop))
    survey.comps.resid<-cbind(survey.comps.resid,survey.prop.resid)
    survey.long<-melt(survey.comps.resid,id.vars=c("Year","Reg"))
  }
  
  
  if(npops>1){
    
    #combine movements age_comps
    pull.surv.obs<-out[grep("OBS_survey_prop", names(out), value = TRUE)]
    pull.surv.est<-out[grep("EST_survey_age_prop", names(out), value = TRUE)]
    
    surv_obs<-data.frame(do.call("rbind",pull.surv.obs))
    surv_est<-data.frame(do.call("rbind",pull.surv.est))
    
    survey.prop.resid<-(surv_est-surv_obs)
    survey.comps.resid<-cbind(survey.comps.resid,survey.prop.resid)
    survey.long<-melt(survey.comps.resid,id.vars=c("Year","Reg"))
  }
  
  
  survey.comp.plot<-
    ggplot(survey.long, aes(x = as.numeric(variable), y = Year)) + 
    geom_raster(aes(fill=value)) + 
    #scale_fill_gradient2(low="red",mid="grey99",high ="blue",limits=c(-100, 100))+
    scale_fill_gradient2(low="red",mid="grey99",high ="blue")+
    labs(fill = "% Dif")+
    scale_y_continuous(trans = "reverse")+
    labs(x="Age", y="Year", title="Survey Age Comp Residuals") +
    facet_grid(Reg~.)+
    theme_bw() + 
    theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
          axis.text.y=element_text(size=9),
          plot.title=element_text(size=11))+
    #theme(legend.title = element_blank())+
    theme(strip.text.x = element_text(size = 10, colour = "black", face="bold"))
  
  ######################
  #fishery age comps
  
  fishery.comps.resid<-data.frame(Year=rep(years,fleets),Flt=rep(c(1:fleets),each=nyrs))
  fishery.comps.resid=fishery.comps.resid[with(fishery.comps.resid,order(Year)),]
  Age=rep(ages,(fleets*nyrs))
  Age=Age[order(Age)]
  fishery.comps.resid=cbind(fishery.comps.resid,"variable"=Age)
  
  if(npops==1){
    
    fishery.prop.resid<-data.frame(out$OBS_catch_prop-out$EST_catch_age_fleet_prop)
    #    fishery.prop.resid<-data.frame((out$OBS_catch_prop-out$EST_catch_age_fleet_prop)/out$OBS_catch_prop)
    fishery.long<-data.frame(fishery.comps.resid,resid=stack(fishery.prop.resid)[1])
    names(fishery.long)[names(fishery.long)=="values"]="value"
    #fishery.long<-melt(fishery.comps.resid,id.vars=c("Year","Flt"))
    
    ###
    #Add plot of fit to overall comp data
    #DECISION - Right now combined years based on consistent Nsamp of 5, but should it be actual comp?
    fishery.long.obs <- data.frame(fishery.comps.resid, stack(data.frame(out$OBS_catch_prop))[1])
    fishery.long.exp <- data.frame(fishery.comps.resid, stack(data.frame(out$EST_catch_age_fleet_prop))[1])
    
    fishery.long.obs.prop <- 
      fishery.long.obs %>%
      group_by(Flt) %>%
      mutate("prop" = values/sum(values))
    
    #Currently, this is the aggregate across all years, not just the years with observations
    #This is not the plot that is output
    fishery.long.exp.prop <- 
      fishery.long.exp %>%
      group_by(Flt) %>%
      mutate("prop" = values/sum(values))
    
    agg.fishery.comp.plot <- 
      ggplot() +
      geom_bar(data = fishery.long.obs.prop, aes(variable,prop, fill = "gray"), stat = "Identity") +
      geom_line(data = aggregate(prop~Flt+variable, fishery.long.exp.prop, sum), aes(variable, prop, color = "red"), lwd=line.wd) +
      facet_wrap(~Flt, scale = "free") +
      scale_color_manual(values = c(e.col),labels = c("Estimated"))+
      scale_fill_manual(values = c("gray35"),labels = c("Observed"))+
      ylab("Proportion")+
      xlab("Ages")+
      ggtitle("Catch comps aggregated across ALL years")+
      diag_theme+
      theme(legend.position = c(0.5, 0.15),
            legend.spacing.y = unit(0.01,"cm"),
            legend.text = element_text(size = 12))
    
    #DECISION - This is aggregated across only those years that have observations. This is the plot outputted
    obs.yr.flt <- unique(fishery.long.obs.prop[which(fishery.long.obs.prop$values>0),c("Year","Flt")]) %>% mutate("obs" = 1)
    fishery.long.exp.prop.subset <- 
      fishery.long.exp %>%
      group_by(Flt) %>%
      left_join(obs.yr.flt) %>%
      filter(obs == 1) %>%
      mutate("prop" = values/sum(values))
    
    agg.fishery.comp.plot.subset <- 
      ggplot() +
      geom_bar(data = fishery.long.obs.prop, aes(variable,prop, fill = "gray"), stat = "Identity") +
      geom_line(data = aggregate(prop~Flt+variable, fishery.long.exp.prop.subset, sum), aes(variable, prop, color = "red"), lwd=line.wd) +
      facet_wrap(~Flt, scale = "free") +
      scale_color_manual(values = c(e.col),labels = c("Estimated"))+
      scale_fill_manual(values = c("gray35"),labels = c("Observed"))+
      ylab("Proportion")+
      xlab("Ages")+
      ggtitle("Catch comps aggregated across years with observed data")+
      diag_theme+
      theme(legend.position = c(0.5, 0.15),
            legend.spacing.y = unit(0.01,"cm"),
            legend.text = element_text(size = 12))
    
    
    ###
    #Add plot of years/fleets with comps data avilable
    comp.data.avail <- 
      ggplot(data = fishery.long.obs[fishery.long.obs$values>0, ], aes(x = Year, y = Flt))+
      geom_point(shape = 1, size = 2)+
      ylab("Fleet")+
      xlab("Year")+
      ggtitle("Years with comp data by fleet")
  }
  
  
  
  if(npops>1){
    #combine movements age_comps
    pull.catch.obs<-out[grep("OBS_catch_prop", names(out), value = TRUE)]
    pull.catch.est<-out[grep("EST_catch_age_fleet_prop", names(out), value = TRUE)]
    
    catch_obs<-data.frame(do.call("rbind",pull.catch.obs))
    catch_est<-data.frame(do.call("rbind",pull.catch.est))
    
    fishery_resid<-(catch_est-catch_obs)
    
    fishery.comps.resid<-cbind(fishery.comps.resid,fishery_resid)
    fishery.long<-melt(fishery.comps.resid,id.vars=c("Year","Reg"))
  }
  
  
  fishery.comp.plot<-
    ggplot(fishery.long, aes(x = as.numeric(variable), y = Year)) + 
    geom_raster(aes(fill=value)) + 
    #scale_fill_gradient2(low="red",mid="grey99",high ="blue",limits=c(-100, 100))+
    labs(fill = "% Dif")+
    scale_fill_gradient2(low="red",mid="grey99",high ="blue")+
    scale_y_continuous(trans = "reverse")+
    labs(x="Age", y="Year", title="Fishery Age Comp Residuals") +
    facet_grid(Flt~.)+
    theme_bw() + 
    theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
          axis.text.y=element_text(size=9),
          plot.title=element_text(size=11))+
    #theme(legend.title = element_blank())+
    theme(strip.text.x = element_text(size = 10, colour = "black", face="bold"))
  
  
  #Add a bubble plot of comps in years where observations are
  fishery.long.bubble <- fishery.long[fishery.long.obs$values>0,]
  fishery.long.bubble$col = as.factor(fishery.long.bubble$value<0)
  
  fishery.comp.plot.bubble<-
    ggplot(fishery.long.bubble[fishery.long.bubble$Flt %in% c(1:3),], aes(x = Year, y = as.numeric(variable), size = abs(value), shape = col)) + 
    geom_point(alpha = 0.7) +
    scale_size_area(max_size = 5, trans = "identity", name="Residual") +
    scale_shape_manual(values = c(19, 1), labels = c("Obs-Exp > 0", "Obs-Exp < 0")) +
    facet_grid(Flt~.) +
    labs(shape = "Direction")+
    labs(x="Age", y="Year", title="Fishery Age Comp Residuals Fleets") +
    theme_bw() + 
    theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
          axis.text.y=element_text(size=9),
          plot.title=element_text(size=11),
          legend.position = "bottom") +
    #theme(legend.title = element_blank())+
    theme(strip.text.x = element_text(size = 10, colour = "black", face="bold"))
  
  fishery.comp.plot.bubble2<-
    ggplot(fishery.long.bubble[fishery.long.bubble$Flt %in% c(4:7),], aes(x = Year, y = as.numeric(variable), size = abs(value), shape = col)) + 
    geom_point(alpha = 0.7) +
    scale_size_area(max_size = 5, trans = "identity", name="Residual") +
    scale_shape_manual(values = c(19, 1), labels = c("Obs-Exp > 0", "Obs-Exp < 0")) +
    facet_grid(Flt~.) +
    labs(shape = "Direction")+
    labs(x="Age", y="Year", title="Fishery Age Comp Residuals Fleets") +
    theme_bw() + 
    theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
          axis.text.y=element_text(size=9),
          plot.title=element_text(size=11),
          legend.position = "bottom") +
    #theme(legend.title = element_blank())+
    theme(strip.text.x = element_text(size = 10, colour = "black", face="bold"))
  
  
  #yearly fishery age comps
  for(i in 1:ceiling(nyrs/5)){
    assign(paste0("yr.fishery.comp.plot",i), 
           ggplot() +
             geom_bar(data = fishery.long.obs.prop, aes(variable, values, fill = "gray"), stat = "Identity") +
             geom_line(data = fishery.long.exp.prop, aes(variable, values, color = "red"), lwd=line.wd) +
             facet_wrap_paginate(~Year+Flt, ncol = 7, nrow = 5, page = i)+
             ylab("Proportion")+
             xlab("Ages")+
             ggtitle("Catch comps by year (rows - top number) by fleet (columns - bottom number)")+
             labs(fill = "",
                  color = "",
                  x = NULL,
                  y = NULL)+
             scale_color_manual(values = c(e.col),labels = c("Estimated"))+
             scale_fill_manual(values = c("gray35"),labels = c("Observed"))+
             diag_theme)
  }
  
  
  
  
  ##############################################
  setwd(EM_direct)
  # Create a text grob
  ############################################
  # CONVERGENCE etc
  ###########################################
  
  #did model converge?
  if(file.exists(paste0(EM_name,".cor"))==TRUE)
  {cor.text<-paste("Model Converged")}
  
  if(file.exists(paste0(EM_name,".cor"))==FALSE)
  {cor.text<-paste("Model did NOT Converge")}
  
  #max gradient
  
  grad<-readLines(paste0(EM_name,".par"))[1]
  pos = regexpr('Max', grad)
  grad.val<-substr(grad,pos,nchar(grad))
  
  tgrob.mod <- textGrob(paste(cor.text," ",grad.val,sep = "\n"),just = "centre")
  tgrob <- textGrob("YFT Diagnostics",just = "centre")
  gt=textGrob("YFT Diagnostics",just = "centre")
  #tgrob.mod=textGrob("YFT Diagnostics",just = "centre")
  #gt_est=textGrob("YFT Diagnostics",just = "centre")
  #gt_true=textGrob("YFT Diagnostics",just = "centre")
  
  #generate pdf with plots
  pdf("Model_Diagnostics.pdf",paper='letter') 
  
  #grid.arrange(ncol=1,
  #            top=textGrob("TIM - YFT Diagnostics", gp=gpar(fontsize=18,font=3)),
  #           tgrob,
  #          gt)
  # grid.arrange(ncol=1,textGrob("TIM - YFT Diagnostics", gp=gpar(fontsize=18,font=3)))
  
  # for first layout
  #ly<-rbind(c(1,2),c(3,4))
  grid.arrange(ncol=1,
               top=textGrob("TIM - YFT Diagnostics", gp=gpar(fontsize=18,font=3)),tgrob.mod,like.p)
  
  grid.arrange(ncol=1,
               #top="Recruitment",
               rec1,rec2)
  
  grid.arrange(ncol=1,init.ab,bio.p,ssb.p)
  
  grid.arrange(ncol=1,f.select.p,s.select.p)
  
  grid.arrange(ncol=1,F.plot.p)
  
  grid.arrange(ncol=1,yield.p,Y.resid.plot)
  
  grid.arrange(ncol=1,yield.p.zoomIn)
  
  grid.arrange(ncol=1,survey.p,Surv.resid.plot)
  
  grid.arrange(ncol=1, fishery.comp.plot,survey.comp.plot)
  
  grid.arrange(ncol=1, fishery.comp.plot.bubble)
  
  grid.arrange(ncol=1, fishery.comp.plot.bubble2)
  
  grid.arrange(ncol=1, agg.fishery.comp.plot.subset)
  
  grid.arrange(ncol=1, comp.data.avail)
  
  if(plot.comps){  
    for(i in 1:ceiling(nyrs/5)){
      grid.arrange(ncol=1, get(paste0("yr.fishery.comp.plot",i)))
    }
  }
  
  if(plot.yearly.abund){
    for(i in 1:ceiling(nyrs/10)){
      grid.arrange(ncol=1, get(paste0("yearly.ab",i)))
    }
  }
  
  
  
  
  
  ################################################
  ###############################################
  #print these plots to pdf in the EM folder
  #save as PDF
  dev.off()
  
  ################################################
  #Plot corrlation Matrix
  ################################################
  
  #if cor file exists
  if (file.exists(cor.name)) {
    
    cor.mat<-as.matrix(cor)
    
    #removing diag
    cor.mat[cor.mat==1]<-0
    #removing low correlations
    cor.mat[abs(cor.mat)<cor.level]<-0
    
    rm<-which(colSums(cor.mat)==0)
    
    #remove the parameter combinations with weak correlations.
    cor2<-as.matrix(cor[-rm,-rm])
    
    
    #create a corplot
    pdf("Correlation_Matrix.pdf", paper = "a4", width = 8, height = 11)
    
    if(nrow(cor2)==0){
      corrplot(cor.mat, type = "upper", order = "original", 
               tl.col = "black", tl.srt = 45,tl.cex = 0.5,diag = F)
    }
    
    if(nrow(cor2)>0){
      corrplot(cor2, type = "upper", order = "original", 
               tl.col = "black", tl.srt = 45,tl.cex = 0.5,diag = F)
    }
    
    #save as PDF
    dev.off()
  }
  
} #end make.plots function

#make.plots()

#############################################end code from YFT diagnostics


for(i in 1:100){
  #dat <- get(paste0("dat_1A_",i))
  mod_name <- paste0("YFT_1area_normcomp_100sets",i)
  #om_rep <- mungeData(mod_name, reduce = NULL, run = FALSE)
  #cat(paste0("\n Data set",i),"\n ")
  #################################################################################
  ########### INPUTS FOR RUNNING MODELS ###########################################
  #################################################################################
  
  #name ESTIMATION MODEL of the .exe
  #EM_name<-EM_name ###name of .dat, .tpl., .rep, etc.
  EM_name<-mod_name 
  
  #EM Location
  EM_direct<-paste0(mod_loc,"\\",mod_name,"\\Estimation_Model",sep="") #location of run(s)
  #EM_direct<-"C:\\Spatial_SPASAM_2021_Sim\\Spatial-Workshop-SPASAM-main\\YFT_1area_7fleets_9bhess\\Estimation_Model"
  
  ###########################################################################
  #make.plots()
  setwd(EM_direct)
  
  #get rep file with all the results
  out<-readList(paste(EM_direct,paste0(EM_name,".rep"),sep="\\")) #read in .rep file
  
  #pull info about the model
  na<-out$nages
  nyrs<-out$nyrs
  npops_OM<-out$npops_OM
  npops<-out$npops
  nreg_OM<-out$nregions_OM
  nreg<-out$nregions
  years<-seq(1:out$nyrs)
  ages<-seq(1:out$nages)
  fleets<-out$nfleets
  tag.age.switch<-out$fit_tag_age_switch
 
  #calc runtime
  dat.time=file.info(list.files()[which(list.files()==paste0(mod_name,".dat"))])$ctime
  rep.time=file.info(list.files()[which(list.files()==paste0(mod_name,".rep"))])$ctime
  run.time=as.numeric(difftime(rep.time,dat.time,units="mins"))
  #get gradient
  grad<-readLines(paste0(EM_name,".par"))[1]
  pos = regexpr('Max', grad)
  grad.val<-substr(grad,pos,nchar(grad))
  grad.val=as.numeric(unlist(strsplit(grad.val, "="))[2])
  
  
  results.run<-data.frame(Year=years, Rec_Est = as.vector(t(out$recruits_BM)),SSB_est = as.vector(t(out$SSB_region)),F=out$F_year,Y.resid=((out$yieldN_fleet-out$OBS_yield_fleet)/out$OBS_yield_fleet*100),Surv.resid=(out$survey_fleet_bio-out$OBS_survey_fleet_bio)/out$OBS_survey_fleet_bio*100,Iter=rep(i,length(years))) #recruitment
  run.stats=data.frame(run.time,grad.val,i)
  Rzero.run=data.frame(Rzero=out$R_ave,Iter=i)
  if(i==1){ 
    runstats=run.stats
    results=results.run
    Rzero=Rzero.run
  } else { 
    runstats=rbind(runstats,run.stats)
    results=rbind(results,results.run)
    Rzero=rbind(Rzero,Rzero.run)
  }
} #end for i loop

badruns=runstats[runstats$grad.val>1,]
gradientplot=ggplot(runstats, aes(x=grad.val)) + 
  geom_boxplot() +
  coord_flip()
#plot(runstats$run.time,runstats$grad.val)
results$Year=as.factor(results$Year)

ssbplot=ggplot(results, aes(x=Year,y=SSB_est)) + 
  theme(panel.grid.minor.x = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=20)) +
  labs(x="Year",y="SSB")+
  scale_x_discrete(breaks = seq(1, 151, by = 10))+
  geom_boxplot()
ggsave(filename = "ssbplot.png",path=mod_loc,device="png",width=15,height = 10,units="in")

recplot=ggplot(results, aes(x=Year,y=Rec_Est)) + 
  theme(panel.grid.minor.x = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=20)) +
  labs(x="Year",y="Recruitment (000s)")+
  scale_x_discrete(breaks = seq(1, 151, by = 10))+
  geom_boxplot()
ggsave(filename = "recplot.png",path=mod_loc,device="png",width=15,height = 10,units="in")

Rzero$lnRzero=log(Rzero$Rzero)
Rzeroplot=ggplot(Rzero,aes(y=lnRzero))+
  geom_hline(aes(yintercept=11.48),linetype=2)+
  labs(x=" ",y="ln(Rzero)")+
  theme(axis.text.x=element_blank(),axis.text=element_text(size = 20),axis.title=element_text(size=20))+
  geom_boxplot()
ggsave(filename = "RzeroBox.png",path=mod_loc,device="png",width=10,height = 10,units="in")

#reshape the annual F estimates
F_est=results[,names(results) %in% c(paste0("F.",seq(1:fleets)),"Year","Iter")]
F_est=melt(F_est,id=c("Year","Iter"))
Fplot=ggplot(F_est,aes(y=value,x=Year))+
  theme(panel.grid.minor.x = element_blank(),axis.text.x=element_text(size=15),axis.text.y=element_text(size=20),axis.title=element_text(size=20)) +
  labs(x="Year",y="Fully-selected F")+
  scale_x_discrete(breaks = seq(1, 151, by = 15))+
  facet_wrap(vars(variable))+
  geom_boxplot()
ggsave(filename = "Fplot.png",path=mod_loc,device="png",width=15,height = 10,units="in")

##Yield residuals
Y.resid=results[,names(results) %in% c(paste0("Y.resid.",seq(1:fleets)),"Year","Iter")]
Y.resid<-melt(Y.resid, id=c("Year","Iter"))
Y.residplot=ggplot(Y.resid,aes(y=value,x=Year))+
  theme(panel.grid.minor.x = element_blank(),axis.text.x=element_text(size=15),axis.text.y=element_text(size=20),axis.title=element_text(size=20)) +
  labs(x="Year",y="Catch Residual")+
  scale_x_discrete(breaks = seq(1, 151, by = 15))+
  facet_wrap(vars(variable),scales="free_y")+
  ylim(-5,5)+
  geom_boxplot()
ggsave(filename = "Yresidplot_scaled.png",path=mod_loc,device="png",width=15,height = 10,units="in")
Y.residplot=ggplot(Y.resid,aes(y=value,x=Year))+
  theme(panel.grid.minor.x = element_blank(),axis.text.x=element_text(size=15),axis.text.y=element_text(size=20),axis.title=element_text(size=20)) +
  labs(x="Year",y="Catch Residual")+
  scale_x_discrete(breaks = seq(1, 151, by = 15))+
  facet_wrap(vars(variable),scales="free_y")+
  geom_boxplot()
ggsave(filename = "Yresidplot.png",path=mod_loc,device="png",width=15,height = 10,units="in")

#survey index resids
survresidplot=ggplot(results, aes(x=Year,y=Surv.resid)) + 
  theme(panel.grid.minor.x = element_blank(),axis.text=element_text(size=20),axis.title=element_text(size=20)) +
  labs(x="Year",y="Survey Residual")+
  scale_x_discrete(breaks = seq(1, 151, by = 10))+
  geom_boxplot()
ggsave(filename = "survresidplot.png",path=mod_loc,device="png",width=15,height = 10,units="in")

#windows(width = 15,height = 10)
#print(ssbplot)

#remove bad runs
if(FALSE){
ssbplot=ggplot(results[!(results$Iter %in% badruns$i),], aes(x=Year,y=SSB_est)) + 
  theme(panel.grid.minor.x = element_blank()) +
  labs(x="Year",y="SSB")+
  scale_x_discrete(breaks = seq(1, 151, by = 5))+
  geom_boxplot()
}