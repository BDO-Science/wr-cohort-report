# fish-model-outputs.R
# Last Update :: 31 Dec 2024
# Code from Nick Beer nickbeer@uw.edu: https://github.com/nickobeer/SACPAS/blob/main/SacPas.report1.R
# edited by Cat Pien cpien@usbr.gov

# This code produces fish model outputs and figures for the cohort report.

# Call packages
library(tidyverse)
# source("parameters.R")
report_year = 2021

# Define temp directory ---------------------
tmplocation="tempfiles"        #  <YOUR CHOICE::  CHANGE THIS of a unique directory name to separate your results from others>
figures_folder = "figures/"
data_folder = "data_raw/"

# Define settings --------------------------
# CHECK IF THESE ARE ALL THE SETTINGS
whatyear=report_year # set to the current brood year
usemortality="hatchmort"  # Mortality type: "emergemort" Martin mortality prior to emergence or "hatchmort" Anderson
usedewater="onkwk"        # Dewatering setting: "off" or "onkwk" also option for custom flows but reach out to Nick for that
spawning="dbcarcass"      # Database of redds: "dbcarcass" is carcass, "dbredds" is aerial surveys there are some other options for fall/spring

# Generate values from above inputs -----------------------
string <- paste0("https://www.cbr.washington.edu/sac-bin/fishmodel/getandplottemp.pl?dirUseId=",tmplocation,"&temponly=off&tempsource=dbtemp&mortality=",usemortality)
string1 <- paste0(string,"&reddyear=",whatyear,"&tempyear=",whatyear,"&dewater=",usedewater,"&redds=",spawning)
string2 <- paste0(string1,"&raw=13")
result <- read_csv(string2,show_col_types = FALSE)
print(result)

# write as csv
# write_csv(result, paste0(data_folder,"fish_model_parameters_",whatyear, "_", usemortality, "_", spawning,".csv"))

# Generate plots for fish model -----------------------------
# AFTER the run is complete. Files can be obtained directly from the server's results directory
stringplot <- paste0("http://www.cbr.washington.edu/sacramento/tmp/RESULTS_",tmplocation)
plot1 <- paste0("tempplot.png")
plot2 <- paste0("temphatchplot.png")
plot3 <- paste0("tempemergeplot.png")
plot4 <- paste0("tempplot2.png")

# Download into figures folder
# For WINDOWS need mode="wb" in the download.file function call
download.file(url = paste0(stringplot,"/",plot1),
              destfile = paste0(figures_folder, "fishmodel_",whatyear,"_redddistributionplot.png"),
              mode="wb")
download.file(paste0(stringplot,"/",plot2),
              destfile = paste0(figures_folder,"fishmodel_",whatyear,"_hatchplot.png"),
              mode="wb")
download.file(paste0(stringplot,"/",plot3),
              destfile = paste0(figures_folder,"fishmodel_",whatyear,"_emergenceplot.png"),
              mode="wb")
download.file(paste0(stringplot,"/",plot4),
              destfile = paste0(figures_folder,"fishmodel_",whatyear,"_reddstagesplot.png"),
              mode="wb")

# Reach-by-reach survivals --------------------------------------
#  obtain the necessary details and compute
survs <- read_csv(paste0(stringplot,"/survival.csv"),show_col_types = FALSE) # TDM dewater and density
tdmsurv <- read_csv(paste0(stringplot,"/tdmsurv.csv"),show_col_types = FALSE)
redds <- read_csv(paste0(stringplot,"/redddistribution.csv"),show_col_types = FALSE)
denssurv <- read_csv(paste0(stringplot,"/densityonlysurv.csv"),show_col_types = FALSE)
denssurv.W.bg <- read_csv(paste0(stringplot,"/densitysurv.csv"),show_col_types = FALSE)

tallyS <- tallyN <- 0
output.table <- cbind.data.frame("reach"=character(0),"Count"=numeric(0),"TotS"=numeric(0),"TDMS"=numeric(0),"DensS"=numeric(0),"D&BS"=numeric(0),"DWS"=numeric(0))
for(reach in colnames(survs)[-1]){
  if(reach==colnames(survs)[2]){
    cat("reach ","Count"," TotS"," TDMS","DensS"," D&BS","  DWS","\n")
  }
  x <- sum(survs[reach]*redds[reach])/sum(redds[reach])
  y <- sum(tdmsurv[reach]*redds[reach])/sum(redds[reach])
  z <- sum(denssurv.W.bg[reach]*redds[reach])/sum(redds[reach])
  tallyS <- tallyS + y*sum(redds[reach])
  tallyN <- tallyN + sum(redds[reach])
  cat(reach,
      format(sum(redds[reach]),width=5),
      format(round(x,3),width=5),
      format(round(y,3),width=5),
      format(round(sum(denssurv[reach]*redds[reach])/sum(redds[reach]),3),width=5),
      format(round(z,3),width=5),
      format(round( x / y / z ,3),width=5),
      "\n")
  output.table <- rbind.data.frame(output.table,cbind.data.frame("reach"=reach,"Count"=sum(redds[reach]), "TotS"=round(x,3), "TDMS"=round(y,3), "DensS"=round(sum(denssurv[reach]*redds[reach])/sum(redds[reach]),3), "D&BS"=round(z,3),"DWS"=x / y / z ))
  if(reach==colnames(survs)[ncol(survs)]){
    cat("\nGrand TDM: ",1- round(tallyS/tallyN,3)," Survival: ",round(tallyS/tallyN,3),"\n")
  }
}

# Write reach-specific values as csvs
write_csv(output.table,paste0(data_folder, "fish_model_reachspecific_outputs.csv"))
write_csv(cbind.data.frame("What"=c("Grand TDM","Survival"),"Value"=c( 1- round(tallyS/tallyN,3) , round(tallyS/tallyN,3))),
          paste0(data_folder, "fish_model_TDM_survival.csv"))


