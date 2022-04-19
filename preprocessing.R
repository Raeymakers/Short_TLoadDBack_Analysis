## next time: make a preprocessing code that renames the excel files according to the nodes

##https://app.gorilla.sc/support/walkthrough/RStudio#combiningcsvfilesusingr


##############################
#                            #
# PREPROCESSING DAT TLOADDBACK           #
#                            #
#############################
# This code uses premade csv for TRAIT specific variables and perform analysis and data viz
# Author:  Sofie Raeymakers
# 2022

##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

library(tidyverse)

#Set your working directory to the folder in which all your CSV files are located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Dir = "C:/Users/ASUSTeK/OneDrive/Documenten/Github/Short_TLoadDBack/Data/"

setwd(Dir)

################# Combining CSVs- Questionnaires/Same tasks #################
#This is the script to use if you want to combine questionnaires or identical tasks (differing only e.g. on counterbalancing)
#You list the files you want to combine, each with a "" around them
files <- c("PVT_1_Acc_1_1.csv",
           "PVT_1_Acc_1_2.csv",
           "PVT_2_Acc_1_1.csv",
           "PVT_2_Acc_1_2.csv",
           
           "PVT_1_Acc_0.55_1.csv",
           "PVT_1_Acc_0.55_2.csv",
           "PVT_2_Acc_0.55_1.csv",
           "PVT_2_Acc_0.55_2.csv"
          #blabla
  
           )


#You can combine the CSVs using either base R or tidyverse (subject to preference)
#using tidyverse
combined_PVT <- lapply(files, read.csv) %>% 
  bind_rows()
#using base R
combined_PVT<-do.call("rbind",lapply(files,read.csv,header=TRUE,fill=TRUE))

#Your dataset also has some rows that contain "END OF FILE" and nothing else. You can exclude these rows using this line.
combined_PVT<-combined_PVT[combined_PVT$ï..Event.Index!="END OF FILE",]

#This line exports your combined data as a CSV. This new CSV will be called "combineddata.csv" and will appear in your working directory
write.csv(combined_PVT,"combined_PVT.csv",row.names=FALSE)


PVT <- read.csv(paste0(Dir, "combined_PVT.csv"), header = TRUE, sep = )


# Replace tree nodes: make new column with names conditions
