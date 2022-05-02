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


# calculate PVT result per participant 
# The performance score is calculated as 100% minus the number of lapses and false starts relative to the number of valid stimuli and false starts. 
#It ranges from 100% (optimal performance, no lapses or false starts) to 0% (worst possible performance, only lapses and false starts). 
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3197786/


# make columns: time = 1 or 2, task= PVT1, PVT2, etc
PVT$Time[PVT$Tree.Node.Key == "task-1lhj"] = 1
PVT$Time[PVT$Tree.Node.Key == "task-ofgl"] = 1
PVT$Time[PVT$Tree.Node.Key == "task-3l6b"] = 2
PVT$Time[PVT$Tree.Node.Key == "task-5too"] = 2

head(PVT$Time)

#create sequential IDs
levs<-unique(PVT$Participant.Private.ID)
PVT$ID <- factor(PVT$Participant.Private.ID, levels=levs, labels=seq_along(levs))


# calculate mean RT
aggdf <- aggregate (Reaction.Time ~ Participant.Private.ID, PVT, mean)
names(aggdf)[2] <- "MeanOfRT"
PVT <- merge(PVT, aggdf, by="Participant.Private.ID")


# create new dataframe with only necessary data
MeanPVT <- data.frame(PVT$Time, PVT$ID, PVT$MeanOfRT)





#TaskName
#Reaction_Time

# task-1lhj = PVT1_LCL_1
# questionnaire-krgc = VAS1_LCL_1
# task-yesg = Pics_LCL_1
# task-fatb = Colors_LCL_1
# task-2i7q = Exp_LCL_1
# task-ofgl = PVT2_LCL_1
# questionnaire-37ue = VAS2_LCL_1

# task-3l6b = PVT1_LCL_2
# questionnaire-w32a = VAS1_LCL_2
# task-onds = Pics_LCL_2
# task-59ft = Colors_LCL_2
# task-v1am = Exp_LCL_2
# task-3l6b = PVT2_LCL_2
# questionnaire-zkqn = VAS2_LCL_2

# questionnaire-p393 = emails
# questionnaire-nny5 = Informed_Consent

# task-jmi7 = PVT1_HCL_1
# questionnaire-53zf = VAS1_HCL_1
# task-snpf = Pics_HCL_1
# task-2gi1 = Colors_HCL_1
# task-e9oa = Exp_HCL_1
# task-82mr = PVT2_HCL_1
# questionnaire-mxm5 = VAS2_HCL_1
# task-dt2v = PVT1_HCL_2
# questionnaire-5mfo = VAS1_HCL_2
# task-mxe3 = Pics_HCL_2
# task-nzlp = Colors_HCL_2
# task-wgpj = Exp_HCL_2
# task-5too = PVT2_HCL_2
# questionnaire-ty7à = VAS2_HCL_2

