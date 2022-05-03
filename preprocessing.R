# Author: Sofie Raeymakers
# 2022


##https://app.gorilla.sc/support/walkthrough/RStudio#combiningcsvfilesusingr


##############################
#                            #
# PREPROCESSING DATA TLOADDBACK           #
#                            #
#############################


##############################
#                            #
# PVT (Psychomotor Vigilance Task)       #
#                            #
#############################


##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console
dev.off() # Clear plot window

library(tidyverse)

#Set your working directory to the folder in which all your CSV files are located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Dir = "C:/Users/ASUSTeK/OneDrive/Documenten/Github/Short_TLoadDBack/Data/"
setwd(Dir)

### Combining CSVs #
files <- c("data_exp_73265-v15_task-1lhj.csv",
           "data_exp_73265-v15_task-ofgl.csv",
           "data_exp_73265-v15_task-3l6b.csv",
           "data_exp_73265-v15_task-6xhv.csv",
          
           "data_exp_73265-v15_task-jmi7.csv",
           "data_exp_73265-v15_task-82mr.csv",
           "data_exp_73265-v15_task-dt2v.csv",
           "data_exp_73265-v15_task-5too.csv"
           )

combined_PVT <- lapply(files, read.csv) %>% 
  bind_rows()

# exclude rows that contain END OF FILE and BEGIN TASK
combined_PVT<-combined_PVT[combined_PVT$ï..Event.Index!="END OF FILE",]
combined_PVT<-combined_PVT[combined_PVT$Trial.Number!="BEGIN TASK",]
# remove instructions
combined_PVT<-combined_PVT[combined_PVT$display!="instructions",]

# export combined data as a CSV. 
write.csv(combined_PVT,"combined_PVT.csv",row.names=FALSE)
 


### filtering

#re-download
combined_PVT <- read.csv(paste0(Dir, "combined_PVT.csv"), header = TRUE, sep = )

#create sequential IDs
levs<-unique(combined_PVT$Participant.Private.ID)
combined_PVT$ID <- factor(combined_PVT$Participant.Private.ID, levels=levs, labels=seq_along(levs))

#rename column
combined_PVT <- combined_PVT %>% rename(Accuracy_Level = randomiser.2vg5)

#round RT
combined_PVT$RT = round(combined_PVT$Reaction.Time)


# make columns for conditions: time = 1 or 2, task= PVT1, PVT2, etc

combined_PVT$Condition[combined_PVT$Accuracy_Level=="Accuracy_level_1"] = 'LCL' # Low Cognitive Load
combined_PVT$Condition[combined_PVT$Accuracy_Level=="Accuracy_level_0.55"] = 'HCL'

combined_PVT$Test[combined_PVT$Task.Name=="PVT-1"] = 1 # before or after TloadDback
combined_PVT$Test[combined_PVT$Task.Name=="PVT-2"] = 2

combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-1lhj"] = 1 # was taken on day 1
combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-ofgl"] = 1
combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-3l6b"] = 2 # was taken on day 2
combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-6xhv"] = 2

combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-jmi7"] = 1
combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-82mr"] = 1
combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-dt2v"] = 2
combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-5too"] = 2


#Drop unnecessary columns
PVT = subset(combined_PVT, select = -c(ï..Event.Index, UTC.Timestamp, UTC.Date, Local.Timestamp, Local.Timezone, Experiment.ID,
                                        Tree.Node.Key, Repeat.Key, Schedule.ID, Participant.Public.ID, Participant.Private.ID,
                                        Participant.Starting.Group, Participant.Status, Participant.Completion.Code, Participant.External.Session.ID, Participant.Device,
                                        Checkpoint, checkpoint.yiku, checkpoint.gn26, Spreadsheet, Spreadsheet.Name, Reaction.Onset, Response.Type, Reaction.Time,
                                        X.Coordinate, Y.Coordinate, randomise_blocks, X, display, Dishonest,) )

  

#export combined data as a CSV. 
write.csv(PVT,"PVT.csv",row.names=FALSE)




##############################
#                            #
# VAS-f (Visual Analogue Scale for Fatigue)       #
#                            #
#############################


##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console

library(tidyverse)

#Set your working directory to the folder in which all your CSV files are located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Dir = "C:/Users/ASUSTeK/OneDrive/Documenten/Github/Short_TLoadDBack/Data/"
setwd(Dir)


### Combining CSVs #
files <- c("data_exp_73265-v15_questionnaire-krgc.csv",
           "data_exp_73265-v15_questionnaire-37ue.csv",
           "data_exp_73265-v15_questionnaire-w32a.csv",
           "data_exp_73265-v15_questionnaire-zkqn.csv",
           
           "data_exp_73265-v15_questionnaire-53zf.csv",
           "data_exp_73265-v15_questionnaire-mxm5.csv",
           "data_exp_73265-v15_questionnaire-5mfo.csv",
           "data_exp_73265-v15_questionnaire-ty7a.csv"
)

combined_VAS <- lapply(files, read.csv) %>% 
  bind_rows()

# exclude rows that contain END OF FILE and BEGIN TASK
combined_VAS<-combined_VAS[combined_VAS$ï..Event.Index!="END OF FILE",]
combined_PVT<-combined_PVT[combined_PVT$Question.Key!="BEGIN QUESTIONNAIRE",]

# export combined data as a CSV. 
write.csv(combined_VAS,"combined_VAS.csv",row.names=FALSE)


### filtering

#re-download
combined_VAS <- read.csv(paste0(Dir, "combined_VAS.csv"), header = TRUE, sep = )

#create sequential IDs
levs<-unique(combined_VAS$Participant.Private.ID)
combined_VAS$ID <- factor(combined_VAS$Participant.Private.ID, levels=levs, labels=seq_along(levs))

#rename column
combined_VAS <- combined_VAS %>% rename(Accuracy_Level = randomiser.2vg5)



# make columns for conditions: time = 1 or 2, task= VAS1, VAS2, etc

combined_VAS$Condition[combined_VAS$Accuracy_Level=="Accuracy_level_1"] = 'LCL' # Low Cognitive Load
combined_VAS$Condition[combined_VAS$Accuracy_Level=="Accuracy_level_0.55"] = 'HCL'

combined_VAS$Test[combined_VAS$Task.Name=="VAS-f-1"] = 1 # before or after TloadDback
combined_VAS$Test[combined_VAS$Task.Name=="VAS-f-2"] = 2

combined_VAS$Day[combined_VAS$Tree.Node.Key == "questionnaire-krgc"] = 1 # was taken on day 1
combined_VAS$Day[combined_VAS$Tree.Node.Key == "questionnaire-37ue"] = 1
combined_VAS$Day[combined_VAS$Tree.Node.Key == "questionnaire-w32a"] = 2 # was taken on day 2
combined_VAS$Day[combined_VAS$Tree.Node.Key == "questionnaire-zkqn"] = 2

combined_VAS$Day[combined_VAS$Tree.Node.Key == "questionnaire-53zf"] = 1
combined_VAS$Day[combined_VAS$Tree.Node.Key == "questionnaire-mxm5"] = 1
combined_VAS$Day[combined_VAS$Tree.Node.Key == "questionnaire-5mfo"] = 2
combined_VAS$Day[combined_VAS$Tree.Node.Key == "questionnaire-ty7a"] = 2

#check participant status
combined_VAS[combined_VAS$Participant.Status != "complete"]


#Drop unnecessary columns
VAS = subset(combined_VAS, select = -c(ï..Event.Index, UTC.Timestamp, UTC.Date, Local.Timestamp, Local.Timezone, Experiment.ID, Accuracy_Level,
                                        Tree.Node.Key, Repeat.Key, Schedule.ID, Participant.Public.ID, Participant.Private.ID,
                                        Participant.Starting.Group, Participant.Status, Participant.Completion.Code, Participant.External.Session.ID,
                                        Checkpoint, checkpoint.yiku, checkpoint.gn26, Randomise.questionnaire.elements.) )

#export combined data as a CSV. 
write.csv(VAS,"VAS.csv",row.names=FALSE)



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
# task-6xhv = PVT2_LCL_2
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

