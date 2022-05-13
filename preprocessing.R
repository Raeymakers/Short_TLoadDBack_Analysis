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

combined <- lapply(files, read.csv) %>% 
  bind_rows()

combined$ID[combined$Participant.Device.Type =="mobile"]
#one participant tried to do on mobile, but quitted when realised doesn't work.

# exclude rows that contain END OF FILE and BEGIN TASK
combined<-combined[combined$ï..Event.Index!="END OF FILE",]
combined<-combined[combined$Trial.Number!="BEGIN TASK",]
# remove instructions
combined<-combined[combined$display!="instructions",]

#Remove: all rows that are not Attempt == 1
combined <- combined[!is.na(combined$Attempt),]

# export combined data as a CSV. 
write.csv(combined,"combined_PVT.csv",row.names=FALSE)
 


### filtering

#re-download
data <- read.csv(paste0(Dir, "combined_PVT.csv"), header = TRUE, sep = )

#create sequential IDs
levs<-unique(data$Participant.Private.ID)
data$ID <- factor(data$Participant.Private.ID, levels=levs, labels=seq_along(levs))

#rename column
data <- data %>% rename(Accuracy_Level = randomiser.2vg5)

#round RT
data$RT = round(data$Reaction.Time, digits=2)


# make columns for conditions: time = 1 or 2, task= PVT1, PVT2, etc

data$Condition[data$Accuracy_Level=="Accuracy_level_1"] = 'LCL' # Low Cognitive Load
data$Condition[data$Accuracy_Level=="Accuracy_level_0.55"] = 'HCL'

data$Test[data$Task.Name=="PVT-1"] = 1 # before or after TloadDback
data$Test[data$Task.Name=="PVT-2"] = 2

data$Day[data$Tree.Node.Key == "task-1lhj"] = 1 # was taken on day 1
data$Day[data$Tree.Node.Key == "task-ofgl"] = 1
data$Day[data$Tree.Node.Key == "task-3l6b"] = 2 # was taken on day 2
data$Day[data$Tree.Node.Key == "task-6xhv"] = 2

data$Day[data$Tree.Node.Key == "task-jmi7"] = 1
data$Day[data$Tree.Node.Key == "task-82mr"] = 1
data$Day[data$Tree.Node.Key == "task-dt2v"] = 2
data$Day[data$Tree.Node.Key == "task-5too"] = 2

#Drop unnecessary columns
PVT = subset(data, select = -c(ï..Event.Index, UTC.Timestamp, UTC.Date, Local.Timestamp, Local.Timezone, Experiment.ID, Accuracy_Level, reset,
                                        Tree.Node.Key, Repeat.Key, Schedule.ID, Participant.Public.ID, Experiment.Version, Task.Version,
                                        Participant.Starting.Group, Participant.Status, Participant.Completion.Code, Participant.External.Session.ID, Participant.Device,
                                        Checkpoint, checkpoint.yiku, checkpoint.gn26, Spreadsheet, Spreadsheet.Name, Reaction.Onset, Response.Type, Reaction.Time,
                                        X.Coordinate, Y.Coordinate, randomise_blocks, X, display, Dishonest, Spreadsheet.Row, Screen.Number, Screen.Name, Zone.Name,
                                        Zone.Type, Response, Attempt, Timed.Out, randomise_trials, Incorrect) )

PVT = subset(PVT, select = -c(Participant.Device.Type,  Participant.OS, Participant.Browser, Participant.Monitor.Size, Participant.Viewport.Size) ) 

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

combined <- lapply(files, read.csv) %>% 
  bind_rows()

# exclude rows that contain END OF FILE and BEGIN TASK
combined<-combined[combined$ï..Event.Index!="END OF FILE",]
combined<-combined[combined$Question.Key!="BEGIN QUESTIONNAIRE",]
combined<-combined[combined$Question.Key!="END QUESTIONNAIRE",]

# export combined data as a CSV. 
write.csv(combined,"combined_VAS.csv",row.names=FALSE)


### filtering

#re-download
data <- read.csv(paste0(Dir, "combined_VAS.csv"), header = TRUE, sep = )

#create sequential IDs
levs<-unique(data$Participant.Private.ID)
data$ID <- factor(data$Participant.Private.ID, levels=levs, labels=seq_along(levs))

#rename column
data <- data %>% rename(Accuracy_Level = randomiser.2vg5)



# make columns for conditions: time = 1 or 2, task= VAS1, VAS2, etc

data$Condition[data$Accuracy_Level=="Accuracy_level_1"] = 'LCL' # Low Cognitive Load
data$Condition[data$Accuracy_Level=="Accuracy_level_0.55"] = 'HCL'

data$Test[data$Task.Name=="VAS-f-1"] = 1 # before or after TloadDback
data$Test[data$Task.Name=="VAS-f-2"] = 2

data$Day[data$Tree.Node.Key == "questionnaire-krgc"] = 1 # was taken on day 1
data$Day[data$Tree.Node.Key == "questionnaire-37ue"] = 1
data$Day[data$Tree.Node.Key == "questionnaire-w32a"] = 2 # was taken on day 2
data$Day[data$Tree.Node.Key == "questionnaire-zkqn"] = 2

data$Day[data$Tree.Node.Key == "questionnaire-53zf"] = 1
data$Day[data$Tree.Node.Key == "questionnaire-mxm5"] = 1
data$Day[data$Tree.Node.Key == "questionnaire-5mfo"] = 2
data$Day[data$Tree.Node.Key == "questionnaire-ty7a"] = 2

#check participant status
data[data$Participant.Status != "complete"]


#Drop unnecessary columns
VAS = subset(data, select = -c(ï..Event.Index, UTC.Timestamp, UTC.Date, Local.Timestamp, Local.Timezone, Experiment.ID, Accuracy_Level, Experiment.Version, Task.Version,
                                        Tree.Node.Key, Repeat.Key, Schedule.ID, Participant.Public.ID,
                                        Participant.Starting.Group, Participant.Status, Participant.Completion.Code, Participant.External.Session.ID,
                                       Checkpoint, checkpoint.yiku, checkpoint.gn26, Randomise.questionnaire.elements.) )

VAS = subset(VAS, select = -c(Participant.Device.Type, Participant.Device, Participant.OS, Participant.Browser, Participant.Monitor.Size, Participant.Viewport.Size) )

#export combined data as a CSV. 
write.csv(VAS,"VAS.csv",row.names=FALSE)



##############################
#                            #
# TLOADDBACk      #
#                            #
#############################


##### Set environment #####
rm(list = ls()) # Clear environment
cat("\014") # Clear console

library(tidyverse)
library(dplyr)

#Set your working directory to the folder in which all your CSV files are located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Dir = "C:/Users/ASUSTeK/OneDrive/Documenten/Github/Short_TLoadDBack/Data/"
setwd(Dir)


### Combining CSVs #
files <- c("data_exp_73265-v15_task-2i7q.csv",
           "data_exp_73265-v15_task-v1am.csv",
           "data_exp_73265-v15_task-e9oa.csv",
           "data_exp_73265-v15_task-wgpj.csv")

combined <- lapply(files, read.csv) %>% 
  bind_rows()

# export combined data as a CSV. 
write.csv(combined,"combined.csv",row.names=FALSE)


### filtering

#re-download
data <- read.csv(paste0(Dir, "combined_EXP.csv"), header = TRUE, sep = )

#remove row with no participant private ID
data <- subset(data, !is.na(Participant.Private.ID))

#create sequential IDs
levs<-unique(data$Participant.Private.ID)
data$ID <- factor(data$Participant.Private.ID, levels=levs, labels=seq_along(levs))

#rename column
data <- dplyr::rename(data, Accuracy_Level = randomiser.2vg5)


# wether answer was correct or not
data$accuracy [is.na(data$accuracy)] <- 0

unique(data$Accuracy_Level)


# create condition for Pic or Color
data$Stim[grepl("elephant", data$Stimulus)] = "Pic"
data$Stim[grepl("house", data$Stimulus)] = "Pic"
data$Stim[grepl("truck", data$Stimulus)] = "Pic"
data$Stim[grepl("glass", data$Stimulus)] = "Pic"
data$Stim[grepl("plane", data$Stimulus)] = "Pic"
data$Stim[grepl("pear", data$Stimulus)] = "Pic"
data$Stim[grepl("glass", data$Stimulus)] = "Pic"
data$Stim[grepl("key", data$Stimulus)] = "Pic"
data$Stim[grepl("comb", data$Stimulus)] = "Pic"

data$Stim[grepl("blue", data$Stimulus)] = "Color"
data$Stim[grepl("green", data$Stimulus)] = "Color"
data$Stim[grepl("pink", data$Stimulus)] = "Color"
data$Stim[grepl("purple", data$Stimulus)] = "Color"
data$Stim[grepl("red", data$Stimulus)] = "Color"
data$Stim[grepl("yellow", data$Stimulus)] = "Color"
data$Stim[grepl("black", data$Stimulus)] = "Color"
data$Stim[grepl("green", data$Stimulus)] = "Color"


# make columns for conditions: time = 1 or 2, task= VAS1, VAS2, etc

data$Condition[data$Accuracy_Level=="Accuracy_level_1"] = 'LCL' # Low Cognitive Load
data$Condition[data$Accuracy_Level=="Accuracy_level_0.55"] = 'HCL'

data$Day[data$Tree.Node.Key == "task-2i7q"] = 1 # was taken on day 1
data$Day[data$Tree.Node.Key == "task-v1am"] = 2
data$Day[data$Tree.Node.Key == "task-e9oa"] = 1 # was taken on day 2
data$Day[data$Tree.Node.Key == "task-wgpj"] = 2

#round RT
data$RT = round(data$Reaction.Time, digits=2)

#remove fullscreen and instruction 
data<-data[!(data$Trial.Type=="instructions"|data$Trial.Type=="fullscreen"),]

# if no RT, it means that RT = 0
data$RT [is.na(data$RT)] <- 0

#Drop unnecessary columns
EXP = subset(data, select = -c(ï..Event.Index, UTC.Timestamp, UTC.Date, Local.Timestamp, Local.Timezone, Experiment.ID, Experiment.Version,
                                        Tree.Node.Key, Repeat.Key, Schedule.ID, Participant.Public.ID, Task.Version, Key.Press, Test.Part,
                                        Participant.Starting.Group, Participant.Status, Participant.Completion.Code, Participant.External.Session.ID,
                                        Checkpoint, checkpoint.yiku, checkpoint.gn26, Trial.Type, Internal.Node.ID, correct_resp, trial_duration, Correct.Response,
                                response, X.1, X.2, X, accuracy_percentage, Participant.Device.Type, Participant.Device, Participant.OS, Participant.Browser,
                               Participant.Viewport.Size, Participant.Monitor.Size, accuracy_pic_trial, match, phase, correct_response, Correct, Stimulus,
                               accuracy_color_trial, minutes, trial_dur, Reaction.Time) )

#export combined data as a CSV. 
write.csv(EXP,"EXP.csv",row.names=FALSE)






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

