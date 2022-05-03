## next time: make a preprocessing code that renames the excel files according to the nodes

##https://app.gorilla.sc/support/walkthrough/RStudio#combiningcsvfilesusingr


##############################
#                            #
# PREPROCESSING DATA TLOADDBACK           #
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



################  PVT (Psychomotor Vigilance Task) Preprocessing   ###############

## Combining CSVs #
files <- c("PVT_1_Acc_1_1.csv",
           "PVT_1_Acc_1_2.csv",
           "PVT_2_Acc_1_1.csv",
           "PVT_2_Acc_1_2.csv",
           
           "PVT_1_Acc_0.55_1.csv",
           "PVT_1_Acc_0.55_2.csv",
           "PVT_2_Acc_0.55_1.csv",
           "PVT_2_Acc_0.55_2.csv"
           )

combined_PVT <- lapply(files, read.csv) %>% 
  bind_rows()

# exclude rows that contain END OF FILE 
combined_PVT<-combined_PVT[combined_PVT$ï..Event.Index!="END OF FILE",]
combined_PVT<-combined_PVT[combined_PVT$Trial.Number!="BEGIN TASK",]

#remove instructions
combined_PVT<-combined_PVT[combined_PVT$display!="instructions",]


#export combined data as a CSV. 
write.csv(combined_PVT,"combined_PVT.csv",row.names=FALSE)

#re-download
combined_PVT <- read.csv(paste0(Dir, "combined_PVT.csv"), header = TRUE, sep = )


# make columns for conditions: time = 1 or 2, task= PVT1, PVT2, etc

combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-1lhj"] = 1 # was taken on day 1
combined_PVT$Test[combined_PVT$Tree.Node.Key == "task-1lhj"] = 1 # before TloadDback
combined_PVT$Condition[combined_PVT$Tree.Node.Key == "task-1lhj"] = 'LCL' # Acc= 1

combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-ofgl"] = 1
combined_PVT$Test[combined_PVT$Tree.Node.Key == "task-ofgl"] = 2
combined_PVT$Condition[combined_PVT$Tree.Node.Key == "task-ofgl"] = 'LCL'

combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-3l6b"] = 2
combined_PVT$Test[combined_PVT$Tree.Node.Key == "task-3l6b"] = 1
combined_PVT$Condition[combined_PVT$Tree.Node.Key == "task-3l6b"] = 'LCL'

combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-6xhv"] = 2
combined_PVT$Test[combined_PVT$Tree.Node.Key == "task-6xhv"] = 2
combined_PVT$Condition[combined_PVT$Tree.Node.Key == "task-6xhv"] = 'LCL'


combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-jmi7"] = 1
combined_PVT$Test[combined_PVT$Tree.Node.Key == "task-jmi7"] = 1
combined_PVT$Condition[combined_PVT$Tree.Node.Key == "task-jmi7"] = 'HCL'

combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-82mr"] = 1
combined_PVT$Test[combined_PVT$Tree.Node.Key == "task-82mr"] = 2
combined_PVT$Condition[combined_PVT$Tree.Node.Key == "task-82mr"] = 'HCL'

combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-dt2v"] = 2
combined_PVT$Test[combined_PVT$Tree.Node.Key == "task-dt2v"] = 1
combined_PVT$Condition[combined_PVT$Tree.Node.Key == "task-dt2v"] = 'HCL'

combined_PVT$Day[combined_PVT$Tree.Node.Key == "task-5too"] = 2
combined_PVT$Test[combined_PVT$Tree.Node.Key == "task-5too"] = 2
combined_PVT$Condition[combined_PVT$Tree.Node.Key == "task-5too"] = 'HCL'

combined_PVT$Day <- factor(combined_PVT$Day)
combined_PVT$Test <- factor(combined_PVT$Test)
combined_PVT$Condition <- factor(combined_PVT$Condition)

levels(combined_PVT$Condition)

#create sequential IDs
levs<-unique(combined_PVT$Participant.Private.ID)
combined_PVT$ID <- factor(combined_PVT$Participant.Private.ID, levels=levs, labels=seq_along(levs))

#rename column
combined_PVT %>% 
  rename(randomiser.2vg5 = Accuracy_Level)


#round RT
combined_PVT$RT = round(combined_PVT$Reaction.Time)

#people that used mobile instead of computer
combined_PVT$ID[combined_PVT$Participant.Device.Type =="mobile"]


#check Participant.Browser and Participant.OS and Participant.Monitor.size
unique(combined_PVT$Participant.Browser)

#Drop unnecessary columns
PVT = subset(combined_PVT, select = -c(UTC.Timestamp,Local.Timestamp, Local.Timezone, Experiment.ID,Repeat.Key, Schedule.ID, Participant.Public.ID, Participant.Status, Participant.Starting.Group, Participant.Completion.Code,
                              Participant.External.Session.ID, Participant.Device, UTC.Date, checkpoint.gn26, display, Dishonest, Participant.Private.ID, Tree.Node.Key,
                               Checkpoint, checkpoint.yiku, Spreadsheet, Spreadsheet.Name, Zone.Name, Reaction.Onset, Response.Type, ï..Event.Index,
                              X.Coordinate, Y.Coordinate, randomise_blocks, X, Reaction.Time) )

#export combined data as a CSV. 
write.csv(PVT,"PVT.csv",row.names=FALSE)


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

