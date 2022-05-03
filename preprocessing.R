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

#export combined data as a CSV. 
write.csv(combined_PVT,"combined_PVT.csv",row.names=FALSE)

#re-download
PVT <- read.csv(paste0(Dir, "combined_PVT.csv"), header = TRUE, sep = )


# make columns for conditions: time = 1 or 2, task= PVT1, PVT2, etc

PVT$Day[PVT$Tree.Node.Key == "task-1lhj"] = 1 # was taken on day 1
PVT$Test[PVT$Tree.Node.Key == "task-1lhj"] = 1 # before TloadDback
PVT$Condition[PVT$Tree.Node.Key == "task-1lhj"] = 'LCL' # Acc= 1

PVT$Day[PVT$Tree.Node.Key == "task-ofgl"] = 1
PVT$Test[PVT$Tree.Node.Key == "task-ofgl"] = 2
PVT$Condition[PVT$Tree.Node.Key == "task-ofgl"] = 'LCL'

PVT$Day[PVT$Tree.Node.Key == "task-3l6b"] = 2
PVT$Test[PVT$Tree.Node.Key == "task-3l6b"] = 1
PVT$Condition[PVT$Tree.Node.Key == "task-3l6b"] = 'LCL'

PVT$Day[PVT$Tree.Node.Key == "task-6xhv"] = 2
PVT$Test[PVT$Tree.Node.Key == "task-6xhv"] = 2
PVT$Condition[PVT$Tree.Node.Key == "task-6xhv"] = 'LCL'


PVT$Day[PVT$Tree.Node.Key == "task-jmi7"] = 1
PVT$Test[PVT$Tree.Node.Key == "task-jmi7"] = 1
PVT$Condition[PVT$Tree.Node.Key == "task-jmi7"] = 'HCL'

PVT$Day[PVT$Tree.Node.Key == "task-82mr"] = 1
PVT$Test[PVT$Tree.Node.Key == "task-82mr"] = 2
PVT$Condition[PVT$Tree.Node.Key == "task-82mr"] = 'HCL'

PVT$Day[PVT$Tree.Node.Key == "task-dt2v"] = 2
PVT$Test[PVT$Tree.Node.Key == "task-dt2v"] = 1
PVT$Condition[PVT$Tree.Node.Key == "task-dt2v"] = 'HCL'

PVT$Day[PVT$Tree.Node.Key == "task-5too"] = 2
PVT$Test[PVT$Tree.Node.Key == "task-5too"] = 2
PVT$Condition[PVT$Tree.Node.Key == "task-5too"] = 'HCL'

PVT$Day <- factor(PVT$Day)
PVT$Test <- factor(PVT$Test)
PVT$Condition <- factor(PVT$Condition)

levels(PVT$Condition)

#create sequential IDs
levs<-unique(PVT$Participant.Private.ID)
PVT$ID <- factor(PVT$Participant.Private.ID, levels=levs, labels=seq_along(levs))

#round RT
PVT$RT = round(PVT$Reaction.Time)

#check timezone: if Local.Timezone != 1, remove!

#Check Participant Status

#check that participant.Device.Type == computer

#check Participant.Browser and Participant.OS and Participant.Monitor.size

#Drop unnecessary columns
PVT = subset(PVT, select = -c(UTC.Timestamp,Local.Timestamp,Experiment.ID,Repeat.Key, Schedule.ID, Participant.Public.ID, Participant.Starting.Group, Participant.Completeion.Code,
                              Participant.External.Session.ID, Participant.Device.Type, Participant.Device, Participant.Browser, Participant.Monitor.Size, 
                              Participant.OS, Checkpoint, checkpoint.yiku, Spreadsheet, Spreadsheet.Name, Zone.Name, Reaction.Onset, Response.Type,
                              X.Coordinate, Y.Coordinate, randomise_blocks, X) )

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

