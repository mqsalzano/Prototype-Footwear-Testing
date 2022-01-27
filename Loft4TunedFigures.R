library(readxl)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(reshape2)
library(extrafont)
library(ggpubr)
font_import()
loadfonts(device = "win", quiet = TRUE)

setwd("C:/Users/msalzano/OneDrive - University of Massachusetts/UMass/Brooks/Loftv4Tuned")
savefolder = paste0(getwd(), '/ReportPlots/')

#### load in deviation and survey data ####
Loft4T = as.data.frame(read_excel("C:/Users/msalzano/OneDrive - University of Massachusetts/Brooks/Loft_v4_Tuned/Analysis_withStatic/Loft4Data_081221.xlsx"))
Loft4Survey = as.data.frame(read_excel("C:/Users/msalzano/OneDrive - University of Massachusetts/Brooks/Loft_v4_Tuned/Analysis_withStatic/BrooksLoft4Survey.xlsx"))
names(Loft4T)[1] = 'ID'

Loft4ALL = Loft4T[order(Loft4T$ID),]
Loft4ALL$TrueKneeDev = Loft4ALL$TibRotDev + Loft4ALL$KneeAddDev

Loft4SurveyNew = data.frame()
ShoeQs = c('ShoeCnd', 'CushionSatisfaction', 'LikeCushion', 'CushionExplain', 'Transition', 'ThisShoe_')


#several questions of interest are asked for each shoe, so the same question
#will have several columns - each repeat matching a shoe (R auto handles the 
# names on import). Need to gather those sets of questions so that each shoe 
# is a row.
for (i in 1:4) {
  
  shoe_col = (i*6) - 3
  tmpData = Loft4Survey[,c(2, shoe_col:(shoe_col+5))]
  names(tmpData) = c('ID', ShoeQs)
  Loft4SurveyNew = rbind(Loft4SurveyNew, tmpData)
  
}

Loft4SurveyNew$ThisShoe_ = factor(Loft4SurveyNew$ThisShoe_)
Loft4SurveyNew = Loft4SurveyNew[order(Loft4SurveyNew$ID),]

#### create dataframe for ID, Sex, Deviator Group, & Total Knee Deviation ####
unqID.idx = duplicated(Loft4SurveyNew$ID)
unqSubj = Loft4SurveyNew[!unqID.idx, c(1)] # add second column back when Sex is added
SubjSex = c('Male', 'Male', 'Male', 'Male', 'Female')
Subj = data.frame( "ID" = unqSubj, "Sex" = SubjSex)
#names(Subj) = c('ID', 'Sex')
#Subj = Subj[order(Subj$ID),]
ShoeList = unique(Loft4T$ShoeCnd)
Sock = Loft4T[(which(Loft4T$ShoeCnd == 'Sock_8min')), c(1,2,35,40)] # ID, Shoe (Sock), Total Knee Dev, Dev Group
names(Sock)[1] = 'ID'

Subj = merge(Subj, Sock, by = "ID", all = TRUE)
head(Subj)
Subj$DevGroup[5] = 'Unknown'
#allcolors = c(
  cb_blue = '#0C7BDC'
cb_yellow = '#FFC20A'
cb_darkpurple = '#5D3A9B'
cb_orange = '#E66100'
cb_darkred = '#DC3220'
cb_darkblue = '#005AB5'
umass_red = '#881C1C'
umass_grey = '#ACA39A'
umass_darkgrey = '#63666A'

malecolors = c(cb_darkblue,'black' )
femalecolors = c(cb_darkred,'black')


plot_theme =   theme(text = element_text(family = "Calibri", face = "bold"),
                     panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.background = element_blank(), 
                    plot.title = element_text(face = "bold", size = 18, color = "black", hjust = 0.5),
                     axis.text.x = element_text(face = "bold", size = 18, color = "black"),
                     axis.title.x = element_blank(),
                     axis.title.y = element_text(face = "bold", size = 18, color = "black"),
                     axis.text.y = element_text(face = "bold", size = 16, color = "black"),
                     legend.title = element_text(face = "bold", size = 18, color = "black"),
                     legend.text = element_text(face = "bold", size = 18, color = "black"))#,
                     #legend.position = "top")

#### create bar plots showing # of males/females per group & total knee deviation ####
SubjCount = Subj %>% count(Sex, DevGroup)
#bar plot showing counts males and females in each deviator group
SubjCountPlot = ggplot(data = Subj,
                       aes(x = DevGroup, color = Sex, fill = Sex)) +
 geom_bar(aes(color = Sex, fill = Sex), 
          position = position_stack(), width = .5) +
  geom_text(data = SubjCount, aes(y = n, label = n), color = 'white', size= 8,
            position = position_stack(vjust = 0.5), show.legend = FALSE)+
  scale_color_manual(values = c('black', 'black')) +
  scale_fill_manual(values = c(cb_darkred,cb_darkblue)) + 
  xlab('Deviator Group') + plot_theme + geom_hline(yintercept = 0, linetype = 1)+ theme(legend.position = "top") 
ggsave(filename = paste0(savefolder, 'SubjCountPlot.png'), plot = SubjCountPlot, width = 4, height = 6)
print(SubjCountPlot)
#column bar chart showing deviation values for each subject
SubjDevPlot = 
ggplot(data = Subj, aes(x = ID, y = TotalKneeDev))+ 
  geom_hline(yintercept = c(0,7), linetype = c(1,2), size = c(1,1)) +
  geom_col(aes(color = Sex, fill = Sex), width = .5) + ylim(0, 25) +
  geom_text(aes(label = round(TotalKneeDev, digits = 1)), vjust = -0.5) +
  scale_color_manual(values = c('black', 'black')) +
  scale_fill_manual(values = c(cb_darkblue, cb_darkred)) +  #had to switch colors since no female deviation
  xlab('Deviator Group') + ylab('Total Knee Dev. (deg.)') +
  plot_theme + theme(legend.position = "none") 
ggsave(filename = paste0(savefolder, 'SubjDevPlot.png'), plot = SubjDevPlot, width = 9, height = 6)
print(SubjDevPlot)

#### Research Question 1 ####
# How do jerk, transition perception, and re-supination differ between C1 & C2? #
RQ1.JerkRF = Subj[-5,c(1,2,5)]
for (i in 1:(length(ShoeList)-2)) {
  tmp_col = Loft4ALL$JerkRF[which(Loft4ALL$ShoeCnd == ShoeList[i])]
  RQ1.JerkRF = cbind(RQ1.JerkRF, tmp_col)
}

names(RQ1.JerkRF)[4:length(RQ1.JerkRF)] = ShoeList[1:4]
names(RQ1.JerkRF)[3] = 'DevGroup'

C1_JerkRF = RQ1.JerkRF$C1_8min
for (i in 1:(length(ShoeList)-2)) {
  col_num = 3+i
  tmp_col = abs(RQ1.JerkRF[,col_num]) - abs(C1_JerkRF)
  RQ1.JerkRF[,col_num] = tmp_col
}


RQ1.JerkRFNew = gather(RQ1.JerkRF, Shoe, JerkRF, 'C1_8min':'C4_8min', factor_key = TRUE)
names(RQ1.JerkRFNew)[3] = 'DevGroup'
RQ1.JerkRFMEAN =  aggregate(JerkRF ~ Sex*Shoe*DevGroup, RQ1.JerkRFNew, mean)

options(scipen = 10)
RQ1.JerkRF.New =
  ggplot(data = RQ1.JerkRFNew[which(RQ1.JerkRFNew$Sex == 'Male'),], aes(x =Shoe, y = JerkRF, color = DevGroup)) + 
  #annotate("rect", xmax = Inf, xmin = -Inf, ymax = 2, ymin = -2, fill = "gray", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 1)+
  geom_line(aes(group = ID, linetype = DevGroup), size = 1.25, alpha = 1) +
  #geom_line(aes(color = DevGroup), size = 1, alpha = 1) + 
  #scale_linetype_discrete(values = c(1,5)) +
  geom_point(aes(shape = DevGroup), size = 4, alpha =1) + 
  # geom_point(data = RQ1.JerkRFMEAN[which(RQ1.JerkRFMEAN$Sex == 'Male'),], aes(x = Shoe, y = JerkRF, color = DevGroup), alpha = 1, shape = 4, size = 2.5, stroke = 3) +
  scale_shape_manual(values = c(16,17)) +
  scale_color_manual(values= malecolors) +
  #scale_fill_manual(values = malecolors) +
  scale_x_discrete(limits = c("C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                   labels = c("C1", "C2"))+#, "C3", "C4")) +
  #geom_point(data = meansEv, aes(x = Shoe, y = JerkRF, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
  ylab('Jerk Cost') +
  labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
  #ylim(c(-10, 10)) +
  scale_y_continuous(limits = c(-10000, 10000), breaks = seq(-10000, 10000, 2000)) +
  plot_theme + theme(legend.position = "bottom") + labs(title = "Rearfoot") 
#geom_hline(yintercept = 0, linetype = 1)
print(RQ1.JerkRF.New)
ggsave(filename = paste0(savefolder, 'RQ1JerkRFNew.png') ,plot = RQ1.JerkRF.New, height = 4, width = 6 )

RQ1.JerkFF = Subj[-5,c(1,2,5)]
for (i in 1:(length(ShoeList)-2)) {
  tmp_col = Loft4ALL$JerkFF[which(Loft4ALL$ShoeCnd == ShoeList[i])]
  RQ1.JerkFF = cbind(RQ1.JerkFF, tmp_col)
}

names(RQ1.JerkFF)[4:length(RQ1.JerkFF)] = ShoeList[1:4]
names(RQ1.JerkFF)[3] = 'DevGroup'

C1_JerkFF = RQ1.JerkFF$C1_8min
for (i in 1:(length(ShoeList)-2)) {
  col_num = 3+i
  tmp_col = abs(RQ1.JerkFF[,col_num]) - abs(C1_JerkFF)
  RQ1.JerkFF[,col_num] = tmp_col
}

RQ1.JerkFFNew = gather(RQ1.JerkFF, Shoe, JerkFF, 'C1_8min':'C4_8min', factor_key = TRUE)
names(RQ1.JerkFFNew)[3] = 'DevGroup'
RQ1.JerkFFMEAN =  aggregate(JerkFF ~ Sex*Shoe*DevGroup, RQ1.JerkFFNew, mean)


RQ1.JerkFF.New =
  ggplot(data = RQ1.JerkFFNew[which(RQ1.JerkFFNew$Sex == 'Male'),], aes(x =Shoe, y = JerkFF, color = DevGroup)) + 
  #annotate("rect", xmax = Inf, xmin = -Inf, ymax = 2, ymin = -2, fill = "gray", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 1)+
  geom_line(aes(group = ID, linetype = DevGroup), size = 1.25, alpha = 1) +
  #geom_line(aes(color = DevGroup), size = 1, alpha = 1) + 
  #scale_linetype_discrete(values = c(1,5)) +
  geom_point(aes(shape = DevGroup), size = 4, alpha =1) + 
  # geom_point(data = RQ1.JerkFFMEAN[which(RQ1.JerkFFMEAN$Sex == 'Male'),], aes(x = Shoe, y = JerkFF, color = DevGroup), alpha = 1, shape = 4, size = 2.5, stroke = 3) +
  scale_shape_manual(values = c(16,17)) +
  scale_color_manual(values= malecolors) +
  #scale_fill_manual(values = malecolors) +
  scale_x_discrete(limits = c("C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                   labels = c("C1", "C2"))+#, "C3", "C4")) +
  #geom_point(data = meansEv, aes(x = Shoe, y = JerkFF, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
  ylab('Jerk Cost') +
  labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
  #ylim(c(-10, 10)) +
  scale_y_continuous(limits = c(-25000, 25000), breaks = seq(-25000, 25000, 5000)) +
  plot_theme  + theme(legend.position = "bottom") + labs(title = "Forefoot")
#geom_hline(yintercept = 0, linetype = 1)
print(RQ1.JerkFF.New)
ggsave(filename = paste0(savefolder, 'RQ1JerkFFNew.png') ,plot = RQ1.JerkFF.New, height = 4, width = 6 )


#### Research Question 1 - Transition Perception ####
RQ1.Transition = Loft4SurveyNew[,c(1,2,6)]

for (i in 1:length(unqSubj)) {

  RQ1.Transition$Sex[which(RQ1.Transition$ID == unqSubj[i])] = Subj$Sex[which(Subj$ID == unqSubj[i])]
  RQ1.Transition$DevGroup[which(RQ1.Transition$ID == unqSubj[i])] = Subj$DevGroup[which(Subj$ID == unqSubj[i])]
}


RQ1.Transition.Male =
  ggplot(data = RQ1.Transition, aes(x =ShoeCnd, y = Transition, color = DevGroup)) + 
  #annotate("rect", xmax = Inf, xmin = -Inf, ymax = 2, ymin = -2, fill = "gray", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_line(aes(group = ID, linetype = DevGroup), size = 1.25, alpha = 1) +
  #geom_line(aes(color = DevGroup), size = 1, alpha = 1) + 
 # scale_linetype_discrete(values = c(1,5)) +
  geom_point(aes(shape = DevGroup), size = 4, alpha =1) + 
  # geom_point(data = RQ1.TransitionMEAN[which(RQ1.TransitionMEAN$Sex == 'Male'),], aes(x = Shoe, y = Transition, color = DevGroup), alpha = 1, shape = 4, size = 2.5, stroke = 3) +
  scale_shape_manual(values = c(16,17,16)) +
  scale_color_manual(values= c(malecolors, femalecolors[1])) +
  #scale_fill_manual(values = malecolors) +
  scale_x_discrete(limits = c("C1", "C4"),#,"C3_8min","C4_8min"),
                   labels = c("C1", "C4"))+#, "C3", "C4")) +
  #geom_point(data = meansEv, aes(x = Shoe, y = Transition, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
  ylab('Transition Score') +
  labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
  #ylim(c(-10, 10)) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10, 1)) +
  plot_theme  + theme(legend.position = "bottom") #+ labs(title = "Males")
#geom_hline(yintercept = 0, linetype = 1)
print(RQ1.Transition.Male)
ggsave(filename = paste0(savefolder, 'RQ1TransitionMale.png') ,plot = RQ1.Transition.Male, height = 5, width = 8 )



#### Research Question 1 - Resupination ####

ResupinationStance = as.data.frame(read_excel("C:/Users/msalzano/OneDrive - University of Massachusetts/Brooks/Loft_v4_Tuned/Analysis_withStatic/ResupinationStance.xlsx"))
maleID = Subj$ID[which(Subj$Sex == 'Male')]
femaleID = Subj$ID[which(Subj$Sex == 'Female')]

for (i in 1:nrow(ResupinationStance)) {
  if (is.element(ResupinationStance$Subj_ID[i], maleID) == TRUE) {
    ResupinationStance$Sex[i] = 'Male'
  } else {
    ResupinationStance$Sex[i] = 'Female'
  }
} 


ResupinationMale = ResupinationStance[which(ResupinationStance$Sex == 'Male'),]
ResupinationFemale = ResupinationStance[which(ResupinationStance$Sex == 'Female'),]

ResupinationMC1 = ResupinationMale[,c(1,2,3,4)]
ResupinationMC2 = ResupinationMale[,c(1,2,3,5)]
ResupinationMC4 = ResupinationMale[,c(1,2,3,7)]
ResupinationMC1$Shoe = 'C1'
ResupinationMC2$Shoe = 'C2'
ResupinationMC4$Shoe = 'C4'

names(ResupinationMC1)[4] = 'Resupination'
names(ResupinationMC2)[4] = 'Resupination'
names(ResupinationMC4)[4] = 'Resupination'
ResupinationMale = rbind(ResupinationMC1, ResupinationMC2, ResupinationMC4)

ResupinationMaleList = vector(mode = "list", length = length(maleID))  
for (i in 1:length(maleID)) {
  
  ResupinationMaleList[[i]] = ggplot(data = ResupinationMale[which(ResupinationMale$Subj_ID == maleID[i]),],
                             aes(x = PctStance, y = Resupination)) + plot_theme + theme(legend.position = "none") + 
    annotate("rect", xmax = 100, xmin = 60, ymax = Inf, ymin = -Inf, fill = umass_grey, alpha = 0.25) +
    geom_vline(xintercept = c(0), color = c("black")) + geom_hline(yintercept = 0) +
    geom_line(aes(color = Shoe, linetype = Shoe), size = 1) + scale_color_manual(values =c(malecolors, cb_orange))+
  scale_y_continuous(limits = c(-2.5, 2.5), breaks = seq(-2.0, 2.0, 1.0))+
    labs(x = '% Stance', y = 'Resupination', title = maleID[i]) 
  
}  
print(ggarrange(plotlist = ResupinationMaleList, ncol = 2, nrow = 2))
ggsave(ggarrange(plotlist = ResupinationMaleList, ncol = 2, nrow = 2), filename = paste0(savefolder, 'ResupinationMalePlot.png'), height = 4, width = 8)

#### Research Question 2 - Deviations ####
  # Do biomechanics differ between C1 & C2? #
RQ2.EvDev = Subj[-5,c(1,2,5)]
for (i in 1:(length(ShoeList)-2)) {
  tmp_col = Loft4ALL$EvDev[which(Loft4ALL$ShoeCnd == ShoeList[i])]
  RQ2.EvDev = cbind(RQ2.EvDev, tmp_col)
}

names(RQ2.EvDev)[4:length(RQ2.EvDev)] = ShoeList[1:4]
names(RQ2.EvDev)[3] = 'DevGroup'

C1_EvDev = RQ2.EvDev$C1_8min
for (i in 1:(length(ShoeList)-2)) {
  col_num = 3+i
  tmp_col = abs(RQ2.EvDev[,col_num]) - abs(C1_EvDev)
  RQ2.EvDev[,col_num] = tmp_col
}


RQ2.EvDevNew = gather(RQ2.EvDev, Shoe, EversionDev, 'C1_8min':'C4_8min', factor_key = TRUE)
names(RQ2.EvDevNew)[3] = 'DevGroup'
RQ2.EvDevMEAN =  aggregate(EversionDev ~ Sex*Shoe*DevGroup, RQ2.EvDevNew, mean)


RQ2.EvDev.Male =
  ggplot(data = RQ2.EvDevNew[which(RQ2.EvDevNew$Sex == 'Male'),], aes(x =Shoe, y = EversionDev, color = DevGroup)) + 
  annotate("rect", xmax = Inf, xmin = -Inf, ymax = 2, ymin = -2, fill = "gray", alpha = 0.5) +
  geom_hline(yintercept = c(0,-2,2), linetype = c(1,3,3))+
  geom_line(aes(group = ID, linetype = DevGroup), size = 1.25, alpha = 1) +
  #geom_line(aes(color = DevGroup), size = 1, alpha = 1) + 
  #scale_linetype_discrete(values = c(1,5)) +
  geom_point(aes(shape = DevGroup), size = 5, alpha =1) + 
  # geom_point(data = RQ2.EvDevMEAN[which(RQ2.EvDevMEAN$Sex == 'Male'),], aes(x = Shoe, y = EversionDev, color = DevGroup), alpha = 1, shape = 4, size = 2.5, stroke = 3) +
  scale_shape_manual(values = c(16,17)) +
  scale_color_manual(values= malecolors) +
  #scale_fill_manual(values = malecolors) +
  scale_x_discrete(limits = c("C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                   labels = c("C1", "C2"))+#, "C3", "C4")) +
  #geom_point(data = meansEv, aes(x = Shoe, y = EversionDev, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
  ylab('\u0394 Eversion Deviation (deg.)') +
  labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
  #ylim(c(-10, 10)) +
  scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 2)) +
  plot_theme + theme(legend.position = "bottom") 
#geom_hline(yintercept = 0, linetype = 1)
print(RQ2.EvDev.Male)
ggsave(filename = paste0(savefolder, 'RQ2EvDevMale.png') ,plot = RQ2.EvDev.Male, height = 5, width = 8 )


#Tibial Rotation Plots
RQ2.TibRotDev = Subj[-5,c(1,2,5)]
for (i in 1:(length(ShoeList)-2)) {
  tmp_col = Loft4ALL$TibRotDev[which(Loft4ALL$ShoeCnd == ShoeList[i])]
  RQ2.TibRotDev = cbind(RQ2.TibRotDev, tmp_col)
}

names(RQ2.TibRotDev)[4:length(RQ2.TibRotDev)] = ShoeList[1:4]

C1_TibRot = RQ2.TibRotDev$C1_8min
for (i in 1:(length(ShoeList)-2)) {
  col_num = 3+i
  tmp_col = abs(RQ2.TibRotDev[,col_num]) - abs(C1_TibRot)
  RQ2.TibRotDev[,col_num] = tmp_col
}

RQ2.TibRotDevNew = gather(RQ2.TibRotDev, Shoe, TibRotDev, 'C1_8min':'C4_8min', factor_key = TRUE)
names(RQ2.TibRotDevNew)[3] = 'DevGroup'
RQ2.TibRotMEAN =  aggregate(TibRotDev ~ Sex*Shoe*DevGroup, RQ2.TibRotDevNew, mean)


RQ2.TibRot.Male =
ggplot(data = RQ2.TibRotDevNew[which(RQ2.TibRotDevNew$Sex == 'Male'),], aes(x = Shoe, y = TibRotDev, color = DevGroup)) + 
  annotate("rect", xmax = Inf, xmin = -Inf, ymax = 1.5, ymin = -1.5, fill = "gray", alpha = 0.5) +
  geom_hline(yintercept = c(0,-1.5,1.5), linetype = c(1,3,3))+
  geom_line(aes(group = ID, linetype = DevGroup), size = 1.25, alpha = 1) + 
  geom_point(aes(color = DevGroup, shape = DevGroup), size = 5, alpha =1) + 
 # geom_point(data = RQ2.TibRotMEAN[which(RQ2.TibRotMEAN$Sex == 'Male'),], aes(x = Shoe, y = TibRotDev, color = DevGroup), alpha = 1, shape = 4, size = 2.5, stroke = 3) +
  scale_shape_manual(values = c(16, 17)) +
  scale_color_manual(values=malecolors) +
  scale_fill_manual(values = malecolors) +
  scale_x_discrete(limits = c("C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                   labels = c("C1", "C2"))+#, "C3", "C4")) +
  #geom_point(data = meansEv, aes(x = Shoe, y = TibRotDev, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
  ylab('\u0394 Tib. Rotation Deviation (deg.)') +
  labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
  #ylim(c(-10, 10)) +
  scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 2)) +
plot_theme  + theme(legend.position = "bottom")  
  #geom_hline(yintercept = 0, linetype = 1)
print(RQ2.TibRot.Male)
ggsave(file = paste0(savefolder, 'RQ2TibRotMale.png'), plot = RQ2.TibRot.Male, height = 5, width = 8)



#Knee Abd/Add Deviation Plots
RQ2.KneeAddDev = Subj[-5,c(1,2,5)]
for (i in 1:(length(ShoeList)-2)) {
  tmp_col = Loft4ALL$KneeAddDev[which(Loft4ALL$ShoeCnd == ShoeList[i])]
  RQ2.KneeAddDev = cbind(RQ2.KneeAddDev, tmp_col)
}

names(RQ2.KneeAddDev)[4:length(RQ2.KneeAddDev)] = ShoeList[1:4]

C1_KneeAddDev = RQ2.KneeAddDev$C1_8min
for (i in 1:(length(ShoeList)-2)) {
  col_num = 3+i
  tmp_col = abs(RQ2.KneeAddDev[,col_num]) - abs(C1_KneeAddDev)
  RQ2.KneeAddDev[,col_num] = tmp_col
}

RQ2.KneeAddDevNew = gather(RQ2.KneeAddDev, Shoe, KneeAddDev, 'C1_8min':'C4_8min', factor_key = TRUE)

RQ2.KA.Male = 
ggplot(data = RQ2.KneeAddDevNew[which(RQ2.KneeAddDevNew$Sex == 'Male'),], aes(x =Shoe, y = KneeAddDev, group = ID)) + 
  annotate("rect", xmax = Inf, xmin = -Inf, ymax = 1, ymin = -1, fill = "gray", alpha = 0.5) +
  geom_hline(yintercept = c(0,-1,1), linetype = c(1,3,3))+
  geom_line(aes(color = DevGroup, linetype = DevGroup), size = 1.25, alpha = 1) + 
  geom_point(aes(color = DevGroup, shape = DevGroup), size = 5, alpha =1) + 
  scale_shape_manual(values = c(16, 17)) +
  scale_color_manual(values=malecolors) +
  scale_fill_manual(values = malecolors) +
  scale_x_discrete(limits = c("C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                   labels = c("C1", "C2"))+#, "C3", "C4")) +
  #geom_point(data = meansEv, aes(x = Shoe, y = KneeAddDev, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
  ylab('\u0394 Knee Abd/Add. Deviation (deg.)') +
  labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
  #ylim(c(-10, 10)) +
  scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5,1)) +
plot_theme  + theme(legend.position = "bottom") 
  #geom_hline(yintercept = 0, linetype = 1)
print(RQ2.KA.Male)
ggsave(filename = paste0(savefolder, 'RQ2KAMale.png'), plot = RQ2.KA.Male, height = 5, width = 8)



#Max Eversion Velocity plots 
RQ2.MaxEV = Subj[-5,c(1,2,5)]
for (i in 1:(length(ShoeList)-2)) {
  tmp_col = Loft4ALL$NEG_POS_peak_EversionVel[which(Loft4ALL$ShoeCnd == ShoeList[i])]
  RQ2.MaxEV = cbind(RQ2.MaxEV, tmp_col)
}

names(RQ2.MaxEV)[4:length(RQ2.MaxEV)] = ShoeList[1:4]

C1_MaxEv = RQ2.MaxEV$C1_8min
for (i in 1:(length(ShoeList)-2)) {
  col_num = 3+i
  tmp_col = abs(RQ2.MaxEV[,col_num]) - abs(C1_MaxEv)
  RQ2.MaxEV[,col_num] = tmp_col
}

RQ2.MaxEVNew = gather(RQ2.MaxEV, Shoe, MaxEV, 'C1_8min':'C4_8min', factor_key = TRUE)

RQ2.MaxEV.Male = 
ggplot(data = RQ2.MaxEVNew[which(RQ2.MaxEVNew$Sex == 'Male'),], aes(x =Shoe, y = MaxEV, group = ID)) + 
  annotate("rect", xmax = Inf, xmin = -Inf, ymax = 40, ymin = -40, fill = "gray", alpha = 0.5) +
  geom_hline(yintercept = c(0,-40,40), linetype = c(1,3,3))+
  geom_line(aes(color = DevGroup, linetype = DevGroup), size = 1.25, alpha = 1) + 
  geom_point(aes(color = DevGroup, shape = DevGroup), size = 5, alpha =1) + 
  scale_shape_manual(values = c(16, 17)) +
  scale_color_manual(values=malecolors) +
  scale_fill_manual(values = malecolors) +
  scale_x_discrete(limits = c("C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                   labels = c("C1", "C2"))+#, "C3", "C4")) +
  #geom_point(data = meansEv, aes(x = Shoe, y = MaxEV, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
  ylab('\u0394 Eversion Velocity (deg./s)') +
  labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
  #ylim(c(-10, 10)) +
  scale_y_continuous(limits = c(-100,100), breaks = seq(-100,100,20)) +
 plot_theme  + theme(legend.position = "bottom") 
  #geom_hline(yintercept = 0, linetype = 1)
print(RQ2.MaxEV.Male)
ggsave(filename = paste0(savefolder, 'RQ2MaxEVMale.png'), plot = RQ2.MaxEV.Male, height = 5, width = 8)


#### Research Question 2 - Impact Metrics ####
  RQ2.ImpactPeak = Subj[-5,c(1,2,5)]
  for (i in 1:(length(ShoeList)-2)) {
    tmp_col = Loft4ALL$ImpactPeak[which(Loft4ALL$ShoeCnd == ShoeList[i])]
    RQ2.ImpactPeak = cbind(RQ2.ImpactPeak, tmp_col)
  }
  
  names(RQ2.ImpactPeak)[4:length(RQ2.ImpactPeak)] = ShoeList[1:4]
  names(RQ2.ImpactPeak)[3] = 'DevGroup'
  
  C1_ImpactPeak = RQ2.ImpactPeak$C1_8min
  for (i in 1:(length(ShoeList)-2)) {
    col_num = 3+i
    tmp_col = abs(RQ2.ImpactPeak[,col_num]) - abs(C1_ImpactPeak)
    RQ2.ImpactPeak[,col_num] = tmp_col
  }
  
  RQ2.ImpactPeakNew = gather(RQ2.ImpactPeak, Shoe, ImpactPeak, 'C1_8min':'C4_8min', factor_key = TRUE)
  names(RQ2.ImpactPeakNew)[3] = 'DevGroup'
  RQ2.ImpactPeakMEAN =  aggregate(ImpactPeak ~ Sex*Shoe*DevGroup, RQ2.ImpactPeakNew, mean)
  
 options(scipen = 0)
  RQ2.ImpactPeak.New =
    ggplot(data = RQ2.ImpactPeakNew[which(RQ2.ImpactPeakNew$Sex == 'Male'),], aes(x =Shoe, y = ImpactPeak, color = DevGroup)) + 
    #annotate("rect", xmax = Inf, xmin = -Inf, ymax = 2, ymin = -2, fill = "gray", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = 1)+
    geom_line(aes(group = ID, linetype = DevGroup), size = 1.25, alpha = 1) +
    #geom_line(aes(color = DevGroup), size = 1, alpha = 1) + 
    #scale_linetype_discrete(values = c(1,5)) +
    geom_point(aes(shape = DevGroup), size = 4, alpha =1) + 
    # geom_point(data = RQ2.ImpactPeakMEAN[which(RQ2.ImpactPeakMEAN$Sex == 'Male'),], aes(x = Shoe, y = ImpactPeak, color = DevGroup), alpha = 1, shape = 4, size = 2.5, stroke = 3) +
    scale_shape_manual(values = c(16,17)) +
    scale_color_manual(values= malecolors) +
    #scale_fill_manual(values = malecolors) +
    scale_x_discrete(limits = c("C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                     labels = c("C1", "C2"))+#, "C3", "C4")) +
    #geom_point(data = meansEv, aes(x = Shoe, y = ImpactPeak, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
    ylab('Impact Peak (BW)') +
    labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
    #ylim(c(-10, 10)) +
    scale_y_continuous(limits = c(-0.25, 0.25), breaks = seq(-0.25, 0.25, 0.05)) +
    plot_theme + labs(title = "Impact Peak") + theme(legend.position = "bottom") 
  #geom_hline(yintercept = 0, linetype = 1)
  print(RQ2.ImpactPeak.New)
  ggsave(filename = paste0(savefolder, 'RQ2ImpactPeakNew.png') ,plot = RQ2.ImpactPeak.New, height = 4, width = 6.5 )
  
  #VALR
  RQ2.VALR = Subj[-5,c(1,2,5)]
  for (i in 1:(length(ShoeList)-2)) {
    tmp_col = Loft4ALL$VALR[which(Loft4ALL$ShoeCnd == ShoeList[i])]
    RQ2.VALR = cbind(RQ2.VALR, tmp_col)
  }
  
  names(RQ2.VALR)[4:length(RQ2.VALR)] = ShoeList[1:4]
  names(RQ2.VALR)[3] = 'DevGroup'
  
  C1_VALR = RQ2.VALR$C1_8min
  for (i in 1:(length(ShoeList)-2)) {
    col_num = 3+i
    tmp_col = abs(RQ2.VALR[,col_num]) - abs(C1_VALR)
    RQ2.VALR[,col_num] = tmp_col
  }
  
  RQ2.VALRNew = gather(RQ2.VALR, Shoe, VALR, 'C1_8min':'C4_8min', factor_key = TRUE)
  names(RQ2.VALRNew)[3] = 'DevGroup'
  RQ2.VALRMEAN =  aggregate(VALR ~ Sex*Shoe*DevGroup, RQ2.VALRNew, mean)
  
  
  RQ2.VALR.New =
    ggplot(data = RQ2.VALRNew[which(RQ2.VALRNew$Sex == 'Male'),], aes(x =Shoe, y = VALR, color = DevGroup)) + 
    #annotate("rect", xmax = Inf, xmin = -Inf, ymax = 2, ymin = -2, fill = "gray", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = 1)+
    geom_line(aes(group = ID, linetype = DevGroup), size = 1.25, alpha = 1) +
    #geom_line(aes(color = DevGroup), size = 1, alpha = 1) + 
    #scale_linetype_discrete(values = c(1,5)) +
    geom_point(aes(shape = DevGroup), size = 4, alpha =1) + 
    # geom_point(data = RQ2.VALRMEAN[which(RQ2.VALRMEAN$Sex == 'Male'),], aes(x = Shoe, y = VALR, color = DevGroup), alpha = 1, shape = 4, size = 2.5, stroke = 3) +
    scale_shape_manual(values = c(16,17)) +
    scale_color_manual(values= malecolors) +
    #scale_fill_manual(values = malecolors) +
    scale_x_discrete(limits = c("C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                     labels = c("C1", "C2"))+#, "C3", "C4")) +
    #geom_point(data = meansEv, aes(x = Shoe, y = VALR, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
    ylab('BW/s') +
    labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
    #ylim(c(-10, 10)) +
    scale_y_continuous(limits = c(-0.025, 0.025), breaks = seq(-0.025, 0.025, 0.005)) +
    plot_theme + labs(title = "VALR") + theme(legend.position = "bottom") 
  #geom_hline(yintercept = 0, linetype = 1)
  print(RQ2.VALR.New)
  ggsave(filename = paste0(savefolder, 'RQ2VALRNew.png') ,plot = RQ2.VALR.New, height = 4, width = 6.5 )
  
  
  
  GRFzStance = as.data.frame(read_excel("C:/Users/msalzano/OneDrive - University of Massachusetts/Brooks/Loft_v4_Tuned/Analysis_withStatic/GRFzStance.xlsx"))
  maleID = Subj$ID[which(Subj$Sex == 'Male')]
  femaleID = Subj$ID[which(Subj$Sex == 'Female')]
  
  for (i in 1:nrow(GRFzStance)) {
    if (is.element(GRFzStance$Subj_ID[i], maleID) == TRUE) {
      GRFzStance$Sex[i] = 'Male'
    } else {
      GRFzStance$Sex[i] = 'Female'
    }
  } 
  
  
  GRFzMale = GRFzStance[which(GRFzStance$Sex == 'Male'),]
  GRFzFemale = GRFzStance[which(GRFzStance$Sex == 'Female'),]
  
  GRFzMC1 = GRFzMale[,c(1,2,3,4)]
  GRFzMC2 = GRFzMale[,c(1,2,3,5)]
  GRFzMC1$Shoe = 'C1'
  GRFzMC2$Shoe = 'C2'
  names(GRFzMC1)[4] = 'GRFz'
  names(GRFzMC2)[4] = 'GRFz'
  GRFzMale = rbind(GRFzMC1, GRFzMC2)
  
  GRFzWC1 = GRFzFemale[,c(1,2,3,4)]
  GRFzWC2 = GRFzFemale[,c(1,2,3,5)]
  GRFzWC1$Shoe = 'WC1'
  GRFzWC2$Shoe = 'WC2'
  names(GRFzWC1)[4] = 'GRFz'
  names(GRFzWC2)[4] = 'GRFz'
  GRFzFemale = rbind(GRFzWC1, GRFzWC2)
  
  GRFzMaleList = vector(mode = "list", length = length(maleID))  
  for (i in 1:length(maleID)) {
    
    GRFzMaleList[[i]] = ggplot(data = GRFzMale[which(GRFzMale$Subj_ID == maleID[i]),],
                               aes(x = PctStance, y = GRFz)) + plot_theme + theme(legend.position = "none") + 
      geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
      geom_line(aes(color = Shoe, linetype = Shoe), size = 1) + scale_color_manual(values = malecolors) +
      labs(x = '% Stance', y = 'GRFz', title = maleID[i]) 
    
  }  
  print(ggarrange(plotlist = GRFzMaleList, ncol = 2, nrow = 2))
  ggsave(ggarrange(plotlist = GRFzMaleList, ncol = 2, nrow = 2), filename = paste0(savefolder, 'GRFzMalePlot.png'), height = 4, width = 12)
  
  
  GRFzFemaleList = vector(mode = "list", length = length(femaleID))  
  for (i in 1:length(femaleID)) {
    
    GRFzFemaleList[[i]] = ggplot(data = GRFzFemale[which(GRFzFemale$Subj_ID == femaleID[i]),],
                                 aes(x = PctStance, y = GRFz)) + plot_theme + theme(legend.position = "none") + 
      geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
      geom_line(aes(color = Shoe, linetype = Shoe), size = 1) + scale_color_manual(values = femalecolors) +
      labs(x = '% Stance', y = 'GRFz', title = femaleID[i]) 
    
  }  
  print(ggarrange(plotlist = GRFzFemaleList, ncol = 3, nrow = 3))
  ggsave(ggarrange(plotlist = GRFzFemaleList, ncol = 3, nrow = 3), filename = paste0(savefolder, 'GRFzFemalePlot.png'), height = 6, width = 12)
  
  
#### Research Question 2 - Runmetrics ####
  RQ2.Cadence = Subj[-5,c(1,2,5)]
  for (i in 1:(length(ShoeList)-2)) {
    tmp_col = Loft4ALL$Cadence[which(Loft4ALL$ShoeCnd == ShoeList[i])]
    RQ2.Cadence = cbind(RQ2.Cadence, tmp_col)
  }
  
  names(RQ2.Cadence)[4:length(RQ2.Cadence)] = ShoeList[1:4]
  names(RQ2.Cadence)[3] = 'DevGroup'
  
  C1_Cadence = RQ2.Cadence$C1_8min
  for (i in 1:(length(ShoeList)-2)) {
    col_num = 3+i
    tmp_col = abs(RQ2.Cadence[,col_num]) - abs(C1_Cadence)
    RQ2.Cadence[,col_num] = tmp_col
  }
  
  RQ2.CadenceNew = gather(RQ2.Cadence, Shoe, Cadence, 'C1_8min':'C4_8min', factor_key = TRUE)
  names(RQ2.CadenceNew)[3] = 'DevGroup'
  RQ2.CadenceMEAN =  aggregate(Cadence ~ Sex*Shoe*DevGroup, RQ2.CadenceNew, mean)
  
  
  RQ2.Cadence.New =
    ggplot(data = RQ2.CadenceNew[which(RQ2.CadenceNew$Sex == 'Male'),], aes(x =Shoe, y = Cadence, color = DevGroup)) + 
    #annotate("rect", xmax = Inf, xmin = -Inf, ymax = 2, ymin = -2, fill = "gray", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = 1)+
    geom_line(aes(group = ID, linetype = DevGroup), size = 1.25, alpha = 1) +
    #geom_line(aes(color = DevGroup), size = 1, alpha = 1) + 
    #scale_linetype_discrete(values = c(1,5)) +
    geom_point(aes(shape = DevGroup), size = 4, alpha =1) + 
    # geom_point(data = RQ2.CadenceMEAN[which(RQ2.CadenceMEAN$Sex == 'Male'),], aes(x = Shoe, y = Cadence, color = DevGroup), alpha = 1, shape = 4, size = 2.5, stroke = 3) +
    scale_shape_manual(values = c(16,17)) +
    scale_color_manual(values= malecolors) +
    #scale_fill_manual(values = malecolors) +
    scale_x_discrete(limits = c("C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                     labels = c("C1", "C2"))+#, "C3", "C4")) +
    #geom_point(data = meansEv, aes(x = Shoe, y = Cadence, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
    ylab('Steps/min.') +
    labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
    #ylim(c(-10, 10)) +
    scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
    plot_theme + labs(title = "Cadence") + theme(legend.position = "bottom") 
  #geom_hline(yintercept = 0, linetype = 1)
  print(RQ2.Cadence.New)
  ggsave(filename = paste0(savefolder, 'RQ2CadenceNew.png') ,plot = RQ2.Cadence.New, height = 4, width = 6.5 )
  
  
  RQ2.ContactTime = Subj[-5,c(1,2,5)]
  for (i in 1:(length(ShoeList)-2)) {
    tmp_col = Loft4ALL$ContactTime[which(Loft4ALL$ShoeCnd == ShoeList[i])]
    RQ2.ContactTime = cbind(RQ2.ContactTime, tmp_col)
  }
  
  names(RQ2.ContactTime)[4:length(RQ2.ContactTime)] = ShoeList[1:4]
  names(RQ2.ContactTime)[3] = 'DevGroup'
  
  C1_ContactTime = RQ2.ContactTime$C1_8min
  for (i in 1:(length(ShoeList)-2)) {
    col_num = 3+i
    tmp_col = abs(RQ2.ContactTime[,col_num]) - abs(C1_ContactTime)
    RQ2.ContactTime[,col_num] = tmp_col
  }
  
  RQ2.ContactTimeNew = gather(RQ2.ContactTime, Shoe, ContactTime, 'C1_8min':'C4_8min', factor_key = TRUE)
  names(RQ2.ContactTimeNew)[3] = 'DevGroup'
  RQ2.ContactTimeMEAN =  aggregate(ContactTime ~ Sex*Shoe*DevGroup, RQ2.ContactTimeNew, mean)
  
  
  RQ2.ContactTime.New =
    ggplot(data = RQ2.ContactTimeNew[which(RQ2.ContactTimeNew$Sex == 'Male'),], aes(x =Shoe, y = ContactTime, color = DevGroup)) + 
    #annotate("rect", xmax = Inf, xmin = -Inf, ymax = 2, ymin = -2, fill = "gray", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = 1)+
    geom_line(aes(group = ID, linetype = DevGroup), size = 1.25, alpha = 1) +
    #geom_line(aes(color = DevGroup), size = 1, alpha = 1) + 
    #scale_linetype_discrete(values = c(1,5)) +
    geom_point(aes(shape = DevGroup), size = 4, alpha =1) + 
    # geom_point(data = RQ2.ContactTimeMEAN[which(RQ2.ContactTimeMEAN$Sex == 'Male'),], aes(x = Shoe, y = ContactTime, color = DevGroup), alpha = 1, shape = 4, size = 2.5, stroke = 3) +
    scale_shape_manual(values = c(16,17)) +
    scale_color_manual(values= malecolors) +
    #scale_fill_manual(values = malecolors) +
    scale_x_discrete(limits = c("C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                     labels = c("C1", "C2"))+#, "C3", "C4")) +
    #geom_point(data = meansEv, aes(x = Shoe, y = ContactTime, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
    ylab('seconds') +
    labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
    #ylim(c(-10, 10)) +
    scale_y_continuous(limits = c(-0.015, 0.015), breaks = seq(-0.015, 0.015, 0.005)) +
    plot_theme + labs(title = "Contact time") + theme(legend.position = "bottom") 
  #geom_hline(yintercept = 0, linetype = 1)
  print(RQ2.ContactTime.New)
  ggsave(filename = paste0(savefolder, 'RQ2ContactTimeNew.png') ,plot = RQ2.ContactTime.New, height = 4, width = 6.5 )
  

#### Research Question 2 - Foot Posture at Landing ####
  RQ2.InclinationAngle = Subj[-5,c(1,2,5)]
  for (i in 1:(length(ShoeList)-2)) {
    tmp_col = Loft4ALL$InclinationAngleFS[which(Loft4ALL$ShoeCnd == ShoeList[i])]
    RQ2.InclinationAngle = cbind(RQ2.InclinationAngle, tmp_col)
  }
  
  names(RQ2.InclinationAngle)[4:length(RQ2.InclinationAngle)] = ShoeList[1:4]
  names(RQ2.InclinationAngle)[3] = 'DevGroup'
  
  C1_InclinationAngle = RQ2.InclinationAngle$C1_8min
  for (i in 1:(length(ShoeList)-2)) {
    col_num = 3+i
    tmp_col = abs(RQ2.InclinationAngle[,col_num]) - abs(C1_InclinationAngle)
    RQ2.InclinationAngle[,col_num] = tmp_col
  }
  
  RQ2.InclinationAngleNew = gather(RQ2.InclinationAngle, Shoe, InclinationAngle, 'C1_8min':'C4_8min', factor_key = TRUE)
  names(RQ2.InclinationAngleNew)[3] = 'DevGroup'
  RQ2.InclinationAngleMEAN =  aggregate(InclinationAngle ~ Sex*Shoe*DevGroup, RQ2.InclinationAngleNew, mean)
  
  
  RQ2.InclinationAngle.New =
    ggplot(data = RQ2.InclinationAngleNew[which(RQ2.InclinationAngleNew$Sex == 'Male'),], aes(x =Shoe, y = InclinationAngle, color = DevGroup)) + 
    #annotate("rect", xmax = Inf, xmin = -Inf, ymax = 2, ymin = -2, fill = "gray", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = 1)+
    geom_line(aes(group = ID, linetype = DevGroup), size = 1.25, alpha = 1) +
    #geom_line(aes(color = DevGroup), size = 1, alpha = 1) + 
    #scale_linetype_discrete(values = c(1,5)) +
    geom_point(aes(shape = DevGroup), size = 4, alpha =1) + 
    # geom_point(data = RQ2.InclinationAngleMEAN[which(RQ2.InclinationAngleMEAN$Sex == 'Male'),], aes(x = Shoe, y = InclinationAngle, color = DevGroup), alpha = 1, shape = 4, size = 2.5, stroke = 3) +
    scale_shape_manual(values = c(16,17)) +
    scale_color_manual(values= malecolors) +
    #scale_fill_manual(values = malecolors) +
    scale_x_discrete(limits = c("C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                     labels = c("C1", "C2"))+#, "C3", "C4")) +
    #geom_point(data = meansEv, aes(x = Shoe, y = InclinationAngle, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
    ylab('\u0394 Angle (deg.)') +
    labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
    #ylim(c(-10, 10)) +
    scale_y_continuous(limits = c(-5, 5), breaks = seq(-5, 5, 1)) +
    plot_theme + theme(legend.position = "bottom") 
  #geom_hline(yintercept = 0, linetype = 1)
  print(RQ2.InclinationAngle.New)
  ggsave(filename = paste0(savefolder, 'RQ2InclinationAngleNew.png') ,plot = RQ2.InclinationAngle.New, height = 4, width = 6.5 )
  
  
#### Research Question 3 - Deviations ####
  RQ3.EvDev = Subj[-5,c(1,2,5)]
  for (i in 1:(length(ShoeList)-2)) {
    tmp_col = Loft4ALL$EvDev[which(Loft4ALL$ShoeCnd == ShoeList[i])]
    RQ3.EvDev = cbind(RQ3.EvDev, tmp_col)
  }
  
  names(RQ3.EvDev)[4:length(RQ3.EvDev)] = ShoeList[1:4]
  names(RQ3.EvDev)[3] = 'DevGroup'
  
  C3_EvDev = RQ3.EvDev$C3_8min
  for (i in 1:(length(ShoeList)-2)) {
    col_num = 3+i
    tmp_col = abs(RQ3.EvDev[,col_num]) - abs(C3_EvDev)
    RQ3.EvDev[,col_num] = tmp_col
  }
  
  
  RQ3.EvDevNew = gather(RQ3.EvDev, Shoe, EversionDev, 'C1_8min':'C4_8min', factor_key = TRUE)
  names(RQ3.EvDevNew)[3] = 'DevGroup'
  RQ3.EvDevMEAN =  aggregate(EversionDev ~ Sex*Shoe*DevGroup, RQ3.EvDevNew, mean)
  
  
  RQ3.EvDev.Male =
    ggplot(data = RQ3.EvDevNew[which(RQ3.EvDevNew$Sex == 'Male'),], aes(x =Shoe, y = EversionDev, color = DevGroup)) + 
    annotate("rect", xmax = Inf, xmin = -Inf, ymax = 2, ymin = -2, fill = "gray", alpha = 0.5) +
    geom_hline(yintercept = c(0,-2,2), linetype = c(1,3,3))+
    geom_line(aes(group = ID, linetype = DevGroup), size = 1.25, alpha = 1) +
    #geom_line(aes(color = DevGroup), size = 1, alpha = 1) + 
    #scale_linetype_discrete(values = c(1,5)) +
    geom_point(aes(shape = DevGroup), size = 5, alpha =1) + 
    # geom_point(data = RQ3.EvDevMEAN[which(RQ3.EvDevMEAN$Sex == 'Male'),], aes(x = Shoe, y = EversionDev, color = DevGroup), alpha = 1, shape = 4, size = 2.5, stroke = 3) +
    scale_shape_manual(values = c(16,17)) +
    scale_color_manual(values= malecolors) +
    #scale_fill_manual(values = malecolors) +
    scale_x_discrete(limits = c("C3_8min", "C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                     labels = c("C3","C1", "C2"))+#, "C3", "C4")) +
    #geom_point(data = meansEv, aes(x = Shoe, y = EversionDev, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
    ylab('\u0394 Eversion Deviation (deg.)') +
    labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
    #ylim(c(-10, 10)) +
    scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 2)) +
    plot_theme + theme(legend.position = "bottom") 
  #geom_hline(yintercept = 0, linetype = 1)
  print(RQ3.EvDev.Male)
  ggsave(filename = paste0(savefolder, 'RQ3EvDevMale.png') ,plot = RQ3.EvDev.Male, height = 5, width = 8 )
  
  
  #Tibial Rotation Plots
  RQ3.TibRotDev = Subj[-5,c(1,2,5)]
  for (i in 1:(length(ShoeList)-2)) {
    tmp_col = Loft4ALL$TibRotDev[which(Loft4ALL$ShoeCnd == ShoeList[i])]
    RQ3.TibRotDev = cbind(RQ3.TibRotDev, tmp_col)
  }
  
  names(RQ3.TibRotDev)[4:length(RQ3.TibRotDev)] = ShoeList[1:4]
  
  C3_TibRot = RQ3.TibRotDev$C3_8min
  for (i in 1:(length(ShoeList)-2)) {
    col_num = 3+i
    tmp_col = abs(RQ3.TibRotDev[,col_num]) - abs(C3_TibRot)
    RQ3.TibRotDev[,col_num] = tmp_col
  }
  
  RQ3.TibRotDevNew = gather(RQ3.TibRotDev, Shoe, TibRotDev, 'C1_8min':'C4_8min', factor_key = TRUE)
  names(RQ3.TibRotDevNew)[3] = 'DevGroup'
  RQ3.TibRotMEAN =  aggregate(TibRotDev ~ Sex*Shoe*DevGroup, RQ3.TibRotDevNew, mean)
  
  
  RQ3.TibRot.Male =
    ggplot(data = RQ3.TibRotDevNew[which(RQ3.TibRotDevNew$Sex == 'Male'),], aes(x = Shoe, y = TibRotDev, color = DevGroup)) + 
    annotate("rect", xmax = Inf, xmin = -Inf, ymax = 1.5, ymin = -1.5, fill = "gray", alpha = 0.5) +
    geom_hline(yintercept = c(0,-1.5,1.5), linetype = c(1,3,3))+
    geom_line(aes(group = ID, linetype = DevGroup), size = 1.25, alpha = 1) + 
    geom_point(aes(color = DevGroup, shape = DevGroup), size = 5, alpha =1) + 
    # geom_point(data = RQ3.TibRotMEAN[which(RQ3.TibRotMEAN$Sex == 'Male'),], aes(x = Shoe, y = TibRotDev, color = DevGroup), alpha = 1, shape = 4, size = 2.5, stroke = 3) +
    scale_shape_manual(values = c(16, 17)) +
    scale_color_manual(values=malecolors) +
    scale_fill_manual(values = malecolors) +
    scale_x_discrete(limits = c("C3_8min", "C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                     labels = c("C3","C1", "C2"))+#, "C3", "C4")) +
    #geom_point(data = meansEv, aes(x = Shoe, y = TibRotDev, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
    ylab('\u0394 Tib. Rotation Deviation (deg.)') +
    labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
    #ylim(c(-10, 10)) +
    scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, 2)) +
    plot_theme  + theme(legend.position = "bottom")  
  #geom_hline(yintercept = 0, linetype = 1)
  print(RQ3.TibRot.Male)
  ggsave(file = paste0(savefolder, 'RQ3TibRotMale.png'), plot = RQ3.TibRot.Male, height = 5, width = 8)
  
  
  
  #Knee Abd/Add Deviation Plots
  RQ3.KneeAddDev = Subj[-5,c(1,2,5)]
  for (i in 1:(length(ShoeList)-2)) {
    tmp_col = Loft4ALL$KneeAddDev[which(Loft4ALL$ShoeCnd == ShoeList[i])]
    RQ3.KneeAddDev = cbind(RQ3.KneeAddDev, tmp_col)
  }
  
  names(RQ3.KneeAddDev)[4:length(RQ3.KneeAddDev)] = ShoeList[1:4]
  
  C3_KneeAddDev = RQ3.KneeAddDev$C3_8min
  for (i in 1:(length(ShoeList)-2)) {
    col_num = 3+i
    tmp_col = abs(RQ3.KneeAddDev[,col_num]) - abs(C3_KneeAddDev)
    RQ3.KneeAddDev[,col_num] = tmp_col
  }
  
  RQ3.KneeAddDevNew = gather(RQ3.KneeAddDev, Shoe, KneeAddDev, 'C1_8min':'C4_8min', factor_key = TRUE)
  
  RQ3.KA.Male = 
    ggplot(data = RQ3.KneeAddDevNew[which(RQ3.KneeAddDevNew$Sex == 'Male'),], aes(x =Shoe, y = KneeAddDev, group = ID)) + 
    annotate("rect", xmax = Inf, xmin = -Inf, ymax = 1, ymin = -1, fill = "gray", alpha = 0.5) +
    geom_hline(yintercept = c(0,-1,1), linetype = c(1,3,3))+
    geom_line(aes(color = DevGroup, linetype = DevGroup), size = 1.25, alpha = 1) + 
    geom_point(aes(color = DevGroup, shape = DevGroup), size = 5, alpha =1) + 
    scale_shape_manual(values = c(16, 17)) +
    scale_color_manual(values=malecolors) +
    scale_fill_manual(values = malecolors) +
    scale_x_discrete(limits = c("C3_8min", "C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                     labels = c("C3","C1", "C2"))+#, "C3", "C4")) +
    #geom_point(data = meansEv, aes(x = Shoe, y = KneeAddDev, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
    ylab('\u0394 Knee Abd/Add. Deviation (deg.)') +
    labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
    #ylim(c(-10, 10)) +
    scale_y_continuous(limits = c(-5,5), breaks = seq(-5,5,1)) +
    plot_theme  + theme(legend.position = "bottom") 
  #geom_hline(yintercept = 0, linetype = 1)
  print(RQ3.KA.Male)
  ggsave(filename = paste0(savefolder, 'RQ3KAMale.png'), plot = RQ3.KA.Male, height = 5, width = 8)
  
  
  
  #Max Eversion Velocity plots 
  RQ3.MaxEV = Subj[-5,c(1,2,5)]
  for (i in 1:(length(ShoeList)-2)) {
    tmp_col = Loft4ALL$NEG_POS_peak_EversionVel[which(Loft4ALL$ShoeCnd == ShoeList[i])]
    RQ3.MaxEV = cbind(RQ3.MaxEV, tmp_col)
  }
  
  names(RQ3.MaxEV)[4:length(RQ3.MaxEV)] = ShoeList[1:4]
  
  C3_MaxEv = RQ3.MaxEV$C3_8min
  for (i in 1:(length(ShoeList)-2)) {
    col_num = 3+i
    tmp_col = abs(RQ3.MaxEV[,col_num]) - abs(C3_MaxEv)
    RQ3.MaxEV[,col_num] = tmp_col
  }
  
  RQ3.MaxEVNew = gather(RQ3.MaxEV, Shoe, MaxEV, 'C1_8min':'C4_8min', factor_key = TRUE)
  
  RQ3.MaxEV.Male = 
    ggplot(data = RQ3.MaxEVNew[which(RQ3.MaxEVNew$Sex == 'Male'),], aes(x =Shoe, y = MaxEV, group = ID)) + 
    annotate("rect", xmax = Inf, xmin = -Inf, ymax = 40, ymin = -40, fill = "gray", alpha = 0.5) +
    geom_hline(yintercept = c(0,-40,40), linetype = c(1,3,3))+
    geom_line(aes(color = DevGroup, linetype = DevGroup), size = 1.25, alpha = 1) + 
    geom_point(aes(color = DevGroup, shape = DevGroup), size = 5, alpha =1) + 
    scale_shape_manual(values = c(16, 17)) +
    scale_color_manual(values=malecolors) +
    scale_fill_manual(values = malecolors) +
    scale_x_discrete(limits = c("C3_8min", "C1_8min", "C2_8min"),#,"C3_8min","C4_8min"),
                     labels = c("C3","C1", "C2"))+#, "C3", "C4")) +
    #geom_point(data = meansEv, aes(x = Shoe, y = MaxEV, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
    ylab('\u0394 Eversion Velocity (deg./s)') +
    labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
    #ylim(c(-10, 10)) +
    scale_y_continuous(limits = c(-100,100), breaks = seq(-100,100,20)) +
    plot_theme  + theme(legend.position = "bottom") 
  #geom_hline(yintercept = 0, linetype = 1)
  print(RQ3.MaxEV.Male)
  ggsave(filename = paste0(savefolder, 'RQ3MaxEVMale.png'), plot = RQ3.MaxEV.Male, height = 5, width = 8)

  #### Research Question 3 - Perception of Support ####
  
  unqSupportResponse = unique(Loft4SurveyNew$ThisShoe_)
  Loft4SurveyNew$Supported = 0
  Loft4SurveyNew$Supported[which(Loft4SurveyNew$ThisShoe_ == unqSupportResponse[1])] = 'C'
  Loft4SurveyNew$Supported[which(Loft4SurveyNew$ThisShoe_ == unqSupportResponse[2])] = 'D'
  Loft4SurveyNew$Supported[which(Loft4SurveyNew$ThisShoe_ == unqSupportResponse[3])] = 'A'
  Loft4SurveyNew$Supported[which(Loft4SurveyNew$ThisShoe_ == unqSupportResponse[4])] = 'B'
  
  SupportedCount = Loft4SurveyNew %>% count(Supported, ShoeCnd)

  SupportedAll = 
    ggplot(data = Loft4SurveyNew, aes(x = ShoeCnd, fill = Supported, color = Supported)) +
    geom_bar() + plot_theme  + theme(legend.position = "none") +
    scale_color_manual(values = c('black', 'black','black','black'))+
    scale_fill_manual(values = c(cb_darkblue, cb_yellow, 'black', umass_grey)) +
    geom_text(data = SupportedCount, aes(y = n, label = Supported), color = 'white', size = 6,  position = position_stack(vjust = 0.5)) +
    scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1))+
    scale_x_discrete(limits = c("C3", "C1","C2"),
                     labels = c("C3", "C1", "C2"))
  print(SupportedAll)
  ggsave(filename = paste0(savefolder, 'SupportedAllPlot.png'), plot = SupportedAll, width = 4, height = 6)
  
#### Research Question 4 - Impact Metrics ####
  maleID = Subj$ID[which(Subj$Sex == 'Male')]
  femaleID = Subj$ID[which(Subj$Sex == 'Female')]
  
  for (i in 1:nrow(GRFzStance)) {
    if (is.element(GRFzStance$Subj_ID[i], maleID) == TRUE) {
      GRFzStance$Sex[i] = 'Male'
    } else {
      GRFzStance$Sex[i] = 'Female'
    }
  } 
  
  
  GRFzMale = GRFzStance[which(GRFzStance$Sex == 'Male'),]
  
  RQ4.GRFzC1 = GRFzMale[,c(1,2,3,4)]
  RQ4.GRFzC4 = GRFzMale[,c(1,2,3,7)]
  RQ4.GRFzC1$Shoe = 'C1'
  RQ4.GRFzC4$Shoe = 'C4'
  names(RQ4.GRFzC1)[4] = 'GRFz'
  names(RQ4.GRFzC4)[4] = 'GRFz'
  RQ4.GRFzMale = rbind(RQ4.GRFzC1, RQ4.GRFzC4)
  
  
  RQ4.GRFzMaleList = vector(mode = "list", length = length(maleID))  
  for (i in 1:length(maleID)) {
    
    RQ4.GRFzMaleList[[i]] = ggplot(data = RQ4.GRFzMale[which(RQ4.GRFzMale$Subj_ID == maleID[i]),],
                                   aes(x = PctStance, y = GRFz)) + plot_theme + theme(legend.position = "none") + 
      geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
      geom_line(aes(color = Shoe, linetype = Shoe), size = 1) + scale_color_manual(values = malecolors) +
      labs(x = '% Stance', y = 'GRFz', title = maleID[i]) 
    
  }  
  print(ggarrange(plotlist = RQ4.GRFzMaleList, ncol = 2, nrow = 2))
  ggsave(ggarrange(plotlist = RQ4.GRFzMaleList, ncol = 2, nrow = 2), filename = paste0(savefolder, 'RQ4_GRFzPlot.png'), height = 4, width = 8)
  

#### Research Question 4 - Cushion Perception ####
  RQ4.Cushion = Loft4SurveyNew[,c(1,2,3)]
  names(RQ4.Cushion)[3] = 'Cushion'
  
  for (i in 1:length(unqSubj)) {
    
    RQ4.Cushion$Sex[which(RQ4.Cushion$ID == unqSubj[i])] = Subj$Sex[which(Subj$ID == unqSubj[i])]
    RQ4.Cushion$DevGroup[which(RQ4.Cushion$ID == unqSubj[i])] = Subj$DevGroup[which(Subj$ID == unqSubj[i])]
  }
  
  #names(RQ4.CushionNew)[c(2,4,5)] = c('Shoe','Sex','DevGroup')
  
  #RQ4.Cushion$ID = factor(RQ4.Cushion$ID)
  ##RQ4.Cushion$Shoe = factor(RQ4.Cushion$Shoe)
  #RQ4.Cushion$Sex = factor(RQ4.Cushion$Sex)
  #RQ4.Cushion$DevGroup = factor(RQ4.Cushion$DevGroup)
  
  RQ4.Cushion.All =
    ggplot(data = RQ4.Cushion, aes(x =ShoeCnd, y = Cushion, color = DevGroup)) + 
    #annotate("rect", xmax = Inf, xmin = -Inf, ymax = 2, ymin = -2, fill = "gray", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = 1) +
    geom_line(aes(group = ID, linetype = DevGroup), size = 1.25, alpha = 1) +
    #geom_line(aes(color = DevGroup), size = 1, alpha = 1) + 
    # scale_linetype_discrete(values = c(1,5)) +
    geom_point(aes(shape = DevGroup), size = 4, alpha =1) + 
    # geom_point(data = RQ4.CushionMEAN[which(RQ4.CushionMEAN$Sex == 'Male'),], aes(x = Shoe, y = Cushion, color = DevGroup), alpha = 1, shape = 4, size = 2.5, stroke = 3) +
    scale_shape_manual(values = c(16,17,16)) +
    scale_color_manual(values= c(malecolors, femalecolors[1])) +
    #scale_fill_manual(values = malecolors) +
    scale_x_discrete(limits = c("C1", "C4"),#,"C3_8min","C4_8min"),
                     labels = c("C1", "C4"))+#, "C3", "C4")) +
    #geom_point(data = meansEv, aes(x = Shoe, y = Cushion, group = DevGroup, color = DevGroup), shape = 18, size = 10) + 
    ylab('Cushion Score') +
    labs(color = 'Deviation Group', shape = 'Deviation Group', linetype = 'Deviation Group') +
    #ylim(c(-10, 10)) +
    scale_y_continuous(limits = c(0,10), breaks = seq(0, 10, 1)) +
    plot_theme  + theme(legend.position = "bottom") #+ labs(title = "Males")
  #geom_hline(yintercept = 0, linetype = 1)
  print(RQ4.Cushion.All)
  ggsave(filename = paste0(savefolder, 'RQ4CushionAll.png') ,plot = RQ4.Cushion.All, height = 5, width = 8 )
  
  unqLikeCushion = unique(Loft4SurveyNew$LikeCushion)
  Loft4SurveyNew$CushionLikeNew = 0
  Loft4SurveyNew$CushionLikeNew[which(Loft4SurveyNew$LikeCushion == unqLikeCushion[1])] = 'Y'
  Loft4SurveyNew$CushionLikeNew[which(Loft4SurveyNew$LikeCushion == unqLikeCushion[2])] = 'M'
  Loft4SurveyNew$CushionLikeNew[which(Loft4SurveyNew$LikeCushion == unqLikeCushion[3])] = 'N'
  
  
  LikeCushionCount = Loft4SurveyNew %>% count(CushionLikeNew, ShoeCnd)
  
  LikeCushionAll = 
    ggplot(data = Loft4SurveyNew, aes(x = ShoeCnd, fill = CushionLikeNew, color = CushionLikeNew)) +
    geom_bar() + plot_theme  + theme(legend.position = "none") +
    scale_color_manual(values = c('black', 'black','black','black'))+
    scale_fill_manual(values = c(cb_darkblue, cb_yellow, 'black', umass_grey)) +
    geom_text(data = LikeCushionCount, aes(y = n, label = CushionLikeNew), color = 'white', size = 6,  position = position_stack(vjust = 0.5)) +
    scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1))+
    scale_x_discrete(limits = c("C1","C4"),
                     labels = c("C1", "C4"))
  print(LikeCushionAll)
  ggsave(filename = paste0(savefolder, 'LikeCushionAllPlot.png'), plot = LikeCushionAll, width = 4, height = 6)
  
#### Overall Cushion Rank ####
  OverallCushion = Loft4Survey[,c(2,3,27:30)]
  names(OverallCushion) = c('ID', 'Sex', 'C1', 'C2', 'C3','C4')
  OverallCushion = OverallCushion[order(OverallCushion$ID),]
  OverallCushion$DevGroup = Subj$DevGroup
  names(OverallCushion)[length(OverallCushion)] = 'DevGroup'
  
  
  OverallCushionLong = gather(OverallCushion, Shoe, Ranking, 'C1':'C4', factor_key = TRUE)
  OverallCushionLong$NumRank = 0
  
  
  OverallCushionLong$NumRank[which(OverallCushionLong$Ranking == '1st')] = 4
  OverallCushionLong$NumRank[which(OverallCushionLong$Ranking == '2nd')] = 3
  OverallCushionLong$NumRank[which(OverallCushionLong$Ranking == '3rd')] = 2
  OverallCushionLong$NumRank[which(OverallCushionLong$Ranking == '4th')] = 1
  
  RankCountAll = OverallCushionLong %>% count(Ranking, Shoe)
  
  
  OverallCushionPlot =
    ggplot(data = OverallCushionLong,
           aes(x = Shoe, color = Ranking, fill = Ranking)) +
    geom_bar(aes(color = Ranking, fill = Ranking), 
             position = position_stack(), width = .75) +
    geom_text(data = RankCountAll, aes(y = n, label = Ranking), color = 'white', size= 6, 
              position = position_stack(vjust = 0.5), show.legend = FALSE)+
    scale_color_manual(values = c('black', 'black','black','black')) +
    scale_fill_manual(values = c(cb_darkblue, cb_yellow, 'black', umass_red)) + 
    plot_theme + theme(legend.position = "none") +
    geom_hline(yintercept = 0, linetype = 1) + labs(x = 'Shoe', title = 'Male') +
    scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +
    scale_x_discrete(limits = c("C1","C2","C3", "C4"),
                     labels = c("C1", "C2", "C3", "C4"))
  print(OverallCushionPlot)
  ggsave(filename = paste0(savefolder, 'OverallCushionPlot.png'), plot = OverallCushionPlot, width = 4, height = 6)
  