library(ggplot2) # v. 3.2.0
library(purrr) # v. 0.3.2
library(ggpubr)
library(dplyr)
library(psych)



# Select altered ----------------------------------------------------------

setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/")
Family_MI <- read.csv("Macroinvertebrates_family_Spain_att.csv")

Family_MI_Summer <- Family_MI %>%
  filter(MONTH == "6" | MONTH == "7" | MONTH == "8"| MONTH == "9"| MONTH == "10")%>%
filter(A_COND20SITU < 800)


#Family_MI_Summer_ranges <- describeBy(Family_MI_Summer, Family_MI_Summer$Flow.regim)
#Family_MI_Summer_ranges_natural <- Family_MI_Summer_ranges$Natural
#Family_MI_Summer_ranges_altered <- Family_MI_Summer_ranges$Altered
#write.csv2(Family_MI_Summer_ranges_natural, "Family_MI_Summer_ranges_natural.csv")
#write.csv2(Family_MI_Summer_ranges_altered, "Family_MI_Summer_ranges_altered.csv")

#MI_ranges_natural <- read.csv2("Family_MI_Summer_natural.csv", row.names = 1)

Altered <- Family_MI %>%
  filter(Flow.regim == "Altered") %>%
  filter(MONTH == "6" | MONTH == "7" | MONTH == "8"| MONTH == "9"| MONTH == "10" )  


Natural <- Family_MI %>%
  filter(Flow.regim == "Natural") %>%
  filter(MONTH == "6" | MONTH == "7" | MONTH == "8"| MONTH == "9"| MONTH == "10") %>%
  filter(A_COND20SITU < 800)



Altered_filter_range <- Altered %>% 
  #filter(between(A_TAGUA, min(Natural$A_TAGUA, na.rm = T), max(Natural$A_TAGUA, na.rm = T)))%>% 
  filter(between(A_COND20SITU, min(Natural$A_COND20SITU, na.rm = T), max(Natural$A_COND20SITU, na.rm = T)))%>% 
  filter(between(A_PHSITU, min(Natural$A_PHSITU, na.rm = T), max(Natural$A_PHSITU, na.rm = T)))%>% 
  filter(between(MN_UHD, min(Natural$MN_UHD), max(Natural$MN_UHD)))%>% 
  filter(between(MN_AGR, min(Natural$MN_AGR), max(Natural$MN_AGR)))%>% 
  filter(between(MN_PAS, min(Natural$MN_PAS), max(Natural$MN_PAS)))%>% 
  filter(between(MN_SSH, min(Natural$MN_SSH), max(Natural$MN_PAS)))%>% 
  filter(between(MN_TEMP, min(Natural$MN_TEMP), max(Natural$MN_TEMP)))%>% 
  filter(between(MN_PREC, min(Natural$MN_PREC), max(Natural$MN_PREC)))%>% 
  filter(between(MN_ETP, min(Natural$MN_ETP), max(Natural$MN_ETP)))%>% 
  filter(between(MN_HARD, min(Natural$MN_HARD), max(Natural$MN_HARD)))%>% 
  filter(between(MN_COND, min(Natural$MN_COND), max(Natural$MN_COND)))%>% 
  filter(between(AREA_KM2, min(Natural$AREA_KM2), max(Natural$AREA_KM2)))%>% 
  filter(between(DEPTH, min(Natural$DEPTH), max(Natural$DEPTH)))%>% 
  filter(between(WIDTH, min(Natural$WIDTH), max(Natural$WIDTH)))%>% 
  filter(between(ELEV_M, min(Natural$ELEV_M), max(Natural$ELEV_M)))%>% 
  filter(between(SINUOSITY, min(Natural$SINUOSITY), max(Natural$SINUOSITY)))%>% 
  filter(between(MnSlope, min(Natural$MnSlope), max(Natural$MnSlope))) 


Nat_alt_summer <- merge(Natural, Altered_filter_range, all = T)
write.csv2(Nat_alt_summer, "Nat_alt_summer_614.csv", row.names = T)


# Graphs Hyd --------------------------------------------------------------


setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/")
Nat_alt_summer <- read.csv2("Nat_alt_summer_614.csv", stringsAsFactors=F)

#Nat_alt_summer2 <- Nat_alt_summer[!rownames(Nat_alt_summer) %in% c("CAN_M_097", "GAL_M_010", "GAL_M_011", "GAL_M_012", "GAL_M_013"), ]

FQ <- select(Nat_alt_summer, A_O2_SAT:A_PHSITU)
Hyd <- select(Nat_alt_summer, MeanJan:X95per)
Env <- select(Nat_alt_summer, AREA_KM2:MN_SSH)
Lev_20 <- as.factor(Nat_alt_summer$Lev_20)
Tipologia <- as.factor(Nat_alt_summer$Tipologia)
FQ_names = names(FQ)
Hyd_names = names(Hyd)
Env_names = names(Env)

Nat_alt_summer_3 <- Nat_alt_summer %>% filter(Lev_20 == "3")
Nat_alt_summer_8 <- Nat_alt_summer %>% filter(Lev_20 == "8")
Nat_alt_summer_10 <- Nat_alt_summer %>% filter(Lev_20 == "10")
Nat_alt_summer_13 <- Nat_alt_summer %>% filter(Lev_20 == "13")

theme_set(theme_bw())


myplots <- lapply(Hyd_names, function(q){
    Hyd_plots <- ggplot(Nat_alt_summer, aes_string(x = "Flow.regim", y = q)) +
    geom_boxplot(width = 0.45, fill = "white", notch = T) +
    geom_jitter(aes(color = Flow.regim, shape = Flow.regim), width = 0.1, size = 0.5) +
    scale_color_manual(values = c("tomato2", "palegreen3")) + theme(legend.position = "none") + labs(x = NULL) 
    #+ annotate("text", x = 4, y = 25, label = "Test_p_value", parse = TRUE)

   return(Hyd_plots)
  
})

myplots_env <- lapply(Env_names, function(q){
  Env_plots <- ggplot(Nat_alt_summer, aes_string(x = "Flow.regim", y = q)) +
    geom_boxplot(width = 0.45, fill = "white", notch = T) +
    geom_jitter(aes(color = Flow.regim, shape = Flow.regim), width = 0.1, size = 0.5) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) + theme(legend.position = "none") + labs(x = NULL) 
  #+ annotate("text", x = 4, y = 25, label = "Test_p_value", parse = TRUE)
  
  return(Env_plots)
  
})

myplots_FQ <- lapply(FQ_names, function(q){
  FQ_plots <- ggplot(Nat_alt_summer, aes_string(x = "Flow.regim", y = q)) +
    geom_boxplot(width = 0.45, fill = "white", notch = T) +
    geom_jitter(aes(color = Flow.regim, shape = Flow.regim), width = 0.1, size = 0.5) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) + theme(legend.position = "none") + labs(x = NULL) 
  #+ annotate("text", x = 4, y = 25, label = "Test_p_value", parse = TRUE)
  
  return(FQ_plots)
  
})


p.value.list <- lapply(Hyd_names, function(resp) { 
  mF <- formula(paste(resp, " ~ as.factor(Flow.regim)"))
  Test <- wilcox.test(mF, data=Nat_alt_summer)
  return(Test$p.value)
})

class(p.value.list)

modelList <- cbind(Hyd_names,p.value.list)

write.csv2(modelList, "p_values_Hyd2.csv", row.names = F)


multi.page <- ggarrange(plotlist = myplots,
                        nrow = 3, ncol = 4)

ggexport(multi.page, filename = "Graphs_Hyd_614.pdf")

multi.page_env <- ggarrange(plotlist = myplots_env,
                      
                        nrow = 3, ncol = 4)

ggexport(multi.page_env, filename = "Graphs_Env.pdf")

multi.page_FQ <- ggarrange(plotlist = myplots_FQ,
                            nrow = 3, ncol = 4)

ggexport(multi.page_FQ, filename = "Graphs_FQ.pdf")



# Classes hidrológicas ----------------------------------------------------

##Classe 10

Nat_alt_summer_10 <- Nat_alt_summer %>% filter(Lev_20 == "10")
myplots_10 <- lapply(Hyd_names, function(q){

  Hyd_plots_10 <- ggplot(Nat_alt_summer_10, aes_string(x = "Flow.regim", y = q)) +
    geom_boxplot(width = 0.45, fill = "white", notch = T) +
    geom_jitter(aes(color = Flow.regim, shape = Flow.regim), width = 0.1, size = 0.5) +
    scale_color_manual(values = c("tomato2", "palegreen3")) + theme(legend.position = "none") + labs(x = NULL) 

  return(Hyd_plots_10)
})

multi.page <- ggarrange(plotlist = myplots_10,
                        nrow = 3, ncol = 4)

ggexport(multi.page, filename = "Graphs_Hyd_Class10.pdf")


p.value.list <- lapply(Hyd_names, function(resp) { 
  mF <- formula(paste(resp, " ~ as.factor(Flow.regim)"))
  Test <- wilcox.test(mF, data=Nat_alt_summer_10)
  return(Test$p.value)
})
class(p.value.list)
modelList <- cbind(Hyd_names,p.value.list)

write.csv2(modelList, "p_values_Class10.csv", row.names = F)






Nat_alt_summer_13 <- Nat_alt_summer %>%
    filter(Lev_20 == "13")


  myplots_13 <- lapply(Hyd_names, function(q){
  Hyd_plots_13 <- ggplot(Nat_alt_summer_13, aes_string(x = "Flow.regim", y = q)) +
    geom_boxplot(width = 0.45, fill = "white", notch = T) +
    geom_jitter(aes(color = Flow.regim, shape = Flow.regim), width = 0.1, size = 0.5) +
    scale_color_manual(values = c("tomato2", "palegreen3")) + theme(legend.position = "none") + labs(x = NULL) 
  
  return(Hyd_plots_13)
  
})

  multi.page <- ggarrange(plotlist = myplots_13,
                          nrow = 3, ncol = 4)
  
  ggexport(multi.page, filename = "Graphs_Hyd_Class13.pdf")
  
  
  p.value.list <- lapply(Hyd_names, function(resp) { 
    mF <- formula(paste(resp, " ~ as.factor(Flow.regim)"))
    Test <- wilcox.test(mF, data=Nat_alt_summer_13)
    return(Test$p.value)
  })
  class(p.value.list)
  modelList <- cbind(Hyd_names,p.value.list)
  
  write.csv2(modelList, "p_values_Class13.csv", row.names = F)
  


  Nat_alt_summer_3 <- Nat_alt_summer %>% filter(Lev_20 == "3")
  myplots_3 <- lapply(Hyd_names, function(q){
    
    Hyd_plots_3 <- ggplot(Nat_alt_summer_3, aes_string(x = "Flow.regim", y = q)) +
      geom_boxplot(width = 0.45, fill = "white", notch = T) +
      geom_jitter(aes(color = Flow.regim, shape = Flow.regim), width = 0.1, size = 0.5) +
      scale_color_manual(values = c("tomato2", "palegreen3")) + theme(legend.position = "none") + labs(x = NULL) 
    
    return(Hyd_plots_3)
  })
  
  multi.page <- ggarrange(plotlist = myplots_3,
                          nrow = 3, ncol = 4)
  
  ggexport(multi.page, filename = "Graphs_Hyd_Class3.pdf")
  
  
  p.value.list <- lapply(Hyd_names, function(resp) { 
    mF <- formula(paste(resp, " ~ as.factor(Flow.regim)"))
    Test <- wilcox.test(mF, data=Nat_alt_summer_3)
    return(Test$p.value)
  })
  class(p.value.list)
  modelList <- cbind(Hyd_names,p.value.list)
  
  write.csv2(modelList, "p_values_Class3.csv", row.names = F)
  
  
  
  Nat_alt_summer_8 <- Nat_alt_summer %>% filter(Lev_20 == "8")
  myplots_8 <- lapply(Hyd_names, function(q){
    
    Hyd_plots_8 <- ggplot(Nat_alt_summer_8, aes_string(x = "Flow.regim", y = q)) +
      geom_boxplot(width = 0.45, fill = "white", notch = T) +
      geom_jitter(aes(color = Flow.regim, shape = Flow.regim), width = 0.1, size = 0.5) +
      scale_color_manual(values = c("tomato2", "palegreen3")) + theme(legend.position = "none") + labs(x = NULL) 
    
    return(Hyd_plots_8)
  })
  
  multi.page <- ggarrange(plotlist = myplots_8,
                          nrow = 3, ncol = 4)
  
  ggexport(multi.page, filename = "Graphs_Hyd_Class8.pdf")
  
  
  p.value.list <- lapply(Hyd_names, function(resp) { 
    mF <- formula(paste(resp, " ~ as.factor(Flow.regim)"))
    Test <- wilcox.test(mF, data=Nat_alt_summer_8)
    return(Test$p.value)
  })
  class(p.value.list)
  modelList <- cbind(Hyd_names,p.value.list)
  
  write.csv2(modelList, "p_values_Class8.csv", row.names = F)
  
  
# Gráficos 1x1 ------------------------------------------------------------


###Estes funcionan bien:


MeanJan <- ggplot(Nat_alt_summer, aes(x = factor(Flow.regim), y = MeanJan)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = Flow.regim, shape = Flow.regim), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + labs(x = NULL) + theme(legend.position = "none")  # Remove x axis label

MeanFeb <- ggplot(Nat_alt_summer, aes(x = factor(Flow.regim), y = MeanFeb)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = Flow.regim, shape = Flow.regim), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + labs(x = NULL) + theme(legend.position = "none")  # Remove x axis label

MeanMar <- ggplot(Nat_alt_summer, aes(x = factor(Flow.regim), y = MeanMar)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = Flow.regim, shape = Flow.regim), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + labs(x = NULL) + theme(legend.position = "none")   # Remove x axis label

MeanApr <- ggplot(Nat_alt_summer, aes(x = factor(Flow.regim), y = MeanApr)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = Flow.regim, shape = Flow.regim), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + labs(x = NULL) + theme(legend.position = "none")   # Remove x axis label

MeanMay <- ggplot(Nat_alt_summer, aes(x = factor(Flow.regim), y = MeanMay)) +
  geom_boxplot(width = 0.4, fill = "white") +
  geom_jitter(aes(color = Flow.regim, shape = Flow.regim), 
              width = 0.1, size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + labs(x = NULL) + theme(legend.position = "none")   # Remove x axis label

MeanJun <- ggplot(Nat_alt_summer, aes(x = factor(Flow.regim), y = MeanJun)) +
  geom_boxplot(width = 0.4, fill = "white", show.legend = FALSE) +
  geom_jitter(aes(color = Flow.regim, shape = Flow.regim), 
              width = 0.1, size = 1) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + labs(x = NULL)+ theme(legend.position = "none") # Remove x axis label

MeanJul <- ggplot(Nat_alt_summer, aes(x = factor(Flow.regim), y = MeanJul)) +
  geom_boxplot(width = 0.4, fill = "white", show.legend = FALSE) +
  geom_jitter(aes(color = Flow.regim, shape = Flow.regim), 
              width = 0.1, size = 1) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + labs(x = NULL)+ theme(legend.position = "none") # Remove x axis label

MeanAug <- ggplot(Nat_alt_summer, aes(x = factor(Flow.regim), y = MeanAug)) +
  geom_boxplot(width = 0.4, fill = "white", show.legend = FALSE) +
  geom_jitter(aes(color = Flow.regim, shape = Flow.regim), 
              width = 0.1, size = 1) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + labs(x = NULL)+ theme(legend.position = "none") # Remove x axis label

MeanSep <- ggplot(Nat_alt_summer, aes(x = factor(Flow.regim), y = MeanSep)) +
  geom_boxplot(width = 0.4, fill = "white", show.legend = FALSE) +
  geom_jitter(aes(color = Flow.regim, shape = Flow.regim), 
              width = 0.1, size = 1) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + labs(x = NULL)+ theme(legend.position = "none") 


library("cowplot")
  Hyd_1_plot <- plot_grid(MeanJan, MeanFeb, MeanMar, MeanApr, MeanMay, MeanJun, MeanJul, MeanAug, MeanSep,
            ncol = 3, nrow = 3)
  save_plot("Hyd1.png", Hyd_1_plot, base_width = 5)

  
  
  #for (i in seq_along(Hyd1)) { 
  print(i)
  myfile <- file.path(paste("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Coord/Graphs/", Hyd1[[i]],".png",sep=""))
  print(myfile)
  print(
    ggplot(Nat_alt_summer, aes_string(x = "Flow.regim", y = Hyd[i])) +
      geom_boxplot(width = 0.3, fill = "white") +
      geom_jitter(aes(color = Flow.regim, shape = Flow.regim), width = 0.1, size = 1.5) +
      scale_color_manual(values = c("#00AFBB", "#E7B800")) + theme(legend.position = "none") + ggsave(myfile)
  )
  #plot(value)}
  
  
  
  
  Test_MeanJan <- wilcox.test(MeanJan ~ as.factor(Flow.regim), data=Nat_alt_summer)
  wilcox.test(JulianMin ~ Flow.regim, data=Nat_alt_summer2)
  wilcox.test(MeanMar ~ as.factor(Flow.regim), data=Nat_alt_summer)
  wilcox.test( ~ as.factor(Flow.regim), data=Nat_alt_summer)

  
  

# Biotic indices ----------------------------------------------------------
library(biomonitoR)
library(biotic)
library(BBI)
require(data.table)
  
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Coord")
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/")
  Nat_alt_summer <- read.csv2("Nat_alt_summer_614.csv")
  
  #Nat_alt_summer <- Nat_alt_summer[!rownames(Nat_alt_summer) %in% c("GAL_M_010", "GAL_M_011", "GAL_M_012", "GAL_M_013"), ]
 
  Taxa <- select(Nat_alt_summer, ID, Hydrobiidae:Plumatellidae)
  setnames(Taxa, "Ancylidae", "Ancylus")
  Taxa_transp <- dcast(melt(Taxa, id.vars = "ID"), variable ~ ID) ##Ítranspose to use the packages
  setnames(Taxa_transp, "variable", "Taxa")
  
  #Indices <- select(Nat_alt_summer, EPT:IASPT)
  #Bio_indices <- names(Indices)
  #Indices_p_value <- c('MeanJan',	'MeanFeb',	'MeanMar',	'MeanApr',	'MeanJun',	'MeanJul',	'MeanAug',	'MeanSep',	'MeanNov',	'MeanDec',	'Mean1DayFlowMaxs',	'Mean3DayFlowMaxs',	'Mean7DayFlowMaxs', 'Mean90DayFlowMins',	'ZeroFlowDays',	'JulianMin',	'JulianMax',	'nPulsesLow',	'MeanPulseLengthLow',	'nPos',	'nNeg',	'meanNeg',	'FRE3',	'FRE7',	'Reversals',	'StDevMeanMay',	'StDevMeanJun',	'StDevMeanJul',	'StDevMeanAug',	'StDevMeanSep',	'StDevMeanOct',	'StDevMeanNov',	'StDevMean1DayFlowMins',	'StDevMean3DayFlowMins',	'StDevMean3DayFlowMaxs',	'StDevMean7DayFlowMins',	'StDevMean7DayFlowMaxs',	'StDevMean30DayFlowMins',	'StDevMean30DayFlowMaxs',	'StDevMean90DayFlowMins',	'StDevMean90DayFlowMaxs',	'StDevZeroFlowDays',	'StDevBFI',	'StDevJulianMin',	'StDevJulianMax',	'StDevnPulsesLow',	'StDevnPulsesHigh',	'StDevMeanPulseLengthHigh',	'StDevnPos',	'StDevnNeg',	'StDevFRE1',	'StDevFRE3',	'StDevFRE7',	'StDevReversals',	'Predictability',	'X5per')


# Calculate indices -------------------------------------------------------

    
library(biotic)
  
  Index <- calcindex(Taxa_transp)
  Index$Life_biotic <- calcindex(Taxa_transp, "LIFE", "num")

  library("biomonitoR")
  
  data(macro_ex)
  
  # Prepare data for the analysis.

  data.bio <- asBiomonitor(Taxa_transp)
  
  data.agR <- aggregatoR(data.bio)
  
  richness(data.agR, "Genus")
  richness(data.agR, "Family")
  Index$Richness <- allrich(data.agR)


Index$Shannon <- biomonitoR::shannon(data.agR)
Index$Simpson <- simpson(data.agR)
Index$Margalef <- margalef(data.agR)
Index$Menhinick <- menhinick(data.agR)
Index$Life2 <- life(data.agR)
 
# calculate iberian bmwp and aspt
Index$IBMWP <- bmwp(data.agR, method="spa")
Index$IASPT <- aspt(data.agR, method="spa")
Index$BMWP <- bmwp(data.agR)
Index$Div_Shannon <- diversity(Taxa, index = "shannon")

write.csv2(Index, "indices_614.csv", dec = "," )



# plot indices bio --------------------------------------------------------


#FQ
plots_indices_bio <- lapply(FQ_names, function(q){
  Bio_plots <- ggplot(Nat_alt_summer, aes_string(x = q, y = "EPT", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_EPT <- ggarrange(plotlist = plots_indices_bio,
                            nrow = 2, ncol =2)
ggexport(multi.page_EPT, filename = "Graphs_EPT_FQ.pdf")

plots_indices_bio <- lapply(Indices_p_value, function(q){
  Bio_plots <- ggplot(Nat_alt_summer, aes_string(x = q, y = "EPT", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
   theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
  })

multi.page_EPT <- ggarrange(plotlist = plots_indices_bio,
                        nrow = 2, ncol =2)
ggexport(multi.page_EPT, filename = "Graphs_EPT.pdf")

#----
  
plots_indices_bio <- lapply(Indices_p_value, function(q){
  Bio_plots <- ggplot(Nat_alt_summer, aes_string(x = q, y = "OCH", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("brown2", "seagreen")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_OCH <- ggarrange(plotlist = plots_indices_bio,
                            nrow = 2, ncol =2)
ggexport(multi.page_OCH, filename = "Graphs_OCH.pdf")


#----
  
plots_indices_bio <- lapply(Indices_p_value, function(q){
  Bio_plots <- ggplot(Nat_alt_summer, aes_string(x = q, y = "Diptera", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("brown2", "seagreen")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_Diptera <- ggarrange(plotlist = plots_indices_bio,
                            nrow = 2, ncol =2)
ggexport(multi.page_OCH, filename = "Graphs_Diptera.pdf")

#----
  
plots_indices_bio <- lapply(Indices_p_value, function(q){
  Bio_plots <- ggplot(Nat_alt_summer, aes_string(x = q, y = "EPT_OCH", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("brown2", "seagreen")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_EPT_OCH <- ggarrange(plotlist = plots_indices_bio,
                                nrow = 2, ncol =2)
ggexport(multi.page_EPT_OCH, filename = "Graphs_EPT_OCH.pdf")

#----

plots_indices_bio <- lapply(Indices_p_value, function(q){
  Bio_plots <- ggplot(Nat_alt_summer, aes_string(x = q, y = "Non.Insect", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("brown2", "seagreen")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_Non_insect <- ggarrange(plotlist = plots_indices_bio,
                                nrow = 2, ncol =2)
ggexport(multi.page_Non_insect, filename = "Graphs_Non_insect.pdf")

#----
  
  plots_indices_bio <- lapply(Indices_p_value, function(q){
    Bio_plots <- ggscatter(Nat_alt_summer, x = q, y = "Insect", color = "Flow.regim", palette = "nrc",
                           add = "reg.line", conf.int = T) + stat_cor(aes(color = Flow.regim),  label.x = 2.5, label.y.npc = "bottom") + theme_bw()+ theme(legend.position = "none") 
    return(Bio_plots)
    
  })

multi.page_Insect <- ggarrange(plotlist = plots_indices_bio,
                                   nrow = 1, ncol =2)
ggexport(multi.page_Insect, filename = "Graphs_Insect.pdf") 

#----


plots_indices_bio <- lapply(Bio_indices, function(q){
  Bio_plots <- ggplot(Nat_alt_summer, aes_string(x = "MeanJul", y = q, color="Flow.regim")) + geom_point(shape=20, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("brown2", "seagreen")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_MeanJul <- ggarrange(plotlist = plots_indices_bio,
                            nrow = 2, ncol =2)
ggexport(multi.page_MeanJul, filename = "Graphs_MeanJul.pdf")


plots_indices_bio <- lapply(Bio_indices, function(q){
  Bio_plots <- ggplot(Nat_alt_summer, aes_string(x = "nPos", y = q, color="Flow.regim")) + geom_point(shape=20, size=0.8)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("brown1", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_nPos <- ggarrange(plotlist = plots_indices_bio,
                                nrow = 2, ncol =2)
ggexport(multi.page_nPos, filename = "Graphs_nPos.pdf")


plots_indices_bio <- lapply(Bio_indices, function(q){
  Bio_plots <- ggplot(Nat_alt_summer, aes_string(x = "Mean90DayFlowMins", y = q, color="Flow.regim")) + geom_point(shape=20, size=0.8)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("brown1", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_Mean90Min <- ggarrange(plotlist = plots_indices_bio,
                             nrow = 2, ncol =2)
ggexport(multi.page_Mean90Min, filename = "Graphs_Mean90Min.pdf")















# Plot Bio_Class ----------------------------------------------------------



##### EPT #####
##Class 3
Indices_p_value_3 <- c('StDevZeroFlowDays',	'ZeroFlowDays',	'nPos',	'nNeg',	'MeanApr',	'JulianMax',	'StDevMean7DayFlowMaxs',	'StDevnPos',	'MeanJul',	'StDevMean30DayFlowMaxs',	'Mean90DayFlowMaxs',	'StDevnNeg',	'StDevJulianMax',	'MeanAug',	'StDevMeanSep',	'StDevMean3DayFlowMaxs',	'MeanSep',	'StDevMeanAug',	'l2',	'MeanMar',	'StDevMeanJul',	'Mean30DayFlowMaxs',	'MeanMay',	'X75per',	'MeanDec',	'lca',	'StDevnPulsesHigh',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'Mean7DayFlowMins',	'StDevMean90DayFlowMaxs',	'Mean3DayFlowMins',	'Mean7DayFlowMaxs',	'X95per',	'nPulsesHigh',	'Mean30DayFlowMins',	'StDevReversals')
plots_indices_bio <- lapply(Indices_p_value_3, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_3, aes_string(x = q, y = "EPT", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_EPT <- ggarrange(plotlist = plots_indices_bio,
                            nrow = 2, ncol =2)
ggexport(multi.page_EPT, filename = "Graphs_EPT_Class3.pdf")

##Class 8

Indices_p_value_8 <- c('StDevnPulsesHigh',	'nPos',	'StDevMeanFeb',	'StDevReversals',	'StDevnPulsesLow',	'ZeroFlowDays',	'StDevZeroFlowDays',	'StDevMean7DayFlowMaxs',	'StDevMean3DayFlowMaxs',	'Predictability',	'MeanMay',	'StDevMeanSep',	'lkur',	'nPulsesLow',	'meanNeg',	'StDevMeanAug',	'MeanApr',	'StDevMean30DayFlowMaxs',	'Reversals',	'FRE3',	'StDevBFI',	'StDevnNeg',	'StDevMeanJul',	'StDevnPos',	'StDevJulianMin',	'MeanMar',	'nPulsesHigh',	'nNeg',	'StDevMeanMar',	'StDevMeanJun',	'StDevMean1DayFlowMaxs',	'X95per',	'X25per',	'StDevMean90DayFlowMaxs',	'FRE7',	'Mean1DayFlowMins',	'StDevJulianMax',	'StDevMean90DayFlowMins',	'StDevFRE1',	'JulianMin',	'Mean90DayFlowMaxs',	'MeanSep',	'Mean7DayFlowMaxs',	'Mean30DayFlowMaxs')
plots_indices_bio <- lapply(Indices_p_value_8, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_8, aes_string(x = q, y = "EPT", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_EPT <- ggarrange(plotlist = plots_indices_bio,
                            nrow = 2, ncol =2)
ggexport(multi.page_EPT, filename = "Graphs_EPT_Class8.pdf")


##Class 10

Indices_p_value_10 <- c('JulianMin',	'MeanSep',	'MeanAug',	'nPos',	'StDevnPulsesLow',	'Mean90DayFlowMins',	'StDevJulianMin',	'Mean90DayFlowMaxs',	'l2',	'FRE7',	'nNeg',	'Mean3DayFlowMaxs',	'MeanJun',	'nPulsesLow',	'Mean1DayFlowMaxs',	'MeanJul',	'Mean30DayFlowMins',	'Mean30DayFlowMaxs',	'BFI',	'MeanJan',	'X75per',	'Reversals',	'Mean7DayFlowMaxs',	'Mean7DayFlowMins',	'StDevBFI',	'Mean3DayFlowMins',	'StDevMeanNov',	'StDevMeanAug',	'X95per',	'StDevMean30DayFlowMins',	'X5per',	'StDevMean90DayFlowMins',	'StDevnPos',	'Mean1DayFlowMins',	'StDevnNeg',	'lca',	'StDevMeanSep',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean3DayFlowMins',	'MeanDec',	'MeanFeb',	'StDevFRE1',	'StDevMean1DayFlowMins',	'StDevMean1DayFlowMaxs',	'MeanMar',	'MeanPulseLengthLow',	'StDevReversals',	'StDevFRE7',	'StDevMeanDec',	'StDevnPulsesHigh',	'FRE3',	'StDevMean3DayFlowMaxs',	'meanPos',	'MeanMay')
plots_indices_bio <- lapply(Indices_p_value_10, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_10, aes_string(x = q, y = "EPT", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_EPT <- ggarrange(plotlist = plots_indices_bio,
                            nrow = 2, ncol =2)
ggexport(multi.page_EPT, filename = "Graphs_EPT_Class10.pdf")

##Class 13

Indices_p_value_13 <- c('Mean90DayFlowMins',	'nNeg',	'nPos',	'Mean1DayFlowMaxs',	'StDevMean30DayFlowMins',	'StDevJulianMin',	'Mean7DayFlowMaxs',	'MeanJul',	'Mean3DayFlowMaxs',	'MeanSep',	'StDevnPulsesLow',	'Reversals',	'nPulsesLow',	'MeanAug',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean90DayFlowMins',	'X5per',	'StDevBFI',	'l2',	'MeanDec',	'Mean30DayFlowMins',	'StDevMeanNov',	'X75per',	'MeanJun',	'Mean30DayFlowMaxs',	'FRE7',	'MeanJan',	'StDevFRE7',	'StDevMean3DayFlowMins',	'Mean90DayFlowMaxs',	'StDevMeanDec',	'X95per',	'StDevMeanApr',	'FRE3',	'StDevMean1DayFlowMins',	'Mean7DayFlowMins',	'BFI',	'MeanFeb',	'lca',	'StDevnNeg',	'Mean3DayFlowMins',	'MeanMar',	'MeanPulseLengthLow',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'StDevFRE1',	'StDevMeanJul',	'MeanMay',	'StDevMeanJan',	'StDevJulianMax',	'lkur',	'JulianMin',	'StDevMean3DayFlowMaxs',	'StDevMeanJun',	'StDevMeanSep',	'StDevMean7DayFlowMaxs',	'StDevMeanFeb',	'StDevMeanMar',	'MeanApr')
plots_indices_bio <- lapply(Indices_p_value_13, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_13, aes_string(x = q, y = "EPT", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_EPT <- ggarrange(plotlist = plots_indices_bio,
                            nrow = 2, ncol =2)
ggexport(multi.page_EPT, filename = "Graphs_EPT_Class13.pdf")



##### OCH #####
##Class 3
Indices_p_value_3 <- c('StDevZeroFlowDays',	'ZeroFlowDays',	'nPos',	'nNeg',	'MeanApr',	'JulianMax',	'StDevMean7DayFlowMaxs',	'StDevnPos',	'MeanJul',	'StDevMean30DayFlowMaxs',	'Mean90DayFlowMaxs',	'StDevnNeg',	'StDevJulianMax',	'MeanAug',	'StDevMeanSep',	'StDevMean3DayFlowMaxs',	'MeanSep',	'StDevMeanAug',	'l2',	'MeanMar',	'StDevMeanJul',	'Mean30DayFlowMaxs',	'MeanMay',	'X75per',	'MeanDec',	'lca',	'StDevnPulsesHigh',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'Mean7DayFlowMins',	'StDevMean90DayFlowMaxs',	'Mean3DayFlowMins',	'Mean7DayFlowMaxs',	'X95per',	'nPulsesHigh',	'Mean30DayFlowMins',	'StDevReversals')
plots_indices_bio <- lapply(Indices_p_value_3, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_3, aes_string(x = q, y = "OCH", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_OCH <- ggarrange(plotlist = plots_indices_bio,
                            nrow = 2, ncol =2)
ggexport(multi.page_OCH, filename = "Graphs_OCH_Class3.pdf")

##Class 8

Indices_p_value_8 <- c('StDevnPulsesHigh',	'nPos',	'StDevMeanFeb',	'StDevReversals',	'StDevnPulsesLow',	'ZeroFlowDays',	'StDevZeroFlowDays',	'StDevMean7DayFlowMaxs',	'StDevMean3DayFlowMaxs',	'Predictability',	'MeanMay',	'StDevMeanSep',	'lkur',	'nPulsesLow',	'meanNeg',	'StDevMeanAug',	'MeanApr',	'StDevMean30DayFlowMaxs',	'Reversals',	'FRE3',	'StDevBFI',	'StDevnNeg',	'StDevMeanJul',	'StDevnPos',	'StDevJulianMin',	'MeanMar',	'nPulsesHigh',	'nNeg',	'StDevMeanMar',	'StDevMeanJun',	'StDevMean1DayFlowMaxs',	'X95per',	'X25per',	'StDevMean90DayFlowMaxs',	'FRE7',	'Mean1DayFlowMins',	'StDevJulianMax',	'StDevMean90DayFlowMins',	'StDevFRE1',	'JulianMin',	'Mean90DayFlowMaxs',	'MeanSep',	'Mean7DayFlowMaxs',	'Mean30DayFlowMaxs')
plots_indices_bio <- lapply(Indices_p_value_8, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_8, aes_string(x = q, y = "OCH", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_OCH <- ggarrange(plotlist = plots_indices_bio,
                            nrow = 2, ncol =2)
ggexport(multi.page_OCH, filename = "Graphs_OCH_Class8.pdf")


##Class 10

Indices_p_value_10 <- c('JulianMin',	'MeanSep',	'MeanAug',	'nPos',	'StDevnPulsesLow',	'Mean90DayFlowMins',	'StDevJulianMin',	'Mean90DayFlowMaxs',	'l2',	'FRE7',	'nNeg',	'Mean3DayFlowMaxs',	'MeanJun',	'nPulsesLow',	'Mean1DayFlowMaxs',	'MeanJul',	'Mean30DayFlowMins',	'Mean30DayFlowMaxs',	'BFI',	'MeanJan',	'X75per',	'Reversals',	'Mean7DayFlowMaxs',	'Mean7DayFlowMins',	'StDevBFI',	'Mean3DayFlowMins',	'StDevMeanNov',	'StDevMeanAug',	'X95per',	'StDevMean30DayFlowMins',	'X5per',	'StDevMean90DayFlowMins',	'StDevnPos',	'Mean1DayFlowMins',	'StDevnNeg',	'lca',	'StDevMeanSep',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean3DayFlowMins',	'MeanDec',	'MeanFeb',	'StDevFRE1',	'StDevMean1DayFlowMins',	'StDevMean1DayFlowMaxs',	'MeanMar',	'MeanPulseLengthLow',	'StDevReversals',	'StDevFRE7',	'StDevMeanDec',	'StDevnPulsesHigh',	'FRE3',	'StDevMean3DayFlowMaxs',	'meanPos',	'MeanMay')
plots_indices_bio <- lapply(Indices_p_value_10, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_10, aes_string(x = q, y = "OCH", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_OCH <- ggarrange(plotlist = plots_indices_bio,
                            nrow = 2, ncol =2)
ggexport(multi.page_OCH, filename = "Graphs_OCH_Class10.pdf")

##Class 13

Indices_p_value_13 <- c('Mean90DayFlowMins',	'nNeg',	'nPos',	'Mean1DayFlowMaxs',	'StDevMean30DayFlowMins',	'StDevJulianMin',	'Mean7DayFlowMaxs',	'MeanJul',	'Mean3DayFlowMaxs',	'MeanSep',	'StDevnPulsesLow',	'Reversals',	'nPulsesLow',	'MeanAug',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean90DayFlowMins',	'X5per',	'StDevBFI',	'l2',	'MeanDec',	'Mean30DayFlowMins',	'StDevMeanNov',	'X75per',	'MeanJun',	'Mean30DayFlowMaxs',	'FRE7',	'MeanJan',	'StDevFRE7',	'StDevMean3DayFlowMins',	'Mean90DayFlowMaxs',	'StDevMeanDec',	'X95per',	'StDevMeanApr',	'FRE3',	'StDevMean1DayFlowMins',	'Mean7DayFlowMins',	'BFI',	'MeanFeb',	'lca',	'StDevnNeg',	'Mean3DayFlowMins',	'MeanMar',	'MeanPulseLengthLow',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'StDevFRE1',	'StDevMeanJul',	'MeanMay',	'StDevMeanJan',	'StDevJulianMax',	'lkur',	'JulianMin',	'StDevMean3DayFlowMaxs',	'StDevMeanJun',	'StDevMeanSep',	'StDevMean7DayFlowMaxs',	'StDevMeanFeb',	'StDevMeanMar',	'MeanApr')
plots_indices_bio <- lapply(Indices_p_value_13, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_13, aes_string(x = q, y = "OCH", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_OCH <- ggarrange(plotlist = plots_indices_bio,
                            nrow = 2, ncol =2)
ggexport(multi.page_OCH, filename = "Graphs_OCH_Class13.pdf")


##### IASPT #####
##Class 3
Indices_p_value_3 <- c('StDevZeroFlowDays',	'ZeroFlowDays',	'nPos',	'nNeg',	'MeanApr',	'JulianMax',	'StDevMean7DayFlowMaxs',	'StDevnPos',	'MeanJul',	'StDevMean30DayFlowMaxs',	'Mean90DayFlowMaxs',	'StDevnNeg',	'StDevJulianMax',	'MeanAug',	'StDevMeanSep',	'StDevMean3DayFlowMaxs',	'MeanSep',	'StDevMeanAug',	'l2',	'MeanMar',	'StDevMeanJul',	'Mean30DayFlowMaxs',	'MeanMay',	'X75per',	'MeanDec',	'lca',	'StDevnPulsesHigh',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'Mean7DayFlowMins',	'StDevMean90DayFlowMaxs',	'Mean3DayFlowMins',	'Mean7DayFlowMaxs',	'X95per',	'nPulsesHigh',	'Mean30DayFlowMins',	'StDevReversals')
plots_indices_bio <- lapply(Indices_p_value_3, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_3, aes_string(x = q, y = "IASPT", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3"))
  return(Bio_plots)
})

multi.page_IASPT <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_IASPT, filename = "Graphs_IASPT_Class3.pdf")

##Class 8

Indices_p_value_8 <- c('StDevnPulsesHigh',	'nPos',	'StDevMeanFeb',	'StDevReversals',	'StDevnPulsesLow',	'ZeroFlowDays',	'StDevZeroFlowDays',	'StDevMean7DayFlowMaxs',	'StDevMean3DayFlowMaxs',	'Predictability',	'MeanMay',	'StDevMeanSep',	'lkur',	'nPulsesLow',	'meanNeg',	'StDevMeanAug',	'MeanApr',	'StDevMean30DayFlowMaxs',	'Reversals',	'FRE3',	'StDevBFI',	'StDevnNeg',	'StDevMeanJul',	'StDevnPos',	'StDevJulianMin',	'MeanMar',	'nPulsesHigh',	'nNeg',	'StDevMeanMar',	'StDevMeanJun',	'StDevMean1DayFlowMaxs',	'X95per',	'X25per',	'StDevMean90DayFlowMaxs',	'FRE7',	'Mean1DayFlowMins',	'StDevJulianMax',	'StDevMean90DayFlowMins',	'StDevFRE1',	'JulianMin',	'Mean90DayFlowMaxs',	'MeanSep',	'Mean7DayFlowMaxs',	'Mean30DayFlowMaxs')
plots_indices_bio <- lapply(Indices_p_value_8, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_8, aes_string(x = q, y = "IASPT", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3"))
  return(Bio_plots)
})

multi.page_IASPT <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_IASPT, filename = "Graphs_IASPT_Class8.pdf")


##Class 10

Indices_p_value_10 <- c('JulianMin',	'MeanSep',	'MeanAug',	'nPos',	'StDevnPulsesLow',	'Mean90DayFlowMins',	'StDevJulianMin',	'Mean90DayFlowMaxs',	'l2',	'FRE7',	'nNeg',	'Mean3DayFlowMaxs',	'MeanJun',	'nPulsesLow',	'Mean1DayFlowMaxs',	'MeanJul',	'Mean30DayFlowMins',	'Mean30DayFlowMaxs',	'BFI',	'MeanJan',	'X75per',	'Reversals',	'Mean7DayFlowMaxs',	'Mean7DayFlowMins',	'StDevBFI',	'Mean3DayFlowMins',	'StDevMeanNov',	'StDevMeanAug',	'X95per',	'StDevMean30DayFlowMins',	'X5per',	'StDevMean90DayFlowMins',	'StDevnPos',	'Mean1DayFlowMins',	'StDevnNeg',	'lca',	'StDevMeanSep',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean3DayFlowMins',	'MeanDec',	'MeanFeb',	'StDevFRE1',	'StDevMean1DayFlowMins',	'StDevMean1DayFlowMaxs',	'MeanMar',	'MeanPulseLengthLow',	'StDevReversals',	'StDevFRE7',	'StDevMeanDec',	'StDevnPulsesHigh',	'FRE3',	'StDevMean3DayFlowMaxs',	'meanPos',	'MeanMay')
plots_indices_bio <- lapply(Indices_p_value_10, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_10, aes_string(x = q, y = "IASPT", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) 
  return(Bio_plots)
})

multi.page_IASPT <- ggarrange(plotlist = plots_indices_bio,
                            nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_IASPT, filename = "Graphs_IASPT_Class10.pdf")

##Class 13

Indices_p_value_13 <- c('Mean90DayFlowMins',	'nNeg',	'nPos',	'Mean1DayFlowMaxs',	'StDevMean30DayFlowMins',	'StDevJulianMin',	'Mean7DayFlowMaxs',	'MeanJul',	'Mean3DayFlowMaxs',	'MeanSep',	'StDevnPulsesLow',	'Reversals',	'nPulsesLow',	'MeanAug',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean90DayFlowMins',	'X5per',	'StDevBFI',	'l2',	'MeanDec',	'Mean30DayFlowMins',	'StDevMeanNov',	'X75per',	'MeanJun',	'Mean30DayFlowMaxs',	'FRE7',	'MeanJan',	'StDevFRE7',	'StDevMean3DayFlowMins',	'Mean90DayFlowMaxs',	'StDevMeanDec',	'X95per',	'StDevMeanApr',	'FRE3',	'StDevMean1DayFlowMins',	'Mean7DayFlowMins',	'BFI',	'MeanFeb',	'lca',	'StDevnNeg',	'Mean3DayFlowMins',	'MeanMar',	'MeanPulseLengthLow',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'StDevFRE1',	'StDevMeanJul',	'MeanMay',	'StDevMeanJan',	'StDevJulianMax',	'lkur',	'JulianMin',	'StDevMean3DayFlowMaxs',	'StDevMeanJun',	'StDevMeanSep',	'StDevMean7DayFlowMaxs',	'StDevMeanFeb',	'StDevMeanMar',	'MeanApr')
plots_indices_bio <- lapply(Indices_p_value_13, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_13, aes_string(x = q, y = "IASPT", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3"))
  return(Bio_plots)
})

multi.page_IASPT <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_IASPT, filename = "Graphs_IASPT_Class13.pdf")


##### IBMWP #####
##Class 3
Indices_p_value_3 <- c('StDevZeroFlowDays',	'ZeroFlowDays',	'nPos',	'nNeg',	'MeanApr',	'JulianMax',	'StDevMean7DayFlowMaxs',	'StDevnPos',	'MeanJul',	'StDevMean30DayFlowMaxs',	'Mean90DayFlowMaxs',	'StDevnNeg',	'StDevJulianMax',	'MeanAug',	'StDevMeanSep',	'StDevMean3DayFlowMaxs',	'MeanSep',	'StDevMeanAug',	'l2',	'MeanMar',	'StDevMeanJul',	'Mean30DayFlowMaxs',	'MeanMay',	'X75per',	'MeanDec',	'lca',	'StDevnPulsesHigh',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'Mean7DayFlowMins',	'StDevMean90DayFlowMaxs',	'Mean3DayFlowMins',	'Mean7DayFlowMaxs',	'X95per',	'nPulsesHigh',	'Mean30DayFlowMins',	'StDevReversals')
plots_indices_bio <- lapply(Indices_p_value_3, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_3, aes_string(x = q, y = "IBMWP", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) 
  return(Bio_plots)
})

multi.page_IBMWP <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_IBMWP, filename = "Graphs_IBMWP_Class3.pdf")

##Class 8

Indices_p_value_8 <- c('StDevnPulsesHigh',	'nPos',	'StDevMeanFeb',	'StDevReversals',	'StDevnPulsesLow',	'ZeroFlowDays',	'StDevZeroFlowDays',	'StDevMean7DayFlowMaxs',	'StDevMean3DayFlowMaxs',	'Predictability',	'MeanMay',	'StDevMeanSep',	'lkur',	'nPulsesLow',	'meanNeg',	'StDevMeanAug',	'MeanApr',	'StDevMean30DayFlowMaxs',	'Reversals',	'FRE3',	'StDevBFI',	'StDevnNeg',	'StDevMeanJul',	'StDevnPos',	'StDevJulianMin',	'MeanMar',	'nPulsesHigh',	'nNeg',	'StDevMeanMar',	'StDevMeanJun',	'StDevMean1DayFlowMaxs',	'X95per',	'X25per',	'StDevMean90DayFlowMaxs',	'FRE7',	'Mean1DayFlowMins',	'StDevJulianMax',	'StDevMean90DayFlowMins',	'StDevFRE1',	'JulianMin',	'Mean90DayFlowMaxs',	'MeanSep',	'Mean7DayFlowMaxs',	'Mean30DayFlowMaxs')
plots_indices_bio <- lapply(Indices_p_value_8, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_8, aes_string(x = q, y = "IBMWP", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_IBMWP <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_IBMWP, filename = "Graphs_IBMWP_Class8.pdf")


##Class 10

Indices_p_value_10 <- c('JulianMin',	'MeanSep',	'MeanAug',	'nPos',	'StDevnPulsesLow',	'Mean90DayFlowMins',	'StDevJulianMin',	'Mean90DayFlowMaxs',	'l2',	'FRE7',	'nNeg',	'Mean3DayFlowMaxs',	'MeanJun',	'nPulsesLow',	'Mean1DayFlowMaxs',	'MeanJul',	'Mean30DayFlowMins',	'Mean30DayFlowMaxs',	'BFI',	'MeanJan',	'X75per',	'Reversals',	'Mean7DayFlowMaxs',	'Mean7DayFlowMins',	'StDevBFI',	'Mean3DayFlowMins',	'StDevMeanNov',	'StDevMeanAug',	'X95per',	'StDevMean30DayFlowMins',	'X5per',	'StDevMean90DayFlowMins',	'StDevnPos',	'Mean1DayFlowMins',	'StDevnNeg',	'lca',	'StDevMeanSep',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean3DayFlowMins',	'MeanDec',	'MeanFeb',	'StDevFRE1',	'StDevMean1DayFlowMins',	'StDevMean1DayFlowMaxs',	'MeanMar',	'MeanPulseLengthLow',	'StDevReversals',	'StDevFRE7',	'StDevMeanDec',	'StDevnPulsesHigh',	'FRE3',	'StDevMean3DayFlowMaxs',	'meanPos',	'MeanMay')
plots_indices_bio <- lapply(Indices_p_value_10, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_10, aes_string(x = q, y = "IBMWP", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_IBMWP <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_IBMWP, filename = "Graphs_IBMWP_Class10.pdf")

##Class 13

Indices_p_value_13 <- c('Mean90DayFlowMins',	'nNeg',	'nPos',	'Mean1DayFlowMaxs',	'StDevMean30DayFlowMins',	'StDevJulianMin',	'Mean7DayFlowMaxs',	'MeanJul',	'Mean3DayFlowMaxs',	'MeanSep',	'StDevnPulsesLow',	'Reversals',	'nPulsesLow',	'MeanAug',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean90DayFlowMins',	'X5per',	'StDevBFI',	'l2',	'MeanDec',	'Mean30DayFlowMins',	'StDevMeanNov',	'X75per',	'MeanJun',	'Mean30DayFlowMaxs',	'FRE7',	'MeanJan',	'StDevFRE7',	'StDevMean3DayFlowMins',	'Mean90DayFlowMaxs',	'StDevMeanDec',	'X95per',	'StDevMeanApr',	'FRE3',	'StDevMean1DayFlowMins',	'Mean7DayFlowMins',	'BFI',	'MeanFeb',	'lca',	'StDevnNeg',	'Mean3DayFlowMins',	'MeanMar',	'MeanPulseLengthLow',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'StDevFRE1',	'StDevMeanJul',	'MeanMay',	'StDevMeanJan',	'StDevJulianMax',	'lkur',	'JulianMin',	'StDevMean3DayFlowMaxs',	'StDevMeanJun',	'StDevMeanSep',	'StDevMean7DayFlowMaxs',	'StDevMeanFeb',	'StDevMeanMar',	'MeanApr')
plots_indices_bio <- lapply(Indices_p_value_13, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_13, aes_string(x = q, y = "IBMWP", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_IBMWP <- ggarrange(plotlist = plots_indices_bio,nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_IBMWP, filename = "Graphs_IBMWP_Class13.pdf")

class(Nat_alt_summer_10$IBMWP)



##### LIFE #####
##Class 3
Indices_p_value_3 <- c('StDevZeroFlowDays',	'ZeroFlowDays',	'nPos',	'nNeg',	'MeanApr',	'JulianMax',	'StDevMean7DayFlowMaxs',	'StDevnPos',	'MeanJul',	'StDevMean30DayFlowMaxs',	'Mean90DayFlowMaxs',	'StDevnNeg',	'StDevJulianMax',	'MeanAug',	'StDevMeanSep',	'StDevMean3DayFlowMaxs',	'MeanSep',	'StDevMeanAug',	'l2',	'MeanMar',	'StDevMeanJul',	'Mean30DayFlowMaxs',	'MeanMay',	'X75per',	'MeanDec',	'lca',	'StDevnPulsesHigh',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'Mean7DayFlowMins',	'StDevMean90DayFlowMaxs',	'Mean3DayFlowMins',	'Mean7DayFlowMaxs',	'X95per',	'nPulsesHigh',	'Mean30DayFlowMins',	'StDevReversals')
plots_indices_bio <- lapply(Indices_p_value_3, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_3, aes_string(x = q, y = "LIFE", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) 
  return(Bio_plots)
})

multi.page_LIFE <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_LIFE, filename = "Graphs_LIFE_Class3.pdf")

##Class 8

Indices_p_value_8 <- c('StDevnPulsesHigh',	'nPos',	'StDevMeanFeb',	'StDevReversals',	'StDevnPulsesLow',	'ZeroFlowDays',	'StDevZeroFlowDays',	'StDevMean7DayFlowMaxs',	'StDevMean3DayFlowMaxs',	'Predictability',	'MeanMay',	'StDevMeanSep',	'lkur',	'nPulsesLow',	'meanNeg',	'StDevMeanAug',	'MeanApr',	'StDevMean30DayFlowMaxs',	'Reversals',	'FRE3',	'StDevBFI',	'StDevnNeg',	'StDevMeanJul',	'StDevnPos',	'StDevJulianMin',	'MeanMar',	'nPulsesHigh',	'nNeg',	'StDevMeanMar',	'StDevMeanJun',	'StDevMean1DayFlowMaxs',	'X95per',	'X25per',	'StDevMean90DayFlowMaxs',	'FRE7',	'Mean1DayFlowMins',	'StDevJulianMax',	'StDevMean90DayFlowMins',	'StDevFRE1',	'JulianMin',	'Mean90DayFlowMaxs',	'MeanSep',	'Mean7DayFlowMaxs',	'Mean30DayFlowMaxs')
plots_indices_bio <- lapply(Indices_p_value_8, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_8, aes_string(x = q, y = "LIFE", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_LIFE <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_LIFE, filename = "Graphs_LIFE_Class8.pdf")


##Class 10

Indices_p_value_10 <- c('JulianMin',	'MeanSep',	'MeanAug',	'nPos',	'StDevnPulsesLow',	'Mean90DayFlowMins',	'StDevJulianMin',	'Mean90DayFlowMaxs',	'l2',	'FRE7',	'nNeg',	'Mean3DayFlowMaxs',	'MeanJun',	'nPulsesLow',	'Mean1DayFlowMaxs',	'MeanJul',	'Mean30DayFlowMins',	'Mean30DayFlowMaxs',	'BFI',	'MeanJan',	'X75per',	'Reversals',	'Mean7DayFlowMaxs',	'Mean7DayFlowMins',	'StDevBFI',	'Mean3DayFlowMins',	'StDevMeanNov',	'StDevMeanAug',	'X95per',	'StDevMean30DayFlowMins',	'X5per',	'StDevMean90DayFlowMins',	'StDevnPos',	'Mean1DayFlowMins',	'StDevnNeg',	'lca',	'StDevMeanSep',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean3DayFlowMins',	'MeanDec',	'MeanFeb',	'StDevFRE1',	'StDevMean1DayFlowMins',	'StDevMean1DayFlowMaxs',	'MeanMar',	'MeanPulseLengthLow',	'StDevReversals',	'StDevFRE7',	'StDevMeanDec',	'StDevnPulsesHigh',	'FRE3',	'StDevMean3DayFlowMaxs',	'meanPos',	'MeanMay')
plots_indices_bio <- lapply(Indices_p_value_10, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_10, aes_string(x = q, y = "LIFE", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_LIFE <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_LIFE, filename = "Graphs_LIFE_Class10.pdf")

##Class 13

Indices_p_value_13 <- c('Mean90DayFlowMins',	'nNeg',	'nPos',	'Mean1DayFlowMaxs',	'StDevMean30DayFlowMins',	'StDevJulianMin',	'Mean7DayFlowMaxs',	'MeanJul',	'Mean3DayFlowMaxs',	'MeanSep',	'StDevnPulsesLow',	'Reversals',	'nPulsesLow',	'MeanAug',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean90DayFlowMins',	'X5per',	'StDevBFI',	'l2',	'MeanDec',	'Mean30DayFlowMins',	'StDevMeanNov',	'X75per',	'MeanJun',	'Mean30DayFlowMaxs',	'FRE7',	'MeanJan',	'StDevFRE7',	'StDevMean3DayFlowMins',	'Mean90DayFlowMaxs',	'StDevMeanDec',	'X95per',	'StDevMeanApr',	'FRE3',	'StDevMean1DayFlowMins',	'Mean7DayFlowMins',	'BFI',	'MeanFeb',	'lca',	'StDevnNeg',	'Mean3DayFlowMins',	'MeanMar',	'MeanPulseLengthLow',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'StDevFRE1',	'StDevMeanJul',	'MeanMay',	'StDevMeanJan',	'StDevJulianMax',	'lkur',	'JulianMin',	'StDevMean3DayFlowMaxs',	'StDevMeanJun',	'StDevMeanSep',	'StDevMean7DayFlowMaxs',	'StDevMeanFeb',	'StDevMeanMar',	'MeanApr')
plots_indices_bio <- lapply(Indices_p_value_13, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_13, aes_string(x = q, y = "LIFE", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_LIFE <- ggarrange(plotlist = plots_indices_bio,nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_LIFE, filename = "Graphs_LIFE_Class13.pdf")


##### EPT_OCH #####
##Class 3
Indices_p_value_3 <- c('StDevZeroFlowDays',	'ZeroFlowDays',	'nPos',	'nNeg',	'MeanApr',	'JulianMax',	'StDevMean7DayFlowMaxs',	'StDevnPos',	'MeanJul',	'StDevMean30DayFlowMaxs',	'Mean90DayFlowMaxs',	'StDevnNeg',	'StDevJulianMax',	'MeanAug',	'StDevMeanSep',	'StDevMean3DayFlowMaxs',	'MeanSep',	'StDevMeanAug',	'l2',	'MeanMar',	'StDevMeanJul',	'Mean30DayFlowMaxs',	'MeanMay',	'X75per',	'MeanDec',	'lca',	'StDevnPulsesHigh',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'Mean7DayFlowMins',	'StDevMean90DayFlowMaxs',	'Mean3DayFlowMins',	'Mean7DayFlowMaxs',	'X95per',	'nPulsesHigh',	'Mean30DayFlowMins',	'StDevReversals')
plots_indices_bio <- lapply(Indices_p_value_3, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_3, aes_string(x = q, y = "EPT_OCH", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) 
  return(Bio_plots)
})

multi.page_EPT_OCH <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_EPT_OCH, filename = "Graphs_EPT_OCH_Class3.pdf")

##Class 8

Indices_p_value_8 <- c('StDevnPulsesHigh',	'nPos',	'StDevMeanFeb',	'StDevReversals',	'StDevnPulsesLow',	'ZeroFlowDays',	'StDevZeroFlowDays',	'StDevMean7DayFlowMaxs',	'StDevMean3DayFlowMaxs',	'Predictability',	'MeanMay',	'StDevMeanSep',	'lkur',	'nPulsesLow',	'meanNeg',	'StDevMeanAug',	'MeanApr',	'StDevMean30DayFlowMaxs',	'Reversals',	'FRE3',	'StDevBFI',	'StDevnNeg',	'StDevMeanJul',	'StDevnPos',	'StDevJulianMin',	'MeanMar',	'nPulsesHigh',	'nNeg',	'StDevMeanMar',	'StDevMeanJun',	'StDevMean1DayFlowMaxs',	'X95per',	'X25per',	'StDevMean90DayFlowMaxs',	'FRE7',	'Mean1DayFlowMins',	'StDevJulianMax',	'StDevMean90DayFlowMins',	'StDevFRE1',	'JulianMin',	'Mean90DayFlowMaxs',	'MeanSep',	'Mean7DayFlowMaxs',	'Mean30DayFlowMaxs')
plots_indices_bio <- lapply(Indices_p_value_8, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_8, aes_string(x = q, y = "EPT_OCH", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_EPT_OCH <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_EPT_OCH, filename = "Graphs_EPT_OCH_Class8.pdf")


##Class 10

Indices_p_value_10 <- c('JulianMin',	'MeanSep',	'MeanAug',	'nPos',	'StDevnPulsesLow',	'Mean90DayFlowMins',	'StDevJulianMin',	'Mean90DayFlowMaxs',	'l2',	'FRE7',	'nNeg',	'Mean3DayFlowMaxs',	'MeanJun',	'nPulsesLow',	'Mean1DayFlowMaxs',	'MeanJul',	'Mean30DayFlowMins',	'Mean30DayFlowMaxs',	'BFI',	'MeanJan',	'X75per',	'Reversals',	'Mean7DayFlowMaxs',	'Mean7DayFlowMins',	'StDevBFI',	'Mean3DayFlowMins',	'StDevMeanNov',	'StDevMeanAug',	'X95per',	'StDevMean30DayFlowMins',	'X5per',	'StDevMean90DayFlowMins',	'StDevnPos',	'Mean1DayFlowMins',	'StDevnNeg',	'lca',	'StDevMeanSep',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean3DayFlowMins',	'MeanDec',	'MeanFeb',	'StDevFRE1',	'StDevMean1DayFlowMins',	'StDevMean1DayFlowMaxs',	'MeanMar',	'MeanPulseLengthLow',	'StDevReversals',	'StDevFRE7',	'StDevMeanDec',	'StDevnPulsesHigh',	'FRE3',	'StDevMean3DayFlowMaxs',	'meanPos',	'MeanMay')
plots_indices_bio <- lapply(Indices_p_value_10, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_10, aes_string(x = q, y = "EPT_OCH", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_EPT_OCH <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_EPT_OCH, filename = "Graphs_EPT_OCH_Class10.pdf")

##Class 13

Indices_p_value_13 <- c('Mean90DayFlowMins',	'nNeg',	'nPos',	'Mean1DayFlowMaxs',	'StDevMean30DayFlowMins',	'StDevJulianMin',	'Mean7DayFlowMaxs',	'MeanJul',	'Mean3DayFlowMaxs',	'MeanSep',	'StDevnPulsesLow',	'Reversals',	'nPulsesLow',	'MeanAug',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean90DayFlowMins',	'X5per',	'StDevBFI',	'l2',	'MeanDec',	'Mean30DayFlowMins',	'StDevMeanNov',	'X75per',	'MeanJun',	'Mean30DayFlowMaxs',	'FRE7',	'MeanJan',	'StDevFRE7',	'StDevMean3DayFlowMins',	'Mean90DayFlowMaxs',	'StDevMeanDec',	'X95per',	'StDevMeanApr',	'FRE3',	'StDevMean1DayFlowMins',	'Mean7DayFlowMins',	'BFI',	'MeanFeb',	'lca',	'StDevnNeg',	'Mean3DayFlowMins',	'MeanMar',	'MeanPulseLengthLow',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'StDevFRE1',	'StDevMeanJul',	'MeanMay',	'StDevMeanJan',	'StDevJulianMax',	'lkur',	'JulianMin',	'StDevMean3DayFlowMaxs',	'StDevMeanJun',	'StDevMeanSep',	'StDevMean7DayFlowMaxs',	'StDevMeanFeb',	'StDevMeanMar',	'MeanApr')
plots_indices_bio <- lapply(Indices_p_value_13, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_13, aes_string(x = q, y = "EPT_OCH", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_EPT_OCH <- ggarrange(plotlist = plots_indices_bio,nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_EPT_OCH, filename = "Graphs_EPT_OCH_Class13.pdf")


### Non.Insect ###

##Class 3
Indices_p_value_3 <- c('StDevZeroFlowDays',	'ZeroFlowDays',	'nPos',	'nNeg',	'MeanApr',	'JulianMax',	'StDevMean7DayFlowMaxs',	'StDevnPos',	'MeanJul',	'StDevMean30DayFlowMaxs',	'Mean90DayFlowMaxs',	'StDevnNeg',	'StDevJulianMax',	'MeanAug',	'StDevMeanSep',	'StDevMean3DayFlowMaxs',	'MeanSep',	'StDevMeanAug',	'l2',	'MeanMar',	'StDevMeanJul',	'Mean30DayFlowMaxs',	'MeanMay',	'X75per',	'MeanDec',	'lca',	'StDevnPulsesHigh',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'Mean7DayFlowMins',	'StDevMean90DayFlowMaxs',	'Mean3DayFlowMins',	'Mean7DayFlowMaxs',	'X95per',	'nPulsesHigh',	'Mean30DayFlowMins',	'StDevReversals')
plots_indices_bio <- lapply(Indices_p_value_3, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_3, aes_string(x = q, y = "Non.Insect", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) 
  return(Bio_plots)
})

multi.page_Non.Insect <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_Non.Insect, filename = "Graphs_Non.Insect_Class3.pdf")

##Class 8

Indices_p_value_8 <- c('StDevnPulsesHigh',	'nPos',	'StDevMeanFeb',	'StDevReversals',	'StDevnPulsesLow',	'ZeroFlowDays',	'StDevZeroFlowDays',	'StDevMean7DayFlowMaxs',	'StDevMean3DayFlowMaxs',	'Predictability',	'MeanMay',	'StDevMeanSep',	'lkur',	'nPulsesLow',	'meanNeg',	'StDevMeanAug',	'MeanApr',	'StDevMean30DayFlowMaxs',	'Reversals',	'FRE3',	'StDevBFI',	'StDevnNeg',	'StDevMeanJul',	'StDevnPos',	'StDevJulianMin',	'MeanMar',	'nPulsesHigh',	'nNeg',	'StDevMeanMar',	'StDevMeanJun',	'StDevMean1DayFlowMaxs',	'X95per',	'X25per',	'StDevMean90DayFlowMaxs',	'FRE7',	'Mean1DayFlowMins',	'StDevJulianMax',	'StDevMean90DayFlowMins',	'StDevFRE1',	'JulianMin',	'Mean90DayFlowMaxs',	'MeanSep',	'Mean7DayFlowMaxs',	'Mean30DayFlowMaxs')
plots_indices_bio <- lapply(Indices_p_value_8, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_8, aes_string(x = q, y = "Non.Insect", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_Non.Insect <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_Non.Insect, filename = "Graphs_Non.Insect_Class8.pdf")


##Class 10

Indices_p_value_10 <- c('JulianMin',	'MeanSep',	'MeanAug',	'nPos',	'StDevnPulsesLow',	'Mean90DayFlowMins',	'StDevJulianMin',	'Mean90DayFlowMaxs',	'l2',	'FRE7',	'nNeg',	'Mean3DayFlowMaxs',	'MeanJun',	'nPulsesLow',	'Mean1DayFlowMaxs',	'MeanJul',	'Mean30DayFlowMins',	'Mean30DayFlowMaxs',	'BFI',	'MeanJan',	'X75per',	'Reversals',	'Mean7DayFlowMaxs',	'Mean7DayFlowMins',	'StDevBFI',	'Mean3DayFlowMins',	'StDevMeanNov',	'StDevMeanAug',	'X95per',	'StDevMean30DayFlowMins',	'X5per',	'StDevMean90DayFlowMins',	'StDevnPos',	'Mean1DayFlowMins',	'StDevnNeg',	'lca',	'StDevMeanSep',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean3DayFlowMins',	'MeanDec',	'MeanFeb',	'StDevFRE1',	'StDevMean1DayFlowMins',	'StDevMean1DayFlowMaxs',	'MeanMar',	'MeanPulseLengthLow',	'StDevReversals',	'StDevFRE7',	'StDevMeanDec',	'StDevnPulsesHigh',	'FRE3',	'StDevMean3DayFlowMaxs',	'meanPos',	'MeanMay')
plots_indices_bio <- lapply(Indices_p_value_10, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_10, aes_string(x = q, y = "Non.Insect", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_Non.Insect <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_Non.Insect, filename = "Graphs_Non.Insect_Class10.pdf")

##Class 13

Indices_p_value_13 <- c('Mean90DayFlowMins',	'nNeg',	'nPos',	'Mean1DayFlowMaxs',	'StDevMean30DayFlowMins',	'StDevJulianMin',	'Mean7DayFlowMaxs',	'MeanJul',	'Mean3DayFlowMaxs',	'MeanSep',	'StDevnPulsesLow',	'Reversals',	'nPulsesLow',	'MeanAug',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean90DayFlowMins',	'X5per',	'StDevBFI',	'l2',	'MeanDec',	'Mean30DayFlowMins',	'StDevMeanNov',	'X75per',	'MeanJun',	'Mean30DayFlowMaxs',	'FRE7',	'MeanJan',	'StDevFRE7',	'StDevMean3DayFlowMins',	'Mean90DayFlowMaxs',	'StDevMeanDec',	'X95per',	'StDevMeanApr',	'FRE3',	'StDevMean1DayFlowMins',	'Mean7DayFlowMins',	'BFI',	'MeanFeb',	'lca',	'StDevnNeg',	'Mean3DayFlowMins',	'MeanMar',	'MeanPulseLengthLow',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'StDevFRE1',	'StDevMeanJul',	'MeanMay',	'StDevMeanJan',	'StDevJulianMax',	'lkur',	'JulianMin',	'StDevMean3DayFlowMaxs',	'StDevMeanJun',	'StDevMeanSep',	'StDevMean7DayFlowMaxs',	'StDevMeanFeb',	'StDevMeanMar',	'MeanApr')
plots_indices_bio <- lapply(Indices_p_value_13, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_13, aes_string(x = q, y = "Non.Insect", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_Non.Insect <- ggarrange(plotlist = plots_indices_bio,nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_Non.Insect, filename = "Graphs_Non.Insect_Class13.pdf")



### Diptera ###

##Class 3
Indices_p_value_3 <- c('StDevZeroFlowDays',	'ZeroFlowDays',	'nPos',	'nNeg',	'MeanApr',	'JulianMax',	'StDevMean7DayFlowMaxs',	'StDevnPos',	'MeanJul',	'StDevMean30DayFlowMaxs',	'Mean90DayFlowMaxs',	'StDevnNeg',	'StDevJulianMax',	'MeanAug',	'StDevMeanSep',	'StDevMean3DayFlowMaxs',	'MeanSep',	'StDevMeanAug',	'l2',	'MeanMar',	'StDevMeanJul',	'Mean30DayFlowMaxs',	'MeanMay',	'X75per',	'MeanDec',	'lca',	'StDevnPulsesHigh',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'Mean7DayFlowMins',	'StDevMean90DayFlowMaxs',	'Mean3DayFlowMins',	'Mean7DayFlowMaxs',	'X95per',	'nPulsesHigh',	'Mean30DayFlowMins',	'StDevReversals')
plots_indices_bio <- lapply(Indices_p_value_3, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_3, aes_string(x = q, y = "Diptera", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) 
  return(Bio_plots)
})

multi.page_Diptera <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_Diptera, filename = "Graphs_Diptera_Class3.pdf")

##Class 8

Indices_p_value_8 <- c('StDevnPulsesHigh',	'nPos',	'StDevMeanFeb',	'StDevReversals',	'StDevnPulsesLow',	'ZeroFlowDays',	'StDevZeroFlowDays',	'StDevMean7DayFlowMaxs',	'StDevMean3DayFlowMaxs',	'Predictability',	'MeanMay',	'StDevMeanSep',	'lkur',	'nPulsesLow',	'meanNeg',	'StDevMeanAug',	'MeanApr',	'StDevMean30DayFlowMaxs',	'Reversals',	'FRE3',	'StDevBFI',	'StDevnNeg',	'StDevMeanJul',	'StDevnPos',	'StDevJulianMin',	'MeanMar',	'nPulsesHigh',	'nNeg',	'StDevMeanMar',	'StDevMeanJun',	'StDevMean1DayFlowMaxs',	'X95per',	'X25per',	'StDevMean90DayFlowMaxs',	'FRE7',	'Mean1DayFlowMins',	'StDevJulianMax',	'StDevMean90DayFlowMins',	'StDevFRE1',	'JulianMin',	'Mean90DayFlowMaxs',	'MeanSep',	'Mean7DayFlowMaxs',	'Mean30DayFlowMaxs')
plots_indices_bio <- lapply(Indices_p_value_8, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_8, aes_string(x = q, y = "Diptera", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_Diptera <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_Diptera, filename = "Graphs_Diptera_Class8.pdf")


##Class 10

Indices_p_value_10 <- c('JulianMin',	'MeanSep',	'MeanAug',	'nPos',	'StDevnPulsesLow',	'Mean90DayFlowMins',	'StDevJulianMin',	'Mean90DayFlowMaxs',	'l2',	'FRE7',	'nNeg',	'Mean3DayFlowMaxs',	'MeanJun',	'nPulsesLow',	'Mean1DayFlowMaxs',	'MeanJul',	'Mean30DayFlowMins',	'Mean30DayFlowMaxs',	'BFI',	'MeanJan',	'X75per',	'Reversals',	'Mean7DayFlowMaxs',	'Mean7DayFlowMins',	'StDevBFI',	'Mean3DayFlowMins',	'StDevMeanNov',	'StDevMeanAug',	'X95per',	'StDevMean30DayFlowMins',	'X5per',	'StDevMean90DayFlowMins',	'StDevnPos',	'Mean1DayFlowMins',	'StDevnNeg',	'lca',	'StDevMeanSep',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean3DayFlowMins',	'MeanDec',	'MeanFeb',	'StDevFRE1',	'StDevMean1DayFlowMins',	'StDevMean1DayFlowMaxs',	'MeanMar',	'MeanPulseLengthLow',	'StDevReversals',	'StDevFRE7',	'StDevMeanDec',	'StDevnPulsesHigh',	'FRE3',	'StDevMean3DayFlowMaxs',	'meanPos',	'MeanMay')
plots_indices_bio <- lapply(Indices_p_value_10, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_10, aes_string(x = q, y = "Diptera", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_Diptera <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_Diptera, filename = "Graphs_Diptera_Class10.pdf")

##Class 13

Indices_p_value_13 <- c('Mean90DayFlowMins',	'nNeg',	'nPos',	'Mean1DayFlowMaxs',	'StDevMean30DayFlowMins',	'StDevJulianMin',	'Mean7DayFlowMaxs',	'MeanJul',	'Mean3DayFlowMaxs',	'MeanSep',	'StDevnPulsesLow',	'Reversals',	'nPulsesLow',	'MeanAug',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean90DayFlowMins',	'X5per',	'StDevBFI',	'l2',	'MeanDec',	'Mean30DayFlowMins',	'StDevMeanNov',	'X75per',	'MeanJun',	'Mean30DayFlowMaxs',	'FRE7',	'MeanJan',	'StDevFRE7',	'StDevMean3DayFlowMins',	'Mean90DayFlowMaxs',	'StDevMeanDec',	'X95per',	'StDevMeanApr',	'FRE3',	'StDevMean1DayFlowMins',	'Mean7DayFlowMins',	'BFI',	'MeanFeb',	'lca',	'StDevnNeg',	'Mean3DayFlowMins',	'MeanMar',	'MeanPulseLengthLow',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'StDevFRE1',	'StDevMeanJul',	'MeanMay',	'StDevMeanJan',	'StDevJulianMax',	'lkur',	'JulianMin',	'StDevMean3DayFlowMaxs',	'StDevMeanJun',	'StDevMeanSep',	'StDevMean7DayFlowMaxs',	'StDevMeanFeb',	'StDevMeanMar',	'MeanApr')
plots_indices_bio <- lapply(Indices_p_value_13, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_13, aes_string(x = q, y = "Diptera", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_Diptera <- ggarrange(plotlist = plots_indices_bio,nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_Diptera, filename = "Graphs_Diptera_Class13.pdf")



### Insect ###

##Class 3
Indices_p_value_3 <- c('StDevZeroFlowDays',	'ZeroFlowDays',	'nPos',	'nNeg',	'MeanApr',	'JulianMax',	'StDevMean7DayFlowMaxs',	'StDevnPos',	'MeanJul',	'StDevMean30DayFlowMaxs',	'Mean90DayFlowMaxs',	'StDevnNeg',	'StDevJulianMax',	'MeanAug',	'StDevMeanSep',	'StDevMean3DayFlowMaxs',	'MeanSep',	'StDevMeanAug',	'l2',	'MeanMar',	'StDevMeanJul',	'Mean30DayFlowMaxs',	'MeanMay',	'X75per',	'MeanDec',	'lca',	'StDevnPulsesHigh',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'Mean7DayFlowMins',	'StDevMean90DayFlowMaxs',	'Mean3DayFlowMins',	'Mean7DayFlowMaxs',	'X95per',	'nPulsesHigh',	'Mean30DayFlowMins',	'StDevReversals')
plots_indices_bio <- lapply(Indices_p_value_3, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_3, aes_string(x = q, y = "Insect", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) 
  return(Bio_plots)
})

multi.page_Insect <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_Insect, filename = "Graphs_Insect_Class3.pdf")

##Class 8

Indices_p_value_8 <- c('StDevnPulsesHigh',	'nPos',	'StDevMeanFeb',	'StDevReversals',	'StDevnPulsesLow',	'ZeroFlowDays',	'StDevZeroFlowDays',	'StDevMean7DayFlowMaxs',	'StDevMean3DayFlowMaxs',	'Predictability',	'MeanMay',	'StDevMeanSep',	'lkur',	'nPulsesLow',	'meanNeg',	'StDevMeanAug',	'MeanApr',	'StDevMean30DayFlowMaxs',	'Reversals',	'FRE3',	'StDevBFI',	'StDevnNeg',	'StDevMeanJul',	'StDevnPos',	'StDevJulianMin',	'MeanMar',	'nPulsesHigh',	'nNeg',	'StDevMeanMar',	'StDevMeanJun',	'StDevMean1DayFlowMaxs',	'X95per',	'X25per',	'StDevMean90DayFlowMaxs',	'FRE7',	'Mean1DayFlowMins',	'StDevJulianMax',	'StDevMean90DayFlowMins',	'StDevFRE1',	'JulianMin',	'Mean90DayFlowMaxs',	'MeanSep',	'Mean7DayFlowMaxs',	'Mean30DayFlowMaxs')
plots_indices_bio <- lapply(Indices_p_value_8, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_8, aes_string(x = q, y = "Insect", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_Insect <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_Insect, filename = "Graphs_Insect_Class8.pdf")


##Class 10

Indices_p_value_10 <- c('JulianMin',	'MeanSep',	'MeanAug',	'nPos',	'StDevnPulsesLow',	'Mean90DayFlowMins',	'StDevJulianMin',	'Mean90DayFlowMaxs',	'l2',	'FRE7',	'nNeg',	'Mean3DayFlowMaxs',	'MeanJun',	'nPulsesLow',	'Mean1DayFlowMaxs',	'MeanJul',	'Mean30DayFlowMins',	'Mean30DayFlowMaxs',	'BFI',	'MeanJan',	'X75per',	'Reversals',	'Mean7DayFlowMaxs',	'Mean7DayFlowMins',	'StDevBFI',	'Mean3DayFlowMins',	'StDevMeanNov',	'StDevMeanAug',	'X95per',	'StDevMean30DayFlowMins',	'X5per',	'StDevMean90DayFlowMins',	'StDevnPos',	'Mean1DayFlowMins',	'StDevnNeg',	'lca',	'StDevMeanSep',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean3DayFlowMins',	'MeanDec',	'MeanFeb',	'StDevFRE1',	'StDevMean1DayFlowMins',	'StDevMean1DayFlowMaxs',	'MeanMar',	'MeanPulseLengthLow',	'StDevReversals',	'StDevFRE7',	'StDevMeanDec',	'StDevnPulsesHigh',	'FRE3',	'StDevMean3DayFlowMaxs',	'meanPos',	'MeanMay')
plots_indices_bio <- lapply(Indices_p_value_10, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_10, aes_string(x = q, y = "Insect", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_Insect <- ggarrange(plotlist = plots_indices_bio, nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_Insect, filename = "Graphs_Insect_Class10.pdf")

##Class 13

Indices_p_value_13 <- c('Mean90DayFlowMins',	'nNeg',	'nPos',	'Mean1DayFlowMaxs',	'StDevMean30DayFlowMins',	'StDevJulianMin',	'Mean7DayFlowMaxs',	'MeanJul',	'Mean3DayFlowMaxs',	'MeanSep',	'StDevnPulsesLow',	'Reversals',	'nPulsesLow',	'MeanAug',	'StDevMean7DayFlowMins',	'MeanNov',	'StDevMean90DayFlowMins',	'X5per',	'StDevBFI',	'l2',	'MeanDec',	'Mean30DayFlowMins',	'StDevMeanNov',	'X75per',	'MeanJun',	'Mean30DayFlowMaxs',	'FRE7',	'MeanJan',	'StDevFRE7',	'StDevMean3DayFlowMins',	'Mean90DayFlowMaxs',	'StDevMeanDec',	'X95per',	'StDevMeanApr',	'FRE3',	'StDevMean1DayFlowMins',	'Mean7DayFlowMins',	'BFI',	'MeanFeb',	'lca',	'StDevnNeg',	'Mean3DayFlowMins',	'MeanMar',	'MeanPulseLengthLow',	'StDevMean1DayFlowMaxs',	'Mean1DayFlowMins',	'StDevFRE1',	'StDevMeanJul',	'MeanMay',	'StDevMeanJan',	'StDevJulianMax',	'lkur',	'JulianMin',	'StDevMean3DayFlowMaxs',	'StDevMeanJun',	'StDevMeanSep',	'StDevMean7DayFlowMaxs',	'StDevMeanFeb',	'StDevMeanMar',	'MeanApr')
plots_indices_bio <- lapply(Indices_p_value_13, function(q){
  Bio_plots <- ggplot(Nat_alt_summer_13, aes_string(x = q, y = "Insect", color="Flow.regim")) + geom_point(shape=19, size=0.7)+
    theme(legend.position = "none")  + geom_smooth(method=lm, se=F) + scale_color_manual(values = c("tomato2", "springgreen3")) #+ scale_x_continuous(trans = 'log10')
  return(Bio_plots)
})

multi.page_Insect <- ggarrange(plotlist = plots_indices_bio,nrow = 2, ncol =2)
setwd("~/Cássia/11 Nabia/Hyd_alteration/Biological_data/Bio_classes")
ggexport(multi.page_Insect, filename = "Graphs_Insect_Class13.pdf")






# Ordinations -------------------------------------------------------------

## Macroinvertebrates - CLASS 3
Nat_alt_summer_3 <- Nat_alt_summer %>% filter(Lev_20 == "3")
row.names(Nat_alt_summer_3) <- Nat_alt_summer_3$ID
Taxa <- select(Nat_alt_summer_3, Hydrobiidae:Plumatellidae)

Env <- select(Nat_alt_summer_3, AREA_KM2:MN_calc, MN_PERM:MN_HARD, MN_UHD:MN_PLT, MN_DEN)
Hyd <- select(Nat_alt_summer_3, MeanJan:X95per)
FQ <- select(Nat_alt_summer_3, A_COND20SITU,A_PHSITU)
All <- cbind(Env, Hyd, FQ)

Col_EPT <- as.character(Nat_alt_summer_3$Color)
Col_Flow <- as.character(Nat_alt_summer_3$Flow_Color)
Sym_EPT <- Nat_alt_summer_3$Symbol

Taxa_RA <- decostand(Taxa, "total") ## Relative abundance
Taxa_H <- sqrt(Taxa_RA) ## Relative abundance, percentage of each species based on total of the site
Taxa_PA <- decostand(Taxa, "pa") ## Relative abundance, percentage of each species based on total of the site

Taxa_Norm <- decostand(Taxa, "normalize") ## Relative abundance, percentage of each species based on total of the site

rowSums (Taxa, na.rm = FALSE, dims = 1)
rowSums (Taxa_RA, na.rm = FALSE, dims = 1)
rowSums (Taxa_H, na.rm = FALSE, dims = 1)
rowSums (Taxa_PA, na.rm = FALSE, dims = 1)

Dist_bray_H <- vegdist(Taxa_H, method = "bray")
Dist_Jac <- vegdist(Taxa_PA, method = "jac")

PCoA_Com_H <- cmdscale(Dist_bray_H, k = 2, eig = TRUE) ##0.302
PCoA_Com_PA <- cmdscale(Dist_Jac, k = 2, eig = TRUE) ##0.21

(fit_Env_PCoA_Com_H <- envfit(PCoA_Com_H, scale(Env), perm = 1000, na.rm = T))
(fit_Hyd_PCoA_Com_H <- envfit(PCoA_Com_H, scale(Hyd), perm = 1000, na.rm = T))
(fit_FQ_PCoA_Com_H <- envfit(PCoA_Com_H, scale(FQ), perm = 1000, na.rm = T))
(fit_All_PCoA_Com_H <- envfit(PCoA_Com_H, scale(All), perm = 1000, na.rm = T))




pcoa.C.H <- ordiplot(PCoA_Com_H, type = "n", main = "CLASS 3 - MI - Community Structure",#xlim = c(-0.5,0.2),
                     # xlim = c(-0.22,0.17), ylim= c(-0.01,0.01),
                     xlab="PCoA 1 (226,8%)", ylab="PCoA 2 (3,37%)")
abline(h = 0, lty = 1)
abline(v = 0, lty = 1)
#mtext("GOF = 0,213", side=3, cex=0.8)

Macroinv_wa <- wascores(PCoA_Com_H$points[, 1:2], Taxa_H)
###write.csv2(Macroinv_wa, "macroinv_Com_H_taxa.csv")
##text(Macroinv_wa, rownames(Macroinv_wa),cex = 0.7, col = "azure4")
#text(Macroinv_wa, rownames(Macroinv_wa),cex = 0.9, col = "azure4")
#♥points(Macroinv_wa,cex =1, pch=Simbolos, col=Col_insect)

points(pcoa.C.H, "sites", pch = Sym_EPT, cex=1, col = Col_EPT)
points(pcoa.C.H, "sites", pch = 19, cex=1, col = Col_Flow)
#points(pcoa.C.H, "sites", pch = 19, cex=0.55, col = Col_Flow)


plot(fit_Env_PCoA_Com_H, p.max = 0.001, col = "grey20", cex=0.7)
plot(fit_Hyd_PCoA_Com_H, p.max = 0.001, col = "brown2", cex=0.7)
plot(fit_Env_PCoA_Com_H, p.max = 0.001, col = "darkorange", cex=0.7)
plot(fit_FQ_PCoA_Com_H, p.max = 0.001, col = "darkblue", cex=0.7)
plot(fit_All_PCoA_Com_H, p.max = 0.001, col = "darkblue", cex=0.7)



### Macroinvertebrates - CLASS 8 ### 
Nat_alt_summer_8 <- Nat_alt_summer %>% filter(Lev_20 == "8")
row.names(Nat_alt_summer_8) <- Nat_alt_summer_8$ID
Taxa <- select(Nat_alt_summer_8, Hydrobiidae:Plumatellidae)

Env <- select(Nat_alt_summer_8, AREA_KM2:MN_calc, MN_PERM:MN_HARD, MN_UHD:MN_PLT, MN_DEN)
Hyd <- select(Nat_alt_summer_8, MeanJan:X95per)
FQ <- select(Nat_alt_summer_8, A_COND20SITU,A_PHSITU)
All <- cbind(Env, Hyd, FQ)

Col_EPT <- as.character(Nat_alt_summer_8$Color)
Col_Flow <- as.character(Nat_alt_summer_8$Flow_Color)
Sym_EPT <- Nat_alt_summer_8$Symbol

Taxa_RA <- decostand(Taxa, "total") ## Relative abundance
Taxa_H <- sqrt(Taxa_RA) ## Relative abundance, percentage of each species based on total of the site
Taxa_PA <- decostand(Taxa, "pa") ## Relative abundance, percentage of each species based on total of the site

Taxa_Norm <- decostand(Taxa, "normalize") ## Relative abundance, percentage of each species based on total of the site

rowSums (Taxa, na.rm = FALSE, dims = 1)
rowSums (Taxa_RA, na.rm = FALSE, dims = 1)
rowSums (Taxa_H, na.rm = FALSE, dims = 1)
rowSums (Taxa_PA, na.rm = FALSE, dims = 1)

Dist_bray_H <- vegdist(Taxa_H, method = "bray")
Dist_Jac <- vegdist(Taxa_PA, method = "jac")

PCoA_Com_H <- cmdscale(Dist_bray_H, k = 2, eig = TRUE) ##0.302
PCoA_Com_PA <- cmdscale(Dist_Jac, k = 2, eig = TRUE) ##0.21

(fit_Env_PCoA_Com_H <- envfit(PCoA_Com_H, scale(Env), perm = 1000, na.rm = T))
(fit_Hyd_PCoA_Com_H <- envfit(PCoA_Com_H, scale(Hyd), perm = 1000, na.rm = T))
(fit_FQ_PCoA_Com_H <- envfit(PCoA_Com_H, scale(FQ), perm = 1000, na.rm = T))
(fit_All_PCoA_Com_H <- envfit(PCoA_Com_H, scale(All), perm = 1000, na.rm = T))


pcoa.C.H <- ordiplot(PCoA_Com_H, type = "n", main = "CLASS 8 - MI - Community Structure",#xlim = c(-0.5,0.2),
                     # xlim = c(-0.22,0.17), ylim= c(-0.01,0.01),
                     xlab="PCoA 1 (37,68%)", ylab="PCoA 2 (3,0%)")
abline(h = 0, lty = 1)
abline(v = 0, lty = 1)
#mtext("GOF = 0,213", side=3, cex=0.8)

Macroinv_wa <- wascores(PCoA_Com_H$points[, 1:2], Taxa_H)
###write.csv2(Macroinv_wa, "macroinv_Com_H_taxa.csv")
##text(Macroinv_wa, rownames(Macroinv_wa),cex = 0.7, col = "azure4")
#text(Macroinv_wa, rownames(Macroinv_wa),cex = 0.9, col = "azure4")
#♥points(Macroinv_wa,cex =1, pch=Simbolos, col=Col_insect)

points(pcoa.C.H, "sites", pch = Sym_EPT, cex=1, col = Col_EPT)
points(pcoa.C.H, "sites", pch = 19, cex=1, col = Col_Flow)
#points(pcoa.C.H, "sites", pch = 19, cex=0.55, col = Col_Flow)


plot(fit_Env_PCoA_Com_H, p.max = 0.001, col = "grey20", cex=0.7)
plot(fit_Hyd_PCoA_Com_H, p.max = 0.001, col = "brown2", cex=0.7)
plot(fit_Env_PCoA_Com_H, p.max = 0.001, col = "darkorange", cex=0.7)
plot(fit_FQ_PCoA_Com_H, p.max = 0.001, col = "darkblue", cex=0.7)
plot(fit_All_PCoA_Com_H, p.max = 0.001, col = "darkblue", cex=0.7)





## Macroinvertebrates - CLASS 10
Nat_alt_summer_10 <- Nat_alt_summer %>% filter(Lev_20 == "10")
row.names(Nat_alt_summer_10) <- Nat_alt_summer_10$ID
Taxa <- select(Nat_alt_summer_10, Hydrobiidae:Plumatellidae)

Env <- select(Nat_alt_summer_10, AREA_KM2:MN_calc, MN_PERM:MN_HARD, MN_UHD:MN_PLT, MN_DEN)
Hyd <- select(Nat_alt_summer_10, MeanJan:X95per)
FQ <- select(Nat_alt_summer_10, A_COND20SITU,A_PHSITU)
All <- cbind(Env, Hyd)

Col_EPT <- as.character(Nat_alt_summer_10$Color)
Col_Flow <- as.character(Nat_alt_summer_10$Flow_Color)
Sym_EPT <- Nat_alt_summer_10$Symbol

Taxa_RA <- decostand(Taxa, "total") ## Relative abundance
Taxa_H <- sqrt(Taxa_RA) ## Relative abundance, percentage of each species based on total of the site
Taxa_PA <- decostand(Taxa, "pa") ## Relative abundance, percentage of each species based on total of the site

Taxa_Norm <- decostand(Taxa, "normalize") ## Relative abundance, percentage of each species based on total of the site

rowSums (Taxa, na.rm = FALSE, dims = 1)
rowSums (Taxa_RA, na.rm = FALSE, dims = 1)
rowSums (Taxa_H, na.rm = FALSE, dims = 1)
rowSums (Taxa_PA, na.rm = FALSE, dims = 1)

Dist_bray_H <- vegdist(Taxa_H, method = "bray")
Dist_Jac <- vegdist(Taxa_PA, method = "jac")

PCoA_Com_H <- cmdscale(Dist_bray_H, k = 2, eig = TRUE) ##0.302
PCoA_Com_PA <- cmdscale(Dist_Jac, k = 2, eig = TRUE) ##0.21

(fit_Env_PCoA_Com_H <- envfit(PCoA_Com_H, scale(Env), perm = 1000, na.rm = T))
(fit_Hyd_PCoA_Com_H <- envfit(PCoA_Com_H, scale(Hyd), perm = 1000, na.rm = T))
(fit_FQ_PCoA_Com_H <- envfit(PCoA_Com_H, (FQ), perm = 1000, na.rm = T))
(fit_All_PCoA_Com_H <- envfit(PCoA_Com_H, scale(All), perm = 1000, na.rm = T))



pcoa.C.H <- ordiplot(PCoA_Com_H, type = "n", main = "CLASS 10 - MI - Community Structure",#xlim = c(-0.5,0.2),
                     # xlim = c(-0.22,0.17), ylim= c(-0.01,0.01),
                     xlab="PCoA 1 (28,3%)", ylab="PCoA 2 (3,2%)")
abline(h = 0, lty = 1)
abline(v = 0, lty = 1)
#mtext("GOF = 0,213", side=3, cex=0.8)

Macroinv_wa <- wascores(PCoA_Com_H$points[, 1:2], Taxa_H)
###write.csv2(Macroinv_wa, "macroinv_Com_H_taxa.csv")
##text(Macroinv_wa, rownames(Macroinv_wa),cex = 0.7, col = "azure4")
#text(Macroinv_wa, rownames(Macroinv_wa),cex = 0.9, col = "azure4")
#♥points(Macroinv_wa,cex =1, pch=Simbolos, col=Col_insect)

points(pcoa.C.H, "sites", pch = Sym_EPT, cex=1, col = Col_EPT)
points(pcoa.C.H, "sites", pch = 19, cex=1, col = Col_Flow)
#points(pcoa.C.H, "sites", pch = 19, cex=0.55, col = Col_Flow)


plot(fit_Env_PCoA_Com_H, p.max = 0.001, col = "grey20", cex=0.7)
plot(fit_Hyd_PCoA_Com_H, p.max = 0.001, col = "brown2", cex=0.7)
plot(fit_Env_PCoA_Com_H, p.max = 0.001, col = "darkorange", cex=0.7)
plot(fit_FQ_PCoA_Com_H, p.max = 0.001, col = "darkblue", cex=0.7)
plot(fit_All_PCoA_Com_H, p.max = 0.001, col = "darkblue", cex=0.7)





## Macroinvertebrates - CLASS 13
Nat_alt_summer_13 <- Nat_alt_summer %>% filter(Lev_20 == "13")
row.names(Nat_alt_summer_13) <- Nat_alt_summer_13$ID
Taxa <- select(Nat_alt_summer_13, Hydrobiidae:Plumatellidae)

Env <- select(Nat_alt_summer_13, AREA_KM2:MN_calc, MN_PERM:MN_HARD, MN_UHD:MN_PLT, MN_DEN)
Hyd <- select(Nat_alt_summer_13, MeanJan:X95per)
FQ <- select(Nat_alt_summer_13, A_COND20SITU,A_PHSITU)
All <- cbind(Env, Hyd)

Col_EPT <- as.character(Nat_alt_summer_13$Color)
Col_Flow <- as.character(Nat_alt_summer_13$Flow_Color)
Sym_EPT <- Nat_alt_summer_13$Symbol

Taxa_RA <- decostand(Taxa, "total") ## Relative abundance
Taxa_H <- sqrt(Taxa_RA) ## Relative abundance, percentage of each species based on total of the site
Taxa_PA <- decostand(Taxa, "pa") ## Relative abundance, percentage of each species based on total of the site

Dist_bray_H <- vegdist(Taxa_H, method = "bray")
Dist_Jac <- vegdist(Taxa_PA, method = "jac")

PCoA_Com_H <- cmdscale(Dist_bray_H, k = 2, eig = TRUE) ##0.302
PCoA_Com_PA <- cmdscale(Dist_Jac, k = 2, eig = TRUE) ##0.21

(fit_Env_PCoA_Com_H <- envfit(PCoA_Com_H, scale(Env), perm = 1000, na.rm = T))
(fit_Hyd_PCoA_Com_H <- envfit(PCoA_Com_H, scale(Hyd), perm = 1000, na.rm = T))
(fit_FQ_PCoA_Com_H <- envfit(PCoA_Com_H, (FQ), perm = 1000, na.rm = T))
(fit_All_PCoA_Com_H <- envfit(PCoA_Com_H, scale(All), perm = 1000, na.rm = T))

pcoa.C.H <- ordiplot(PCoA_Com_H, type = "n", main = "CLASS 13 - MI - Community Structure",#xlim = c(-0.5,0.2),
                     # xlim = c(-0.22,0.17), ylim= c(-0.01,0.01),
                     xlab="PCoA 1 (25,9%)", ylab="PCoA 2 (2,5%)")
abline(h = 0, lty = 1)
abline(v = 0, lty = 1)
#mtext("GOF = 0,213", side=3, cex=0.8)

Macroinv_wa <- wascores(PCoA_Com_H$points[, 1:2], Taxa_H)

points(pcoa.C.H, "sites", pch = Sym_EPT, cex=1, col = Col_EPT)
points(pcoa.C.H, "sites", pch = 19, cex=1, col = Col_Flow)


plot(fit_Env_PCoA_Com_H, p.max = 0.001, col = "grey20", cex=0.7)
plot(fit_Hyd_PCoA_Com_H, p.max = 0.001, col = "brown2", cex=0.7)
plot(fit_Env_PCoA_Com_H, p.max = 0.001, col = "darkorange", cex=0.7)
plot(fit_FQ_PCoA_Com_H, p.max = 0.001, col = "darkblue", cex=0.7)
plot(fit_All_PCoA_Com_H, p.max = 0.001, col = "darkblue", cex=0.7)

