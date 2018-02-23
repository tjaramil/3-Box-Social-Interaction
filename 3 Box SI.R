# Load Libraries

library(dplyr)
library(readr)

# Load Data

Three.Box <- read.csv("Workbook3.csv",skip = 4, header = T, stringsAsFactors = F)  # Put file name between " "

# Reduce Columns

Three.Box <- Three.Box[,-c(18:29)]



# Take a peak at the data

summary(Three.Box)
names(Three.Box)
str(Three.Box)

Three.Box$Events <- as.logical(Three.Box$Events)

# Determine WT and HET

Three.Box$Geno[Three.Box$Geno == "a"] <- "WT"  #Change "B" to whatever your letter is for WT
Three.Box$Geno[Three.Box$Geno == "b"] <- "KO" #Change "A" to whatever your letter is for HET
Three.Box$Geno[Three.Box$Geno == "c"] <- "HET"  #Change "B" to whatever your letter is for WT

# Get stats

names(Three.Box)
unique(Three.Box$Events)

# set the order you want data arranged to x

x <- c("WT", "HET", "KO")

Three.Box.Stats <- Three.Box %>%
        filter(Trial==1, Events == "Area:Mouse 1 Center In Back Chamber"| Events == "Area:Mouse 1 Center In Front Chamber") %>% # You can change to interaction data or object by just replacing withing " "
        group_by(Geno, Events) %>% 
        summarise(Count = n(),
                  Dur = mean(Duration.Second.),
                  SD = sd(Duration.Second.),
                  SEM = SD/sqrt(Count)) %>% 
        ungroup() %>% 
        mutate(Geno = factor(Geno, levels = x)) %>% 
        arrange(Geno)


# 1st analyze fine movement data

LocoFine <- Loco[,1:16]

# Stats for fine movement data

names(LocoFine)

Fine.Move.Stats <- LocoFine %>%
        group_by(SubjectVars) %>%
        summarise(Count = n(),
                  XF01.Avg = mean(XF01),
                  XF02.Avg = mean(XF02),
                  XF03.Avg = mean(XF03),
                  XF04.Avg = mean(XF04),
                  XF05.Avg = mean(XF05),
                  XF06.Avg = mean(XF06),
                  XF07.Avg = mean(XF07),
                  Tot.Avg = mean(XFTot),
                  XF01.sd = sd(XF01),
                  XF02.sd = sd(XF02),
                  XF03.sd = sd(XF03),
                  XF04.sd = sd(XF04),
                  XF05.sd = sd(XF05),
                  XF06.sd = sd(XF06),
                  XF07.sd = sd(XF07),
                  XFTot.sd = sd(XFTot),
                  XF01.sem = XF01.sd/sqrt(Count),
                  XF02.sem = XF02.sd/sqrt(Count),
                  XF03.sem = XF03.sd/sqrt(Count),
                  XF04.sem = XF04.sd/sqrt(Count),
                  XF05.sem = XF05.sd/sqrt(Count),
                  XF06.sem = XF06.sd/sqrt(Count),
                  XF07.sem = XF07.sd/sqrt(Count),
                  XFTot.sem = XFTot.sd/sqrt(Count)),




sem = 
        