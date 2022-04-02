##Deliverable 1
##RE-SET WORKING SESSION AFTER CLOSING STUDIO

library(dplyr)

library(tidyverse)

mpg_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

lm(mpg ~vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mpg_table )

summary(lm(mpg ~vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mpg_table ))


##Deliverable 2

suspension_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

total_summary <- suspension_table  %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),SD_PSI=sd(PSI), .groups = 'keep') #create summary table with multiple columns

lot_summary <- suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),SD_PSI=sd(PSI), .groups = 'keep') #create summary table with multiple columns


##Deliverable 3

t.test(suspension_table$PSI,mu=1500)

t.test(subset(suspension_table,Manufacturing_Lot=="Lot1")$PSI, mu = 1500)

t.test(subset(suspension_table,Manufacturing_Lot=="Lot2")$PSI, mu = 1500)

t.test(subset(suspension_table,Manufacturing_Lot=="Lot3")$PSI, mu = 1500)