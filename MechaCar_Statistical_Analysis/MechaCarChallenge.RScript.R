#Dependencies
library(dplyr)
library(tidyverse)

#Deliverable 1
MechaCar_table <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_table) 
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_table) )


#Deliverable 2
Suspension_table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
total_summary <- Suspension_table %>% summarize(Mean=mean(PSI), Median=median(PSI),Variance = var(PSI), Std=sd(PSI))
lot_summary <- Suspension_table %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI),Variance = var(PSI), Std=sd(PSI), .groups= 'keep')


#Deliverable 3
t.test(Suspension_table$PSI, mu=1500)

t.test(subset(Suspension_table, Manufacturing_Lot == "Lot 1")$PSI, mu=1500)
t.test(subset(Suspension_table, Manufacturing_Lot == "Lot 2")$PSI, mu=1500)
t.test(subset(Suspension_table, Manufacturing_Lot == "Lot 3")$PSI, mu=1500)

