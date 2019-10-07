# Filename: datacleaning.R
# Original code by: Ryoh
# Purpose: clean the NYC Restaurant Inspection dataset
# Date: 2018/02/20, 2018/02/24

library(ggplot2)
library(plyr)
library(data.table)
library(devtools)

# Optional: set to the folder that contains the original csv file
#setwd("C:/Users/Ryoh/Documents/CSC465/Project")

# Since there are elements that have double quotes, read.csv() is failing to read in records.
# Therefor, using fread() instead.
data <- fread('DOHMH_New_York_City_Restaurant_Inspection_Results.csv')

# Initial record count: 383899
#nrow(data)

# Check if data looks OK
#head(data)

# Check column names
names(data)

write.csv(unique(data[,c("VIOLATION CODE","VIOLATION DESCRIPTION")]),"violation.csv")

# Remove unnecessary columns and create first iteration of df to use: df1
df1 <- data[,-c("BUILDING", "STREET", "PHONE", "ACTION", "GRADE DATE", "RECORD DATE","VIOLATION DESCRIPTION")]

# Check new df
#names(df1)
#head(df1)

# Rename columns to shorten varnames and remove spaces
# "ALL THE TIME YOU HAVE TO LEAVE OUT THE SPACE!" - Fernando Alonso on leaving spaces in column varnames... maybe
df1 <- rename(df1,c("CUISINE DESCRIPTION"="CUISINE","INSPECTION DATE"="DATE","VIOLATION CODE"="VIOCODE",
                    "CRITICAL FLAG"="CRITFLAG","INSPECTION TYPE"="INSTYPE"))
#names(df1)
#head(df1)

# See characteristics of grade
#count(df1,vars=c("GRADE"))
# GRADE   freq
# 1                192774
# 2              A 151845
# 3              B  25510
# 4              C   6485
# 5 Not Yet Graded   2115
# 6              P   1725
# 7              Z   3445

# See characteristics of critical flag
#count(df1,vars=c("CRITFLAG"))
# CRITFLAG   freq
# 1       Critical 210398
# 2 Not Applicable   6896
# 3   Not Critical 166605

# See characteristics of inspection type
#count(df1,vars=c("INSTYPE"))
# INSTYPE   freq
# 1                                                                1086
# 2         Administrative Miscellaneous / Compliance Inspection    173
# 3            Administrative Miscellaneous / Initial Inspection   7461
# 4                 Administrative Miscellaneous / Re-inspection   2522
# 5          Administrative Miscellaneous / Reopening Inspection     87
# 6  Administrative Miscellaneous / Second Compliance Inspection     26
# 7                      Calorie Posting / Compliance Inspection     12
# 8                         Calorie Posting / Initial Inspection    806
# 9                              Calorie Posting / Re-inspection    198
# 10              Calorie Posting / Second Compliance Inspection      1
# 11                    Cycle Inspection / Compliance Inspection   1276
# 12                       Cycle Inspection / Initial Inspection 220466
# 13                            Cycle Inspection / Re-inspection  94617
# 14                     Cycle Inspection / Reopening Inspection   3583
# 15             Cycle Inspection / Second Compliance Inspection     29
# 16                Inter-Agency Task Force / Initial Inspection   1034
# 17                     Inter-Agency Task Force / Re-inspection      1
# 18        Pre-permit (Non-operational) / Compliance Inspection     17
# 19           Pre-permit (Non-operational) / Initial Inspection   3881
# 20                Pre-permit (Non-operational) / Re-inspection    295
# 21            Pre-permit (Operational) / Compliance Inspection    697
# 22               Pre-permit (Operational) / Initial Inspection  25176
# 23                    Pre-permit (Operational) / Re-inspection  10905
# 24             Pre-permit (Operational) / Reopening Inspection    706
# 25     Pre-permit (Operational) / Second Compliance Inspection     37
# 26                  Smoke-Free Air Act / Compliance Inspection     44
# 27                     Smoke-Free Air Act / Initial Inspection   3642
# 28                     Smoke-Free Air Act / Limited Inspection      2
# 29                          Smoke-Free Air Act / Re-inspection   1308
# 30           Smoke-Free Air Act / Second Compliance Inspection      6
# 31                           Trans Fat / Compliance Inspection    252
# 32                              Trans Fat / Initial Inspection   2574
# 33                              Trans Fat / Limited Inspection      1
# 34                                   Trans Fat / Re-inspection    948
# 35                    Trans Fat / Second Compliance Inspection     30

# After looking around at the data, the row with empty spaces in inspection types have no restaurant names,
# a cuisine type of "other", and a date of 01/01/1900. Therefor, will omit these records: df2
df2 <- df1[!(df1$INSTYPE==""),]
#nrow(df2)
#head(df2)

# Omit all records with null or empty scores: df3
df3 <- df2[!(is.na(df2$SCORE)),]
#nrow(df3)

# Check if grades and scores look OK
#unique(df3$GRADE)
#summary(df3$SCORE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.00   11.00   14.00   18.92   24.00  151.00

# Omit all records with scores below 0: df4
df4 <- df3[!(df3$SCORE<0),]
#nrow(df4)
# Check if all scores are above 0
#summary(df4$SCORE)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   11.00   14.00   18.92   24.00  151.00

# Check records with null violation codes
#unique(df4$VIOCODE)
#head(df4[df4$VIOCODE=="",], 25)

# What do the scores associated with viocode of "" look like?
#unique(df4[df4$VIOCODE=="","SCORE"])
# SCORE
# 1:     0
# 2:    17
# 3:     7
# 4:    29
# 5:     5
# 6:    12
# 7:    38
# 8:    10

# I assumed that a record with no violation code is associated with an inspection with no violations.
# So, the records with no violation codes but with a greater than 0 score doesn't make sense
#df4[(df4$VIOCODE=="" & df4$SCORE>0)]
# CAMIS                        DBA      BORO ZIPCODE
# 1: 40875290              SEAFOOD  CITY     BRONX   10464
# 2: 40554369                   CATCH 22  BROOKLYN   11209
# 3: 41702610     THE STRAND SMOKE HOUSE    QUEENS   11106
# 4: 40645073               LA SABROSURA     BRONX   10452
# 5: 50038914              HARLEM NIGHTS MANHATTAN   10030
# 6: 50004682               I LOVE PIZZA     BRONX   10451
# 7: 41660888 FANTASY BAR AND RESTAURANT     BRONX   10453
# CUISINE       DATE VIOCODE       CRITFLAG SCORE GRADE
# 1:                                                          Seafood 03/11/2017         Not Applicable    17      
# 2:                                                         American 10/29/2016         Not Applicable     7      
# 3:                                                         Barbecue 07/17/2017         Not Applicable    29      
# 4: Latin (Cuban, Dominican, Puerto Rican, South & Central American) 05/09/2015         Not Applicable     5      
# 5:                                                         American 03/12/2017         Not Applicable    12      
# 6:                                                            Pizza 04/11/2017         Not Applicable    38      
# 7:                                                          Spanish 01/30/2016         Not Applicable    10      
# INSTYPE
# 1: Inter-Agency Task Force / Initial Inspection
# 2: Inter-Agency Task Force / Initial Inspection
# 3:        Cycle Inspection / Initial Inspection
# 4: Inter-Agency Task Force / Initial Inspection
# 5: Inter-Agency Task Force / Initial Inspection
# 6:        Cycle Inspection / Initial Inspection
# 7: Inter-Agency Task Force / Initial Inspection

# ... so, omit these records that don't make sense: df5
df5 <- df4[!(df4$VIOCODE=="" & df4$SCORE>0),]
# Check if all records with no violation codes have a score of 0
#summary(df5[df5$VIOCODE=="","SCORE"])
# SCORE  
# Min.   :0  
# 1st Qu.:0  
# Median :0  
# Mean   :0  
# 3rd Qu.:0  
# Max.   :0  

# Final record count ver1: 362645
#nrow(df5)

# Found which borough the "Missing" entities had; zip of 11249 == Brooklyn
df5$BORO[df5$BORO %in% "Missing"] <- "BROOKLYN"

# Number of violations with weird zip codes: 3124 (aka decent number)
# length(df5$CAMIS[df5$ZIPCODE %in% c(10048, 10057, 10104, 10105, 10106, 10107, 10118, 10121, 10123,
#                                     10155, 10158, 10166, 10175, 10176, 10178, 10179, 10270, 10281,
#                                     10285, 10317, 11241, 11242, 11245, 11249, 11256, 11352)])

# Modifying zip code using USPS zip code map
df5$ZIPCODE[df5$ZIPCODE %in% 10048] <- 10006
df5$ZIPCODE[df5$ZIPCODE %in% 10057] <- 10035
df5$ZIPCODE[df5$ZIPCODE %in% c(10104,10105,10106,10107)] <- 10019
df5$ZIPCODE[df5$ZIPCODE %in% 10118] <- 10001
df5$ZIPCODE[df5$ZIPCODE %in% 10155] <- 10022
df5$ZIPCODE[df5$ZIPCODE %in% 10158] <- 10016
df5$ZIPCODE[df5$ZIPCODE %in% c(10121,10123,10166,10175,10176,10178,10179,10270,10285)] <- 10025
df5$ZIPCODE[df5$ZIPCODE %in% 10281] <- 10280
df5$ZIPCODE[df5$ZIPCODE %in% 10317] <- 10314
df5$ZIPCODE[df5$ZIPCODE %in% c(11241,11242,11245,11249)] <- 11226
df5$ZIPCODE[df5$ZIPCODE %in% 11256] <- 11208
df5$ZIPCODE[df5$ZIPCODE %in% 11352] <- 11415

# Missing zips: 11003, 10162, 10177, 10115, 11351, 11359, 11424, 11425, 10278

# Cuisine modification
count(df5,vars="CUISINE")

# Dataframe of unique inspections: df6
df6 <- df5[,-c("VIOCODE","CRITFLAG","GRADE")]
df6 <- unique(df6)

# Final record count ver2: 129955
#nrow(df6)

# For myself: delete those damn restaurant names
#df5 <- df5[,-c("DBA")]
#df6 <- df6[,-c("DBA")]

# Export each dataframe to different files:
write.csv(df5,'NYCRestaurantV1.csv')
write.csv(df6,'NYCRestaurantV2.csv')
