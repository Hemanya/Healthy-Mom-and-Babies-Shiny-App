[33mcommit 5041b1c23142ad7379cb4c6a7b0b5d4645006e64[m[33m ([m[1;36mHEAD -> [m[1;32mmaster[m[33m)[m
Author: Hemanya Tyagi <hemanyatyagiofficial@gmail.com>
Date:   Sat Oct 12 20:01:49 2019 -0400

    First Commit R Shiny APp

[1mdiff --git a/app.R b/app.R[m
[1mindex 7a8a5a1..d97e13e 100644[m
[1m--- a/app.R[m
[1m+++ b/app.R[m
[36m@@ -23,6 +23,8 @@[m [mlibrary(caret)[m
 [m
 #This function is not called. THis is just show the data visualization done for ML model used in shiny app. [m
 #The result of this preprocessing is stored in tr,csv which is imported in this app.[m
[32m+[m[32m# Also, the following file  - indiana-births-and-infant-deaths.csv - is not uploaded on git[m
[32m+[m[32m# It is not required for the execution of this file but still can be downloaded from https://hub.mph.in.gov/dataset/indiana-births-and-infant-deaths[m
 preprocessForML <- function(){[m
   data <- read.csv('indiana-births-and-infant-deaths.csv')[m
   tr<-data[m
[1mdiff --git a/indiana-births-and-infant-deaths.csv b/indiana-births-and-infant-deaths.csv[m
[1mdeleted file mode 100644[m
[1mindex c654874..0000000[m
[1m--- a/indiana-births-and-infant-deaths.csv[m
[1m+++ /dev/null[m
[36m@@ -1,735190 +0,0 @@[m
[31m-MOTHER_ID,NUM_BIRTHS_BY_MOTHER,CHILD_ID,CHILD_BIRTH_YR_GRP,MOTHER_AGE_GRP,MOTHER_MARITALSTATUS_AT_BIRTH,MOTHER_RESID_COUNTY_TYPE,BIRTH_COMPLICATIONS,DIABETES_RISK_PREPREGNANCY,DIABETES_RISK_GESTATIONAL,SMOKING_IND,SMOKING_DURING_PREG_IND,SMOKING_BEFORE_PREG_IND,VISITS_IN_1ST_TRIMESTER_IND,UNDERGRAD_DEGREE_IND,GRADUATE_DEGREE_IND,COLLEGE_ENROLLMENT_IND,HIGH_SCHOOL_DIPLOMA_IND,HIGH_SCHOOL_NON_COMPLETER_IND,WAGE_BIRTH_QTR_IND,MULTI_WAGE_BIRTH_QTR_IND,WAGE_THRD_QTR_BEFORE_BIRTH_IND,MULTI_WAGE_THRD_QTR_BEFORE_BIRTH_IND,CHILD_GENDER,CHILD_TRANSFERRED,CHILD_ALIVE,LOW_BIRTH_WEIGHT,CHILD_BREASTFED,CHILD_MORT_IND,[m
[31m-6902C2213B3BBC1CDAFED68F9565FC13,1,312B8455CF0E0200BC0963E920A30AF8,2010 - 2012,25-35 Years,Married,Urban,N,N,N,N,N,N,Y,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Male,N,Y,N,Y,N,[m
[31m-F15CAB471BAC31814CE3C62A902E7451,1,B903E54A8AF64F2690A52ED9480193F3,2010 - 2012,25-35 Years,Married,Urban,N,N,N,N,N,N,Y,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Male,N,Y,N,Y,N,[m
[31m-EBC56C64057252BBF21BCAC7DD72E649,1,ECDCCEA84717F0E2459645C936E38425,2010 - 2012,25-35 Years,Married,Urban,N,N,N,N,N,N,Y,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Male,N,Y,N,Y,N,[m
[31m-4D005560E6A967508FCD9D30AEC40E5D,1,354EFBF01BBF24EFB47106A563B25E0C,2010 - 2012,25-35 Years,Married,Urban,N,N,N,N,N,N,Y,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Male,N,Y,N,Y,N,[m
[31m-352A9B67B8224D611C3D16F9390CFCB2,1,B5E4B8F1462578B4E0A62FF4084F28BF,2010 - 2012,25-35 Years,Married,Urban,N,N,N,N,N,N,Y,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Male,N,Y,N,Y,N,[m
[31m-8590809DC1C3406E2193D6088BD0168B,1,076860241C57497E40661D83C9D5576A,2010 - 2012,25-35 Years,Married,Urban,N,N,N,N,N,N,Y,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Male,N,Y,N,Y,N,[m
[31m-E0BF6095F741E0FFF17AA9FF1BDC4530,1,2F985CB82B398D8DDAFEBA73BA5D9DF4,2010 - 2012,25-35 Years,Married,Urban,N,N,N,N,N,N,Y,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Male,N,Y,N,Y,N,[m
[31m-D7269C2E204F58A636AC9D8FCE91CDD1,1,80E9522EFC1D06696FC232FDB82694FB,2010 - 2012,25-35 Years,Married,Urban,N,N,N,N,N,N,Y,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Male,N,Y,N,Y,N,[m
[31m-65110263827BE243BB278CABBE4BB8BF,1,38D5E8D0801CB1D8B9C032F9B5A6695C,2010 - 2012,25-35 Years,Married,Urban,N,N,N,N,N,N,Y,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Male,N,Y,N,Y,N,[m
[31m-3695B9ABE52A208CBD1A0D8B3DB1FEA9,1,88A394F2BD1863F5D8E13112A4E06E40,2010 - 2012,25-35 Years,Married,Urban,N,N,N,N,N,N,Y,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Unknown,Male,N,Y,N,Y,N,[m
[31m-B1AD788F6C8B781D18F6501B7002DC53,1,496084BC7B4A3EA6091A5