library(nhanesA)
library(tidyverse)
library(janitor)
library(survey)

source("00_fun_data_helpers.R")

# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BIOPRO_H.htm
# ALT in lab tests
# The current upper limit of serum ALT, though varied among laboratories, is generally around 40 IU/L.
biopro <- nhanes('BIOPRO_H') 
biopro_labels <- Hmisc::label(biopro)
biopro <- clear_labels(biopro)
biopro <- biopro %>%
  select(SEQN,LBXSATSI,LBXSTR) %>%
  na.omit() %>%
  rename(trigs=LBXSTR,
         alt=LBXSATSI) %>%
  mutate(trigs_cut=cut(trigs,c(0,200,500,6100)),
         alt_low = 1*(alt<60))

#biopro %>% janitor::tabyl(trigs_cut,alt_low)

# https://wwwn.cdc.gov/nchs/nhanes/2013-2014/demo_h.htm
demo <- nhanes('DEMO_H')
demo_labels <- Hmisc::label(demo)
demo <- clear_labels(demo)
#nhanesTranslate('DEMO_H', 'RIAGENDR')
demo_vars = c("RIAGENDR","RIDEXPRG","RIDRETH3")
demo <- nhanesTranslate('DEMO_H', demo_vars, data=demo)
demo <- demo %>% mutate(sex=RIAGENDR,pregnant=RIDEXPRG,race=RIDRETH3,age=RIDAGEYR)

# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/MCQ_H.htm
mcq <- nhanes('MCQ_H')
mcq_labels <- Hmisc::label(mcq)
mcq <- clear_labels(mcq)
# MCQ160b - Ever told had congestive heart failure
# MCQ160c - Ever told you had coronary heart disease
# MCQ160d - Ever told you had angina/angina pectoris
# MCQ160e - Ever told you had heart attack
# MCQ160f - Ever told you had a stroke
mcq_vars = c("MCQ160B","MCQ160C","MCQ160D",
              "MCQ160E","MCQ160F")
mcq <- nhanesTranslate('MCQ_H', mcq_vars, data=mcq)
mcq <- mcq %>% 
  select(SEQN,one_of(mcq_vars))
colnames(mcq)[-1] <- c("chf","chd","angina","MI","stroke")
mcq <- mcq%>%
  mutate(
    cvd = 1*((chd=="Yes")|(angina=="Yes")|(MI=="Yes")|(stroke=="Yes")),
    cvd_nohf = 1*(
    (chf=="No")&(
    (chd=="Yes")|(angina=="Yes")|(MI=="Yes")|(stroke=="Yes"))))
mcq%>%tabyl(cvd_nohf,cvd)

mydata <- left_join(demo,mcq)
mydata <- left_join(mydata,biopro)
mydata$cvd_nohf[is.na(mydata$cvd_nohf)] = 0

# tmp <- nhanesSearch("claudication",ystart=2013,ystop=2014,nchar=100)
# View(tmp)

#bmx <- nhanes("BMX_H")

# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/CDQ_H.htm
#cdq <- nhanes("CDQ_H")