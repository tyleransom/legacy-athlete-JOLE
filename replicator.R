library(tidyverse)
library(readxl)
library(magrittr)
library(tabulizer)
library(xtable)
library(scales)
library(ggrepel)

###############################################################################
# Table 1
###############################################################################

#------------------------------------------------------------------------------
# read in a bunch of tables from the expert reports
#------------------------------------------------------------------------------
tb31r <- read_excel("Inputs/TablesB3_1R.xlsx",col_names=F,skip=4) %>% 
    select(-`...1`) %>% filter(!is.na(`...3`)) %>%
    rename(variable = `...2`, 
           rejectW = `...3`,  admitW = `...4`,  totW = `...5`,
           rejectB = `...6`,  admitB = `...7`,  totB = `...8`,
           rejectH = `...9`,  admitH = `...10`, totH = `...11`,
           rejectA = `...12`, admitA = `...13`, totA = `...14`,
           reject  = `...15`, admit  = `...16`, tot  = `...17`) %>%
    add_row(variable = "Early action applicant", 
            rejectW = 0, admitW = 0, totW = 0,
            rejectB = 0, admitB = 0, totB = 0,
            rejectH = 0, admitH = 0, totH = 0,
            rejectA = 0, admitA = 0, totA = 0,
            reject  = 0, admit  = 0, tot  = 0,
            .after  = 4) %>%
    add_row(variable = "Athlete", 
            rejectW = 0, admitW = 0, totW = 0,
            rejectB = 0, admitB = 0, totB = 0,
            rejectH = 0, admitH = 0, totH = 0,
            rejectA = 0, admitA = 0, totA = 0,
            reject  = 0, admit  = 0, tot  = 0,
            .after  = 5) %>%
    add_row(variable = "Legacy", 
            rejectW = 0, admitW = 0, totW = 0,
            rejectB = 0, admitB = 0, totB = 0,
            rejectH = 0, admitH = 0, totH = 0,
            rejectA = 0, admitA = 0, totA = 0,
            reject  = 0, admit  = 0, tot  = 0,
            .after  = 6) %>%
    add_row(variable = "Faculty child", 
            rejectW = 0, admitW = 0, totW = 0,
            rejectB = 0, admitB = 0, totB = 0,
            rejectH = 0, admitH = 0, totH = 0,
            rejectA = 0, admitA = 0, totA = 0,
            reject  = 0, admit  = 0, tot  = 0,
            .after  = 7) %>%
    add_row(variable = "Staff child", 
            rejectW = 0, admitW = 0, totW = 0,
            rejectB = 0, admitB = 0, totB = 0,
            rejectH = 0, admitH = 0, totH = 0,
            rejectA = 0, admitA = 0, totA = 0,
            reject  = 0, admit  = 0, tot  = 0,
            .after  = 8) %>%
    add_row(variable = "Dean / Director's List", 
            rejectW = 0, admitW = 0, totW = 0,
            rejectB = 0, admitB = 0, totB = 0,
            rejectH = 0, admitH = 0, totH = 0,
            rejectA = 0, admitA = 0, totA = 0,
            reject  = 0, admit  = 0, tot  = 0,
            .after  = 9)

tb32r <- read_excel("Inputs/TablesB3_2R.xlsx",col_names=F,skip=4) %>% 
    select(-`...1`) %>% filter(!is.na(`...3`)) %>%
    rename(variable = `...2`, 
           rejectW = `...3`,  admitW = `...4`,  totW = `...5`,
           rejectB = `...6`,  admitB = `...7`,  totB = `...8`,
           rejectH = `...9`,  admitH = `...10`, totH = `...11`,
           rejectA = `...12`, admitA = `...13`, totA = `...14`,
           reject  = `...15`, admit  = `...16`, tot  = `...17`) %>%
    add_row(variable = "Athlete", 
            rejectW = 0, admitW = 0, totW = 0,
            rejectB = 0, admitB = 0, totB = 0,
            rejectH = 0, admitH = 0, totH = 0,
            rejectA = 0, admitA = 0, totA = 0,
            reject  = 0, admit  = 0, tot  = 0,
            .before = 6)

tb32 <- read_excel("Inputs/TablesB3_2.xlsx",col_names=F,skip=4) %>% 
    select(-`...1`) %>% filter(!is.na(`...3`)) %>%
    rename(variable = `...2`, 
           rejectW = `...3`,  admitW = `...4`,  totW = `...5`,
           rejectB = `...6`,  admitB = `...7`,  totB = `...8`,
           rejectH = `...9`,  admitH = `...10`, totH = `...11`,
           rejectA = `...12`, admitA = `...13`, totA = `...14`,
           reject  = `...15`, admit  = `...16`, tot  = `...17`)

t51r  <-  read_excel("Inputs/Tables5_1_7R.xlsx",col_names=F,range="B5:M16") %>% 
    select(-`...7`) %>% filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           N.W = `...2`, 
           N.B = `...3`, 
           N.H = `...4`,
           N.A = `...5`,
           N.T = `...6`,
           S.W = `...8`, 
           S.B = `...9`, 
           S.H = `...10`,
           S.A = `...11`,
           S.T = `...12`)
t51r %<>% mutate(
    S.W = replace(S.W,acad.idx=="Total",N.W[11]),
    S.B = replace(S.B,acad.idx=="Total",N.B[11]),
    S.H = replace(S.H,acad.idx=="Total",N.H[11]),
    S.A = replace(S.A,acad.idx=="Total",N.A[11]),
    S.T = replace(S.T,acad.idx=="Total",N.T[11])
)

tb51r <-   read_excel("Inputs/TablesB5_1_6R.xlsx",col_names=F,range="B5:M16") %>% 
    select(-`...7`) %>% filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           N.W = `...2`, 
           N.B = `...3`, 
           N.H = `...4`,
           N.A = `...5`,
           N.T = `...6`,
           S.W = `...8`, 
           S.B = `...9`, 
           S.H = `...10`,
           S.A = `...11`,
           S.T = `...12`)
tb51r %<>% mutate(
    S.W = replace(S.W,acad.idx=="Total",N.W[11]),
    S.B = replace(S.B,acad.idx=="Total",N.B[11]),
    S.H = replace(S.H,acad.idx=="Total",N.H[11]),
    S.A = replace(S.A,acad.idx=="Total",N.A[11]),
    S.T = replace(S.T,acad.idx=="Total",N.T[11])
)

t52r  <- read_excel("Inputs/Tables5_1_7R.xlsx",col_names=F,range="B22:G33") %>% 
    filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           adm.rate.W = `...2`, 
           adm.rate.B = `...3`, 
           adm.rate.H = `...4`,
           adm.rate.A = `...5`,
           adm.rate.T = `...6`) %>%
    mutate(
        adm.rate.W = adm.rate.W*100, 
        adm.rate.B = adm.rate.B*100, 
        adm.rate.H = adm.rate.H*100,
        adm.rate.A = adm.rate.A*100,
        adm.rate.T = adm.rate.T*100
    )

tb52r <- read_excel("Inputs/TablesB5_1_6R.xlsx",col_names=F,range="B22:G33") %>% 
    filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           adm.rate.W = `...2`, 
           adm.rate.B = `...3`, 
           adm.rate.H = `...4`,
           adm.rate.A = `...5`,
           adm.rate.T = `...6`) %>%
    mutate(
        adm.rate.W = adm.rate.W*100, 
        adm.rate.B = adm.rate.B*100, 
        adm.rate.H = adm.rate.H*100,
        adm.rate.A = adm.rate.A*100,
        adm.rate.T = adm.rate.T*100
    )

# Import PDF (Exhibit D746) into R using tabulizer package
D746 <- extract_tables("https://github.com/tyleransom/SFFAvHarvard-Docs/raw/master/TrialExhibits/D746.pdf", method = "stream")
D746 <- D746[[1]] %>% as_tibble %>%
    select(V2,V4,V5,V6,V7) %>%
    rename(class=V2,A=V4,B=V5,HO=V6,M=V7) %>%
    separate(class, c("class","W"), sep=" ", extra = "merge") %>%
    drop_na %>% slice(-1) %>% filter(A!="") %>% 
    mutate(categ = c(rep("Athlete",7),rep("Legacy",7),rep("Dean",7),rep("Child",7),rep("ALDC",7))) %>%
    select(categ,class,W,B,HO,A,M) %>%
    mutate(categ = as.factor(categ), class = as.factor(class),
           W =as.numeric(gsub(",","",W)),
           B =as.numeric(gsub(",","",B)),
           HO=as.numeric(gsub(",","",HO)),
           A =as.numeric(gsub(",","",A)),
           M =as.numeric(gsub(",","",M)),
           T =W+B+HO+A+M)


# use Table B.3.1R to calculate admit rates by race for non-ALDCs
nonALDCadmitrateT <- tb31r %>% slice(1) %>% select(tot)
nonALDCadmitrateW <- tb31r %>% slice(1) %>% select(totW)
nonALDCadmitrateB <- tb31r %>% slice(1) %>% select(totB)
nonALDCadmitrateH <- tb31r %>% slice(1) %>% select(totH)
nonALDCadmitrateA <- tb31r %>% slice(1) %>% select(totA)
nALDCappT         <- tb31r %>% slice(42) %>% select(tot)

# use Table B.3.2 to calculate admit rates by race for athletes
totT <- tb32 %>% slice(42) %>% select(tot)  # total N applicants
totW <- tb32 %>% slice(42) %>% select(totW)
totB <- tb32 %>% slice(42) %>% select(totB)
totH <- tb32 %>% slice(42) %>% select(totH)
totA <- tb32 %>% slice(42) %>% select(totA)
AshrT <- tb32 %>% slice( 6) %>% select(tot)  # share of applicants who are athletes
AshrW <- tb32 %>% slice( 6) %>% select(totW)
AshrB <- tb32 %>% slice( 6) %>% select(totB)
AshrH <- tb32 %>% slice( 6) %>% select(totH)
AshrA <- tb32 %>% slice( 6) %>% select(totA)
AappT <- round(AshrT*totT/100) # number of athlete applicants
AappW <- round(AshrW*totW/100)
AappB <- round(AshrB*totB/100)
AappH <- round(AshrH*totH/100)
AappA <- round(AshrA*totA/100)

# use Table B.3.2R to calculate admit rates by race for legacies
totT <- tb32r %>% slice(42) %>% select(tot)  # total N applicants
totW <- tb32r %>% slice(42) %>% select(totW)
totB <- tb32r %>% slice(42) %>% select(totB)
totH <- tb32r %>% slice(42) %>% select(totH)
totA <- tb32r %>% slice(42) %>% select(totA)
LshrT <- tb32r %>% slice( 7) %>% select(tot)  # share of applicants who are legacies
LshrW <- tb32r %>% slice( 7) %>% select(totW)
LshrB <- tb32r %>% slice( 7) %>% select(totB)
LshrH <- tb32r %>% slice( 7) %>% select(totH)
LshrA <- tb32r %>% slice( 7) %>% select(totA)
LappT <- round(LshrT*totT/100) # number of legacy applicants
LappW <- round(LshrW*totW/100)
LappB <- round(LshrB*totB/100)
LappH <- round(LshrH*totH/100)
LappA <- round(LshrA*totA/100)

# use Table B.3.2R to calculate admit rates by race for dean/director
DshrT <- tb32r %>% slice(10) %>% select(tot)  # share of applicants who are on dean/director's list
DshrW <- tb32r %>% slice(10) %>% select(totW)
DshrB <- tb32r %>% slice(10) %>% select(totB)
DshrH <- tb32r %>% slice(10) %>% select(totH)
DshrA <- tb32r %>% slice(10) %>% select(totA)
DappT <- round(DshrT*totT/100) # number of dean/director applicants
DappW <- round(DshrW*totW/100)
DappB <- round(DshrB*totB/100)
DappH <- round(DshrH*totH/100)
DappA <- round(DshrA*totA/100)

# use Table B.3.2R to calculate admit rates of children of faculty/staff
FshrT <- tb32r %>% slice( 8) %>% select(tot)  # share of applicants who are children of faculty
FshrW <- tb32r %>% slice( 8) %>% select(totW)
FshrB <- tb32r %>% slice( 8) %>% select(totB)
FshrH <- tb32r %>% slice( 8) %>% select(totH)
FshrA <- tb32r %>% slice( 8) %>% select(totA)
SshrT <- tb32r %>% slice( 9) %>% select(tot)  # share of applicants who are children of staff
SshrW <- tb32r %>% slice( 9) %>% select(totW)
SshrB <- tb32r %>% slice( 9) %>% select(totB)
SshrH <- tb32r %>% slice( 9) %>% select(totH)
SshrA <- tb32r %>% slice( 9) %>% select(totA)
FappT <- round(FshrT*totT/100) # number of children of faculty applicants
FappW <- round(FshrW*totW/100)
FappB <- round(FshrB*totB/100)
FappH <- round(FshrH*totH/100)
FappA <- round(FshrA*totA/100)
SappT <- round(SshrT*totT/100) # number of children of staff applicants
SappW <- round(SshrW*totW/100)
SappB <- round(SshrB*totB/100)
SappH <- round(SshrH*totH/100)
SappA <- round(SshrA*totA/100)
CappT <- FappT+SappT # Add together
CappW <- FappW+SappW
CappB <- FappB+SappB
CappH <- FappH+SappH
CappA <- FappA+SappA

# use Table B.3.1R to calculate distribution of non-ALDC admits
nonALDCT <- tb31r %>% slice(42) %>% select(admit)  # total N admits limited sample
nonALDCW <- tb31r %>% slice(42) %>% select(admitW)
nonALDCB <- tb31r %>% slice(42) %>% select(admitB)
nonALDCH <- tb31r %>% slice(42) %>% select(admitH)
nonALDCA <- tb31r %>% slice(42) %>% select(admitA)

# use Table B.3.2 to calculate distribution of athlete admits
totT <- tb32 %>% slice(42) %>% select(admit)  # total N admits
totW <- tb32 %>% slice(42) %>% select(admitW)
totB <- tb32 %>% slice(42) %>% select(admitB)
totH <- tb32 %>% slice(42) %>% select(admitH)
totA <- tb32 %>% slice(42) %>% select(admitA)
AshrT <- tb32 %>% slice( 6) %>% select(admit)  # share of admits who are athletes
AshrW <- tb32 %>% slice( 6) %>% select(admitW)
AshrB <- tb32 %>% slice( 6) %>% select(admitB)
AshrH <- tb32 %>% slice( 6) %>% select(admitH)
AshrA <- tb32 %>% slice( 6) %>% select(admitA)
AadmT <- round(AshrT*totT/100) # number of athlete admits
AadmW <- round(AshrW*totW/100)
AadmB <- round(AshrB*totB/100)
AadmH <- round(AshrH*totH/100)
AadmA <- round(AshrA*totA/100)

# use Table B.3.2R to calculate distribution of legacy admits
totT <- tb32r %>% slice(42) %>% select(admit)  # total N admits
totW <- tb32r %>% slice(42) %>% select(admitW)
totB <- tb32r %>% slice(42) %>% select(admitB)
totH <- tb32r %>% slice(42) %>% select(admitH)
totA <- tb32r %>% slice(42) %>% select(admitA)
LshrT <- tb32r %>% slice( 7) %>% select(admit)  # share of admits who are legacies
LshrW <- tb32r %>% slice( 7) %>% select(admitW)
LshrB <- tb32r %>% slice( 7) %>% select(admitB)
LshrH <- tb32r %>% slice( 7) %>% select(admitH)
LshrA <- tb32r %>% slice( 7) %>% select(admitA)
LadmT <- round(LshrT*totT/100) # number of legacy admits
LadmW <- round(LshrW*totW/100)
LadmB <- round(LshrB*totB/100)
LadmH <- round(LshrH*totH/100)
LadmA <- round(LshrA*totA/100)

# use Table B.3.2R to calculate distribution of dean/director admits
DshrT <- tb32r %>% slice(10) %>% select(admit)  # share of admits who are on dean/director's list
DshrW <- tb32r %>% slice(10) %>% select(admitW)
DshrB <- tb32r %>% slice(10) %>% select(admitB)
DshrH <- tb32r %>% slice(10) %>% select(admitH)
DshrA <- tb32r %>% slice(10) %>% select(admitA)
DadmT <- round(DshrT*totT/100) # number of dean/director admits
DadmW <- round(DshrW*totW/100)
DadmB <- round(DshrB*totB/100)
DadmH <- round(DshrH*totH/100)
DadmA <- round(DshrA*totA/100)

# use Table B.3.2R to calculate distribution of children of faculty/staff admits
FshrT <- tb32r %>% slice( 8) %>% select(admit)  # share of admits who are children of faculty
FshrW <- tb32r %>% slice( 8) %>% select(admitW)
FshrB <- tb32r %>% slice( 8) %>% select(admitB)
FshrH <- tb32r %>% slice( 8) %>% select(admitH)
FshrA <- tb32r %>% slice( 8) %>% select(admitA)
SshrT <- tb32r %>% slice( 9) %>% select(admit)  # share of admits who are children of staff
SshrW <- tb32r %>% slice( 9) %>% select(admitW)
SshrB <- tb32r %>% slice( 9) %>% select(admitB)
SshrH <- tb32r %>% slice( 9) %>% select(admitH)
SshrA <- tb32r %>% slice( 9) %>% select(admitA)
FadmT <- round(FshrT*totT/100) # number of children of faculty admits
FadmW <- round(FshrW*totW/100)
FadmB <- round(FshrB*totB/100)
FadmH <- round(FshrH*totH/100)
FadmA <- round(FshrA*totA/100)
SadmT <- round(SshrT*totT/100) # number of children of staff admits
SadmW <- round(SshrW*totW/100)
SadmB <- round(SshrB*totB/100)
SadmH <- round(SshrH*totH/100)
SadmA <- round(SshrA*totA/100)
CadmT <- FadmT+SadmT # Add together
CadmW <- FadmW+SadmW
CadmB <- FadmB+SadmB
CadmH <- FadmH+SadmH
CadmA <- FadmA+SadmA

# use Table B.3.2R to calculate number of admits who aren't athletes
admT.except.A <- tb32r %>% slice(42) %>% select(admit)  # total N admits
admW.except.A <- tb32r %>% slice(42) %>% select(admitW)
admB.except.A <- tb32r %>% slice(42) %>% select(admitB)
admH.except.A <- tb32r %>% slice(42) %>% select(admitH)
admA.except.A <- tb32r %>% slice(42) %>% select(admitA)

TadmW <- admW.except.A+AadmW
TadmB <- admB.except.A+AadmB
TadmH <- admH.except.A+AadmH
TadmA <- admA.except.A+AadmA

# use B.3.2R to get total number of applicants
TappW <- tb32r %>% slice(42) %>% select(totW)
TappB <- tb32r %>% slice(42) %>% select(totB)
TappH <- tb32r %>% slice(42) %>% select(totH)
TappA <- tb32r %>% slice(42) %>% select(totA)

TappW <- TappW + AappW
TappB <- TappB + AappB
TappH <- TappH + AappH
TappA <- TappA + AappA

# use B.3.2R to get total number of applicants
TappW.nALDC <- tb31r %>% slice(42) %>% select(totW)
TappB.nALDC <- tb31r %>% slice(42) %>% select(totB)
TappH.nALDC <- tb31r %>% slice(42) %>% select(totH)
TappA.nALDC <- tb31r %>% slice(42) %>% select(totA)


#------------------------------------------------------------------------------
# Panel A: Admit rates by race-ALDC cell 
#------------------------------------------------------------------------------
T1a <- tibble(race = c("White","Black","Hispanic","Asian"), nonALDC = 0, athlete = 0, legacy = 0, dean = 0, faculty = 0)

T1a %<>% mutate(nonALDC = replace(nonALDC,race=="White"   ,nonALDCadmitrateW[[1]]))
T1a %<>% mutate(nonALDC = replace(nonALDC,race=="Black"   ,nonALDCadmitrateB[[1]]))
T1a %<>% mutate(nonALDC = replace(nonALDC,race=="Hispanic",nonALDCadmitrateH[[1]]))
T1a %<>% mutate(nonALDC = replace(nonALDC,race=="Asian"   ,nonALDCadmitrateA[[1]]))
T1a %<>% mutate(athlete = replace(athlete,race=="White"   ,AadmW[[1]]/AappW[[1]]*100))
T1a %<>% mutate(athlete = replace(athlete,race=="Black"   ,AadmB[[1]]/AappB[[1]]*100))
T1a %<>% mutate(athlete = replace(athlete,race=="Hispanic",AadmH[[1]]/AappH[[1]]*100))
T1a %<>% mutate(athlete = replace(athlete,race=="Asian"   ,AadmA[[1]]/AappA[[1]]*100))
T1a %<>% mutate(legacy  = replace(legacy ,race=="White"   ,LadmW[[1]]/LappW[[1]]*100))
T1a %<>% mutate(legacy  = replace(legacy ,race=="Black"   ,LadmB[[1]]/LappB[[1]]*100))
T1a %<>% mutate(legacy  = replace(legacy ,race=="Hispanic",LadmH[[1]]/LappH[[1]]*100))
T1a %<>% mutate(legacy  = replace(legacy ,race=="Asian"   ,LadmA[[1]]/LappA[[1]]*100))
T1a %<>% mutate(dean    = replace(dean   ,race=="White"   ,DadmW[[1]]/DappW[[1]]*100))
T1a %<>% mutate(dean    = replace(dean   ,race=="Black"   ,DadmB[[1]]/DappB[[1]]*100))
T1a %<>% mutate(dean    = replace(dean   ,race=="Hispanic",DadmH[[1]]/DappH[[1]]*100))
T1a %<>% mutate(dean    = replace(dean   ,race=="Asian"   ,DadmA[[1]]/DappA[[1]]*100))
T1a %<>% mutate(faculty = replace(faculty,race=="White"   ,CadmW[[1]]/CappW[[1]]*100))
T1a %<>% mutate(faculty = replace(faculty,race=="Black"   ,CadmB[[1]]/CappB[[1]]*100))
T1a %<>% mutate(faculty = replace(faculty,race=="Hispanic",CadmH[[1]]/CappH[[1]]*100))
T1a %<>% mutate(faculty = replace(faculty,race=="Asian"   ,CadmA[[1]]/CappA[[1]]*100))

# Export to LaTeX
T1a %>% xtable(caption="Table 1a") %>% print(file="Outputs/T1a.tex", floating=T, type="latex", booktabs=T, include.rownames=F, caption.placement="top")


#------------------------------------------------------------------------------
# Panel B: Racial distribution of applicants by ALDC category 
#------------------------------------------------------------------------------
T1b <- tibble(race = c("White","Black","Hispanic","Asian"), nonALDC = 0, athlete = 0, legacy = 0, dean = 0, faculty = 0)

T1b %<>% mutate(nonALDC = replace(nonALDC,race=="White"   ,TappW.nALDC[[1]]/nALDCappT[[1]]*100))
T1b %<>% mutate(nonALDC = replace(nonALDC,race=="Black"   ,TappB.nALDC[[1]]/nALDCappT[[1]]*100))
T1b %<>% mutate(nonALDC = replace(nonALDC,race=="Hispanic",TappH.nALDC[[1]]/nALDCappT[[1]]*100))
T1b %<>% mutate(nonALDC = replace(nonALDC,race=="Asian"   ,TappA.nALDC[[1]]/nALDCappT[[1]]*100))
T1b %<>% mutate(athlete = replace(athlete,race=="White"   ,AappW[[1]]/AappT[[1]]*100))
T1b %<>% mutate(athlete = replace(athlete,race=="Black"   ,AappB[[1]]/AappT[[1]]*100))
T1b %<>% mutate(athlete = replace(athlete,race=="Hispanic",AappH[[1]]/AappT[[1]]*100))
T1b %<>% mutate(athlete = replace(athlete,race=="Asian"   ,AappA[[1]]/AappT[[1]]*100))
T1b %<>% mutate(legacy  = replace(legacy ,race=="White"   ,LappW[[1]]/LappT[[1]]*100))
T1b %<>% mutate(legacy  = replace(legacy ,race=="Black"   ,LappB[[1]]/LappT[[1]]*100))
T1b %<>% mutate(legacy  = replace(legacy ,race=="Hispanic",LappH[[1]]/LappT[[1]]*100))
T1b %<>% mutate(legacy  = replace(legacy ,race=="Asian"   ,LappA[[1]]/LappT[[1]]*100))
T1b %<>% mutate(dean    = replace(dean   ,race=="White"   ,DappW[[1]]/DappT[[1]]*100))
T1b %<>% mutate(dean    = replace(dean   ,race=="Black"   ,DappB[[1]]/DappT[[1]]*100))
T1b %<>% mutate(dean    = replace(dean   ,race=="Hispanic",DappH[[1]]/DappT[[1]]*100))
T1b %<>% mutate(dean    = replace(dean   ,race=="Asian"   ,DappA[[1]]/DappT[[1]]*100))
T1b %<>% mutate(faculty = replace(faculty,race=="White"   ,CappW[[1]]/CappT[[1]]*100))
T1b %<>% mutate(faculty = replace(faculty,race=="Black"   ,CappB[[1]]/CappT[[1]]*100))
T1b %<>% mutate(faculty = replace(faculty,race=="Hispanic",CappH[[1]]/CappT[[1]]*100))
T1b %<>% mutate(faculty = replace(faculty,race=="Asian"   ,CappA[[1]]/CappT[[1]]*100))

# Export to LaTeX
T1b %>% xtable(caption="Table 1b") %>% print(file="Outputs/T1b.tex", floating=T, type="latex", booktabs=T, include.rownames=F, caption.placement="top")


#------------------------------------------------------------------------------
# Panel C: Racial distribution of admits by ALDC category
#------------------------------------------------------------------------------
T1c <- tibble(race = c("White","Black","Hispanic","Asian"), nonALDC = 0, athlete = 0, legacy = 0, dean = 0, faculty = 0)

# Put numbers in table
T1c %<>% mutate(nonALDC = replace(nonALDC,race=="White"   ,nonALDCW[[1]]/nonALDCT[[1]]*100))
T1c %<>% mutate(nonALDC = replace(nonALDC,race=="Black"   ,nonALDCB[[1]]/nonALDCT[[1]]*100))
T1c %<>% mutate(nonALDC = replace(nonALDC,race=="Hispanic",nonALDCH[[1]]/nonALDCT[[1]]*100))
T1c %<>% mutate(nonALDC = replace(nonALDC,race=="Asian"   ,nonALDCA[[1]]/nonALDCT[[1]]*100))
T1c %<>% mutate(athlete = replace(athlete,race=="White"   ,AadmW[[1]]/AadmT[[1]]*100))
T1c %<>% mutate(athlete = replace(athlete,race=="Black"   ,AadmB[[1]]/AadmT[[1]]*100))
T1c %<>% mutate(athlete = replace(athlete,race=="Hispanic",AadmH[[1]]/AadmT[[1]]*100))
T1c %<>% mutate(athlete = replace(athlete,race=="Asian"   ,AadmA[[1]]/AadmT[[1]]*100))
T1c %<>% mutate(legacy  = replace(legacy ,race=="White"   ,LadmW[[1]]/LadmT[[1]]*100))
T1c %<>% mutate(legacy  = replace(legacy ,race=="Black"   ,LadmB[[1]]/LadmT[[1]]*100))
T1c %<>% mutate(legacy  = replace(legacy ,race=="Hispanic",LadmH[[1]]/LadmT[[1]]*100))
T1c %<>% mutate(legacy  = replace(legacy ,race=="Asian"   ,LadmA[[1]]/LadmT[[1]]*100))
T1c %<>% mutate(dean    = replace(dean   ,race=="White"   ,DadmW[[1]]/DadmT[[1]]*100))
T1c %<>% mutate(dean    = replace(dean   ,race=="Black"   ,DadmB[[1]]/DadmT[[1]]*100))
T1c %<>% mutate(dean    = replace(dean   ,race=="Hispanic",DadmH[[1]]/DadmT[[1]]*100))
T1c %<>% mutate(dean    = replace(dean   ,race=="Asian"   ,DadmA[[1]]/DadmT[[1]]*100))
T1c %<>% mutate(faculty = replace(faculty,race=="White"   ,CadmW[[1]]/CadmT[[1]]*100))
T1c %<>% mutate(faculty = replace(faculty,race=="Black"   ,CadmB[[1]]/CadmT[[1]]*100))
T1c %<>% mutate(faculty = replace(faculty,race=="Hispanic",CadmH[[1]]/CadmT[[1]]*100))
T1c %<>% mutate(faculty = replace(faculty,race=="Asian"   ,CadmA[[1]]/CadmT[[1]]*100))

# Export to LaTeX
T1c %>% xtable(caption="Table 1c") %>% print(file="Outputs/T1c.tex", floating=T, type="latex", booktabs=T, include.rownames=F, caption.placement="top")


#------------------------------------------------------------------------------
# Panel D: Frequencies of ALDC admits by race
#------------------------------------------------------------------------------
T1d <- tibble(race = c("White","Black","Hispanic","Asian"), nonALDC = 0, athlete = 0, legacy = 0, dean = 0, faculty = 0)

T1d %<>% mutate(nonALDC = replace(nonALDC,race=="White"   ,nonALDCW[[1]]/TadmW[[1]]*100)) %>%
    mutate(nonALDC = replace(nonALDC,race=="Black"   ,nonALDCB[[1]]/TadmB[[1]]*100)) %>%
    mutate(nonALDC = replace(nonALDC,race=="Hispanic",nonALDCH[[1]]/TadmH[[1]]*100)) %>%
    mutate(nonALDC = replace(nonALDC,race=="Asian"   ,nonALDCA[[1]]/TadmA[[1]]*100)) %>%
    mutate(athlete = replace(athlete,race=="White"   ,AadmW[[1]]/TadmW[[1]]*100)) %>%
    mutate(athlete = replace(athlete,race=="Black"   ,AadmB[[1]]/TadmB[[1]]*100)) %>%
    mutate(athlete = replace(athlete,race=="Hispanic",AadmH[[1]]/TadmH[[1]]*100)) %>%
    mutate(athlete = replace(athlete,race=="Asian"   ,AadmA[[1]]/TadmA[[1]]*100)) %>%
    mutate(legacy  = replace(legacy, race=="White"   ,LadmW[[1]]/TadmW[[1]]*100)) %>%
    mutate(legacy  = replace(legacy, race=="Black"   ,LadmB[[1]]/TadmB[[1]]*100)) %>%
    mutate(legacy  = replace(legacy, race=="Hispanic",LadmH[[1]]/TadmH[[1]]*100)) %>%
    mutate(legacy  = replace(legacy, race=="Asian"   ,LadmA[[1]]/TadmA[[1]]*100)) %>%
    mutate(dean    = replace(dean,   race=="White"   ,DadmW[[1]]/TadmW[[1]]*100)) %>%
    mutate(dean    = replace(dean,   race=="Black"   ,DadmB[[1]]/TadmB[[1]]*100)) %>%
    mutate(dean    = replace(dean,   race=="Hispanic",DadmH[[1]]/TadmH[[1]]*100)) %>%
    mutate(dean    = replace(dean,   race=="Asian"   ,DadmA[[1]]/TadmA[[1]]*100)) %>%
    mutate(faculty = replace(faculty,race=="White"   ,CadmW[[1]]/TadmW[[1]]*100)) %>%
    mutate(faculty = replace(faculty,race=="Black"   ,CadmB[[1]]/TadmB[[1]]*100)) %>%
    mutate(faculty = replace(faculty,race=="Hispanic",CadmH[[1]]/TadmH[[1]]*100)) %>%
    mutate(faculty = replace(faculty,race=="Asian"   ,CadmA[[1]]/TadmA[[1]]*100))

# Export to LaTeX
T1d %>% xtable(caption="Table 1d") %>% print(file="Outputs/T1d.tex", floating=T, type="latex", booktabs=T, include.rownames=F, caption.placement="top")



###############################################################################
# Table 2: Ratings distribution by race and non-ALDC / LDC
###############################################################################

#------------------------------------------------------------------------------
# Demographics of all applicants by non-ALDC and LDC
#------------------------------------------------------------------------------
rownms       <- c("Admitted","Disadvantaged","First-generation college","Applied for fee waiver","Financial Aid","Application read by 3rd reader","N")

Tda.nALDC    <- tb31r %>% filter(variable %in% rownms) %>%
                          select(variable,starts_with("adm"),starts_with("rej")) %>%
                          mutate(across(c(variable),as_factor))

Tda.nALDCtot <- tb31r %>% filter(variable %in% rownms) %>%
                          select(variable,starts_with("tot")) %>%
                          mutate(across(c(variable),as_factor)) %>%
                          rename_with(~paste0(., ".nALDC"), starts_with("tot"))

Tda.all      <- tb32r %>% filter(variable %in% rownms) %>%
                          select(variable,starts_with("adm"),starts_with("rej")) %>%
                          mutate(across(c(variable),as_factor))

# Put B.3.1R and B.3.2R next to each other in the same table
Tda  <- left_join(Tda.nALDC,Tda.all, by = "variable", suffix = c(".nALDC",".all"))

# Get difference in Ns, copy Ns into each row, and create effective Ns
Tda %<>% mutate(
                nrW.nALDC  = last(rejectW.nALDC), # copy overall reject Ns into each row
                nrB.nALDC  = last(rejectB.nALDC),
                nrH.nALDC  = last(rejectH.nALDC),
                nrA.nALDC  = last(rejectA.nALDC),
                nrT.nALDC  = last( reject.nALDC),
                naW.nALDC  = last( admitW.nALDC), # copy overall admit Ns into each row
                naB.nALDC  = last( admitB.nALDC),
                naH.nALDC  = last( admitH.nALDC),
                naA.nALDC  = last( admitA.nALDC),
                naT.nALDC  = last(  admit.nALDC),
                nW.nALDC   = nrW.nALDC+naW.nALDC, # create total as sum of admit and reject
                nB.nALDC   = nrB.nALDC+naB.nALDC,
                nH.nALDC   = nrH.nALDC+naH.nALDC,
                nA.nALDC   = nrA.nALDC+naA.nALDC,
                nT.nALDC   = nrT.nALDC+naT.nALDC,
                nrW.all    = last(rejectW.all), # copy overall reject Ns into each row
                nrB.all    = last(rejectB.all),
                nrH.all    = last(rejectH.all),
                nrA.all    = last(rejectA.all),
                nrT.all    = last( reject.all),
                naW.all    = last( admitW.all), # copy overall admit Ns into each row
                naB.all    = last( admitB.all),
                naH.all    = last( admitH.all),
                naA.all    = last( admitA.all),
                naT.all    = last(  admit.all),
                nW.all     = nrW.all+naW.all, # create total as sum of admit and reject
                nB.all     = nrB.all+naB.all,
                nH.all     = nrH.all+naH.all,
                nA.all     = nrA.all+naA.all,
                nT.all     = nrT.all+naT.all,
                sW.nALDC   = (admitW.nALDC/100*naW.nALDC)+(rejectW.nALDC/100*nrW.nALDC), # Create effective Ns for each group
                sB.nALDC   = (admitB.nALDC/100*naB.nALDC)+(rejectB.nALDC/100*nrB.nALDC),
                sH.nALDC   = (admitH.nALDC/100*naH.nALDC)+(rejectH.nALDC/100*nrH.nALDC),
                sA.nALDC   = (admitA.nALDC/100*naA.nALDC)+(rejectA.nALDC/100*nrA.nALDC),
                sT.nALDC   = ( admit.nALDC/100*naT.nALDC)+( reject.nALDC/100*nrT.nALDC),
                sW.all     = (admitW.all  /100*naW.all  )+(rejectW.all  /100*nrW.all  ),
                sB.all     = (admitB.all  /100*naB.all  )+(rejectB.all  /100*nrB.all  ),
                sH.all     = (admitH.all  /100*naH.all  )+(rejectH.all  /100*nrH.all  ),
                sA.all     = (admitA.all  /100*naA.all  )+(rejectA.all  /100*nrA.all  ),
                sT.all     = ( admit.all  /100*naT.all  )+( reject.all  /100*nrT.all  ),
                nW.LDC     = nW.all - nW.nALDC, # Compute difference in overall Ns
                nB.LDC     = nB.all - nB.nALDC,
                nH.LDC     = nH.all - nH.nALDC,
                nA.LDC     = nA.all - nA.nALDC,
                nT.LDC     = nT.all - nT.nALDC,
                totW.LDC   = round((sW.all - sW.nALDC)/nW.LDC*100,2), # Re-compute means using difference in overall Ns as denominator
                totB.LDC   = round((sB.all - sB.nALDC)/nB.LDC*100,2),
                totH.LDC   = round((sH.all - sH.nALDC)/nH.LDC*100,2),
                totA.LDC   = round((sA.all - sA.nALDC)/nA.LDC*100,2),
                totT.LDC   = round((sT.all - sT.nALDC)/nT.LDC*100,2),
                totW.LDC   = replace(totW.LDC,variable=="N",last(nW.LDC)), # Fix Ns for LDC sample
                totB.LDC   = replace(totB.LDC,variable=="N",last(nB.LDC)),
                totH.LDC   = replace(totH.LDC,variable=="N",last(nH.LDC)),
                totA.LDC   = replace(totA.LDC,variable=="N",last(nA.LDC)),
                totT.LDC   = replace(totT.LDC,variable=="N",last(nT.LDC))
                )

# Now merge in non-ALDCs directly from report
Tda  <- left_join(Tda.nALDCtot,Tda, by = "variable")

# Rearrange columns (and only keep the ones I'm interested in)
Tda %<>% select(variable,ends_with("LDC")) %>% 
         select(variable,starts_with("totW"),starts_with("totB"),starts_with("totH"),starts_with("totA"),tot.nALDC,totT.LDC)

Tda %<>% slice(match(rownms,variable))


#------------------------------------------------------------------------------
# Demographics of admits by non-ALDC and LDC
#------------------------------------------------------------------------------
Tdb.nALDC <- tb31r %>% filter(variable %in% c("Disadvantaged","First-generation college","Applied for fee waiver","Financial Aid","Application read by 3rd reader","N")) %>%
                       select(variable,starts_with("admit")) %>%
                       mutate(across(c(variable),as_factor))


Tdb.all   <- tb32r %>% filter(variable %in% c("Disadvantaged","First-generation college","Applied for fee waiver","Financial Aid","Application read by 3rd reader","N")) %>%
                       select(variable,starts_with("admit")) %>%
                       mutate(across(c(variable),as_factor))


# Put B.3.1R and B.3.2R next to each other in the same table
Tdb <- left_join(Tdb.nALDC,Tdb.all, by = "variable", suffix = c(".nALDC",".all"))

# Get difference in Ns, copy Ns into each row, and create effective Ns
Tdb %<>% mutate(
                nW.nALDC   = last(admitW.nALDC), # copy overall Ns into each row
                nB.nALDC   = last(admitB.nALDC),
                nH.nALDC   = last(admitH.nALDC),
                nA.nALDC   = last(admitA.nALDC),
                nT.nALDC   = last( admit.nALDC),
                nW.all     = last(admitW.all),
                nB.all     = last(admitB.all),
                nH.all     = last(admitH.all),
                nA.all     = last(admitA.all),
                nT.all     = last( admit.all),
                sW.nALDC   = admitW.nALDC/100*nW.nALDC, # Create effective Ns for each group
                sB.nALDC   = admitB.nALDC/100*nB.nALDC,
                sH.nALDC   = admitH.nALDC/100*nH.nALDC,
                sA.nALDC   = admitA.nALDC/100*nA.nALDC,
                sT.nALDC   =  admit.nALDC/100*nT.nALDC,
                sW.all     = admitW.all  /100*nW.all,
                sB.all     = admitB.all  /100*nB.all,
                sH.all     = admitH.all  /100*nH.all,
                sA.all     = admitA.all  /100*nA.all,
                sT.all     =  admit.all  /100*nT.all,
                nW.LDC     = nW.all - nW.nALDC, # Compute difference in overall Ns
                nB.LDC     = nB.all - nB.nALDC,
                nH.LDC     = nH.all - nH.nALDC,
                nA.LDC     = nA.all - nA.nALDC,
                nT.LDC     = nT.all - nT.nALDC,
                admitW.LDC = round((sW.all - sW.nALDC)/nW.LDC*100,2), # Re-compute means using difference in overall Ns as denominator
                admitB.LDC = round((sB.all - sB.nALDC)/nB.LDC*100,2),
                admitH.LDC = round((sH.all - sH.nALDC)/nH.LDC*100,2),
                admitA.LDC = round((sA.all - sA.nALDC)/nA.LDC*100,2),
                admitT.LDC = round((sT.all - sT.nALDC)/nT.LDC*100,2),
                admitW.LDC = replace(admitW.LDC,variable=="N",last(nW.LDC)), # Fix Ns for LDC sample
                admitB.LDC = replace(admitB.LDC,variable=="N",last(nB.LDC)),
                admitH.LDC = replace(admitH.LDC,variable=="N",last(nH.LDC)),
                admitA.LDC = replace(admitA.LDC,variable=="N",last(nA.LDC)),
                admitT.LDC = replace(admitT.LDC,variable=="N",last(nT.LDC))
                )

# Rearrange columns (and only keep the ones I'm interested in)
Tdb %<>% select(variable,ends_with("LDC")) %>% 
         select(variable,starts_with("admitW"),starts_with("admitB"),starts_with("admitH"),starts_with("admitA"),admit.nALDC,admitT.LDC)

Tdb %<>% slice(match(rownms,variable))


# Import PDFs into R using tabulizer package
P621 <- extract_tables("https://github.com/tyleransom/SFFAvHarvard-Docs/raw/master/TrialExhibits/P621.pdf", method = "stream")
P623 <- extract_tables("https://github.com/tyleransom/SFFAvHarvard-Docs/raw/master/TrialExhibits/P623.pdf", method = "stream")

# Create data frames
df621a <- P621[[ 2]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Overall")
df621b <- P621[[ 3]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Academic")
df621c <- P621[[ 4]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Extracurricular")
df621d <- P621[[ 5]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Athletic")
df621e <- P621[[ 6]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Personal")
df621f <- P621[[ 7]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Teacher 1")
df621g <- P621[[ 8]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Teacher 2")
df621h <- P621[[ 9]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Counselor")
df621i <- P621[[10]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Alumni Personal")
df621j <- P621[[11]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Alumni Overall")
df621  <- bind_rows(list(df621a,df621b,df621c,df621d,df621e,df621f,df621g,df621h,df621i,df621j))

df623a <- P623[[ 2]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Overall")
df623b <- P623[[ 3]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Academic")
df623c <- P623[[ 4]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Extracurricular")
df623d <- P623[[ 5]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Athletic")
df623e <- P623[[ 6]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Personal")
df623f <- P623[[ 7]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Teacher 1")
df623g <- P623[[ 8]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Teacher 2")
df623h <- P623[[ 9]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Counselor")
df623i <- P623[[10]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Alumni Personal")
df623j <- P623[[11]][,seq(3,32)] %>% as_tibble() %>% filter(V1!="") %>% mutate(ratnum=c(1,2,3,4)) %>% mutate(rating="Alumni Overall")
df623  <- bind_rows(list(df623a,df623b,df623c,df623d,df623e,df623f,df623g,df623h,df623i,df623j))

# convert character columns to numeric and rename variables
df621 %<>% mutate(across(everything(),~str_replace(., "%"  ,  ""))) %>%
    mutate(across(everything(),~str_replace(., ","  ,  ""))) %>%
    mutate(across(everything(),~str_replace(., "\u2019", "0"))) %>%
    mutate(across(c(starts_with("V")),~as.numeric(.)))  %>%
    mutate(rating = as_factor(rating))  %>%
    rename(
        NadmW= V1, SadmW= V2, NrejW= V3, SrejW= V4, NtotW= V5, StotW= V6,
        NadmA= V7, SadmA= V8, NrejA= V9, SrejA=V10, NtotA=V11, StotA=V12,
        NadmB=V13, SadmB=V14, NrejB=V15, SrejB=V16, NtotB=V17, StotB=V18,
        NadmH=V19, SadmH=V20, NrejH=V21, SrejH=V22, NtotH=V23, StotH=V24,
        NadmT=V25, SadmT=V26, NrejT=V27, SrejT=V28, NtotT=V29, StotT=V30
    ) %>%
    replace(.,is.na(.),0)

df623 %<>% mutate(across(everything(),~str_replace(., "%"  ,  ""))) %>%
    mutate(across(everything(),~str_replace(., ","  ,  ""))) %>%
    mutate(across(everything(),~str_replace(., "\u2019", "0"))) %>%
    mutate(across(c(starts_with("V")),~as.numeric(.)))  %>%
    mutate(rating = as_factor(rating))  %>%
    rename(
        NadmW= V1, SadmW= V2, NrejW= V3, SrejW= V4, NtotW= V5, StotW= V6,
        NadmA= V7, SadmA= V8, NrejA= V9, SrejA=V10, NtotA=V11, StotA=V12,
        NadmB=V13, SadmB=V14, NrejB=V15, SrejB=V16, NtotB=V17, StotB=V18,
        NadmH=V19, SadmH=V20, NrejH=V21, SrejH=V22, NtotH=V23, StotH=V24,
        NadmT=V25, SadmT=V26, NrejT=V27, SrejT=V28, NtotT=V29, StotT=V30
    ) %>%
    replace(.,is.na(.),0)

# Now put in true total N's (not just N's corresponding to number of non-missing ratings)
df621 %<>% group_by(rating) %>%
    mutate(
        Nsum.admW=sum(NadmW),
        Nsum.admB=sum(NadmB),
        Nsum.admH=sum(NadmH),
        Nsum.admA=sum(NadmA),
        Nsum.admT=sum(NadmT),
        Nsum.rejW=sum(NrejW),
        Nsum.rejB=sum(NrejB),
        Nsum.rejH=sum(NrejH),
        Nsum.rejA=sum(NrejA),
        Nsum.rejT=sum(NrejT),
        Nsum.totW=sum(NtotW),
        Nsum.totB=sum(NtotB),
        Nsum.totH=sum(NtotH),
        Nsum.totA=sum(NtotA),
        Nsum.totT=sum(NtotT)
    ) %>%
    ungroup() %>%
    mutate(
        Ntru.admW=Tdb[6,2 ][[1]],
        Ntru.admB=Tdb[6,4 ][[1]],
        Ntru.admH=Tdb[6,6 ][[1]],
        Ntru.admA=Tdb[6,8 ][[1]],
        Ntru.admT=Tdb[6,10][[1]],
        Ntru.rejW=Tda[7,2 ][[1]]-Tdb[6,2 ][[1]],
        Ntru.rejB=Tda[7,4 ][[1]]-Tdb[6,4 ][[1]],
        Ntru.rejH=Tda[7,6 ][[1]]-Tdb[6,6 ][[1]],
        Ntru.rejA=Tda[7,8 ][[1]]-Tdb[6,8 ][[1]],
        Ntru.rejT=Tda[7,10][[1]]-Tdb[6,10][[1]],
        Ntru.totW=Tda[7,2 ][[1]],
        Ntru.totB=Tda[7,4 ][[1]],
        Ntru.totH=Tda[7,6 ][[1]],
        Ntru.totA=Tda[7,8 ][[1]],
        Ntru.totT=Tda[7,10][[1]]
    )

df623 %<>% group_by(rating) %>%
    mutate(
        Nsum.admW=sum(NadmW),
        Nsum.admB=sum(NadmB),
        Nsum.admH=sum(NadmH),
        Nsum.admA=sum(NadmA),
        Nsum.admT=sum(NadmT),
        Nsum.rejW=sum(NrejW),
        Nsum.rejB=sum(NrejB),
        Nsum.rejH=sum(NrejH),
        Nsum.rejA=sum(NrejA),
        Nsum.rejT=sum(NrejT),
        Nsum.totW=sum(NtotW),
        Nsum.totB=sum(NtotB),
        Nsum.totH=sum(NtotH),
        Nsum.totA=sum(NtotA),
        Nsum.totT=sum(NtotT)
    ) %>%
    ungroup() %>%
    mutate(
        Ntru.admW=Tdb[6,2 ][[1]]+Tdb[6,3 ][[1]],
        Ntru.admB=Tdb[6,4 ][[1]]+Tdb[6,5 ][[1]],
        Ntru.admH=Tdb[6,6 ][[1]]+Tdb[6,7 ][[1]],
        Ntru.admA=Tdb[6,8 ][[1]]+Tdb[6,9 ][[1]],
        Ntru.admT=Tdb[6,10][[1]]+Tdb[6,11][[1]],
        Ntru.rejW=(Tda[7,2 ][[1]]+Tda[7,3 ][[1]])-(Tdb[6,2 ][[1]]+Tdb[6,3 ][[1]]),
        Ntru.rejB=(Tda[7,4 ][[1]]+Tda[7,5 ][[1]])-(Tdb[6,4 ][[1]]+Tdb[6,5 ][[1]]),
        Ntru.rejH=(Tda[7,6 ][[1]]+Tda[7,7 ][[1]])-(Tdb[6,6 ][[1]]+Tdb[6,7 ][[1]]),
        Ntru.rejA=(Tda[7,8 ][[1]]+Tda[7,9 ][[1]])-(Tdb[6,8 ][[1]]+Tdb[6,9 ][[1]]),
        Ntru.rejT=(Tda[7,10][[1]]+Tda[7,11][[1]])-(Tdb[6,10][[1]]+Tdb[6,11][[1]]),
        Ntru.totW=Tda[7,2 ][[1]]+Tda[7,3 ][[1]],
        Ntru.totB=Tda[7,4 ][[1]]+Tda[7,5 ][[1]],
        Ntru.totH=Tda[7,6 ][[1]]+Tda[7,7 ][[1]],
        Ntru.totA=Tda[7,8 ][[1]]+Tda[7,9 ][[1]],
        Ntru.totT=Tda[7,10][[1]]+Tda[7,11][[1]]
    )

# Now assume that any difference in N's goes into worst rating categories
df621 %<>% mutate(
    N.new.admW = Ntru.admW-Nsum.admW,
    N.new.admB = Ntru.admB-Nsum.admB,
    N.new.admH = Ntru.admH-Nsum.admH,
    N.new.admA = Ntru.admA-Nsum.admA,
    N.new.admT = Ntru.admT-Nsum.admT,
    N.new.rejW = Ntru.rejW-Nsum.rejW,
    N.new.rejB = Ntru.rejB-Nsum.rejB,
    N.new.rejH = Ntru.rejH-Nsum.rejH,
    N.new.rejA = Ntru.rejA-Nsum.rejA,
    N.new.rejT = Ntru.rejT-Nsum.rejT,
    N.new.totW = Ntru.totW-Nsum.totW,
    N.new.totB = Ntru.totB-Nsum.totB,
    N.new.totH = Ntru.totH-Nsum.totH,
    N.new.totA = Ntru.totA-Nsum.totA,
    N.new.totT = Ntru.totT-Nsum.totT,
    N.new.admW = replace(N.new.admW,ratnum!="4",0),
    N.new.admB = replace(N.new.admB,ratnum!="4",0),
    N.new.admH = replace(N.new.admH,ratnum!="4",0),
    N.new.admA = replace(N.new.admA,ratnum!="4",0),
    N.new.admT = replace(N.new.admT,ratnum!="4",0),
    N.new.rejW = replace(N.new.rejW,ratnum!="4",0),
    N.new.rejB = replace(N.new.rejB,ratnum!="4",0),
    N.new.rejH = replace(N.new.rejH,ratnum!="4",0),
    N.new.rejA = replace(N.new.rejA,ratnum!="4",0),
    N.new.rejT = replace(N.new.rejT,ratnum!="4",0),
    N.new.totW = replace(N.new.totW,ratnum!="4",0),
    N.new.totB = replace(N.new.totB,ratnum!="4",0),
    N.new.totH = replace(N.new.totH,ratnum!="4",0),
    N.new.totA = replace(N.new.totA,ratnum!="4",0),
    N.new.totT = replace(N.new.totT,ratnum!="4",0),
    NadmW      = NadmW+N.new.admW,
    NadmB      = NadmB+N.new.admB,
    NadmH      = NadmH+N.new.admH,
    NadmA      = NadmA+N.new.admA,
    NadmT      = NadmT+N.new.admT,
    NrejW      = NrejW+N.new.rejW,
    NrejB      = NrejB+N.new.rejB,
    NrejH      = NrejH+N.new.rejH,
    NrejA      = NrejA+N.new.rejA,
    NrejT      = NrejT+N.new.rejT,
    NtotW      = NtotW+N.new.totW,
    NtotB      = NtotB+N.new.totB,
    NtotH      = NtotH+N.new.totH,
    NtotA      = NtotA+N.new.totA,
    NtotT      = NtotT+N.new.totT
) %>% select(-starts_with("N.new"),-starts_with("N.tru"),-starts_with("N.sum"))

df623 %<>% mutate(
    N.new.admW = Ntru.admW-Nsum.admW,
    N.new.admB = Ntru.admB-Nsum.admB,
    N.new.admH = Ntru.admH-Nsum.admH,
    N.new.admA = Ntru.admA-Nsum.admA,
    N.new.admT = Ntru.admT-Nsum.admT,
    N.new.rejW = Ntru.rejW-Nsum.rejW,
    N.new.rejB = Ntru.rejB-Nsum.rejB,
    N.new.rejH = Ntru.rejH-Nsum.rejH,
    N.new.rejA = Ntru.rejA-Nsum.rejA,
    N.new.rejT = Ntru.rejT-Nsum.rejT,
    N.new.totW = Ntru.totW-Nsum.totW,
    N.new.totB = Ntru.totB-Nsum.totB,
    N.new.totH = Ntru.totH-Nsum.totH,
    N.new.totA = Ntru.totA-Nsum.totA,
    N.new.totT = Ntru.totT-Nsum.totT,
    N.new.admW = replace(N.new.admW,ratnum!="4",0),
    N.new.admB = replace(N.new.admB,ratnum!="4",0),
    N.new.admH = replace(N.new.admH,ratnum!="4",0),
    N.new.admA = replace(N.new.admA,ratnum!="4",0),
    N.new.admT = replace(N.new.admT,ratnum!="4",0),
    N.new.rejW = replace(N.new.rejW,ratnum!="4",0),
    N.new.rejB = replace(N.new.rejB,ratnum!="4",0),
    N.new.rejH = replace(N.new.rejH,ratnum!="4",0),
    N.new.rejA = replace(N.new.rejA,ratnum!="4",0),
    N.new.rejT = replace(N.new.rejT,ratnum!="4",0),
    N.new.totW = replace(N.new.totW,ratnum!="4",0),
    N.new.totB = replace(N.new.totB,ratnum!="4",0),
    N.new.totH = replace(N.new.totH,ratnum!="4",0),
    N.new.totA = replace(N.new.totA,ratnum!="4",0),
    N.new.totT = replace(N.new.totT,ratnum!="4",0),
    NadmW      = NadmW+N.new.admW,
    NadmB      = NadmB+N.new.admB,
    NadmH      = NadmH+N.new.admH,
    NadmA      = NadmA+N.new.admA,
    NadmT      = NadmT+N.new.admT,
    NrejW      = NrejW+N.new.rejW,
    NrejB      = NrejB+N.new.rejB,
    NrejH      = NrejH+N.new.rejH,
    NrejA      = NrejA+N.new.rejA,
    NrejT      = NrejT+N.new.rejT,
    NtotW      = NtotW+N.new.totW,
    NtotB      = NtotB+N.new.totB,
    NtotH      = NtotH+N.new.totH,
    NtotA      = NtotA+N.new.totA,
    NtotT      = NtotT+N.new.totT
) %>% select(-starts_with("N.new"),-starts_with("N.tru"),-starts_with("N.sum"))

# Now take difference in N across two tables
df625 <- left_join(df621,df623, by=c("rating","ratnum"), suffix = c(".nALDC",".all")) %>%
    mutate(
        g3plus    = ratnum %in% c(1,2),
        NadmW.LDC = NadmW.all - NadmW.nALDC,
        NrejW.LDC = NrejW.all - NrejW.nALDC,
        NtotW.LDC = NtotW.all - NtotW.nALDC,
        NadmB.LDC = NadmB.all - NadmB.nALDC,
        NrejB.LDC = NrejB.all - NrejB.nALDC,
        NtotB.LDC = NtotB.all - NtotB.nALDC,
        NadmH.LDC = NadmH.all - NadmH.nALDC,
        NrejH.LDC = NrejH.all - NrejH.nALDC,
        NtotH.LDC = NtotH.all - NtotH.nALDC,
        NadmA.LDC = NadmA.all - NadmA.nALDC,
        NrejA.LDC = NrejA.all - NrejA.nALDC,
        NtotA.LDC = NtotA.all - NtotA.nALDC,
        NadmT.LDC = NadmT.all - NadmT.nALDC,
        NrejT.LDC = NrejT.all - NrejT.nALDC,
        NtotT.LDC = NtotT.all - NtotT.nALDC,
    ) %>%
    select(rating,ratnum,starts_with("N"),g3plus)

# create shares for applicants
df625.ap1   <- df625 %>% group_by(rating,g3plus) %>% 
    summarize(across(c(starts_with("N")),sum)) %>%
    select(rating,starts_with("Ntot"),g3plus)
df625.ap2   <- df625.ap1 %>%
    group_by(rating) %>% 
    mutate(across(c(starts_with("N")),sum))
df625.app   <- left_join(df625.ap1,df625.ap2, by=c("rating","g3plus"), suffix = c(".1",".2")) %>%
    filter(g3plus==T) %>%
    ungroup() %>%
    mutate(
        StotW.nALDC = NtotW.nALDC.1/NtotW.nALDC.2*100,
        StotB.nALDC = NtotB.nALDC.1/NtotB.nALDC.2*100,
        StotH.nALDC = NtotH.nALDC.1/NtotH.nALDC.2*100,
        StotA.nALDC = NtotA.nALDC.1/NtotA.nALDC.2*100,
        StotW.LDC   = NtotW.LDC.1  /NtotW.LDC.2  *100,
        StotB.LDC   = NtotB.LDC.1  /NtotB.LDC.2  *100,
        StotH.LDC   = NtotH.LDC.1  /NtotH.LDC.2  *100,
        StotA.LDC   = NtotA.LDC.1  /NtotA.LDC.2  *100,
    )
df625.appN  <- df625.app %>% select(rating,starts_with("Ntot")) %>% select(rating,ends_with(".1"))

df625.app %<>% select(rating,StotW.nALDC,StotW.LDC,StotB.nALDC,StotB.LDC,StotH.nALDC,StotH.LDC,StotA.nALDC,StotA.LDC) %>%
               rename(Stot.nALDC.W = StotW.nALDC, Stot.nALDC.B = StotB.nALDC, Stot.nALDC.H = StotH.nALDC, Stot.nALDC.A = StotA.nALDC, Stot.LDC.W = StotW.LDC, Stot.LDC.B = StotB.LDC, Stot.LDC.H = StotH.LDC, Stot.LDC.A = StotA.LDC) %>%
               pivot_longer(cols = !rating, names_to = "raceLDC", names_prefix = "Stot", values_to = "share", values_drop_na = TRUE) %>%
               separate(raceLDC, c("drop","LDC","race"), sep="\\.") %>%
               select(-drop) %>%
               pivot_wider(names_from = LDC, values_from = share) %>%
               rename(app.nALDC = nALDC, app.LDC = LDC)

# create shares for admits
df625.ad1 <- df625 %>% group_by(rating,g3plus) %>% 
    summarize(across(c(starts_with("N")),sum)) %>%
    select(rating,starts_with("Nadm"),g3plus)
df625.ad2 <- df625.ad1 %>%
    group_by(rating) %>% 
    mutate(across(c(starts_with("N")),sum))
df625.adm <- left_join(df625.ad1,df625.ad2, by=c("rating","g3plus"), suffix = c(".1",".2")) %>%
    filter(g3plus==T) %>%
    ungroup() %>%
    mutate(
        SadmW.nALDC = NadmW.nALDC.1/NadmW.nALDC.2*100,
        SadmB.nALDC = NadmB.nALDC.1/NadmB.nALDC.2*100,
        SadmH.nALDC = NadmH.nALDC.1/NadmH.nALDC.2*100,
        SadmA.nALDC = NadmA.nALDC.1/NadmA.nALDC.2*100,
        SadmW.LDC   = NadmW.LDC.1  /NadmW.LDC.2  *100,
        SadmB.LDC   = NadmB.LDC.1  /NadmB.LDC.2  *100,
        SadmH.LDC   = NadmH.LDC.1  /NadmH.LDC.2  *100,
        SadmA.LDC   = NadmA.LDC.1  /NadmA.LDC.2  *100,
    ) %>%
    select(rating,SadmW.nALDC,SadmW.LDC,SadmB.nALDC,SadmB.LDC,SadmH.nALDC,SadmH.LDC,SadmA.nALDC,SadmA.LDC) %>%
    rename(Sadm.nALDC.W = SadmW.nALDC, Sadm.nALDC.B = SadmB.nALDC, Sadm.nALDC.H = SadmH.nALDC, Sadm.nALDC.A = SadmA.nALDC, Sadm.LDC.W = SadmW.LDC, Sadm.LDC.B = SadmB.LDC, Sadm.LDC.H = SadmH.LDC, Sadm.LDC.A = SadmA.LDC) %>%
    pivot_longer(cols = !rating, names_to = "raceLDC", names_prefix = "Stot", values_to = "share", values_drop_na = TRUE) %>%
    separate(raceLDC, c("drop","LDC","race"), sep="\\.") %>%
    select(-drop) %>%
    pivot_wider(names_from = LDC, values_from = share) %>%
    rename(adm.nALDC = nALDC, adm.LDC = LDC)

df625joined <- left_join(df625.app,df625.adm, by=c("rating","race"))


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Bounds for athletes
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Read in Tables that will be used later
tb51        <-   read_excel("Inputs/TablesB5_1_6.xlsx",col_names=F,range="B5:M16") %>% 
    select(-`...7`) %>% filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           N.W = `...2`, 
           N.B = `...3`, 
           N.H = `...4`,
           N.A = `...5`,
           N.T = `...6`,
           S.W = `...8`, 
           S.B = `...9`, 
           S.H = `...10`,
           S.A = `...11`,
           S.T = `...12`)
tb51        %<>% mutate(
    S.W = replace(S.W,acad.idx=="Total",N.W[11]),
    S.B = replace(S.B,acad.idx=="Total",N.B[11]),
    S.H = replace(S.H,acad.idx=="Total",N.H[11]),
    S.A = replace(S.A,acad.idx=="Total",N.A[11]),
    S.T = replace(S.T,acad.idx=="Total",N.T[11])
)

tb52        <- read_excel("Inputs/TablesB5_1_6.xlsx",col_names=F,range="B22:G33") %>% 
    filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           adm.rate.W = `...2`, 
           adm.rate.B = `...3`, 
           adm.rate.H = `...4`,
           adm.rate.A = `...5`,
           adm.rate.T = `...6`) %>%
    mutate(
        adm.rate.W = adm.rate.W*100, 
        adm.rate.B = adm.rate.B*100, 
        adm.rate.H = adm.rate.H*100,
        adm.rate.A = adm.rate.A*100,
        adm.rate.T = adm.rate.T*100
    )

tb53r.acad  <-  read_excel("Inputs/TablesB5_1_6R.xlsx",col_names=F,range="B38:G50") %>% 
    filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           S.W = `...2`, 
           S.B = `...3`, 
           S.H = `...4`,
           S.A = `...5`,
           S.T = `...6`)
tb53r.xtra  <-  read_excel("Inputs/TablesB5_1_6R.xlsx",col_names=F,range="I38:M50") %>% 
    filter(!is.na(`...1`)) %>%
    rename(S.W = `...1`, 
           S.B = `...2`, 
           S.H = `...3`,
           S.A = `...4`,
           S.T = `...5`) %>%
    mutate(acad.idx = tb53r.acad$acad.idx)
tb54r.tea1  <-  read_excel("Inputs/TablesB5_1_6R.xlsx",col_names=F,range="B56:G68") %>% 
    filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           S.W = `...2`, 
           S.B = `...3`, 
           S.H = `...4`,
           S.A = `...5`,
           S.T = `...6`)
tb54r.tea2  <-  read_excel("Inputs/TablesB5_1_6R.xlsx",col_names=F,range="I56:M68") %>% 
    filter(!is.na(`...1`)) %>%
    rename(S.W = `...1`, 
           S.B = `...2`, 
           S.H = `...3`,
           S.A = `...4`,
           S.T = `...5`) %>%
    mutate(acad.idx = tb53r.acad$acad.idx)
tb54r.coun  <-  read_excel("Inputs/TablesB5_1_6R.xlsx",col_names=F,range="O56:S68") %>% 
    filter(!is.na(`...1`)) %>%
    rename(S.W = `...1`, 
           S.B = `...2`, 
           S.H = `...3`,
           S.A = `...4`,
           S.T = `...5`) %>%
    mutate(acad.idx = tb53r.acad$acad.idx)
tb55r.pers  <-  read_excel("Inputs/TablesB5_1_6R.xlsx",col_names=F,range="B74:G86") %>% 
    filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           S.W = `...2`, 
           S.B = `...3`, 
           S.H = `...4`,
           S.A = `...5`,
           S.T = `...6`)
tb55r.alpr  <-  read_excel("Inputs/TablesB5_1_6R.xlsx",col_names=F,range="I74:M86") %>% 
    filter(!is.na(`...1`)) %>%
    rename(S.W = `...1`, 
           S.B = `...2`, 
           S.H = `...3`,
           S.A = `...4`,
           S.T = `...5`) %>%
    mutate(acad.idx = tb53r.acad$acad.idx)
tb56r.over  <-  read_excel("Inputs/TablesB5_1_6R.xlsx",col_names=F,range="B94:G106") %>% 
    filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           S.W = `...2`, 
           S.B = `...3`, 
           S.H = `...4`,
           S.A = `...5`,
           S.T = `...6`)
tb56r.alov  <-  read_excel("Inputs/TablesB5_1_6R.xlsx",col_names=F,range="I94:M106") %>% 
    filter(!is.na(`...1`)) %>%
    rename(S.W = `...1`, 
           S.B = `...2`, 
           S.H = `...3`,
           S.A = `...4`,
           S.T = `...5`) %>%
    mutate(acad.idx = tb53r.acad$acad.idx)
tb53.acad   <-  read_excel("Inputs/TablesB5_1_6.xlsx",col_names=F,range="B38:G50") %>% 
    filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           S.W = `...2`, 
           S.B = `...3`, 
           S.H = `...4`,
           S.A = `...5`,
           S.T = `...6`)
tb53.xtra   <-  read_excel("Inputs/TablesB5_1_6.xlsx",col_names=F,range="I38:M50") %>% 
    filter(!is.na(`...1`)) %>%
    rename(S.W = `...1`, 
           S.B = `...2`, 
           S.H = `...3`,
           S.A = `...4`,
           S.T = `...5`) %>%
    mutate(acad.idx = tb53r.acad$acad.idx)
tb54.tea1   <-  read_excel("Inputs/TablesB5_1_6.xlsx",col_names=F,range="B56:G68") %>% 
    filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           S.W = `...2`, 
           S.B = `...3`, 
           S.H = `...4`,
           S.A = `...5`,
           S.T = `...6`)
tb54.tea2   <-  read_excel("Inputs/TablesB5_1_6.xlsx",col_names=F,range="I56:M68") %>% 
    filter(!is.na(`...1`)) %>%
    rename(S.W = `...1`, 
           S.B = `...2`, 
           S.H = `...3`,
           S.A = `...4`,
           S.T = `...5`) %>%
    mutate(acad.idx = tb53r.acad$acad.idx)
tb54.coun   <-  read_excel("Inputs/TablesB5_1_6.xlsx",col_names=F,range="O56:S68") %>% 
    filter(!is.na(`...1`)) %>%
    rename(S.W = `...1`, 
           S.B = `...2`, 
           S.H = `...3`,
           S.A = `...4`,
           S.T = `...5`) %>%
    mutate(acad.idx = tb53r.acad$acad.idx)
tb55.pers   <-  read_excel("Inputs/TablesB5_1_6.xlsx",col_names=F,range="B74:G86") %>% 
    filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           S.W = `...2`, 
           S.B = `...3`, 
           S.H = `...4`,
           S.A = `...5`,
           S.T = `...6`)
tb55.alpr   <-  read_excel("Inputs/TablesB5_1_6.xlsx",col_names=F,range="I74:M86") %>% 
    filter(!is.na(`...1`)) %>%
    rename(S.W = `...1`, 
           S.B = `...2`, 
           S.H = `...3`,
           S.A = `...4`,
           S.T = `...5`) %>%
    mutate(acad.idx = tb53r.acad$acad.idx)
tb56.over   <-  read_excel("Inputs/TablesB5_1_6.xlsx",col_names=F,range="B94:G106") %>% 
    filter(!is.na(`...1`)) %>%
    rename(acad.idx = `...1`, 
           S.W = `...2`, 
           S.B = `...3`, 
           S.H = `...4`,
           S.A = `...5`,
           S.T = `...6`)
tb56.alov   <-  read_excel("Inputs/TablesB5_1_6.xlsx",col_names=F,range="I94:M106") %>% 
    filter(!is.na(`...1`)) %>%
    rename(S.W = `...1`, 
           S.B = `...2`, 
           S.H = `...3`,
           S.A = `...4`,
           S.T = `...5`) %>%
    mutate(acad.idx = tb53r.acad$acad.idx)

ta5r        <-  read_excel("Inputs/TablesA3R_A5R.xlsx",col_names=T,range="A29:D42") %>% 
    filter(!is.na(`...1`)) %>%
    rename(`Sample cut` = `...1`) %>%
    filter(`Sample cut`!="Additional Baseline Cuts")

tb41a  <- read_excel("Inputs/TablesB4_1.xlsx",col_names=F,range="C6:N8")   %>% mutate(ratnum=c(4,3,2)) %>% mutate(rating="Academic")
tb41b  <- read_excel("Inputs/TablesB4_1.xlsx",col_names=F,range="C10:N12") %>% mutate(ratnum=c(4,3,2)) %>% mutate(rating="Extracurricular")
tb41c  <- read_excel("Inputs/TablesB4_1.xlsx",col_names=F,range="C14:N16") %>% mutate(ratnum=c(4,3,2)) %>% mutate(rating="Athletic")
tb41d  <- read_excel("Inputs/TablesB4_1.xlsx",col_names=F,range="C18:N20") %>% mutate(ratnum=c(4,3,2)) %>% mutate(rating="Personal")
tb41e  <- read_excel("Inputs/TablesB4_1.xlsx",col_names=F,range="C22:N24") %>% mutate(ratnum=c(4,3,2)) %>% mutate(rating="Teacher 1")
tb41f  <- read_excel("Inputs/TablesB4_1.xlsx",col_names=F,range="C26:N28") %>% mutate(ratnum=c(4,3,2)) %>% mutate(rating="Teacher 2")
tb41g  <- read_excel("Inputs/TablesB4_1.xlsx",col_names=F,range="C30:N32") %>% mutate(ratnum=c(4,3,2)) %>% mutate(rating="Counselor")
tb41h  <- read_excel("Inputs/TablesB4_1.xlsx",col_names=F,range="C34:N36") %>% mutate(ratnum=c(4,3,2)) %>% mutate(rating="Alumni Personal")
tb41i  <- read_excel("Inputs/TablesB4_1.xlsx",col_names=F,range="C38:N40") %>% mutate(ratnum=c(4,3,2)) %>% mutate(rating="Alumni Overall")
tb41n  <- read_excel("Inputs/TablesB4_1.xlsx",col_names=F,range="C53:N53") %>% 
    rename(
        NrejW= `...1`, NadmW= `...2`, NtotW= `...3`, 
        NrejB= `...4`, NadmB= `...5`, NtotB= `...6`,
        NrejH= `...7`, NadmH= `...8`, NtotH= `...9`,
        NrejA=`...10`, NadmA=`...11`, NtotA=`...12`
    ) %>%
    mutate(NrejT=tb32$reject[42][[1]],NadmT=tb32$admit[42][[1]],NtotT=tb32$tot[42][[1]])

tb41   <- bind_rows(tb41a,tb41b,tb41c,tb41d,tb41e,tb41f,tb41g,tb41h,tb41i)
tb41 %<>% mutate(across(c(starts_with("rating")),as_factor)) %>%
    rename(
        SrejW= `...1`, SadmW= `...2`, StotW= `...3`, 
        SrejB= `...4`, SadmB= `...5`, StotB= `...6`,
        SrejH= `...7`, SadmH= `...8`, StotH= `...9`,
        SrejA=`...10`, SadmA=`...11`, StotA=`...12`
    )
tb412p <- tb41 %>% slice(seq(3,27,3)) %>% add_row(rating="Overall",ratnum=2,.before=1)

# Need this next bit for the overall rating
t42a   <- read_excel("Inputs/Tables4_2.xlsx",col_names=F,range="C7:J11") %>%
    rename(SadmW = `...1`, SpopW = `...2`, SadmB = `...3`, SpopB = `...4`, SadmH = `...5`, SpopH = `...6`, SadmA = `...7`, SpopA = `...8`) %>%
    mutate(rating = c("3-","3","3+","2","1"),ratgroup = c("FALSE","FALSE","FALSE","TRUE","TRUE"))

t42b   <- read_excel("Inputs/Tables4_2.xlsx",col_names=F,range="C14:J18") %>%
    rename(SadmW = `...1`, SpopW = `...2`, SadmB = `...3`, SpopB = `...4`, SadmH = `...5`, SpopH = `...6`, SadmA = `...7`, SpopA = `...8`) %>%
    mutate(rating = c("3-","3","3+","2","1"),ratgroup = c(FALSE,FALSE,FALSE,TRUE,TRUE)) %>%
    mutate(
        NpopW     = SpopW/100*tb41n$NtotW[[1]],
        NpopB     = SpopB/100*tb41n$NtotB[[1]],
        NpopH     = SpopH/100*tb41n$NtotH[[1]],
        NpopA     = SpopA/100*tb41n$NtotA[[1]],
        NadmW     = SadmW/100*NpopW,
        NadmB     = SadmB/100*NpopB,
        NadmH     = SadmH/100*NpopH,
        NadmA     = SadmA/100*NpopA,
        frac.admW = NadmW*100/tb41n$NadmW[[1]],
        frac.admB = NadmB*100/tb41n$NadmB[[1]],
        frac.admH = NadmH*100/tb41n$NadmH[[1]],
        frac.admA = NadmA*100/tb41n$NadmA[[1]],
    )
# collapse counts and shares by ">3+" and "<=3+"
t42bs  <- t42b %>% group_by(ratgroup) %>% summarize(across(c(starts_with("frac"),contains("Nadm"),contains("Npop"),contains("Spop")),sum)) %>% filter(ratgroup==T)

# merge in overall rating shares into data frame with other ratings information for use later
tb412p%<>%mutate(
    SadmW = replace(SadmW,rating=="Overall",t42bs$frac.admW[[1]]),
    SadmB = replace(SadmB,rating=="Overall",t42bs$frac.admB[[1]]),
    SadmH = replace(SadmH,rating=="Overall",t42bs$frac.admH[[1]]),
    SadmA = replace(SadmA,rating=="Overall",t42bs$frac.admA[[1]]),
    StotW = replace(StotW,rating=="Overall",t42bs$SpopW[[1]]),
    StotB = replace(StotB,rating=="Overall",t42bs$SpopB[[1]]),
    StotH = replace(StotH,rating=="Overall",t42bs$SpopH[[1]]),
    StotA = replace(StotA,rating=="Overall",t42bs$SpopA[[1]]),
)

# create aggregated versions of P621 and P623 to make binary ratings (">3+" vs. "<=3+")
df621s <- df621 %>% mutate(
    ratnum  = as.numeric(ratnum),
    ratnum2 = ratnum<=2
) %>%
    select(rating,ratnum2,NadmW,NtotW,NadmB,NtotB,NadmH,NtotH,NadmA,NtotA) %>%
    group_by(rating,ratnum2) %>%
    summarize(across(where(is.double),sum)) %>%
    group_by(rating) %>%
    mutate(
        DadmW = sum(NadmW),
        DtotW = sum(NtotW),
        DadmB = sum(NadmB),
        DtotB = sum(NtotB),
        DadmH = sum(NadmH),
        DtotH = sum(NtotH),
        DadmA = sum(NadmA),
        DtotA = sum(NtotA)
    ) %>%
    filter(ratnum2==TRUE) %>%
    mutate(
        SadmW = NadmW/DadmW,
        StotW = NtotW/DtotW,
        SadmB = NadmB/DadmB,
        StotB = NtotB/DtotB,
        SadmH = NadmH/DadmH,
        StotH = NtotH/DtotH,
        SadmA = NadmA/DadmA,
        StotA = NtotA/DtotA
    )

df623s <- df623 %>% mutate(
    ratnum  = as.numeric(ratnum),
    ratnum2 = ratnum<=2
) %>%
    select(rating,ratnum2,NadmW,NtotW,NadmB,NtotB,NadmH,NtotH,NadmA,NtotA) %>%
    group_by(rating,ratnum2) %>%
    summarize(across(where(is.double),sum)) %>%
    group_by(rating) %>%
    mutate(
        DadmW = sum(NadmW),
        DtotW = sum(NtotW),
        DadmB = sum(NadmB),
        DtotB = sum(NtotB),
        DadmH = sum(NadmH),
        DtotH = sum(NtotH),
        DadmA = sum(NadmA),
        DtotA = sum(NtotA)
    ) %>%
    filter(ratnum2==TRUE) %>%
    mutate(
        SadmW = NadmW/DadmW,
        StotW = NtotW/DtotW,
        SadmB = NadmB/DadmB,
        StotB = NtotB/DtotB,
        SadmH = NadmH/DadmH,
        StotH = NtotH/DtotH,
        SadmA = NadmA/DadmA,
        StotA = NtotA/DtotA
    )

# fill in values in Table 8 for non-ALDCs
T2athl     <- tibble(Rating=c("LB Overall","UB Overall","LB Academic","UB Academic","LB Extracurricular","UB Extracurricular","LB Personal","UB Personal"),nALDC.app.W=rep(0,8),nALDC.adm.W=rep(0,8),A.adm.W=rep(0,8),nALDC.app.B=rep(0,8),nALDC.adm.B=rep(0,8),A.adm.B=rep(0,8),nALDC.app.H=rep(0,8),nALDC.adm.H=rep(0,8),A.adm.H=rep(0,8),nALDC.app.A=rep(0,8),nALDC.adm.A=rep(0,8),A.adm.A=rep(0,8)) %>%
    mutate(
        nALDC.app.W = replace(nALDC.app.W,Rating=="LB Overall"        ,100*df621s$StotW[df621s$rating=="Overall"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="UB Overall"        ,100*df621s$StotW[df621s$rating=="Overall"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="LB Overall"        ,100*df621s$SadmW[df621s$rating=="Overall"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="UB Overall"        ,100*df621s$SadmW[df621s$rating=="Overall"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="LB Academic"       ,100*df621s$StotW[df621s$rating=="Academic"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="UB Academic"       ,100*df621s$StotW[df621s$rating=="Academic"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="LB Academic"       ,100*df621s$SadmW[df621s$rating=="Academic"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="UB Academic"       ,100*df621s$SadmW[df621s$rating=="Academic"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="LB Extracurricular",100*df621s$StotW[df621s$rating=="Extracurricular"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="UB Extracurricular",100*df621s$StotW[df621s$rating=="Extracurricular"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="LB Extracurricular",100*df621s$SadmW[df621s$rating=="Extracurricular"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="UB Extracurricular",100*df621s$SadmW[df621s$rating=="Extracurricular"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="LB Personal"       ,100*df621s$StotW[df621s$rating=="Personal"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="UB Personal"       ,100*df621s$StotW[df621s$rating=="Personal"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="LB Personal"       ,100*df621s$SadmW[df621s$rating=="Personal"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="UB Personal"       ,100*df621s$SadmW[df621s$rating=="Personal"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="LB Overall"        ,100*df621s$StotB[df621s$rating=="Overall"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="UB Overall"        ,100*df621s$StotB[df621s$rating=="Overall"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="LB Overall"        ,100*df621s$SadmB[df621s$rating=="Overall"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="UB Overall"        ,100*df621s$SadmB[df621s$rating=="Overall"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="LB Academic"       ,100*df621s$StotB[df621s$rating=="Academic"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="UB Academic"       ,100*df621s$StotB[df621s$rating=="Academic"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="LB Academic"       ,100*df621s$SadmB[df621s$rating=="Academic"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="UB Academic"       ,100*df621s$SadmB[df621s$rating=="Academic"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="LB Extracurricular",100*df621s$StotB[df621s$rating=="Extracurricular"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="UB Extracurricular",100*df621s$StotB[df621s$rating=="Extracurricular"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="LB Extracurricular",100*df621s$SadmB[df621s$rating=="Extracurricular"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="UB Extracurricular",100*df621s$SadmB[df621s$rating=="Extracurricular"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="LB Personal"       ,100*df621s$StotB[df621s$rating=="Personal"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="UB Personal"       ,100*df621s$StotB[df621s$rating=="Personal"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="LB Personal"       ,100*df621s$SadmB[df621s$rating=="Personal"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="UB Personal"       ,100*df621s$SadmB[df621s$rating=="Personal"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="LB Overall"        ,100*df621s$StotH[df621s$rating=="Overall"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="UB Overall"        ,100*df621s$StotH[df621s$rating=="Overall"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="LB Overall"        ,100*df621s$SadmH[df621s$rating=="Overall"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="UB Overall"        ,100*df621s$SadmH[df621s$rating=="Overall"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="LB Academic"       ,100*df621s$StotH[df621s$rating=="Academic"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="UB Academic"       ,100*df621s$StotH[df621s$rating=="Academic"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="LB Academic"       ,100*df621s$SadmH[df621s$rating=="Academic"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="UB Academic"       ,100*df621s$SadmH[df621s$rating=="Academic"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="LB Extracurricular",100*df621s$StotH[df621s$rating=="Extracurricular"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="UB Extracurricular",100*df621s$StotH[df621s$rating=="Extracurricular"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="LB Extracurricular",100*df621s$SadmH[df621s$rating=="Extracurricular"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="UB Extracurricular",100*df621s$SadmH[df621s$rating=="Extracurricular"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="LB Personal"       ,100*df621s$StotH[df621s$rating=="Personal"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="UB Personal"       ,100*df621s$StotH[df621s$rating=="Personal"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="LB Personal"       ,100*df621s$SadmH[df621s$rating=="Personal"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="UB Personal"       ,100*df621s$SadmH[df621s$rating=="Personal"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="LB Overall"        ,100*df621s$StotA[df621s$rating=="Overall"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="UB Overall"        ,100*df621s$StotA[df621s$rating=="Overall"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="LB Overall"        ,100*df621s$SadmA[df621s$rating=="Overall"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="UB Overall"        ,100*df621s$SadmA[df621s$rating=="Overall"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="LB Academic"       ,100*df621s$StotA[df621s$rating=="Academic"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="UB Academic"       ,100*df621s$StotA[df621s$rating=="Academic"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="LB Academic"       ,100*df621s$SadmA[df621s$rating=="Academic"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="UB Academic"       ,100*df621s$SadmA[df621s$rating=="Academic"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="LB Extracurricular",100*df621s$StotA[df621s$rating=="Extracurricular"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="UB Extracurricular",100*df621s$StotA[df621s$rating=="Extracurricular"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="LB Extracurricular",100*df621s$SadmA[df621s$rating=="Extracurricular"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="UB Extracurricular",100*df621s$SadmA[df621s$rating=="Extracurricular"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="LB Personal"       ,100*df621s$StotA[df621s$rating=="Personal"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="UB Personal"       ,100*df621s$StotA[df621s$rating=="Personal"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="LB Personal"       ,100*df621s$SadmA[df621s$rating=="Personal"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="UB Personal"       ,100*df621s$SadmA[df621s$rating=="Personal"])
    )

#------------------------------------------------------------------------------
# Bounds on ratings distribution for recruited athletes
#------------------------------------------------------------------------------

# step 1: net out missing GPA observations using Tables B.5.1R-B.5.6R and Trial Exhibit P623
df625.appN %<>% mutate(
    NeffW = case_when(rating=="Academic"        ~ tb51r$N.W[11][[1]]*as.numeric(tb53r.acad$S.W[12][[1]]),
                      rating=="Extracurricular" ~ tb51r$N.W[11][[1]]*as.numeric(tb53r.xtra$S.W[12][[1]]),
                      rating=="Teacher 1"       ~ tb51r$N.W[11][[1]]*as.numeric(tb54r.tea1$S.W[12][[1]]),
                      rating=="Teacher 2"       ~ tb51r$N.W[11][[1]]*as.numeric(tb54r.tea2$S.W[12][[1]]),
                      rating=="Counselor"       ~ tb51r$N.W[11][[1]]*as.numeric(tb54r.coun$S.W[12][[1]]),
                      rating=="Personal"        ~ tb51r$N.W[11][[1]]*as.numeric(tb55r.pers$S.W[12][[1]]),
                      rating=="Alumni Personal" ~ tb51r$N.W[11][[1]]*as.numeric(tb55r.alpr$S.W[12][[1]]),
                      rating=="Overall"         ~ tb51r$N.W[11][[1]]*as.numeric(tb56r.over$S.W[12][[1]]),
                      rating=="Alumni Overall"  ~ tb51r$N.W[11][[1]]*as.numeric(tb56r.alov$S.W[12][[1]])
    ),
    NeffB = case_when(rating=="Academic"        ~ tb51r$N.B[11][[1]]*as.numeric(tb53r.acad$S.B[12][[1]]),
                      rating=="Extracurricular" ~ tb51r$N.B[11][[1]]*as.numeric(tb53r.xtra$S.B[12][[1]]),
                      rating=="Teacher 1"       ~ tb51r$N.B[11][[1]]*as.numeric(tb54r.tea1$S.B[12][[1]]),
                      rating=="Teacher 2"       ~ tb51r$N.B[11][[1]]*as.numeric(tb54r.tea2$S.B[12][[1]]),
                      rating=="Counselor"       ~ tb51r$N.B[11][[1]]*as.numeric(tb54r.coun$S.B[12][[1]]),
                      rating=="Personal"        ~ tb51r$N.B[11][[1]]*as.numeric(tb55r.pers$S.B[12][[1]]),
                      rating=="Alumni Personal" ~ tb51r$N.B[11][[1]]*as.numeric(tb55r.alpr$S.B[12][[1]]),
                      rating=="Overall"         ~ tb51r$N.B[11][[1]]*as.numeric(tb56r.over$S.B[12][[1]]),
                      rating=="Alumni Overall"  ~ tb51r$N.B[11][[1]]*as.numeric(tb56r.alov$S.B[12][[1]])
    ),
    NeffH = case_when(rating=="Academic"        ~ tb51r$N.H[11][[1]]*as.numeric(tb53r.acad$S.H[12][[1]]),
                      rating=="Extracurricular" ~ tb51r$N.H[11][[1]]*as.numeric(tb53r.xtra$S.H[12][[1]]),
                      rating=="Teacher 1"       ~ tb51r$N.H[11][[1]]*as.numeric(tb54r.tea1$S.H[12][[1]]),
                      rating=="Teacher 2"       ~ tb51r$N.H[11][[1]]*as.numeric(tb54r.tea2$S.H[12][[1]]),
                      rating=="Counselor"       ~ tb51r$N.H[11][[1]]*as.numeric(tb54r.coun$S.H[12][[1]]),
                      rating=="Personal"        ~ tb51r$N.H[11][[1]]*as.numeric(tb55r.pers$S.H[12][[1]]),
                      rating=="Alumni Personal" ~ tb51r$N.H[11][[1]]*as.numeric(tb55r.alpr$S.H[12][[1]]),
                      rating=="Overall"         ~ tb51r$N.H[11][[1]]*as.numeric(tb56r.over$S.H[12][[1]]),
                      rating=="Alumni Overall"  ~ tb51r$N.H[11][[1]]*as.numeric(tb56r.alov$S.H[12][[1]])
    ),
    NeffA = case_when(rating=="Academic"        ~ tb51r$N.A[11][[1]]*as.numeric(tb53r.acad$S.A[12][[1]]),
                      rating=="Extracurricular" ~ tb51r$N.A[11][[1]]*as.numeric(tb53r.xtra$S.A[12][[1]]),
                      rating=="Teacher 1"       ~ tb51r$N.A[11][[1]]*as.numeric(tb54r.tea1$S.A[12][[1]]),
                      rating=="Teacher 2"       ~ tb51r$N.A[11][[1]]*as.numeric(tb54r.tea2$S.A[12][[1]]),
                      rating=="Counselor"       ~ tb51r$N.A[11][[1]]*as.numeric(tb54r.coun$S.A[12][[1]]),
                      rating=="Personal"        ~ tb51r$N.A[11][[1]]*as.numeric(tb55r.pers$S.A[12][[1]]),
                      rating=="Alumni Personal" ~ tb51r$N.A[11][[1]]*as.numeric(tb55r.alpr$S.A[12][[1]]),
                      rating=="Overall"         ~ tb51r$N.A[11][[1]]*as.numeric(tb56r.over$S.A[12][[1]]),
                      rating=="Alumni Overall"  ~ tb51r$N.A[11][[1]]*as.numeric(tb56r.alov$S.A[12][[1]])
    ),
    NeffT = case_when(rating=="Academic"        ~ tb51r$N.T[11][[1]]*as.numeric(tb53r.acad$S.T[12][[1]]),
                      rating=="Extracurricular" ~ tb51r$N.T[11][[1]]*as.numeric(tb53r.xtra$S.T[12][[1]]),
                      rating=="Teacher 1"       ~ tb51r$N.T[11][[1]]*as.numeric(tb54r.tea1$S.T[12][[1]]),
                      rating=="Teacher 2"       ~ tb51r$N.T[11][[1]]*as.numeric(tb54r.tea2$S.T[12][[1]]),
                      rating=="Counselor"       ~ tb51r$N.T[11][[1]]*as.numeric(tb54r.coun$S.T[12][[1]]),
                      rating=="Personal"        ~ tb51r$N.T[11][[1]]*as.numeric(tb55r.pers$S.T[12][[1]]),
                      rating=="Alumni Personal" ~ tb51r$N.T[11][[1]]*as.numeric(tb55r.alpr$S.T[12][[1]]),
                      rating=="Overall"         ~ tb51r$N.T[11][[1]]*as.numeric(tb56r.over$S.T[12][[1]]),
                      rating=="Alumni Overall"  ~ tb51r$N.T[11][[1]]*as.numeric(tb56r.alov$S.T[12][[1]])
    )
) %>%
    mutate(
        N2pMissGPA.W = round(NtotW.all.1-NeffW),
        N2pMissGPA.B = round(NtotB.all.1-NeffB),
        N2pMissGPA.H = round(NtotH.all.1-NeffH),
        N2pMissGPA.A = round(NtotA.all.1-NeffA),
        N2pMissGPA.T = round(NtotT.all.1-NeffT)
    ) %>%
    # step 2: repeat for expanded sample in opening report using Tables B.5.1-B.5.6 (but we don't have P623 anymore, so we need to do some gymnastics)
    mutate(
        NeffW = case_when(rating=="Academic"        ~ tb51$N.W[11][[1]]*as.numeric(tb53.acad$S.W[12][[1]]),
                          rating=="Extracurricular" ~ tb51$N.W[11][[1]]*as.numeric(tb53.xtra$S.W[12][[1]]),
                          rating=="Teacher 1"       ~ tb51$N.W[11][[1]]*as.numeric(tb54.tea1$S.W[12][[1]]),
                          rating=="Teacher 2"       ~ tb51$N.W[11][[1]]*as.numeric(tb54.tea2$S.W[12][[1]]),
                          rating=="Counselor"       ~ tb51$N.W[11][[1]]*as.numeric(tb54.coun$S.W[12][[1]]),
                          rating=="Personal"        ~ tb51$N.W[11][[1]]*as.numeric(tb55.pers$S.W[12][[1]]),
                          rating=="Alumni Personal" ~ tb51$N.W[11][[1]]*as.numeric(tb55.alpr$S.W[12][[1]]),
                          rating=="Overall"         ~ tb51$N.W[11][[1]]*as.numeric(tb56.over$S.W[12][[1]]),
                          rating=="Alumni Overall"  ~ tb51$N.W[11][[1]]*as.numeric(tb56.alov$S.W[12][[1]])
        ),
        NeffB = case_when(rating=="Academic"        ~ tb51$N.B[11][[1]]*as.numeric(tb53.acad$S.B[12][[1]]),
                          rating=="Extracurricular" ~ tb51$N.B[11][[1]]*as.numeric(tb53.xtra$S.B[12][[1]]),
                          rating=="Teacher 1"       ~ tb51$N.B[11][[1]]*as.numeric(tb54.tea1$S.B[12][[1]]),
                          rating=="Teacher 2"       ~ tb51$N.B[11][[1]]*as.numeric(tb54.tea2$S.B[12][[1]]),
                          rating=="Counselor"       ~ tb51$N.B[11][[1]]*as.numeric(tb54.coun$S.B[12][[1]]),
                          rating=="Personal"        ~ tb51$N.B[11][[1]]*as.numeric(tb55.pers$S.B[12][[1]]),
                          rating=="Alumni Personal" ~ tb51$N.B[11][[1]]*as.numeric(tb55.alpr$S.B[12][[1]]),
                          rating=="Overall"         ~ tb51$N.B[11][[1]]*as.numeric(tb56.over$S.B[12][[1]]),
                          rating=="Alumni Overall"  ~ tb51$N.B[11][[1]]*as.numeric(tb56.alov$S.B[12][[1]])
        ),
        NeffH = case_when(rating=="Academic"        ~ tb51$N.H[11][[1]]*as.numeric(tb53.acad$S.H[12][[1]]),
                          rating=="Extracurricular" ~ tb51$N.H[11][[1]]*as.numeric(tb53.xtra$S.H[12][[1]]),
                          rating=="Teacher 1"       ~ tb51$N.H[11][[1]]*as.numeric(tb54.tea1$S.H[12][[1]]),
                          rating=="Teacher 2"       ~ tb51$N.H[11][[1]]*as.numeric(tb54.tea2$S.H[12][[1]]),
                          rating=="Counselor"       ~ tb51$N.H[11][[1]]*as.numeric(tb54.coun$S.H[12][[1]]),
                          rating=="Personal"        ~ tb51$N.H[11][[1]]*as.numeric(tb55.pers$S.H[12][[1]]),
                          rating=="Alumni Personal" ~ tb51$N.H[11][[1]]*as.numeric(tb55.alpr$S.H[12][[1]]),
                          rating=="Overall"         ~ tb51$N.H[11][[1]]*as.numeric(tb56.over$S.H[12][[1]]),
                          rating=="Alumni Overall"  ~ tb51$N.H[11][[1]]*as.numeric(tb56.alov$S.H[12][[1]])
        ),
        NeffA = case_when(rating=="Academic"        ~ tb51$N.A[11][[1]]*as.numeric(tb53.acad$S.A[12][[1]]),
                          rating=="Extracurricular" ~ tb51$N.A[11][[1]]*as.numeric(tb53.xtra$S.A[12][[1]]),
                          rating=="Teacher 1"       ~ tb51$N.A[11][[1]]*as.numeric(tb54.tea1$S.A[12][[1]]),
                          rating=="Teacher 2"       ~ tb51$N.A[11][[1]]*as.numeric(tb54.tea2$S.A[12][[1]]),
                          rating=="Counselor"       ~ tb51$N.A[11][[1]]*as.numeric(tb54.coun$S.A[12][[1]]),
                          rating=="Personal"        ~ tb51$N.A[11][[1]]*as.numeric(tb55.pers$S.A[12][[1]]),
                          rating=="Alumni Personal" ~ tb51$N.A[11][[1]]*as.numeric(tb55.alpr$S.A[12][[1]]),
                          rating=="Overall"         ~ tb51$N.A[11][[1]]*as.numeric(tb56.over$S.A[12][[1]]),
                          rating=="Alumni Overall"  ~ tb51$N.A[11][[1]]*as.numeric(tb56.alov$S.A[12][[1]])
        ),
        NeffT = case_when(rating=="Academic"        ~ tb51$N.T[11][[1]]*as.numeric(tb53.acad$S.T[12][[1]]),
                          rating=="Extracurricular" ~ tb51$N.T[11][[1]]*as.numeric(tb53.xtra$S.T[12][[1]]),
                          rating=="Teacher 1"       ~ tb51$N.T[11][[1]]*as.numeric(tb54.tea1$S.T[12][[1]]),
                          rating=="Teacher 2"       ~ tb51$N.T[11][[1]]*as.numeric(tb54.tea2$S.T[12][[1]]),
                          rating=="Counselor"       ~ tb51$N.T[11][[1]]*as.numeric(tb54.coun$S.T[12][[1]]),
                          rating=="Personal"        ~ tb51$N.T[11][[1]]*as.numeric(tb55.pers$S.T[12][[1]]),
                          rating=="Alumni Personal" ~ tb51$N.T[11][[1]]*as.numeric(tb55.alpr$S.T[12][[1]]),
                          rating=="Overall"         ~ tb51$N.T[11][[1]]*as.numeric(tb56.over$S.T[12][[1]]),
                          rating=="Alumni Overall"  ~ tb51$N.T[11][[1]]*as.numeric(tb56.alov$S.T[12][[1]])
        )
    ) %>%
    mutate(
        NewPct2.W = (NeffW+N2pMissGPA.W)/tb41n$NtotW[[1]]*100,
        NewPct2.B = (NeffB+N2pMissGPA.B)/tb41n$NtotB[[1]]*100,
        NewPct2.H = (NeffH+N2pMissGPA.H)/tb41n$NtotH[[1]]*100,
        NewPct2.A = (NeffA+N2pMissGPA.A)/tb41n$NtotA[[1]]*100,
        NewPct2.T = (NeffT+N2pMissGPA.T)/tb41n$NtotT[[1]]*100
    ) %>%
    
    # step 3: now get shares of missing who receive 2 or higher, conditional on accept and reject (uses formulas from appendix B)
    #         then use the missing-conditional shares to back out what the overall shares are (including the missings)                
    mutate(
        Nmiss.tot.W = tb41n$NtotW[[1]]-(tb41n$NtotW[[1]]*NewPct2.W/tb412p$StotW),
        Nmiss.tot.B = tb41n$NtotB[[1]]-(tb41n$NtotB[[1]]*NewPct2.B/tb412p$StotB),
        Nmiss.tot.H = tb41n$NtotH[[1]]-(tb41n$NtotH[[1]]*NewPct2.H/tb412p$StotH),
        Nmiss.tot.A = tb41n$NtotA[[1]]-(tb41n$NtotA[[1]]*NewPct2.A/tb412p$StotA),
        Nmiss.tot.W = replace(Nmiss.tot.W,rating=="Overall",0), 
        Nmiss.tot.B = replace(Nmiss.tot.B,rating=="Overall",0), 
        Nmiss.tot.H = replace(Nmiss.tot.H,rating=="Overall",0), 
        Nmiss.tot.A = replace(Nmiss.tot.A,rating=="Overall",0), 
        Smiss.rej.W = (tb412p$StotW*(tb41n$NtotW[[1]]-Nmiss.tot.W)-tb412p$SadmW*(tb41n$NadmW[[1]]-Nmiss.tot.W)-tb41n$NrejW[[1]]*tb412p$SrejW)/(Nmiss.tot.W*(tb412p$SadmW-tb412p$SrejW)),
        Smiss.rej.B = (tb412p$StotB*(tb41n$NtotB[[1]]-Nmiss.tot.B)-tb412p$SadmB*(tb41n$NadmB[[1]]-Nmiss.tot.B)-tb41n$NrejB[[1]]*tb412p$SrejB)/(Nmiss.tot.B*(tb412p$SadmB-tb412p$SrejB)),
        Smiss.rej.H = (tb412p$StotH*(tb41n$NtotH[[1]]-Nmiss.tot.H)-tb412p$SadmH*(tb41n$NadmH[[1]]-Nmiss.tot.H)-tb41n$NrejH[[1]]*tb412p$SrejH)/(Nmiss.tot.H*(tb412p$SadmH-tb412p$SrejH)),
        Smiss.rej.A = (tb412p$StotA*(tb41n$NtotA[[1]]-Nmiss.tot.A)-tb412p$SadmA*(tb41n$NadmA[[1]]-Nmiss.tot.A)-tb41n$NrejA[[1]]*tb412p$SrejA)/(Nmiss.tot.A*(tb412p$SadmA-tb412p$SrejA)),
        Smiss.rej.W = pmin(pmax(Smiss.rej.W,0),1),
        Smiss.rej.B = pmin(pmax(Smiss.rej.B,0),1),
        Smiss.rej.H = pmin(pmax(Smiss.rej.H,0),1),
        Smiss.rej.A = pmin(pmax(Smiss.rej.A,0),1),
        Smiss.adm.W = 1-Smiss.rej.W,
        Smiss.adm.B = 1-Smiss.rej.B,
        Smiss.adm.H = 1-Smiss.rej.H,
        Smiss.adm.A = 1-Smiss.rej.A,
        Nmiss.rej.W = Smiss.rej.W*Nmiss.tot.W,
        Nmiss.rej.B = Smiss.rej.B*Nmiss.tot.B,
        Nmiss.rej.H = Smiss.rej.H*Nmiss.tot.H,
        Nmiss.rej.A = Smiss.rej.A*Nmiss.tot.A,
        Nmiss.rej.W = Smiss.rej.W*Nmiss.tot.W,
        Nmiss.rej.B = Smiss.rej.B*Nmiss.tot.B,
        Nmiss.rej.H = Smiss.rej.H*Nmiss.tot.H,
        Nmiss.rej.A = Smiss.rej.A*Nmiss.tot.A,
        Nmiss.adm.W = Smiss.adm.W*Nmiss.tot.W,
        Nmiss.adm.B = Smiss.adm.B*Nmiss.tot.B,
        Nmiss.adm.H = Smiss.adm.H*Nmiss.tot.H,
        Nmiss.adm.A = Smiss.adm.A*Nmiss.tot.A,
        Nmiss.adm.W = replace(Nmiss.adm.W,rating=="Overall",0), 
        Nmiss.adm.B = replace(Nmiss.adm.B,rating=="Overall",0), 
        Nmiss.adm.H = replace(Nmiss.adm.H,rating=="Overall",0), 
        Nmiss.adm.A = replace(Nmiss.adm.A,rating=="Overall",0), 
        NtotW.adj.1 = tb412p$StotW*(tb41n$NtotW[[1]]-Nmiss.tot.W)/100,
        NtotB.adj.1 = tb412p$StotB*(tb41n$NtotB[[1]]-Nmiss.tot.B)/100,
        NtotH.adj.1 = tb412p$StotH*(tb41n$NtotH[[1]]-Nmiss.tot.H)/100,
        NtotA.adj.1 = tb412p$StotA*(tb41n$NtotA[[1]]-Nmiss.tot.A)/100,
        NrejW.adj.1 = tb412p$SrejW*(tb41n$NrejW[[1]]-Nmiss.rej.W)/100,
        NrejB.adj.1 = tb412p$SrejB*(tb41n$NrejB[[1]]-Nmiss.rej.B)/100,
        NrejH.adj.1 = tb412p$SrejH*(tb41n$NrejH[[1]]-Nmiss.rej.H)/100,
        NrejA.adj.1 = tb412p$SrejA*(tb41n$NrejA[[1]]-Nmiss.rej.A)/100,
        NadmW.adj.1 = tb412p$SadmW*(tb41n$NadmW[[1]]-Nmiss.adm.W)/100,
        NadmB.adj.1 = tb412p$SadmB*(tb41n$NadmB[[1]]-Nmiss.adm.B)/100,
        NadmH.adj.1 = tb412p$SadmH*(tb41n$NadmH[[1]]-Nmiss.adm.H)/100,
        NadmA.adj.1 = tb412p$SadmA*(tb41n$NadmA[[1]]-Nmiss.adm.A)/100
    )
# step 4: now get shares of missing who receive 2 or higher, conditional on accept and reject (uses formulas from appendix B)

# step 4a: first get difference in Ns between opening expanded sample (includes athletes + missing) and rebuttal expanded sample
tb32r %>% slice(42)
df625.diff <- tibble(oner=rep(1,10)) %>%
    mutate(
        rating = df625.appN$rating,
        NrejW  = df625.appN$NrejW.adj.1-(df623s$NtotW-df623s$NadmW),
        NrejB  = df625.appN$NrejB.adj.1-(df623s$NtotB-df623s$NadmB),
        NrejH  = df625.appN$NrejH.adj.1-(df623s$NtotH-df623s$NadmH),
        NrejA  = df625.appN$NrejA.adj.1-(df623s$NtotA-df623s$NadmA),
        NadmW  = df625.appN$NadmW.adj.1-df623s$NadmW,
        NadmB  = df625.appN$NadmB.adj.1-df623s$NadmB,
        NadmH  = df625.appN$NadmH.adj.1-df623s$NadmH,
        NadmA  = df625.appN$NadmA.adj.1-df623s$NadmA,
        NtotW  = df625.appN$NtotW.adj.1-df623s$NtotW,
        NtotB  = df625.appN$NtotB.adj.1-df623s$NtotB,
        NtotH  = df625.appN$NtotH.adj.1-df623s$NtotH,
        NtotA  = df625.appN$NtotA.adj.1-df623s$NtotA
    ) %>%
    select(-oner)

# step 4b: next get implied number of rejected athletes (using calculations on B.3.2 near the top of this file)
ArejW <- AappW-AadmW
ArejB <- AappB-AadmB
ArejH <- AappH-AadmH
ArejA <- AappA-AadmA

# step 4c: ... and number of missings who are not athletes (using B.3.2R)
MANappW <- tb32r %>% slice(42) %>% select(totW)
MANappB <- tb32r %>% slice(42) %>% select(totB)
MANappH <- tb32r %>% slice(42) %>% select(totH)
MANappA <- tb32r %>% slice(42) %>% select(totA)
MADappW <- tb41n$NtotW[[1]]-MANappW
MADappB <- tb41n$NtotB[[1]]-MANappB
MADappH <- tb41n$NtotH[[1]]-MANappH
MADappA <- tb41n$NtotA[[1]]-MANappA
MNAappW <- MADappW-AappW 
MNAappB <- MADappB-AappB 
MNAappH <- MADappH-AappH 
MNAappA <- MADappA-AappA 
MANadmW <- tb32r %>% slice(42) %>% select(admitW)
MANadmB <- tb32r %>% slice(42) %>% select(admitB)
MANadmH <- tb32r %>% slice(42) %>% select(admitH)
MANadmA <- tb32r %>% slice(42) %>% select(admitA)
MADadmW <- tb41n$NadmW[[1]]-MANadmW
MADadmB <- tb41n$NadmB[[1]]-MANadmB
MADadmH <- tb41n$NadmH[[1]]-MANadmH
MADadmA <- tb41n$NadmA[[1]]-MANadmA
MNAadmW <- MADadmW-AadmW 
MNAadmB <- MADadmB-AadmB 
MNAadmH <- MADadmH-AadmH 
MNAadmA <- MADadmA-AadmA 
MANrejW <- tb32r %>% slice(42) %>% select(rejectW)
MANrejB <- tb32r %>% slice(42) %>% select(rejectB)
MANrejH <- tb32r %>% slice(42) %>% select(rejectH)
MANrejA <- tb32r %>% slice(42) %>% select(rejectA)
MADrejW <- tb41n$NrejW[[1]]-MANrejW
MADrejB <- tb41n$NrejB[[1]]-MANrejB
MADrejH <- tb41n$NrejH[[1]]-MANrejH
MADrejA <- tb41n$NrejA[[1]]-MANrejA
MNArejW <- MADrejW-ArejW 
MNArejB <- MADrejB-ArejB 
MNArejH <- MADrejH-ArejH 
MNArejA <- MADrejA-ArejA 

# step 5: now create upper and lower bounds
# upper bound assumes that no  missing-but-not-athletes got a 2 or higher
# lower bound assumes that all missing-but-not-athletes got a 2 or higher
df625.diff %<>% mutate(
    S2p.adm.W.lb = (round(NadmW)-round(MNAadmW[[1]]))/round(AadmW[[1]]),
    S2p.adm.B.lb = (round(NadmB)-round(MNAadmB[[1]]))/round(AadmB[[1]]),
    S2p.adm.H.lb = (round(NadmH)-round(MNAadmH[[1]]))/round(AadmH[[1]]),
    S2p.adm.A.lb = (round(NadmA)-round(MNAadmA[[1]]))/round(AadmA[[1]]),
    S2p.adm.W.ub =  round(NadmW)                     /round(AadmW[[1]]),
    S2p.adm.B.ub =  round(NadmB)                     /round(AadmB[[1]]),
    S2p.adm.H.ub =  round(NadmH)                     /round(AadmH[[1]]),
    S2p.adm.A.ub =  round(NadmA)                     /round(AadmA[[1]]),
    S2p.adm.W.lb = pmax(S2p.adm.W.lb,0),
    S2p.adm.B.lb = pmax(S2p.adm.B.lb,0),
    S2p.adm.H.lb = pmax(S2p.adm.H.lb,0),
    S2p.adm.A.lb = pmax(S2p.adm.A.lb,0),
    S2p.adm.W.ub = pmin(S2p.adm.W.ub,1),
    S2p.adm.B.ub = pmin(S2p.adm.B.ub,1),
    S2p.adm.H.ub = pmin(S2p.adm.H.ub,1),
    S2p.adm.A.ub = pmin(S2p.adm.A.ub,1),
    S2p.app.W.lb = (round(NtotW)-round(MNAappW[[1]]))/round(AappW[[1]]),
    S2p.app.B.lb = (round(NtotB)-round(MNAappB[[1]]))/round(AappB[[1]]),
    S2p.app.H.lb = (round(NtotH)-round(MNAappH[[1]]))/round(AappH[[1]]),
    S2p.app.A.lb = (round(NtotA)-round(MNAappA[[1]]))/round(AappA[[1]]),
    S2p.app.W.ub =  round(NtotW)                     /round(AappW[[1]]),
    S2p.app.B.ub =  round(NtotB)                     /round(AappB[[1]]),
    S2p.app.H.ub =  round(NtotH)                     /round(AappH[[1]]),
    S2p.app.A.ub =  round(NtotA)                     /round(AappA[[1]]),
    S2p.app.W.lb = pmax(S2p.app.W.lb,0),
    S2p.app.B.lb = pmax(S2p.app.B.lb,0),
    S2p.app.H.lb = pmax(S2p.app.H.lb,0),
    S2p.app.A.lb = pmax(S2p.app.A.lb,0),
    S2p.app.W.ub = pmin(S2p.app.W.ub,1),
    S2p.app.B.ub = pmin(S2p.app.B.ub,1),
    S2p.app.H.ub = pmin(S2p.app.H.ub,1),
    S2p.app.A.ub = pmin(S2p.app.A.ub,1),
)

# now fill in values in Table 8 for athletes
T2athl %<>% mutate(
    A.adm.W = replace(A.adm.W,Rating=="LB Overall"        ,100*df625.diff$S2p.adm.W.lb[df625.diff$rating=="Overall"]),
    A.adm.W = replace(A.adm.W,Rating=="UB Overall"        ,100*df625.diff$S2p.adm.W.ub[df625.diff$rating=="Overall"]),
    A.adm.W = replace(A.adm.W,Rating=="LB Academic"       ,100*df625.diff$S2p.adm.W.lb[df625.diff$rating=="Academic"]),
    A.adm.W = replace(A.adm.W,Rating=="UB Academic"       ,100*df625.diff$S2p.adm.W.ub[df625.diff$rating=="Academic"]),
    A.adm.W = replace(A.adm.W,Rating=="LB Extracurricular",100*df625.diff$S2p.adm.W.lb[df625.diff$rating=="Extracurricular"]),
    A.adm.W = replace(A.adm.W,Rating=="UB Extracurricular",100*df625.diff$S2p.adm.W.ub[df625.diff$rating=="Extracurricular"]),
    A.adm.W = replace(A.adm.W,Rating=="LB Personal"       ,100*df625.diff$S2p.adm.W.lb[df625.diff$rating=="Personal"]),
    A.adm.W = replace(A.adm.W,Rating=="UB Personal"       ,100*df625.diff$S2p.adm.W.ub[df625.diff$rating=="Personal"]),
    A.adm.B = replace(A.adm.B,Rating=="LB Overall"        ,100*df625.diff$S2p.adm.B.lb[df625.diff$rating=="Overall"]),
    A.adm.B = replace(A.adm.B,Rating=="UB Overall"        ,100*df625.diff$S2p.adm.B.ub[df625.diff$rating=="Overall"]),
    A.adm.B = replace(A.adm.B,Rating=="LB Academic"       ,100*df625.diff$S2p.adm.B.lb[df625.diff$rating=="Academic"]),
    A.adm.B = replace(A.adm.B,Rating=="UB Academic"       ,100*df625.diff$S2p.adm.B.ub[df625.diff$rating=="Academic"]),
    A.adm.B = replace(A.adm.B,Rating=="LB Extracurricular",100*df625.diff$S2p.adm.B.lb[df625.diff$rating=="Extracurricular"]),
    A.adm.B = replace(A.adm.B,Rating=="UB Extracurricular",100*df625.diff$S2p.adm.B.ub[df625.diff$rating=="Extracurricular"]),
    A.adm.B = replace(A.adm.B,Rating=="LB Personal"       ,100*df625.diff$S2p.adm.B.lb[df625.diff$rating=="Personal"]),
    A.adm.B = replace(A.adm.B,Rating=="UB Personal"       ,100*df625.diff$S2p.adm.B.ub[df625.diff$rating=="Personal"]),
    A.adm.H = replace(A.adm.H,Rating=="LB Overall"        ,100*df625.diff$S2p.adm.H.lb[df625.diff$rating=="Overall"]),
    A.adm.H = replace(A.adm.H,Rating=="UB Overall"        ,100*df625.diff$S2p.adm.H.ub[df625.diff$rating=="Overall"]),
    A.adm.H = replace(A.adm.H,Rating=="LB Academic"       ,100*df625.diff$S2p.adm.H.lb[df625.diff$rating=="Academic"]),
    A.adm.H = replace(A.adm.H,Rating=="UB Academic"       ,100*df625.diff$S2p.adm.H.ub[df625.diff$rating=="Academic"]),
    A.adm.H = replace(A.adm.H,Rating=="LB Extracurricular",100*df625.diff$S2p.adm.H.lb[df625.diff$rating=="Extracurricular"]),
    A.adm.H = replace(A.adm.H,Rating=="UB Extracurricular",100*df625.diff$S2p.adm.H.ub[df625.diff$rating=="Extracurricular"]),
    A.adm.H = replace(A.adm.H,Rating=="LB Personal"       ,100*df625.diff$S2p.adm.H.lb[df625.diff$rating=="Personal"]),
    A.adm.H = replace(A.adm.H,Rating=="UB Personal"       ,100*df625.diff$S2p.adm.H.ub[df625.diff$rating=="Personal"]),
    A.adm.A = replace(A.adm.A,Rating=="LB Overall"        ,100*df625.diff$S2p.adm.A.lb[df625.diff$rating=="Overall"]),
    A.adm.A = replace(A.adm.A,Rating=="UB Overall"        ,100*df625.diff$S2p.adm.A.ub[df625.diff$rating=="Overall"]),
    A.adm.A = replace(A.adm.A,Rating=="LB Academic"       ,100*df625.diff$S2p.adm.A.lb[df625.diff$rating=="Academic"]),
    A.adm.A = replace(A.adm.A,Rating=="UB Academic"       ,100*df625.diff$S2p.adm.A.ub[df625.diff$rating=="Academic"]),
    A.adm.A = replace(A.adm.A,Rating=="LB Extracurricular",100*df625.diff$S2p.adm.A.lb[df625.diff$rating=="Extracurricular"]),
    A.adm.A = replace(A.adm.A,Rating=="UB Extracurricular",100*df625.diff$S2p.adm.A.ub[df625.diff$rating=="Extracurricular"]),
    A.adm.A = replace(A.adm.A,Rating=="LB Personal"       ,100*df625.diff$S2p.adm.A.lb[df625.diff$rating=="Personal"]),
    A.adm.A = replace(A.adm.A,Rating=="UB Personal"       ,100*df625.diff$S2p.adm.A.ub[df625.diff$rating=="Personal"])
)

T2athl %<>% select(Rating,starts_with("A")) %>%
    add_row(Rating = "LB Athletic", A.adm.W = 100, A.adm.B = 100, A.adm.H = 100, A.adm.A = 100) %>%
    add_row(Rating = "UB Athletic", A.adm.W = 100, A.adm.B = 100, A.adm.H = 100, A.adm.A = 100) %>%
    separate(Rating,c("bound","Rating"), sep=" ") %>%
    pivot_longer(cols = !c(bound,Rating), names_to = "race", names_prefix = "A.adm.", values_to = "share", values_drop_na = TRUE) %>%
    pivot_wider(names_from = bound, values_from = share) %>%
    rename(rating = Rating, ath.LB = LB, athl.UB = UB)

df625joined %<>% left_join(T2athl, by=c("rating","race"))

df625joined %<>% filter(rating %in% c("Overall","Academic","Extracurricular","Personal","Athletic"))

# export to LaTeX
df625joined %>% xtable(caption="Table 2", digits=2) %>% print(file="Outputs/T2.tex", floating=F, type="latex", booktabs=T, include.rownames=F, caption.placement="top")






###############################################################################
# Table 3: Coefficients from report
###############################################################################
# exact replica of existing table; nothing to do





###############################################################################
# Table 4: Admit rates for previous admits
###############################################################################
#Use tb32, tb31r, tb32r

# import numbers from second-to-last panel of Trial Exhibit P287 ("No ALDC prefs" column)
P287 <- tibble(race=as_factor(c("White","Black","Hispanic","Asian")),ARPA.noALDC = c(.6778,.9164,.9352,.8952))

# Compute number of NA admits (includes non-ALDC and LDC) from Table B.3.2R
totNAadmT <- tb32r %>% slice(42) %>% select(admit)  # total N admits
totNAadmW <- tb32r %>% slice(42) %>% select(admitW)
totNAadmB <- tb32r %>% slice(42) %>% select(admitB)
totNAadmH <- tb32r %>% slice(42) %>% select(admitH)
totNAadmA <- tb32r %>% slice(42) %>% select(admitA)

# Compute number of NA applicants (includes non-ALDC and LDC) from Table B.3.2R
totNAappT <- tb32r %>% slice(42) %>% select(tot)  # total N applicants
totNAappW <- tb32r %>% slice(42) %>% select(totW)
totNAappB <- tb32r %>% slice(42) %>% select(totB)
totNAappH <- tb32r %>% slice(42) %>% select(totH)
totNAappA <- tb32r %>% slice(42) %>% select(totA)

# Compute number of Non-ALDC admits from Table B.3.1R
totNALDCadmT <- tb31r %>% slice(42) %>% select(admit)  # total N admits
totNALDCadmW <- tb31r %>% slice(42) %>% select(admitW)
totNALDCadmB <- tb31r %>% slice(42) %>% select(admitB)
totNALDCadmH <- tb31r %>% slice(42) %>% select(admitH)
totNALDCadmA <- tb31r %>% slice(42) %>% select(admitA)

# Compute number of Non-ALDC applicants from Table B.3.1R
totNALDCappT <- tb31r %>% slice(42) %>% select(tot)  # total N applicants
totNALDCappW <- tb31r %>% slice(42) %>% select(totW)
totNALDCappB <- tb31r %>% slice(42) %>% select(totB)
totNALDCappH <- tb31r %>% slice(42) %>% select(totH)
totNALDCappA <- tb31r %>% slice(42) %>% select(totA)

# Fill in numbers of applicants and admits in each group using numbers from just above or computed near the very top of this script
T4  <-  tibble(variable=c("Total Admits","LDC Admits","Athlete Admits","","Total Applicants","LDC Applicants","Athlete Applicants","","Non-ALDC Admit Rate","ALDC Admit Rate","LDC Admit Rate","","Remove ALDC Preferences","Admit Rate for Previous Admits","Admit Rate for Previous ALDC Admits","New ALDC Admit Rate","New LDC Admit Rate, Upper Bound"),W=c(rep(0,3),NA,rep(0,3),NA,rep(0,3),NA,NA,rep(0,4))) %>%
    mutate(B = W, H = W, A = W) %>%
    mutate(
        W = replace(W,variable=="Athlete Admits",AadmW),
        B = replace(B,variable=="Athlete Admits",AadmB),
        H = replace(H,variable=="Athlete Admits",AadmH),
        A = replace(A,variable=="Athlete Admits",AadmA),
        W = replace(W,variable=="Total Admits",totNAadmW+AadmW),
        B = replace(B,variable=="Total Admits",totNAadmB+AadmB),
        H = replace(H,variable=="Total Admits",totNAadmH+AadmH),
        A = replace(A,variable=="Total Admits",totNAadmA+AadmA),
        W = replace(W,variable=="LDC Admits",totNAadmW-totNALDCadmW),
        B = replace(B,variable=="LDC Admits",totNAadmB-totNALDCadmB),
        H = replace(H,variable=="LDC Admits",totNAadmH-totNALDCadmH),
        A = replace(A,variable=="LDC Admits",totNAadmA-totNALDCadmA),
        W = replace(W,variable=="Athlete Applicants",AappW),
        B = replace(B,variable=="Athlete Applicants",AappB),
        H = replace(H,variable=="Athlete Applicants",AappH),
        A = replace(A,variable=="Athlete Applicants",AappA),
        W = replace(W,variable=="Total Applicants",totNAappW+AappW),
        B = replace(B,variable=="Total Applicants",totNAappB+AappB),
        H = replace(H,variable=="Total Applicants",totNAappH+AappH),
        A = replace(A,variable=="Total Applicants",totNAappA+AappA),
        W = replace(W,variable=="LDC Applicants",totNAappW-totNALDCappW),
        B = replace(B,variable=="LDC Applicants",totNAappB-totNALDCappB),
        H = replace(H,variable=="LDC Applicants",totNAappH-totNALDCappH),
        A = replace(A,variable=="LDC Applicants",totNAappA-totNALDCappA),
    )

# Compute admit rates using numbers in immediately preceding code block
ALDCadmitrateW <- (T4$W[[2]]+T4$W[[3]])/(T4$W[[6]]+T4$W[[7]])
ALDCadmitrateB <- (T4$B[[2]]+T4$B[[3]])/(T4$B[[6]]+T4$B[[7]])
ALDCadmitrateH <- (T4$H[[2]]+T4$H[[3]])/(T4$H[[6]]+T4$H[[7]])
ALDCadmitrateA <- (T4$A[[2]]+T4$A[[3]])/(T4$A[[6]]+T4$A[[7]])
LDCadmitrateW  <-  T4$W[[2]]/T4$W[[6]]
LDCadmitrateB  <-  T4$B[[2]]/T4$B[[6]]
LDCadmitrateH  <-  T4$H[[2]]/T4$H[[6]]
LDCadmitrateA  <-  T4$A[[2]]/T4$A[[6]]

# Insert admit rates (calculated near the very top of this script, or in the immediately preceding code block)
T4 %<>% mutate(
    W = replace(W,variable=="Non-ALDC Admit Rate",nonALDCadmitrateW),
    B = replace(B,variable=="Non-ALDC Admit Rate",nonALDCadmitrateB),
    H = replace(H,variable=="Non-ALDC Admit Rate",nonALDCadmitrateH),
    A = replace(A,variable=="Non-ALDC Admit Rate",nonALDCadmitrateA),
    W = replace(W,variable=="ALDC Admit Rate",ALDCadmitrateW*100),
    B = replace(B,variable=="ALDC Admit Rate",ALDCadmitrateB*100),
    H = replace(H,variable=="ALDC Admit Rate",ALDCadmitrateH*100),
    A = replace(A,variable=="ALDC Admit Rate",ALDCadmitrateA*100),
    W = replace(W,variable=="LDC Admit Rate",  LDCadmitrateW*100),
    B = replace(B,variable=="LDC Admit Rate",  LDCadmitrateB*100),
    H = replace(H,variable=="LDC Admit Rate",  LDCadmitrateH*100),
    A = replace(A,variable=="LDC Admit Rate",  LDCadmitrateA*100)
)

# Compute number of ALDC admits in new regime (= number of ALDC admits in old regime minus ARPA times total number of admits in old regime)
P287 %<>% mutate(numALDCadm.new = c(T4$W[[2]]+T4$W[[3]],T4$B[[2]]+T4$B[[3]],T4$H[[2]]+T4$H[[3]],T4$A[[2]]+T4$A[[3]])-(1-ARPA.noALDC)*c(T4$W[[1]],T4$B[[1]],T4$H[[1]],T4$A[[1]])) %>%
    
    # insert number of ALDC applicants
    mutate(numALDCapp.new = c(T4$W[[6]]+T4$W[[7]],T4$B[[6]]+T4$B[[7]],T4$H[[6]]+T4$H[[7]],T4$A[[6]]+T4$A[[7]])) %>%
    
    # create new admit rate for ALDCs
    mutate(ALDCadmitrateNew = numALDCadm.new/numALDCapp.new) %>%
    
    # create new ARPA for ALDCs
    mutate(ARPA.ALDC = ALDCadmitrateNew/c(T4$W[[10]],T4$B[[10]],T4$H[[10]],T4$A[[10]])) %>%
    
    # create upper bound on admit rate for LDCs: assume no athlete admits in new regime
    #mutate(numLDCadm.new.ub = c(T4$W[[2]],T4$B[[2]],T4$H[[2]],T4$A[[2]])-((1-ARPA.noALDC)*c(T4$W[[1]],T4$B[[1]],T4$H[[1]],T4$A[[1]])-c(T4$W[[3]],T4$B[[3]],T4$H[[3]],T4$A[[3]]))) %>%
    mutate(LDCadmitrateNew = numALDCadm.new/c(T4$W[[6]],T4$B[[6]],T4$H[[6]],T4$A[[6]]))

# Now insert admit rates for previous admits (from P287)
T4 %<>% mutate(
    W = replace(W,variable=="Admit Rate for Previous Admits"     ,100*P287$ARPA.noALDC[[1]]),
    B = replace(B,variable=="Admit Rate for Previous Admits"     ,100*P287$ARPA.noALDC[[2]]),
    H = replace(H,variable=="Admit Rate for Previous Admits"     ,100*P287$ARPA.noALDC[[3]]),
    A = replace(A,variable=="Admit Rate for Previous Admits"     ,100*P287$ARPA.noALDC[[4]]),
    W = replace(W,variable=="Admit Rate for Previous ALDC Admits",100*100*P287$ARPA.ALDC[[1]]),
    B = replace(B,variable=="Admit Rate for Previous ALDC Admits",100*100*P287$ARPA.ALDC[[2]]),
    H = replace(H,variable=="Admit Rate for Previous ALDC Admits",100*100*P287$ARPA.ALDC[[3]]),
    A = replace(A,variable=="Admit Rate for Previous ALDC Admits",100*100*P287$ARPA.ALDC[[4]]),
    W = replace(W,variable=="New ALDC Admit Rate"                ,100*P287$ALDCadmitrateNew[[1]]),
    B = replace(B,variable=="New ALDC Admit Rate"                ,100*P287$ALDCadmitrateNew[[2]]),
    H = replace(H,variable=="New ALDC Admit Rate"                ,100*P287$ALDCadmitrateNew[[3]]),
    A = replace(A,variable=="New ALDC Admit Rate"                ,100*P287$ALDCadmitrateNew[[4]]),
    W = replace(W,variable=="New LDC Admit Rate, Upper Bound"    ,100*P287$LDCadmitrateNew[[1]]),
    B = replace(B,variable=="New LDC Admit Rate, Upper Bound"    ,100*P287$LDCadmitrateNew[[2]]),
    H = replace(H,variable=="New LDC Admit Rate, Upper Bound"    ,100*P287$LDCadmitrateNew[[3]]),
    A = replace(A,variable=="New LDC Admit Rate, Upper Bound"    ,100*P287$LDCadmitrateNew[[4]]),
)

# export to LaTeX
mdat<-matrix(0,nrow=dim(T4)[1],ncol=dim(T4)[2]+1)
mdat[seq(9,dim(T4)[1]),]<-2
T4 %>% xtable(caption="Table 4", digits=mdat) %>% print(file="Outputs/T4.tex", floating=F, type="latex", booktabs=T, include.rownames=F, caption.placement="top")



###############################################################################
# Table 5
###############################################################################
# exact replica of existing table; nothing to do



###############################################################################
# Table 6
###############################################################################
# exact replica of existing table; nothing to do






###############################################################################
# Tables D1 and D2
###############################################################################
# similar process as Table 2, but use actual N and not N non-missing
# actual N comes from T2a and T2b data frames above

# Re-create data frames
df621.0  <- bind_rows(list(df621a,df621b,df621c,df621d,df621e,df621f,df621g,df621h,df621i,df621j))
df623.0  <- bind_rows(list(df623a,df623b,df623c,df623d,df623e,df623f,df623g,df623h,df623i,df623j))

# convert character columns to numeric and rename variables
df621.0 %<>% mutate(across(everything(),~str_replace(., "%"  ,  ""))) %>%
    mutate(across(everything(),~str_replace(., ","  ,  ""))) %>%
    mutate(across(everything(),~str_replace(., "\u2019", "0"))) %>%
    mutate(across(c(starts_with("V")),~as.numeric(.)))  %>%
    mutate(rating = as_factor(rating))  %>%
    rename(
        NadmW= V1, SadmW= V2, NrejW= V3, SrejW= V4, NtotW= V5, StotW= V6,
        NadmA= V7, SadmA= V8, NrejA= V9, SrejA=V10, NtotA=V11, StotA=V12,
        NadmB=V13, SadmB=V14, NrejB=V15, SrejB=V16, NtotB=V17, StotB=V18,
        NadmH=V19, SadmH=V20, NrejH=V21, SrejH=V22, NtotH=V23, StotH=V24,
        NadmT=V25, SadmT=V26, NrejT=V27, SrejT=V28, NtotT=V29, StotT=V30
    ) %>%
    replace(.,is.na(.),0)

df623.0 %<>% mutate(across(everything(),~str_replace(., "%"  ,  ""))) %>%
    mutate(across(everything(),~str_replace(., ","  ,  ""))) %>%
    mutate(across(everything(),~str_replace(., "\u2019", "0"))) %>%
    mutate(across(c(starts_with("V")),~as.numeric(.)))  %>%
    mutate(rating = as_factor(rating))  %>%
    rename(
        NadmW= V1, SadmW= V2, NrejW= V3, SrejW= V4, NtotW= V5, StotW= V6,
        NadmA= V7, SadmA= V8, NrejA= V9, SrejA=V10, NtotA=V11, StotA=V12,
        NadmB=V13, SadmB=V14, NrejB=V15, SrejB=V16, NtotB=V17, StotB=V18,
        NadmH=V19, SadmH=V20, NrejH=V21, SrejH=V22, NtotH=V23, StotH=V24,
        NadmT=V25, SadmT=V26, NrejT=V27, SrejT=V28, NtotT=V29, StotT=V30
    ) %>%
    replace(.,is.na(.),0)


# Now put in true total N's (not just N's corresponding to number of non-missing ratings)
df621.0 %<>% group_by(rating) %>%
    mutate(
        Nsum.admW=sum(NadmW),
        Nsum.admB=sum(NadmB),
        Nsum.admH=sum(NadmH),
        Nsum.admA=sum(NadmA),
        Nsum.admT=sum(NadmT),
        Nsum.rejW=sum(NrejW),
        Nsum.rejB=sum(NrejB),
        Nsum.rejH=sum(NrejH),
        Nsum.rejA=sum(NrejA),
        Nsum.rejT=sum(NrejT),
        Nsum.totW=sum(NtotW),
        Nsum.totB=sum(NtotB),
        Nsum.totH=sum(NtotH),
        Nsum.totA=sum(NtotA),
        Nsum.totT=sum(NtotT)
    ) %>%
    ungroup() %>%
    mutate(
        Ntru.admW=Tdb[6,2 ][[1]],
        Ntru.admB=Tdb[6,4 ][[1]],
        Ntru.admH=Tdb[6,6 ][[1]],
        Ntru.admA=Tdb[6,8 ][[1]],
        Ntru.admT=Tdb[6,10][[1]],
        Ntru.rejW=Tda[7,2 ][[1]]-Tdb[6,2 ][[1]],
        Ntru.rejB=Tda[7,4 ][[1]]-Tdb[6,4 ][[1]],
        Ntru.rejH=Tda[7,6 ][[1]]-Tdb[6,6 ][[1]],
        Ntru.rejA=Tda[7,8 ][[1]]-Tdb[6,8 ][[1]],
        Ntru.rejT=Tda[7,10][[1]]-Tdb[6,10][[1]],
        Ntru.totW=Tda[7,2 ][[1]],
        Ntru.totB=Tda[7,4 ][[1]],
        Ntru.totH=Tda[7,6 ][[1]],
        Ntru.totA=Tda[7,8 ][[1]],
        Ntru.totT=Tda[7,10][[1]]
    )

df623.0 %<>% group_by(rating) %>%
    mutate(
        Nsum.admW=sum(NadmW),
        Nsum.admB=sum(NadmB),
        Nsum.admH=sum(NadmH),
        Nsum.admA=sum(NadmA),
        Nsum.admT=sum(NadmT),
        Nsum.rejW=sum(NrejW),
        Nsum.rejB=sum(NrejB),
        Nsum.rejH=sum(NrejH),
        Nsum.rejA=sum(NrejA),
        Nsum.rejT=sum(NrejT),
        Nsum.totW=sum(NtotW),
        Nsum.totB=sum(NtotB),
        Nsum.totH=sum(NtotH),
        Nsum.totA=sum(NtotA),
        Nsum.totT=sum(NtotT)
    ) %>%
    ungroup() %>%
    mutate(
        Ntru.admW=Tdb[6,2 ][[1]]+Tdb[6,3 ][[1]],
        Ntru.admB=Tdb[6,4 ][[1]]+Tdb[6,5 ][[1]],
        Ntru.admH=Tdb[6,6 ][[1]]+Tdb[6,7 ][[1]],
        Ntru.admA=Tdb[6,8 ][[1]]+Tdb[6,9 ][[1]],
        Ntru.admT=Tdb[6,10][[1]]+Tdb[6,11][[1]],
        Ntru.rejW=(Tda[7,2 ][[1]]+Tda[7,3 ][[1]])-(Tdb[6,2 ][[1]]+Tdb[6,3 ][[1]]),
        Ntru.rejB=(Tda[7,4 ][[1]]+Tda[7,5 ][[1]])-(Tdb[6,4 ][[1]]+Tdb[6,5 ][[1]]),
        Ntru.rejH=(Tda[7,6 ][[1]]+Tda[7,7 ][[1]])-(Tdb[6,6 ][[1]]+Tdb[6,7 ][[1]]),
        Ntru.rejA=(Tda[7,8 ][[1]]+Tda[7,9 ][[1]])-(Tdb[6,8 ][[1]]+Tdb[6,9 ][[1]]),
        Ntru.rejT=(Tda[7,10][[1]]+Tda[7,11][[1]])-(Tdb[6,10][[1]]+Tdb[6,11][[1]]),
        Ntru.totW=Tda[7,2 ][[1]]+Tda[7,3 ][[1]],
        Ntru.totB=Tda[7,4 ][[1]]+Tda[7,5 ][[1]],
        Ntru.totH=Tda[7,6 ][[1]]+Tda[7,7 ][[1]],
        Ntru.totA=Tda[7,8 ][[1]]+Tda[7,9 ][[1]],
        Ntru.totT=Tda[7,10][[1]]+Tda[7,11][[1]]
    )

# now merge together baseline and expanded samples to be able to take difference to create LDC sample
df627 <- left_join(df621.0,df623.0, by=c("rating","ratnum"), suffix = c(".nALDC",".all")) %>%
    mutate(
        ratagg        = case_when(ratnum %in% c(1,2) ~ 1, ratnum==3 ~ 2, ratnum==4 ~ 3),
        NadmW.LDC     = NadmW.all     - NadmW.nALDC,
        NrejW.LDC     = NrejW.all     - NrejW.nALDC,
        NtotW.LDC     = NtotW.all     - NtotW.nALDC,
        NadmB.LDC     = NadmB.all     - NadmB.nALDC,
        NrejB.LDC     = NrejB.all     - NrejB.nALDC,
        NtotB.LDC     = NtotB.all     - NtotB.nALDC,
        NadmH.LDC     = NadmH.all     - NadmH.nALDC,
        NrejH.LDC     = NrejH.all     - NrejH.nALDC,
        NtotH.LDC     = NtotH.all     - NtotH.nALDC,
        NadmA.LDC     = NadmA.all     - NadmA.nALDC,
        NrejA.LDC     = NrejA.all     - NrejA.nALDC,
        NtotA.LDC     = NtotA.all     - NtotA.nALDC,
        NadmT.LDC     = NadmT.all     - NadmT.nALDC,
        NrejT.LDC     = NrejT.all     - NrejT.nALDC,
        NtotT.LDC     = NtotT.all     - NtotT.nALDC,
        Ntru.admW.LDC = Ntru.admW.all - Ntru.admW.nALDC,
        Ntru.rejW.LDC = Ntru.rejW.all - Ntru.rejW.nALDC,
        Ntru.totW.LDC = Ntru.totW.all - Ntru.totW.nALDC,
        Ntru.admB.LDC = Ntru.admB.all - Ntru.admB.nALDC,
        Ntru.rejB.LDC = Ntru.rejB.all - Ntru.rejB.nALDC,
        Ntru.totB.LDC = Ntru.totB.all - Ntru.totB.nALDC,
        Ntru.admH.LDC = Ntru.admH.all - Ntru.admH.nALDC,
        Ntru.rejH.LDC = Ntru.rejH.all - Ntru.rejH.nALDC,
        Ntru.totH.LDC = Ntru.totH.all - Ntru.totH.nALDC,
        Ntru.admA.LDC = Ntru.admA.all - Ntru.admA.nALDC,
        Ntru.rejA.LDC = Ntru.rejA.all - Ntru.rejA.nALDC,
        Ntru.totA.LDC = Ntru.totA.all - Ntru.totA.nALDC,
        Ntru.admT.LDC = Ntru.admT.all - Ntru.admT.nALDC,
        Ntru.rejT.LDC = Ntru.rejT.all - Ntru.rejT.nALDC,
        Ntru.totT.LDC = Ntru.totT.all - Ntru.totT.nALDC,
    ) %>%
    select(rating,ratnum,ratagg,starts_with("N"))

# create shares for applicants and admits
df627.1     <- df627 %>% group_by(rating,ratagg) %>% 
    mutate(across(c(starts_with("Nadm"),starts_with("Nrej"),starts_with("Ntot")),sum)) %>%
    select(rating,ratagg,starts_with("Nadm"),starts_with("Nrej"),starts_with("Ntot"),starts_with("Ntru")) %>%
    #mutate(across(c(starts_with("Ntru")),~replace(.,ratagg==1,./2))) %>%
    ungroup() %>%
    mutate(
        StotW.nALDC = NtotW.nALDC/Ntru.totW.nALDC*100,
        StotB.nALDC = NtotB.nALDC/Ntru.totB.nALDC*100,
        StotH.nALDC = NtotH.nALDC/Ntru.totH.nALDC*100,
        StotA.nALDC = NtotA.nALDC/Ntru.totA.nALDC*100,
        StotW.LDC   = NtotW.LDC  /Ntru.totW.LDC  *100,
        StotB.LDC   = NtotB.LDC  /Ntru.totB.LDC  *100,
        StotH.LDC   = NtotH.LDC  /Ntru.totH.LDC  *100,
        StotA.LDC   = NtotA.LDC  /Ntru.totA.LDC  *100,
        SadmW.nALDC = NadmW.nALDC/Ntru.admW.nALDC*100,
        SadmB.nALDC = NadmB.nALDC/Ntru.admB.nALDC*100,
        SadmH.nALDC = NadmH.nALDC/Ntru.admH.nALDC*100,
        SadmA.nALDC = NadmA.nALDC/Ntru.admA.nALDC*100,
        SadmW.LDC   = NadmW.LDC  /Ntru.admW.LDC  *100,
        SadmB.LDC   = NadmB.LDC  /Ntru.admB.LDC  *100,
        SadmH.LDC   = NadmH.LDC  /Ntru.admH.LDC  *100,
        SadmA.LDC   = NadmA.LDC  /Ntru.admA.LDC  *100,
    ) %>%
    distinct()

df627.app <- df627.1 %>% select(rating,ratagg,StotW.nALDC,StotW.LDC,StotB.nALDC,StotB.LDC,StotH.nALDC,StotH.LDC,StotA.nALDC,StotA.LDC)
df627.adm <- df627.1 %>% select(rating,ratagg,SadmW.nALDC,SadmW.LDC,SadmB.nALDC,SadmB.LDC,SadmH.nALDC,SadmH.LDC,SadmA.nALDC,SadmA.LDC)

# Export to LaTeX
df627.app %>% xtable(caption="Table D1") %>% print(file="Outputs/TD1.tex", floating=F, type="latex", booktabs=T, include.rownames=F, caption.placement="top")
df627.adm %>% xtable(caption="Table D2") %>% print(file="Outputs/TD2.tex", floating=F, type="latex", booktabs=T, include.rownames=F, caption.placement="top")









###############################################################################
# Table D3: Bounds on ratings for athlete applicants and admits
###############################################################################
# This is the same process as for Table 8, except we now also compute for athlete applicants (not just admits)
# We also present data for all of the ratings, not just Overall/Academic/Extracurricular/Personal

# fill in values in Table A4 Panel A for non-ALDCs
TD3a   <- tibble(Rating=c("LB Overall","UB Overall","LB Academic","UB Academic","LB Extracurricular","UB Extracurricular","LB Personal","UB Personal","LB Teacher 1","UB Teacher 1","LB Teacher 2","UB Teacher 2","LB Counselor","UB Counselor","LB Alumni Personal","UB Alumni Personal","LB Alumni Overall","UB Alumni Overall"),nALDC.app.W=rep(0,18),A.app.W=rep(0,18),nALDC.app.B=rep(0,18),A.app.B=rep(0,18),nALDC.app.H=rep(0,18),A.app.H=rep(0,18),nALDC.app.A=rep(0,18),A.app.A=rep(0,18)) %>%
    mutate(
        nALDC.app.W = replace(nALDC.app.W,Rating=="LB Overall"        ,100*df621s$StotW[df621s$rating=="Overall"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="UB Overall"        ,100*df621s$StotW[df621s$rating=="Overall"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="LB Academic"       ,100*df621s$StotW[df621s$rating=="Academic"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="UB Academic"       ,100*df621s$StotW[df621s$rating=="Academic"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="LB Extracurricular",100*df621s$StotW[df621s$rating=="Extracurricular"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="UB Extracurricular",100*df621s$StotW[df621s$rating=="Extracurricular"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="LB Personal"       ,100*df621s$StotW[df621s$rating=="Personal"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="UB Personal"       ,100*df621s$StotW[df621s$rating=="Personal"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="LB Teacher 1"      ,100*df621s$StotW[df621s$rating=="Teacher 1"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="UB Teacher 1"      ,100*df621s$StotW[df621s$rating=="Teacher 1"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="LB Teacher 2"      ,100*df621s$StotW[df621s$rating=="Teacher 2"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="UB Teacher 2"      ,100*df621s$StotW[df621s$rating=="Teacher 2"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="LB Counselor"      ,100*df621s$StotW[df621s$rating=="Counselor"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="UB Counselor"      ,100*df621s$StotW[df621s$rating=="Counselor"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="LB Alumni Personal",100*df621s$StotW[df621s$rating=="Alumni Personal"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="UB Alumni Personal",100*df621s$StotW[df621s$rating=="Alumni Personal"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="LB Alumni Overall" ,100*df621s$StotW[df621s$rating=="Alumni Overall"]),
        nALDC.app.W = replace(nALDC.app.W,Rating=="UB Alumni Overall" ,100*df621s$StotW[df621s$rating=="Alumni Overall"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="LB Overall"        ,100*df621s$StotB[df621s$rating=="Overall"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="UB Overall"        ,100*df621s$StotB[df621s$rating=="Overall"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="LB Academic"       ,100*df621s$StotB[df621s$rating=="Academic"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="UB Academic"       ,100*df621s$StotB[df621s$rating=="Academic"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="LB Extracurricular",100*df621s$StotB[df621s$rating=="Extracurricular"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="UB Extracurricular",100*df621s$StotB[df621s$rating=="Extracurricular"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="LB Personal"       ,100*df621s$StotB[df621s$rating=="Personal"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="UB Personal"       ,100*df621s$StotB[df621s$rating=="Personal"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="LB Teacher 1"      ,100*df621s$StotB[df621s$rating=="Teacher 1"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="UB Teacher 1"      ,100*df621s$StotB[df621s$rating=="Teacher 1"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="LB Teacher 2"      ,100*df621s$StotB[df621s$rating=="Teacher 2"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="UB Teacher 2"      ,100*df621s$StotB[df621s$rating=="Teacher 2"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="LB Counselor"      ,100*df621s$StotB[df621s$rating=="Counselor"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="UB Counselor"      ,100*df621s$StotB[df621s$rating=="Counselor"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="LB Alumni Personal",100*df621s$StotB[df621s$rating=="Alumni Personal"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="UB Alumni Personal",100*df621s$StotB[df621s$rating=="Alumni Personal"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="LB Alumni Overall" ,100*df621s$StotB[df621s$rating=="Alumni Overall"]),
        nALDC.app.B = replace(nALDC.app.B,Rating=="UB Alumni Overall" ,100*df621s$StotB[df621s$rating=="Alumni Overall"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="LB Overall"        ,100*df621s$StotH[df621s$rating=="Overall"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="UB Overall"        ,100*df621s$StotH[df621s$rating=="Overall"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="LB Academic"       ,100*df621s$StotH[df621s$rating=="Academic"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="UB Academic"       ,100*df621s$StotH[df621s$rating=="Academic"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="LB Extracurricular",100*df621s$StotH[df621s$rating=="Extracurricular"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="UB Extracurricular",100*df621s$StotH[df621s$rating=="Extracurricular"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="LB Personal"       ,100*df621s$StotH[df621s$rating=="Personal"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="UB Personal"       ,100*df621s$StotH[df621s$rating=="Personal"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="LB Teacher 1"      ,100*df621s$StotH[df621s$rating=="Teacher 1"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="UB Teacher 1"      ,100*df621s$StotH[df621s$rating=="Teacher 1"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="LB Teacher 2"      ,100*df621s$StotH[df621s$rating=="Teacher 2"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="UB Teacher 2"      ,100*df621s$StotH[df621s$rating=="Teacher 2"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="LB Counselor"      ,100*df621s$StotH[df621s$rating=="Counselor"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="UB Counselor"      ,100*df621s$StotH[df621s$rating=="Counselor"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="LB Alumni Personal",100*df621s$StotH[df621s$rating=="Alumni Personal"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="UB Alumni Personal",100*df621s$StotH[df621s$rating=="Alumni Personal"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="LB Alumni Overall" ,100*df621s$StotH[df621s$rating=="Alumni Overall"]),
        nALDC.app.H = replace(nALDC.app.H,Rating=="UB Alumni Overall" ,100*df621s$StotH[df621s$rating=="Alumni Overall"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="LB Overall"        ,100*df621s$StotA[df621s$rating=="Overall"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="UB Overall"        ,100*df621s$StotA[df621s$rating=="Overall"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="LB Academic"       ,100*df621s$StotA[df621s$rating=="Academic"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="UB Academic"       ,100*df621s$StotA[df621s$rating=="Academic"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="LB Extracurricular",100*df621s$StotA[df621s$rating=="Extracurricular"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="UB Extracurricular",100*df621s$StotA[df621s$rating=="Extracurricular"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="LB Personal"       ,100*df621s$StotA[df621s$rating=="Personal"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="UB Personal"       ,100*df621s$StotA[df621s$rating=="Personal"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="LB Teacher 1"      ,100*df621s$StotA[df621s$rating=="Teacher 1"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="UB Teacher 1"      ,100*df621s$StotA[df621s$rating=="Teacher 1"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="LB Teacher 2"      ,100*df621s$StotA[df621s$rating=="Teacher 2"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="UB Teacher 2"      ,100*df621s$StotA[df621s$rating=="Teacher 2"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="LB Counselor"      ,100*df621s$StotA[df621s$rating=="Counselor"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="UB Counselor"      ,100*df621s$StotA[df621s$rating=="Counselor"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="LB Alumni Personal",100*df621s$StotA[df621s$rating=="Alumni Personal"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="UB Alumni Personal",100*df621s$StotA[df621s$rating=="Alumni Personal"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="LB Alumni Overall" ,100*df621s$StotA[df621s$rating=="Alumni Overall"]),
        nALDC.app.A = replace(nALDC.app.A,Rating=="UB Alumni Overall" ,100*df621s$StotA[df621s$rating=="Alumni Overall"]),
    ) %>%
    # and now for athletes
    mutate(
        A.app.W = replace(A.app.W,Rating=="LB Overall"        ,100*df625.diff$S2p.app.W.lb[df625.diff$rating=="Overall"]),
        A.app.W = replace(A.app.W,Rating=="UB Overall"        ,100*df625.diff$S2p.app.W.ub[df625.diff$rating=="Overall"]),
        A.app.W = replace(A.app.W,Rating=="LB Academic"       ,100*df625.diff$S2p.app.W.lb[df625.diff$rating=="Academic"]),
        A.app.W = replace(A.app.W,Rating=="UB Academic"       ,100*df625.diff$S2p.app.W.ub[df625.diff$rating=="Academic"]),
        A.app.W = replace(A.app.W,Rating=="LB Extracurricular",100*df625.diff$S2p.app.W.lb[df625.diff$rating=="Extracurricular"]),
        A.app.W = replace(A.app.W,Rating=="UB Extracurricular",100*df625.diff$S2p.app.W.ub[df625.diff$rating=="Extracurricular"]),
        A.app.W = replace(A.app.W,Rating=="LB Personal"       ,100*df625.diff$S2p.app.W.lb[df625.diff$rating=="Personal"]),
        A.app.W = replace(A.app.W,Rating=="UB Personal"       ,100*df625.diff$S2p.app.W.ub[df625.diff$rating=="Personal"]),
        A.app.W = replace(A.app.W,Rating=="LB Teacher 1"      ,100*df625.diff$S2p.app.W.lb[df625.diff$rating=="Teacher 1"]),
        A.app.W = replace(A.app.W,Rating=="UB Teacher 1"      ,100*df625.diff$S2p.app.W.ub[df625.diff$rating=="Teacher 1"]),
        A.app.W = replace(A.app.W,Rating=="LB Teacher 2"      ,100*df625.diff$S2p.app.W.lb[df625.diff$rating=="Teacher 2"]),
        A.app.W = replace(A.app.W,Rating=="UB Teacher 2"      ,100*df625.diff$S2p.app.W.ub[df625.diff$rating=="Teacher 2"]),
        A.app.W = replace(A.app.W,Rating=="LB Counselor"      ,100*df625.diff$S2p.app.W.lb[df625.diff$rating=="Counselor"]),
        A.app.W = replace(A.app.W,Rating=="UB Counselor"      ,100*df625.diff$S2p.app.W.ub[df625.diff$rating=="Counselor"]),
        A.app.W = replace(A.app.W,Rating=="LB Alumni Personal",100*df625.diff$S2p.app.W.lb[df625.diff$rating=="Alumni Personal"]),
        A.app.W = replace(A.app.W,Rating=="UB Alumni Personal",100*df625.diff$S2p.app.W.ub[df625.diff$rating=="Alumni Personal"]),
        A.app.W = replace(A.app.W,Rating=="LB Alumni Overall" ,100*df625.diff$S2p.app.W.lb[df625.diff$rating=="Alumni Overall"]),
        A.app.W = replace(A.app.W,Rating=="UB Alumni Overall" ,100*df625.diff$S2p.app.W.ub[df625.diff$rating=="Alumni Overall"]),
        A.app.B = replace(A.app.B,Rating=="LB Overall"        ,100*df625.diff$S2p.app.B.lb[df625.diff$rating=="Overall"]),
        A.app.B = replace(A.app.B,Rating=="UB Overall"        ,100*df625.diff$S2p.app.B.ub[df625.diff$rating=="Overall"]),
        A.app.B = replace(A.app.B,Rating=="LB Academic"       ,100*df625.diff$S2p.app.B.lb[df625.diff$rating=="Academic"]),
        A.app.B = replace(A.app.B,Rating=="UB Academic"       ,100*df625.diff$S2p.app.B.ub[df625.diff$rating=="Academic"]),
        A.app.B = replace(A.app.B,Rating=="LB Extracurricular",100*df625.diff$S2p.app.B.lb[df625.diff$rating=="Extracurricular"]),
        A.app.B = replace(A.app.B,Rating=="UB Extracurricular",100*df625.diff$S2p.app.B.ub[df625.diff$rating=="Extracurricular"]),
        A.app.B = replace(A.app.B,Rating=="LB Personal"       ,100*df625.diff$S2p.app.B.lb[df625.diff$rating=="Personal"]),
        A.app.B = replace(A.app.B,Rating=="UB Personal"       ,100*df625.diff$S2p.app.B.ub[df625.diff$rating=="Personal"]),
        A.app.B = replace(A.app.B,Rating=="LB Teacher 1"      ,100*df625.diff$S2p.app.B.lb[df625.diff$rating=="Teacher 1"]),
        A.app.B = replace(A.app.B,Rating=="UB Teacher 1"      ,100*df625.diff$S2p.app.B.ub[df625.diff$rating=="Teacher 1"]),
        A.app.B = replace(A.app.B,Rating=="LB Teacher 2"      ,100*df625.diff$S2p.app.B.lb[df625.diff$rating=="Teacher 2"]),
        A.app.B = replace(A.app.B,Rating=="UB Teacher 2"      ,100*df625.diff$S2p.app.B.ub[df625.diff$rating=="Teacher 2"]),
        A.app.B = replace(A.app.B,Rating=="LB Counselor"      ,100*df625.diff$S2p.app.B.lb[df625.diff$rating=="Counselor"]),
        A.app.B = replace(A.app.B,Rating=="UB Counselor"      ,100*df625.diff$S2p.app.B.ub[df625.diff$rating=="Counselor"]),
        A.app.B = replace(A.app.B,Rating=="LB Alumni Personal",100*df625.diff$S2p.app.B.lb[df625.diff$rating=="Alumni Personal"]),
        A.app.B = replace(A.app.B,Rating=="UB Alumni Personal",100*df625.diff$S2p.app.B.ub[df625.diff$rating=="Alumni Personal"]),
        A.app.B = replace(A.app.B,Rating=="LB Alumni Overall" ,100*df625.diff$S2p.app.B.lb[df625.diff$rating=="Alumni Overall"]),
        A.app.B = replace(A.app.B,Rating=="UB Alumni Overall" ,100*df625.diff$S2p.app.B.ub[df625.diff$rating=="Alumni Overall"]),
        A.app.H = replace(A.app.H,Rating=="LB Overall"        ,100*df625.diff$S2p.app.H.lb[df625.diff$rating=="Overall"]),
        A.app.H = replace(A.app.H,Rating=="UB Overall"        ,100*df625.diff$S2p.app.H.ub[df625.diff$rating=="Overall"]),
        A.app.H = replace(A.app.H,Rating=="LB Academic"       ,100*df625.diff$S2p.app.H.lb[df625.diff$rating=="Academic"]),
        A.app.H = replace(A.app.H,Rating=="UB Academic"       ,100*df625.diff$S2p.app.H.ub[df625.diff$rating=="Academic"]),
        A.app.H = replace(A.app.H,Rating=="LB Extracurricular",100*df625.diff$S2p.app.H.lb[df625.diff$rating=="Extracurricular"]),
        A.app.H = replace(A.app.H,Rating=="UB Extracurricular",100*df625.diff$S2p.app.H.ub[df625.diff$rating=="Extracurricular"]),
        A.app.H = replace(A.app.H,Rating=="LB Personal"       ,100*df625.diff$S2p.app.H.lb[df625.diff$rating=="Personal"]),
        A.app.H = replace(A.app.H,Rating=="UB Personal"       ,100*df625.diff$S2p.app.H.ub[df625.diff$rating=="Personal"]),
        A.app.H = replace(A.app.H,Rating=="LB Teacher 1"      ,100*df625.diff$S2p.app.H.lb[df625.diff$rating=="Teacher 1"]),
        A.app.H = replace(A.app.H,Rating=="UB Teacher 1"      ,100*df625.diff$S2p.app.H.ub[df625.diff$rating=="Teacher 1"]),
        A.app.H = replace(A.app.H,Rating=="LB Teacher 2"      ,100*df625.diff$S2p.app.H.lb[df625.diff$rating=="Teacher 2"]),
        A.app.H = replace(A.app.H,Rating=="UB Teacher 2"      ,100*df625.diff$S2p.app.H.ub[df625.diff$rating=="Teacher 2"]),
        A.app.H = replace(A.app.H,Rating=="LB Counselor"      ,100*df625.diff$S2p.app.H.lb[df625.diff$rating=="Counselor"]),
        A.app.H = replace(A.app.H,Rating=="UB Counselor"      ,100*df625.diff$S2p.app.H.ub[df625.diff$rating=="Counselor"]),
        A.app.H = replace(A.app.H,Rating=="LB Alumni Personal",100*df625.diff$S2p.app.H.lb[df625.diff$rating=="Alumni Personal"]),
        A.app.H = replace(A.app.H,Rating=="UB Alumni Personal",100*df625.diff$S2p.app.H.ub[df625.diff$rating=="Alumni Personal"]),
        A.app.H = replace(A.app.H,Rating=="LB Alumni Overall" ,100*df625.diff$S2p.app.H.lb[df625.diff$rating=="Alumni Overall"]),
        A.app.H = replace(A.app.H,Rating=="UB Alumni Overall" ,100*df625.diff$S2p.app.H.ub[df625.diff$rating=="Alumni Overall"]),
        A.app.A = replace(A.app.A,Rating=="LB Overall"        ,100*df625.diff$S2p.app.A.lb[df625.diff$rating=="Overall"]),
        A.app.A = replace(A.app.A,Rating=="UB Overall"        ,100*df625.diff$S2p.app.A.ub[df625.diff$rating=="Overall"]),
        A.app.A = replace(A.app.A,Rating=="LB Academic"       ,100*df625.diff$S2p.app.A.lb[df625.diff$rating=="Academic"]),
        A.app.A = replace(A.app.A,Rating=="UB Academic"       ,100*df625.diff$S2p.app.A.ub[df625.diff$rating=="Academic"]),
        A.app.A = replace(A.app.A,Rating=="LB Extracurricular",100*df625.diff$S2p.app.A.lb[df625.diff$rating=="Extracurricular"]),
        A.app.A = replace(A.app.A,Rating=="UB Extracurricular",100*df625.diff$S2p.app.A.ub[df625.diff$rating=="Extracurricular"]),
        A.app.A = replace(A.app.A,Rating=="LB Personal"       ,100*df625.diff$S2p.app.A.lb[df625.diff$rating=="Personal"]),
        A.app.A = replace(A.app.A,Rating=="UB Personal"       ,100*df625.diff$S2p.app.A.ub[df625.diff$rating=="Personal"]),
        A.app.A = replace(A.app.A,Rating=="LB Teacher 1"      ,100*df625.diff$S2p.app.A.lb[df625.diff$rating=="Teacher 1"]),
        A.app.A = replace(A.app.A,Rating=="UB Teacher 1"      ,100*df625.diff$S2p.app.A.ub[df625.diff$rating=="Teacher 1"]),
        A.app.A = replace(A.app.A,Rating=="LB Teacher 2"      ,100*df625.diff$S2p.app.A.lb[df625.diff$rating=="Teacher 2"]),
        A.app.A = replace(A.app.A,Rating=="UB Teacher 2"      ,100*df625.diff$S2p.app.A.ub[df625.diff$rating=="Teacher 2"]),
        A.app.A = replace(A.app.A,Rating=="LB Counselor"      ,100*df625.diff$S2p.app.A.lb[df625.diff$rating=="Counselor"]),
        A.app.A = replace(A.app.A,Rating=="UB Counselor"      ,100*df625.diff$S2p.app.A.ub[df625.diff$rating=="Counselor"]),
        A.app.A = replace(A.app.A,Rating=="LB Alumni Personal",100*df625.diff$S2p.app.A.lb[df625.diff$rating=="Alumni Personal"]),
        A.app.A = replace(A.app.A,Rating=="UB Alumni Personal",100*df625.diff$S2p.app.A.ub[df625.diff$rating=="Alumni Personal"]),
        A.app.A = replace(A.app.A,Rating=="LB Alumni Overall" ,100*df625.diff$S2p.app.A.lb[df625.diff$rating=="Alumni Overall"]),
        A.app.A = replace(A.app.A,Rating=="UB Alumni Overall" ,100*df625.diff$S2p.app.A.ub[df625.diff$rating=="Alumni Overall"]),
    )

# fill in values in Table A4 Panel B for non-ALDCs
TD3b   <- tibble(Rating=c("LB Overall","UB Overall","LB Academic","UB Academic","LB Extracurricular","UB Extracurricular","LB Personal","UB Personal","LB Teacher 1","UB Teacher 1","LB Teacher 2","UB Teacher 2","LB Counselor","UB Counselor","LB Alumni Personal","UB Alumni Personal","LB Alumni Overall","UB Alumni Overall"),nALDC.adm.W=rep(0,18),A.adm.W=rep(0,18),nALDC.adm.B=rep(0,18),A.adm.B=rep(0,18),nALDC.adm.H=rep(0,18),A.adm.H=rep(0,18),nALDC.adm.A=rep(0,18),A.adm.A=rep(0,18)) %>%
    mutate(
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="LB Overall"        ,100*df621s$SadmW[df621s$rating=="Overall"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="UB Overall"        ,100*df621s$SadmW[df621s$rating=="Overall"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="LB Academic"       ,100*df621s$SadmW[df621s$rating=="Academic"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="UB Academic"       ,100*df621s$SadmW[df621s$rating=="Academic"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="LB Extracurricular",100*df621s$SadmW[df621s$rating=="Extracurricular"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="UB Extracurricular",100*df621s$SadmW[df621s$rating=="Extracurricular"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="LB Personal"       ,100*df621s$SadmW[df621s$rating=="Personal"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="UB Personal"       ,100*df621s$SadmW[df621s$rating=="Personal"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="LB Teacher 1"      ,100*df621s$SadmW[df621s$rating=="Teacher 1"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="UB Teacher 1"      ,100*df621s$SadmW[df621s$rating=="Teacher 1"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="LB Teacher 2"      ,100*df621s$SadmW[df621s$rating=="Teacher 2"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="UB Teacher 2"      ,100*df621s$SadmW[df621s$rating=="Teacher 2"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="LB Counselor"      ,100*df621s$SadmW[df621s$rating=="Counselor"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="UB Counselor"      ,100*df621s$SadmW[df621s$rating=="Counselor"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="LB Alumni Personal",100*df621s$SadmW[df621s$rating=="Alumni Personal"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="UB Alumni Personal",100*df621s$SadmW[df621s$rating=="Alumni Personal"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="LB Alumni Overall" ,100*df621s$SadmW[df621s$rating=="Alumni Overall"]),
        nALDC.adm.W = replace(nALDC.adm.W,Rating=="UB Alumni Overall" ,100*df621s$SadmW[df621s$rating=="Alumni Overall"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="LB Overall"        ,100*df621s$SadmB[df621s$rating=="Overall"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="UB Overall"        ,100*df621s$SadmB[df621s$rating=="Overall"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="LB Academic"       ,100*df621s$SadmB[df621s$rating=="Academic"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="UB Academic"       ,100*df621s$SadmB[df621s$rating=="Academic"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="LB Extracurricular",100*df621s$SadmB[df621s$rating=="Extracurricular"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="UB Extracurricular",100*df621s$SadmB[df621s$rating=="Extracurricular"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="LB Personal"       ,100*df621s$SadmB[df621s$rating=="Personal"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="UB Personal"       ,100*df621s$SadmB[df621s$rating=="Personal"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="LB Teacher 1"      ,100*df621s$SadmB[df621s$rating=="Teacher 1"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="UB Teacher 1"      ,100*df621s$SadmB[df621s$rating=="Teacher 1"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="LB Teacher 2"      ,100*df621s$SadmB[df621s$rating=="Teacher 2"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="UB Teacher 2"      ,100*df621s$SadmB[df621s$rating=="Teacher 2"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="LB Counselor"      ,100*df621s$SadmB[df621s$rating=="Counselor"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="UB Counselor"      ,100*df621s$SadmB[df621s$rating=="Counselor"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="LB Alumni Personal",100*df621s$SadmB[df621s$rating=="Alumni Personal"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="UB Alumni Personal",100*df621s$SadmB[df621s$rating=="Alumni Personal"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="LB Alumni Overall" ,100*df621s$SadmB[df621s$rating=="Alumni Overall"]),
        nALDC.adm.B = replace(nALDC.adm.B,Rating=="UB Alumni Overall" ,100*df621s$SadmB[df621s$rating=="Alumni Overall"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="LB Overall"        ,100*df621s$SadmH[df621s$rating=="Overall"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="UB Overall"        ,100*df621s$SadmH[df621s$rating=="Overall"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="LB Academic"       ,100*df621s$SadmH[df621s$rating=="Academic"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="UB Academic"       ,100*df621s$SadmH[df621s$rating=="Academic"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="LB Extracurricular",100*df621s$SadmH[df621s$rating=="Extracurricular"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="UB Extracurricular",100*df621s$SadmH[df621s$rating=="Extracurricular"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="LB Personal"       ,100*df621s$SadmH[df621s$rating=="Personal"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="UB Personal"       ,100*df621s$SadmH[df621s$rating=="Personal"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="LB Teacher 1"      ,100*df621s$SadmH[df621s$rating=="Teacher 1"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="UB Teacher 1"      ,100*df621s$SadmH[df621s$rating=="Teacher 1"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="LB Teacher 2"      ,100*df621s$SadmH[df621s$rating=="Teacher 2"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="UB Teacher 2"      ,100*df621s$SadmH[df621s$rating=="Teacher 2"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="LB Counselor"      ,100*df621s$SadmH[df621s$rating=="Counselor"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="UB Counselor"      ,100*df621s$SadmH[df621s$rating=="Counselor"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="LB Alumni Personal",100*df621s$SadmH[df621s$rating=="Alumni Personal"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="UB Alumni Personal",100*df621s$SadmH[df621s$rating=="Alumni Personal"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="LB Alumni Overall" ,100*df621s$SadmH[df621s$rating=="Alumni Overall"]),
        nALDC.adm.H = replace(nALDC.adm.H,Rating=="UB Alumni Overall" ,100*df621s$SadmH[df621s$rating=="Alumni Overall"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="LB Overall"        ,100*df621s$SadmA[df621s$rating=="Overall"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="UB Overall"        ,100*df621s$SadmA[df621s$rating=="Overall"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="LB Academic"       ,100*df621s$SadmA[df621s$rating=="Academic"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="UB Academic"       ,100*df621s$SadmA[df621s$rating=="Academic"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="LB Extracurricular",100*df621s$SadmA[df621s$rating=="Extracurricular"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="UB Extracurricular",100*df621s$SadmA[df621s$rating=="Extracurricular"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="LB Personal"       ,100*df621s$SadmA[df621s$rating=="Personal"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="UB Personal"       ,100*df621s$SadmA[df621s$rating=="Personal"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="LB Teacher 1"      ,100*df621s$SadmA[df621s$rating=="Teacher 1"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="UB Teacher 1"      ,100*df621s$SadmA[df621s$rating=="Teacher 1"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="LB Teacher 2"      ,100*df621s$SadmA[df621s$rating=="Teacher 2"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="UB Teacher 2"      ,100*df621s$SadmA[df621s$rating=="Teacher 2"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="LB Counselor"      ,100*df621s$SadmA[df621s$rating=="Counselor"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="UB Counselor"      ,100*df621s$SadmA[df621s$rating=="Counselor"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="LB Alumni Personal",100*df621s$SadmA[df621s$rating=="Alumni Personal"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="UB Alumni Personal",100*df621s$SadmA[df621s$rating=="Alumni Personal"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="LB Alumni Overall" ,100*df621s$SadmA[df621s$rating=="Alumni Overall"]),
        nALDC.adm.A = replace(nALDC.adm.A,Rating=="UB Alumni Overall" ,100*df621s$SadmA[df621s$rating=="Alumni Overall"]),
    ) %>%
    # and now for athletes
    mutate(
        A.adm.W = replace(A.adm.W,Rating=="LB Overall"        ,100*df625.diff$S2p.adm.W.lb[df625.diff$rating=="Overall"]),
        A.adm.W = replace(A.adm.W,Rating=="UB Overall"        ,100*df625.diff$S2p.adm.W.ub[df625.diff$rating=="Overall"]),
        A.adm.W = replace(A.adm.W,Rating=="LB Academic"       ,100*df625.diff$S2p.adm.W.lb[df625.diff$rating=="Academic"]),
        A.adm.W = replace(A.adm.W,Rating=="UB Academic"       ,100*df625.diff$S2p.adm.W.ub[df625.diff$rating=="Academic"]),
        A.adm.W = replace(A.adm.W,Rating=="LB Extracurricular",100*df625.diff$S2p.adm.W.lb[df625.diff$rating=="Extracurricular"]),
        A.adm.W = replace(A.adm.W,Rating=="UB Extracurricular",100*df625.diff$S2p.adm.W.ub[df625.diff$rating=="Extracurricular"]),
        A.adm.W = replace(A.adm.W,Rating=="LB Personal"       ,100*df625.diff$S2p.adm.W.lb[df625.diff$rating=="Personal"]),
        A.adm.W = replace(A.adm.W,Rating=="UB Personal"       ,100*df625.diff$S2p.adm.W.ub[df625.diff$rating=="Personal"]),
        A.adm.W = replace(A.adm.W,Rating=="LB Teacher 1"      ,100*df625.diff$S2p.adm.W.lb[df625.diff$rating=="Teacher 1"]),
        A.adm.W = replace(A.adm.W,Rating=="UB Teacher 1"      ,100*df625.diff$S2p.adm.W.ub[df625.diff$rating=="Teacher 1"]),
        A.adm.W = replace(A.adm.W,Rating=="LB Teacher 2"      ,100*df625.diff$S2p.adm.W.lb[df625.diff$rating=="Teacher 2"]),
        A.adm.W = replace(A.adm.W,Rating=="UB Teacher 2"      ,100*df625.diff$S2p.adm.W.ub[df625.diff$rating=="Teacher 2"]),
        A.adm.W = replace(A.adm.W,Rating=="LB Counselor"      ,100*df625.diff$S2p.adm.W.lb[df625.diff$rating=="Counselor"]),
        A.adm.W = replace(A.adm.W,Rating=="UB Counselor"      ,100*df625.diff$S2p.adm.W.ub[df625.diff$rating=="Counselor"]),
        A.adm.W = replace(A.adm.W,Rating=="LB Alumni Personal",100*df625.diff$S2p.adm.W.lb[df625.diff$rating=="Alumni Personal"]),
        A.adm.W = replace(A.adm.W,Rating=="UB Alumni Personal",100*df625.diff$S2p.adm.W.ub[df625.diff$rating=="Alumni Personal"]),
        A.adm.W = replace(A.adm.W,Rating=="LB Alumni Overall" ,100*df625.diff$S2p.adm.W.lb[df625.diff$rating=="Alumni Overall"]),
        A.adm.W = replace(A.adm.W,Rating=="UB Alumni Overall" ,100*df625.diff$S2p.adm.W.ub[df625.diff$rating=="Alumni Overall"]),
        A.adm.B = replace(A.adm.B,Rating=="LB Overall"        ,100*df625.diff$S2p.adm.B.lb[df625.diff$rating=="Overall"]),
        A.adm.B = replace(A.adm.B,Rating=="UB Overall"        ,100*df625.diff$S2p.adm.B.ub[df625.diff$rating=="Overall"]),
        A.adm.B = replace(A.adm.B,Rating=="LB Academic"       ,100*df625.diff$S2p.adm.B.lb[df625.diff$rating=="Academic"]),
        A.adm.B = replace(A.adm.B,Rating=="UB Academic"       ,100*df625.diff$S2p.adm.B.ub[df625.diff$rating=="Academic"]),
        A.adm.B = replace(A.adm.B,Rating=="LB Extracurricular",100*df625.diff$S2p.adm.B.lb[df625.diff$rating=="Extracurricular"]),
        A.adm.B = replace(A.adm.B,Rating=="UB Extracurricular",100*df625.diff$S2p.adm.B.ub[df625.diff$rating=="Extracurricular"]),
        A.adm.B = replace(A.adm.B,Rating=="LB Personal"       ,100*df625.diff$S2p.adm.B.lb[df625.diff$rating=="Personal"]),
        A.adm.B = replace(A.adm.B,Rating=="UB Personal"       ,100*df625.diff$S2p.adm.B.ub[df625.diff$rating=="Personal"]),
        A.adm.B = replace(A.adm.B,Rating=="LB Teacher 1"      ,100*df625.diff$S2p.adm.B.lb[df625.diff$rating=="Teacher 1"]),
        A.adm.B = replace(A.adm.B,Rating=="UB Teacher 1"      ,100*df625.diff$S2p.adm.B.ub[df625.diff$rating=="Teacher 1"]),
        A.adm.B = replace(A.adm.B,Rating=="LB Teacher 2"      ,100*df625.diff$S2p.adm.B.lb[df625.diff$rating=="Teacher 2"]),
        A.adm.B = replace(A.adm.B,Rating=="UB Teacher 2"      ,100*df625.diff$S2p.adm.B.ub[df625.diff$rating=="Teacher 2"]),
        A.adm.B = replace(A.adm.B,Rating=="LB Counselor"      ,100*df625.diff$S2p.adm.B.lb[df625.diff$rating=="Counselor"]),
        A.adm.B = replace(A.adm.B,Rating=="UB Counselor"      ,100*df625.diff$S2p.adm.B.ub[df625.diff$rating=="Counselor"]),
        A.adm.B = replace(A.adm.B,Rating=="LB Alumni Personal",100*df625.diff$S2p.adm.B.lb[df625.diff$rating=="Alumni Personal"]),
        A.adm.B = replace(A.adm.B,Rating=="UB Alumni Personal",100*df625.diff$S2p.adm.B.ub[df625.diff$rating=="Alumni Personal"]),
        A.adm.B = replace(A.adm.B,Rating=="LB Alumni Overall" ,100*df625.diff$S2p.adm.B.lb[df625.diff$rating=="Alumni Overall"]),
        A.adm.B = replace(A.adm.B,Rating=="UB Alumni Overall" ,100*df625.diff$S2p.adm.B.ub[df625.diff$rating=="Alumni Overall"]),
        A.adm.H = replace(A.adm.H,Rating=="LB Overall"        ,100*df625.diff$S2p.adm.H.lb[df625.diff$rating=="Overall"]),
        A.adm.H = replace(A.adm.H,Rating=="UB Overall"        ,100*df625.diff$S2p.adm.H.ub[df625.diff$rating=="Overall"]),
        A.adm.H = replace(A.adm.H,Rating=="LB Academic"       ,100*df625.diff$S2p.adm.H.lb[df625.diff$rating=="Academic"]),
        A.adm.H = replace(A.adm.H,Rating=="UB Academic"       ,100*df625.diff$S2p.adm.H.ub[df625.diff$rating=="Academic"]),
        A.adm.H = replace(A.adm.H,Rating=="LB Extracurricular",100*df625.diff$S2p.adm.H.lb[df625.diff$rating=="Extracurricular"]),
        A.adm.H = replace(A.adm.H,Rating=="UB Extracurricular",100*df625.diff$S2p.adm.H.ub[df625.diff$rating=="Extracurricular"]),
        A.adm.H = replace(A.adm.H,Rating=="LB Personal"       ,100*df625.diff$S2p.adm.H.lb[df625.diff$rating=="Personal"]),
        A.adm.H = replace(A.adm.H,Rating=="UB Personal"       ,100*df625.diff$S2p.adm.H.ub[df625.diff$rating=="Personal"]),
        A.adm.H = replace(A.adm.H,Rating=="LB Teacher 1"      ,100*df625.diff$S2p.adm.H.lb[df625.diff$rating=="Teacher 1"]),
        A.adm.H = replace(A.adm.H,Rating=="UB Teacher 1"      ,100*df625.diff$S2p.adm.H.ub[df625.diff$rating=="Teacher 1"]),
        A.adm.H = replace(A.adm.H,Rating=="LB Teacher 2"      ,100*df625.diff$S2p.adm.H.lb[df625.diff$rating=="Teacher 2"]),
        A.adm.H = replace(A.adm.H,Rating=="UB Teacher 2"      ,100*df625.diff$S2p.adm.H.ub[df625.diff$rating=="Teacher 2"]),
        A.adm.H = replace(A.adm.H,Rating=="LB Counselor"      ,100*df625.diff$S2p.adm.H.lb[df625.diff$rating=="Counselor"]),
        A.adm.H = replace(A.adm.H,Rating=="UB Counselor"      ,100*df625.diff$S2p.adm.H.ub[df625.diff$rating=="Counselor"]),
        A.adm.H = replace(A.adm.H,Rating=="LB Alumni Personal",100*df625.diff$S2p.adm.H.lb[df625.diff$rating=="Alumni Personal"]),
        A.adm.H = replace(A.adm.H,Rating=="UB Alumni Personal",100*df625.diff$S2p.adm.H.ub[df625.diff$rating=="Alumni Personal"]),
        A.adm.H = replace(A.adm.H,Rating=="LB Alumni Overall" ,100*df625.diff$S2p.adm.H.lb[df625.diff$rating=="Alumni Overall"]),
        A.adm.H = replace(A.adm.H,Rating=="UB Alumni Overall" ,100*df625.diff$S2p.adm.H.ub[df625.diff$rating=="Alumni Overall"]),
        A.adm.A = replace(A.adm.A,Rating=="LB Overall"        ,100*df625.diff$S2p.adm.A.lb[df625.diff$rating=="Overall"]),
        A.adm.A = replace(A.adm.A,Rating=="UB Overall"        ,100*df625.diff$S2p.adm.A.ub[df625.diff$rating=="Overall"]),
        A.adm.A = replace(A.adm.A,Rating=="LB Academic"       ,100*df625.diff$S2p.adm.A.lb[df625.diff$rating=="Academic"]),
        A.adm.A = replace(A.adm.A,Rating=="UB Academic"       ,100*df625.diff$S2p.adm.A.ub[df625.diff$rating=="Academic"]),
        A.adm.A = replace(A.adm.A,Rating=="LB Extracurricular",100*df625.diff$S2p.adm.A.lb[df625.diff$rating=="Extracurricular"]),
        A.adm.A = replace(A.adm.A,Rating=="UB Extracurricular",100*df625.diff$S2p.adm.A.ub[df625.diff$rating=="Extracurricular"]),
        A.adm.A = replace(A.adm.A,Rating=="LB Personal"       ,100*df625.diff$S2p.adm.A.lb[df625.diff$rating=="Personal"]),
        A.adm.A = replace(A.adm.A,Rating=="UB Personal"       ,100*df625.diff$S2p.adm.A.ub[df625.diff$rating=="Personal"]),
        A.adm.A = replace(A.adm.A,Rating=="LB Teacher 1"      ,100*df625.diff$S2p.adm.A.lb[df625.diff$rating=="Teacher 1"]),
        A.adm.A = replace(A.adm.A,Rating=="UB Teacher 1"      ,100*df625.diff$S2p.adm.A.ub[df625.diff$rating=="Teacher 1"]),
        A.adm.A = replace(A.adm.A,Rating=="LB Teacher 2"      ,100*df625.diff$S2p.adm.A.lb[df625.diff$rating=="Teacher 2"]),
        A.adm.A = replace(A.adm.A,Rating=="UB Teacher 2"      ,100*df625.diff$S2p.adm.A.ub[df625.diff$rating=="Teacher 2"]),
        A.adm.A = replace(A.adm.A,Rating=="LB Counselor"      ,100*df625.diff$S2p.adm.A.lb[df625.diff$rating=="Counselor"]),
        A.adm.A = replace(A.adm.A,Rating=="UB Counselor"      ,100*df625.diff$S2p.adm.A.ub[df625.diff$rating=="Counselor"]),
        A.adm.A = replace(A.adm.A,Rating=="LB Alumni Personal",100*df625.diff$S2p.adm.A.lb[df625.diff$rating=="Alumni Personal"]),
        A.adm.A = replace(A.adm.A,Rating=="UB Alumni Personal",100*df625.diff$S2p.adm.A.ub[df625.diff$rating=="Alumni Personal"]),
        A.adm.A = replace(A.adm.A,Rating=="LB Alumni Overall" ,100*df625.diff$S2p.adm.A.lb[df625.diff$rating=="Alumni Overall"]),
        A.adm.A = replace(A.adm.A,Rating=="UB Alumni Overall" ,100*df625.diff$S2p.adm.A.ub[df625.diff$rating=="Alumni Overall"]),
    )

# export to LaTeX
TD3a %>% xtable(caption="Table D3a", digits=2) %>% print(file="Outputs/TD3a.tex", floating=F, type="latex", booktabs=T, include.rownames=F, caption.placement="top")
TD3b %>% xtable(caption="Table D3b", digits=2) %>% print(file="Outputs/TD3b.tex", floating=F, type="latex", booktabs=T, include.rownames=F, caption.placement="top")




###############################################################################
# Table D4: Select coefficients from an alternative model with athletes incl.
###############################################################################
# exact replica of existing table; nothing to do
