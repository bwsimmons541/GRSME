library(tidyverse)
library(cdmsR)


cdmsR::cdmsLogin('lorat', 'Bull@trout82')

abnd<-get_JuvAbundance(MigratoryYear = 2023)

surv<-get_JuvSurvival(MigratoryYear = 2023)

#load(NPT_RST_Abundance, file = './suvival_smlt_eq/NPT_RST_Abundance.rda')
#load(NPT_RST_Survival, file = './suvival_smlt_eq/NPT_RST_Survival.rda')

names(abnd)
names(surv)

abnd.eq<-abnd %>%
  select(SpeciesRun, Species, Run, StreamName, 
         Origin, BroodYear, MigratoryYear, Lifestage, 
         StartDate, EndDate, Abundance, StdError, 
         Lower95, Upper95)

surv.eq<-surv %>%
  select(ESU_DPS, MPG, POP_NAME, Species, Run, 
         StreamName, TribToName, LocationLabel, 
         SpeciesRun, Origin, Hatchery, ReleaseType, 
         ReleaseGroup, Lifestage, Survival, StdError, 
         Lower95, Upper95)

surv.abnd<-left_join(surv.eq, abnd.eq, by = c("SpeciesRun", "Origin", "StreamName", "Lifestage"), 
                     keep = FALSE)

write.csv(surv.abnd, file = "my23_surv_abnd.csv")
names(surv.abnd)


###file my21_surv_abnd.csv -- changed header names and added hatchery release census data
###can modify with R - just ran out of time
###calculating smolt equivalents and 95% CIs for groups at LGR

smlt_eq <- read_csv("suvival_smlt_eq/my23_surv_abnd_041624.csv")



smlt.eq<-smlt_eq %>%
  mutate(w.smlteq = abd.est.trp * surv.est.lgj) %>%
  mutate(h.smlteq = hat.rls.abd * surv.est.lgj) %>%
  mutate(var.w = ((abd.est.trp^2 * surv.se.lgj^2) + (surv.est.lgj^2 * abd.se.trp^2) +
                    (abd.se.trp^2 * surv.se.lgj^2))) %>%
  mutate(sd.w = sqrt(var.w)) %>%
  mutate(lci.w = w.smlteq - (1.96 * sd.w)) %>%
  mutate(uci.w = w.smlteq + (1.96 * sd.w)) %>%
  mutate(var.h = (hat.rls.abd^2 * surv.se.lgj^2))  %>%
  mutate(sd.h = sqrt(var.h)) %>%
  mutate(lci.h = h.smlteq - (1.96 * sd.h)) %>%
  mutate(uci.h = h.smlteq + (1.96 * sd.h))


write.csv(smlt.eq, "suvival_smlt_eq/smlt_eq_lgr_041624.csv")
