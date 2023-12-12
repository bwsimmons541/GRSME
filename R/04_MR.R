#loaded up the fishr package using:
install.packages("FSA")

library(FSA)

librarian::shelf("FSA")

M<-134 #marked fish passed upstream
C<-8 #total recoveries/recaptures with discernable marking or non-marking
R<-2 #total recoveries/recaptures with marking

mr1 <- mrClosed(M,C,R, method = "Chapman")
summary(mr1)

summary(mr1,incl.SE=TRUE)
confint(mr1) #uses suggested interval from Kreb's 1989 p.18

#confint(mr1, type="binomial")
#confint(mr1, type="normal")

redds_u <- 114
redds_d <- 7

# REMEMBER TO ADD ALL WEIR REMOVALS AND HARVEST TO ESTIMATES 
# A_HAT
  M <- 144 # marked fish passed upstream
  C <- 31 # total recoveries/recaptures with discernable marking or non-marking
  R <- 21 # total recoveries/recaptures with marking
  
  mr_ahat <- mrClosed(M,C,R, method = "Chapman")
  summary(mr_ahat)
  summary(mr_ahat, incl.SE = TRUE)  
  confint(mr_ahat)
  
  A_HAT_NU <- 210
  
  A_HAT_ND <- (A_HAT_NU / redds_u) * redds_d
  
  A_HAT_TRIB <- round((A_HAT_NU + ((A_HAT_NU / redds_u) * redds_d)), 0)
  
# J_HAT
  M <- 5
  C <- 1
  R <- 0
  
  mr_jhat <- mrClosed(M,C,R, method = "Chapman")
  summary(mr_jhat)

  J_HAT_NU <- 11
  
  J_HAT_TRIB <- round((J_HAT_NU + ((J_HAT_NU / redds_u) * redds_d)), 0)
    
# A_NAT
  M <- 159
  C <- 14
  R <- 3

  mr_anat <- mrClosed(M,C,R, method = "Chapman")
  summary(mr_anat)
  summary(mr_anat, incl.SE = TRUE)
  confint(mr_anat)
  
  A_NAT_NU <- 599
    
  A_NAT_TRIB <- round((A_NAT_NU + ((A_NAT_NU / redds_u) * redds_d)), 0)
  
# J_Nat
  M <- 21
  C <- 0
  R <- 0
  
  mr_jnat <- mrClosed(M,C,R, method = "Chapman")
  summary(mr_jnat)

  J_NAT_NU <- 21    
  
  J_NAT_TRIB <- round((J_NAT_NU + ((J_NAT_NU / redds_u) * redds_d)), 0)

  
# Adults combined
  
  M <- 144+159
  C <- 31 + 14
  R <- 21 + 3 

  mr_adult <- mrClosed(M,C,R, method = "Chapman")  
  summary(mr_adult)  

  A_NU <- 558  

  A_TRIB <- round((A_NU + ((A_NU / redds_u) * redds_d)), 0)  

  33/49
  
  
  round(A_TRIB*(33/49),0)
  
  round(A_TRIB*(16/49),0)
  