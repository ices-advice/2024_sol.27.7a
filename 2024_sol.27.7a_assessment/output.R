## Extract results of interest, write TAF output tables

## Before:SAM_fit_sol_7a.RData and sol7a_stock.RData
## After:summary_table.csv, tab_mohn_rho.csv, natage.csv, fatage.csv 
## sole7a_retro.Rdata, sole7a_leaveout_fit.Rdata, sole7a_sim_fit.Rdata, sole7a_jit_fit.Rdata, sole7a_OSA_residuals.Rdata and sole7a_PRC_residuals.Rdata

mkdir("output")

#load SAM output en stock data
load('model/SAM_fit_sol_7a.RData')
load('data/sol7a_stock.RData')


SAM_fit_sol_7a$data$logobs #(to check the 'NA' for 2020 of UKBTS)

#enkel bij gebruik van data2.R 
stock_data<-sam_data

## f at age
fatage<-t(faytable(SAM_fit_sol_7a))
write.taf(fatage, dir = "output")

## n at age
natage<-t(ntable(SAM_fit_sol_7a))
write.taf(natage, dir = "output")

## ssb at age
ssbatage <- t(ntable(SAM_fit_sol_7a)*stock_data$sw * stock_data$mo)
write.taf(ssbatage, dir = "output")

## Residuals
OSA_residuals<- residuals(SAM_fit_sol_7a)
save(OSA_residuals,file="output/sol7a_OSA_residuals.Rdata")

PRC_residuals   <- procres(SAM_fit_sol_7a)
save(PRC_residuals,file="output/sol7a_PRC_residuals.Rdata")

## Standardised catchn to make plot later
catchn<-melt(stock_data$cn)
names(catchn)<-c("year","age","cn")
t<-aggregate(data=catchn,cn~year,sum)
catchn$totalcn<-t$cn[match(catchn$year,t$year)]
#catch proportions at age
catchn$prop<-catchn$cn/catchn$totalcn
#standardized catch proportion at age
t2<-aggregate(data=catchn,prop~age,sum)
t2$meanprop<-t2$prop/(SAM_fit_sol_7a$data$noYears)
catchn$meanprop<-t2$meanprop[match(catchn$age,t2$age)]
catchn$prop2<-(catchn$prop-catchn$meanprop)^2
t3<-aggregate(data=catchn,prop2~age,sum)
t3$meanprop<-t3$prop2/((SAM_fit_sol_7a$data$noYears)-1)
t3$meanprop<-sqrt(t3$meanprop)
catchn$meanprop2<-t3$meanprop[match(catchn$age,t3$age)]
catchn$propstand<-(catchn$prop-catchn$meanprop)/catchn$meanprop2
df_catchn <- as.data.frame(catchn) %>%
  mutate( signs = ifelse(propstand > 0, "Positive", "Negative" ))
save(df_catchn,file="output/sol7a_catchn.Rdata")


## Standardised survey indices to make plot later
tun_bts <- melt(stock_data$tun[[1]])
names(tun_bts)<-c("year","age","index")
tun_stan_bts <- scale(stock_data$tun[[1]], center = FALSE, scale = TRUE)
tun_stan_bts <- melt(tun_stan_bts)
names(tun_stan_bts)<-c("year","age","index_stan")
tun_bts$ID <-paste(tun_bts$year,"_",tun_bts$age, sep= "")
tun_stan_bts$ID <-paste(tun_stan_bts$year,"_",tun_stan_bts$age, sep= "")
tun_bts$index_stan <- tun_stan_bts$index_stan[match(tun_bts$ID,tun_stan_bts$ID)]
tun_bts$yearclass <- as.numeric(as.character(tun_bts$year))-as.numeric(as.character(tun_bts$age))
tun_bts<-(tun_bts[,c(1,6,2,3,5)])
save(tun_bts,file="output/sol7a_index.Rdata")

## Summary table
rec<-as.data.frame(rectable(SAM_fit_sol_7a))
ssb<-as.data.frame(ssbtable(SAM_fit_sol_7a))
tsb<-as.data.frame(tsbtable(SAM_fit_sol_7a))
fbar<-as.data.frame(fbartable(SAM_fit_sol_7a))
catch<-as.data.frame(catchtable(SAM_fit_sol_7a))

summary_table <- data.frame(
  year = min(SAM_fit_sol_7a$data$years):max(SAM_fit_sol_7a$data$years),
  'R low'= round(c(rec$Low)),
  R = round(rec$Estimate),
  'R high'= round(rec$High),
  'TSB low' = round(tsb$Low),
  TSB = round(tsb$Estimate),
  'TSB high' = round(tsb$High),
  'SSB low' = round(ssb$Low),
  SSB = round(ssb$Estimate),
  'SSB high' = round(ssb$High),
  'Catch low'= round(catch$Low),
  Catch = round(catch$Estimate),
  'Catch high'= round(catch$High),
  'Fbar low' = round(fbar$Low,3),
  Fbar = round(fbar$Estimate,3),
  'Fbar high' = round(fbar$High,3)
)
write.taf(summary_table, dir="output")

write.taf(fbar, dir="output")


## Model details
moddet <- as.data.frame(modeltable(SAM_fit_sol_7a))
write.taf(moddet, dir="output")
capture.output(moddet, file = "output/sol7a_moddet.csv")

## Diagnostics
conf<-SAM_fit_sol_7a$conf
capture.output(conf, file = "output/sol7a_configuration.csv")


Tun_list<-get_survey_table(tuning_list = stock_data$tun)
qtable <- qtable(SAM_fit_sol_7a)

Diag_table <- data.frame(
  Name = Tun_list[,1],
  Type = Tun_list[,2],
  Years = Tun_list[,3],
  Ages = Tun_list[,4],
  'LogQ_age1' = round(qtable[,1],3),
  'sd_age1'= round((attr(qtable,"sd")[,1]),3),
  'LogQ_age2' = round(qtable[,2],3),
  'sd_age2'= round((attr(qtable,"sd")[,2]),3),
  'LogQ_age3' = round(qtable[,3],3),
  'sd_age3'= round((attr(qtable,"sd")[,3]),3),
  'LogQ_age4' = round(qtable[,4],3),
  'sd_age4'= round((attr(qtable,"sd")[,4]),3),
  'LogQ_age5' = round(qtable[,5],3),
  'sd_age5'= round((attr(qtable,"sd")[,5]),3),
  'LogQ_age6' = round(qtable[,6],3),
  'sd_age6'= round((attr(qtable,"sd")[,6]),3),
  'LogQ_age7' = round(qtable[,7],3),
  'sd_age7'= round((attr(qtable,"sd")[,7]),3)
)
rownames(Diag_table) <- NULL
Diag_table_1<-Diag_table[,1:12]
Diag_table_2<-Diag_table[,c(1,2,3,4,13:18)]

write.taf(Diag_table, dir="output")
capture.output(print(Diag_table_1, row.names = FALSE), file = "output/sol7a_Diag_table_1.csv")
capture.output(print(Diag_table_2, row.names = FALSE), file = "output/sol7a_Diag_table_2.csv")


## Retro analysis (current assessment + 5 peels)
n_retro_years<-4
retrofits    <- retro(SAM_fit_sol_7a,year = (max(SAM_fit_sol_7a$data$years)):(max(SAM_fit_sol_7a$data$years) - n_retro_years), ncores = 4)
retrofits    <- retro(SAM_fit_sol_7a,year = 5) #similar results 5 peels + current fit

save(retrofits,file="output/sol7a_retro.Rdata")
summary(retrofits[[1]])
summary(retrofits[[2]])
summary(retrofits[[3]])
summary(retrofits[[4]])
summary(retrofits[[5]])


## Mohn's Rho calculation
mohn_retro<-as.data.frame(mohn(retrofits))
tab_mohn_rho <- xtab2taf(mohn_retro)
names(tab_mohn_rho)[1]<-"Parameter"
names(tab_mohn_rho)[2]<-"Mohns Rho value"
tab_mohn_rho$`Mohns Rho value`<-round(tab_mohn_rho$`Mohns Rho value`,5)
write.taf(tab_mohn_rho, dir = "output")

## Leave one out analysis
leaveout_fit <- leaveout(SAM_fit_sol_7a, fleet = as.list(c(2:SAM_fit_sol_7a$data$noFleets)), ncores = 4)
save(leaveout_fit,file="output/sol7a_leaveout_fit.Rdata")

## Simulation analysis
sim_fit <- simstudy(SAM_fit_sol_7a, nsim = 100, ncores = 4)
save(sim_fit,file="output/sol7a_sim_fit.Rdata")

## Jitter analysis
jit_fit <- jit(SAM_fit_sol_7a, nojit = 50, ncores = 4)
save(jit_fit,file="output/sol7a_jit_fit.Rdata")

###-----------------------------------------------------------------------------
###   Short term forcasting
###-----------------------------------------------------------------------------

## Reference points input values
Fmsy            <- 0.164
Fmsy_lw         <- 0.125
Fmsy_up         <- 0.183
Fpa             <- 0.183 
Flim            <- 0.570
Blim            <- 2235
Btrigger        <- 3129

## Previous advice
Advice_int <- 625

## Define years
base_year              <- max(SAM_fit_sol_7a$data$years)   # base year = last data year
first_year             <- min(SAM_fit_sol_7a$data$years)   # first year = first data year
nyears                 <- (base_year - first_year)+1

## fbar of last data year
Flast <- fbartable(SAM_fit_sol_7a)[nrow(fbartable(SAM_fit_sol_7a)),1]# from SAM assessment #0.1525521

# mean F last 3 data years
Fsq <- mean(fbartable(SAM_fit_sol_7a)[((nyears-2):(nyears)),1]) #0.1156986

## Catch intermediate year
catch_int_year  <- 625 #The TAC 2023 (605); prelim catch stats 2023 (524)

## Forecast settings
#recr_years             <- first_year:(base_year-2) # recruitment years to resample from (all minus last 2 data years) - in line with refpoints calculation
recr_years             <- (base_year-8):(base_year-1)
avg_years              <- base_year + (-2:0)                              # nr years to calculate stock weight (SW) and to calculate F (selectivity of the fishery)
#SSB_final              <- ssbtable(SAM_fit_sol_7fg)[SAM_fit_sol_7fg$data$years == base_year,1]
nr_sims                <- 10001



###############################################################################################
#3 options for Intermediate year
set.seed(123)
Fmsy_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                           catchval.exact=c(NA,catch_int_year,NA,NA),
                                           fscale = c(NA,NA,NA,1),
                                           fval = c(Flast,NA,Fmsy,NA),
                                           nosim = nr_sims, year.base = base_year,
                                           ave.years = avg_years,
                                           rec.years = recr_years,
                                           deterministic = FALSE,
                                           processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                           lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "Fmsy", savesim = TRUE)

tab_TAC<-attributes(Fmsy_forecast)$shorttab

set.seed(123)
Fsq_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                          catchval.exact=c(NA,NA,NA,NA),
                                          fscale = c(NA,NA,NA,1),
                                          fval = c(Flast,Fsq,Fmsy,NA),
                                          nosim = nr_sims, year.base = base_year,
                                          ave.years = avg_years,
                                          rec.years = recr_years,
                                          deterministic = FALSE,
                                          processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                          lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "Fsq", savesim = TRUE)

tab_Fsq<-attributes(Fsq_forecast)$shorttab


set.seed(123)
Flast_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                            catchval.exact=c(NA,NA,NA,NA),
                                            fscale = c(NA,NA,NA,1),
                                            fval = c(Flast,Flast,Fmsy,NA),
                                            nosim = nr_sims, year.base = base_year,
                                            ave.years = avg_years,
                                            rec.years = recr_years,
                                            deterministic = FALSE,
                                            processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                            lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "Fsq", savesim = TRUE)

tab_Flast<-attributes(Flast_forecast)$shorttab


tab_options<-rbind(tab_TAC,tab_Fsq,tab_Flast)
save(tab_options,file="output/sol7a_tab_options.Rdata")
#######################################################################################################################


## Make catch scenario table for Faverage
Basis<-c("Fmsy","Fmsy_lower","Fmsy_upper","F=0","Fpa","Flim","SSBintplus2=Blim","SSBintplus2=MSYBtrigger","SSBintplus2=SSBintplus1","F=Fint")
TotalCatch<-0;ProjectedLandings<-0;ProjectedDiscards<-0;Ftotal<-0;FprojectedLandings<-0;FprojectedDiscards<-0;SSBintplus2<-0;SSBchange<-0;TACchange<-0;AdviceChange<-0
catchScenariosSplit<-data.frame(Basis,TotalCatch,ProjectedLandings,ProjectedDiscards,Ftotal,FprojectedLandings,FprojectedDiscards,SSBintplus2,SSBchange,TACchange,AdviceChange)
catchScenariosSplit$Basis<-as.character(catchScenariosSplit$Basis)
catchScenariosSplit

# FMSY catch option
set.seed(123)
Fmsy_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                           catchval.exact=c(NA,NA,NA,NA),
                                           fscale = c(NA,NA,NA,1),
                                           fval = c(Flast,Fsq,Fmsy,NA),
                                           nosim = nr_sims, year.base = base_year,
                                           ave.years = avg_years,
                                           rec.years = recr_years,
                                           deterministic = FALSE,
                                           processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                           lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "Fmsy", savesim = TRUE)

tab<-attributes(Fmsy_forecast)$shorttab
msytab<-attributes(Fmsy_forecast)$shorttab
catchScenariosSplit[1,2:11]<-c(tab[4,3],tab[7,3],tab[8,3],tab[1,3],tab[5,3],tab[6,3],tab[3,4],((tab[3,4]-tab[3,3])/tab[3,3])*100,((tab[4,3]-catch_int_year)/catch_int_year)*100,((tab[4,3]-Advice_int)/Advice_int)*100)                  

taf.png("Fmsy_forecast.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
plot(Fmsy_forecast)
dev.off()

prob_Fmsy<-100*sum(Fmsy_forecast[[length(Fmsy_forecast)]]$ssb < Blim)/nr_sims
check<-Fmsy_forecast[[length(Fmsy_forecast)]]$ssb < Blim
table(check)

####################################################################################################################
#addTSB=TRUE voor de total biomass (SAG template)
set.seed(123)
Fmsy_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                           catchval.exact=c(NA,NA,NA,NA),
                                           fscale = c(NA,NA,NA,1),
                                           fval = c(Flast,Fsq,Fmsy,NA),
                                           nosim = nr_sims, year.base = base_year,
                                           ave.years = avg_years,
                                           rec.years = recr_years,
                                           deterministic = FALSE,
                                           processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                           lagR = FALSE, splitLD = TRUE, addTSB = TRUE, label = "Fmsy", savesim = TRUE)


attributes(Fmsy_forecast)$shorttab
####################################################################################################################
# FMSY_lower catch option
set.seed(123)
Fmsy_lower_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                                 catchval.exact=c(NA,NA,NA,NA),
                                                 fscale = c(NA,NA,NA,1),
                                                 fval = c(Flast,Fsq,Fmsy_lw,NA),
                                                 nosim = nr_sims, year.base = base_year,
                                                 ave.years = avg_years,
                                                 rec.years = recr_years,
                                                 deterministic = FALSE,
                                                 processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                                 lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "Fmsy_lower", savesim = TRUE)

tab<-attributes(Fmsy_lower_forecast)$shorttab
catchScenariosSplit[2,2:11]<-c(tab[4,3],tab[7,3],tab[8,3],tab[1,3],tab[5,3],tab[6,3],tab[3,4],((tab[3,4]-tab[3,3])/tab[3,3])*100,((tab[4,3]-catch_int_year)/catch_int_year)*100,((tab[4,3]-Advice_int)/Advice_int)*100)                  

prob_Fmsy_lower<-100*sum(Fmsy_lower_forecast[[length(Fmsy_lower_forecast)]]$ssb < Blim)/nr_sims
check<-Fmsy_lower_forecast[[length(Fmsy_lower_forecast)]]$ssb < Blim
check2<-Fmsy_lower_forecast[[length(Fmsy_lower_forecast)]]$ssb
table(check)
min(check2)


# FMSY_upper catch option
set.seed(123)
Fmsy_upper_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                                 catchval.exact=c(NA,NA,NA,NA),
                                                 fscale = c(NA,NA,NA,1),
                                                 fval = c(Flast,Fsq,Fmsy_up,NA),
                                                 nosim = nr_sims, year.base = base_year,
                                                 ave.years = avg_years,
                                                 rec.years = recr_years,
                                                 deterministic = FALSE,
                                                 processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                                 lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "Fmsy_upper", savesim = TRUE)

tab<-attributes(Fmsy_upper_forecast)$shorttab
catchScenariosSplit[3,2:11]<-c(tab[4,3],tab[7,3],tab[8,3],tab[1,3],tab[5,3],tab[6,3],tab[3,4],((tab[3,4]-tab[3,3])/tab[3,3])*100,((tab[4,3]-catch_int_year)/catch_int_year)*100,((tab[4,3]-Advice_int)/Advice_int)*100)                  

prob_Fmsy_upper<-100*sum(Fmsy_upper_forecast[[length(Fmsy_upper_forecast)]]$ssb < Blim)/nr_sims
check<-Fmsy_upper_forecast[[length(Fmsy_upper_forecast)]]$ssb < Blim
check2<-Fmsy_upper_forecast[[length(Fmsy_upper_forecast)]]$ssb
table(check)
min(check2)


# F=0 catch option
set.seed(123)
Fnul_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                           catchval.exact=c(NA,NA,NA,NA),
                                           fscale = c(NA,NA,NA,1),
                                           fval = c(Flast,Fsq,0,NA),
                                           nosim = nr_sims, year.base = base_year,
                                           ave.years = avg_years,
                                           rec.years = recr_years,
                                           deterministic = FALSE,
                                           processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                           lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "Fnul", savesim = TRUE)

tab<-attributes(Fnul_forecast)$shorttab
catchScenariosSplit[4,2:11]<-c(tab[4,3],tab[7,3],tab[8,3],tab[1,3],tab[5,3],tab[6,3],tab[3,4],((tab[3,4]-tab[3,3])/tab[3,3])*100,((tab[4,3]-catch_int_year)/catch_int_year)*100,((tab[4,3]-Advice_int)/Advice_int)*100)                  

prob_Fnul_forecast<-100*sum(Fnul_forecast[[length(Fnul_forecast)]]$ssb < Blim)/nr_sims
check<-Fnul_forecast[[length(Fnul_forecast)]]$ssb < Blim
check2<-Fnul_forecast[[length(Fnul_forecast)]]$ssb
table(check)
min(check2)


# F=Fpa catch option
set.seed(123)
Fpa_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                          catchval.exact=c(NA,NA,NA,NA),
                                          fscale = c(NA,NA,NA,1),
                                          fval = c(Flast,Fsq,Fpa,NA),
                                          nosim = nr_sims, year.base = base_year,
                                          ave.years = avg_years,
                                          rec.years = recr_years,
                                          deterministic = FALSE,
                                          processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                          lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "Fpa", savesim = TRUE)

tab<-attributes(Fpa_forecast)$shorttab
catchScenariosSplit[5,2:11]<-c(tab[4,3],tab[7,3],tab[8,3],tab[1,3],tab[5,3],tab[6,3],tab[3,4],((tab[3,4]-tab[3,3])/tab[3,3])*100,((tab[4,3]-catch_int_year)/catch_int_year)*100,((tab[4,3]-Advice_int)/Advice_int)*100)                  

prob_Fpa_forecast<-100*sum(Fpa_forecast[[length(Fpa_forecast)]]$ssb < Blim)/nr_sims
check<-Fpa_forecast[[length(Fpa_forecast)]]$ssb < Blim
check2<-Fpa_forecast[[length(Fpa_forecast)]]$ssb
table(check)
min(check2)

# F=Flim catch option
set.seed(123)
Flim_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                           catchval.exact=c(NA,NA,NA,NA),
                                           fscale = c(NA,NA,NA,1),
                                           fval = c(Flast,Fsq,Flim,NA),
                                           nosim = nr_sims, year.base = base_year,
                                           ave.years = avg_years,
                                           rec.years = recr_years,
                                           deterministic = FALSE,
                                           processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                           lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "Flim", savesim = TRUE)

tab<-attributes(Flim_forecast)$shorttab
catchScenariosSplit[6,2:11]<-c(tab[4,3],tab[7,3],tab[8,3],tab[1,3],tab[5,3],tab[6,3],tab[3,4],((tab[3,4]-tab[3,3])/tab[3,3])*100,((tab[4,3]-catch_int_year)/catch_int_year)*100,((tab[4,3]-Advice_int)/Advice_int)*100)                  

prob_Flim_forecast<-100*sum(Flim_forecast[[length(Flim_forecast)]]$ssb < Blim)/nr_sims
check<-Flim_forecast[[length(Flim_forecast)]]$ssb < Blim
check2<-Flim_forecast[[length(Flim_forecast)]]$ssb
table(check)
min(check2)

# Blim catch option
set.seed(123)
Blim_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                           catchval.exact=c(NA,NA,NA,NA),
                                           fscale = c(NA,NA,NA,1),
                                           fval = c(Flast,Fsq,NA,NA),
                                           nextssb = c(NA,NA,Blim,NA),
                                           nosim = nr_sims, year.base = base_year,
                                           ave.years = avg_years,
                                           rec.years = recr_years,
                                           deterministic = FALSE,
                                           processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                           lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "Blim", savesim = TRUE)

tab<-attributes(Blim_forecast)$shorttab
catchScenariosSplit[7,2:11]<-c(tab[4,3],tab[7,3],tab[8,3],tab[1,3],tab[5,3],tab[6,3],tab[3,4],((tab[3,4]-tab[3,3])/tab[3,3])*100,((tab[4,3]-catch_int_year)/catch_int_year)*100,((tab[4,3]-Advice_int)/Advice_int)*100)                  

prob_Blim_forecast<-100*sum(Blim_forecast[[length(Blim_forecast)]]$ssb < Blim)/nr_sims
check<-Blim_forecast[[length(Blim_forecast)]]$ssb < Blim
check2<-Blim_forecast[[length(Blim_forecast)]]$ssb
table(check)
min(check2)

# Btrigger catch option
set.seed(123)
Btrigger_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                               catchval.exact=c(NA,NA,NA,NA),
                                               fscale = c(NA,NA,NA,1),
                                               fval = c(Flast,Fsq,NA,NA),
                                               nextssb = c(NA,NA,Btrigger,NA),
                                               nosim = nr_sims, year.base = base_year,
                                               ave.years = avg_years,
                                               rec.years = recr_years,
                                               deterministic = FALSE,
                                               processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                               lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "Btrigger", savesim = TRUE)

tab<-attributes(Btrigger_forecast)$shorttab
catchScenariosSplit[8,2:11]<-c(tab[4,3],tab[7,3],tab[8,3],tab[1,3],tab[5,3],tab[6,3],tab[3,4],((tab[3,4]-tab[3,3])/tab[3,3])*100,((tab[4,3]-catch_int_year)/catch_int_year)*100,((tab[4,3]-Advice_int)/Advice_int)*100)                  

SSBintplus1<-tab[3,3]

prob_Btrigger_forecast<-100*sum(Btrigger_forecast[[length(Btrigger_forecast)]]$ssb < Blim)/nr_sims
check<-Btrigger_forecast[[length(Btrigger_forecast)]]$ssb < Blim
check2<-Btrigger_forecast[[length(Btrigger_forecast)]]$ssb
table(check)
min(check2)

# SSBintplus1 catch option
set.seed(123)
SSBintplus1_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                                  catchval.exact=c(NA,NA,NA,NA),
                                                  fscale = c(NA,NA,NA,1),
                                                  fval = c(Flast,Fsq,NA,NA),
                                                  nextssb = c(NA,NA,SSBintplus1,NA),
                                                  nosim = nr_sims, year.base = base_year,
                                                  ave.years = avg_years,
                                                  rec.years = recr_years,
                                                  deterministic = FALSE,
                                                  processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                                  lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "SSBintplus1", savesim = TRUE)

tab<-attributes(SSBintplus1_forecast)$shorttab
catchScenariosSplit[9,2:11]<-c(tab[4,3],tab[7,3],tab[8,3],tab[1,3],tab[5,3],tab[6,3],tab[3,4],((tab[3,4]-tab[3,3])/tab[3,3])*100,((tab[4,3]-catch_int_year)/catch_int_year)*100,((tab[4,3]-Advice_int)/Advice_int)*100)                  

prob_SSBintplus1_forecast<-100*sum(SSBintplus1_forecast[[length(SSBintplus1_forecast)]]$ssb < Blim)/nr_sims
check<-SSBintplus1_forecast[[length(SSBintplus1_forecast)]]$ssb < Blim
check2<-SSBintplus1_forecast[[length(SSBintplus1_forecast)]]$ssb
table(check)
min(check2)


# F=Fint catch option
Fint           <- msytab[1,2]
set.seed(123)
Fint_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                           catchval.exact=c(NA,NA,NA,NA),
                                           fscale = c(NA,NA,NA,1),
                                           fval = c(Flast,Fsq,Fint,NA),
                                           nosim = nr_sims, year.base = base_year,
                                           ave.years = avg_years,
                                           rec.years = recr_years,
                                           deterministic = FALSE,
                                           processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                           lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "Fint", savesim = TRUE)

tab<-attributes(Fint_forecast)$shorttab
catchScenariosSplit[10,2:11]<-c(tab[4,3],tab[7,3],tab[8,3],tab[1,3],tab[5,3],tab[6,3],tab[3,4],((tab[3,4]-tab[3,3])/tab[3,3])*100,((tab[4,3]-catch_int_year)/catch_int_year)*100,((tab[4,3]-Advice_int)/Advice_int)*100)                  

prob_Fint_forecast<-100*sum(Fint_forecast[[length(Fint_forecast)]]$ssb < Blim)/nr_sims
check<-Fint_forecast[[length(Fint_forecast)]]$ssb < Blim
check2<-Fint_forecast[[length(Fint_forecast)]]$ssb
table(check)
min(check2)

catchScenariosSplit<-catchScenariosSplit[c(1:8,10,9),]

catchScenariosSplit$prob<-""
catchScenariosSplit[1,12]<-prob_Fmsy
catchScenariosSplit[2,12]<-prob_Fmsy_lower
catchScenariosSplit[3,12]<-prob_Fmsy_upper
catchScenariosSplit[4,12]<-prob_Fnul_forecast
catchScenariosSplit[5,12]<-prob_Fpa_forecast
catchScenariosSplit[6,12]<-prob_Flim_forecast
catchScenariosSplit[7,12]<-prob_Blim_forecast
catchScenariosSplit[8,12]<-prob_Btrigger_forecast
catchScenariosSplit[9,12]<-prob_Fint_forecast
catchScenariosSplit[10,12]<-prob_SSBintplus1_forecast

save(catchScenariosSplit,file="output/sol7a_catchScenarios.Rdata")

### Extra for INPUT
set.seed(123)
Fmsy_forecast <- stockassessment::forecast(SAM_fit_sol_7a,
                                           catchval.exact=c(NA,NA,NA,NA),
                                           fscale = c(NA,NA,NA,1),
                                           fval = c(Flast,Fsq,Fmsy,NA),
                                           nosim = nr_sims, year.base = base_year,
                                           ave.years = avg_years,
                                           rec.years = recr_years,
                                           deterministic = FALSE,
                                           processNoiseF = TRUE, customWeights = NULL, customSel = NULL,
                                           lagR = FALSE, splitLD = TRUE, addTSB = FALSE, label = "Fmsy", savesim = TRUE)
#SSB by Age intplus2
ssb_by_age_sims <-sweep(exp(Fmsy_forecast[[4]]$sim[,1:10]), MARGIN = 2, FUN = "*", STATS = (apply(SAM_fit_sol_7a$data$stockMeanWeight[as.character(avg_years),],2,mean) * apply(SAM_fit_sol_7a$data$propMat[as.character(avg_years),],2,mean)))
ssb_by_age<-ssb_by_age_sims[which.min(abs(rowSums(ssb_by_age_sims) - median(rowSums(ssb_by_age_sims)))),]
sum(ssb_by_age)

#N by age Intplus1
N_by_age <-apply(exp(Fmsy_forecast[[3]]$sim[,1:10]),2,median)

#C by age Intplus1
C_by_age_sims <- sweep(Fmsy_forecast[[3]]$catchatage[1:10,], MARGIN = 1, FUN = "*", STATS = apply((SAM_fit_sol_7a$data$catchMeanWeight[,,1])[as.character(avg_years),],2,mean))
C_by_age <-  C_by_age_sims[,which.min(abs(colSums(C_by_age_sims) - median(colSums(C_by_age_sims))))] 
sum(C_by_age)

#SAVE
save(C_by_age,file="output/sol7a_C_by_age.Rdata")
save(ssb_by_age,file="output/sol7a_ssb_by_age.Rdata")

save(Fmsy_forecast,file ="output/sol7a_Fmsy_forecast.RData")

###Intermediate year assumptions
Variable<-c(paste0("F_ages_3-7(",(base_year+1),")"),paste0("SBB ",(base_year+2)),paste0("Rage1(",(base_year+1)," and ", (base_year+2),")"),paste0("Total catch ",(base_year+1)),paste0("Projected landings ",(base_year+1)),paste0("Projected discards ",(base_year+1)))
Value<-c(msytab[1,2],msytab[3,3],msytab[2,2],msytab[4,2],msytab[7,2],msytab[8,2])
Notes<-c(paste0("Faverage"),paste0("Tonnes; Fishing at F=",msytab[1,2]),paste0("Median resampled recruitment (2015","–", (base_year-1),") as estimated by a stochastic projection; in thousands"),"Tonnes; TAC int", paste0("Tonnes; Assuming average ", (base_year-2),"-",base_year, " landing fractions at age are used to split the projected catches"),paste0("Tonnes; Assuming average ",(base_year-2),"-",base_year, " landing fractions at age are used to split the projected catches")) 
AssumptionsTableSplit<-data.frame(Variable,Value,Notes)
AssumptionsTableSplit

###save intermediate year assumptions
save(AssumptionsTableSplit,file="output/sol7a_assumptions.Rdata")


















