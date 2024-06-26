## Prepare plots and tables for report

## Before:
## After:

mkdir("report")

#load SAM output STF output and stock data
load('data/sol7a_stock.RData')
stock_data<-sam_data
load('model/SAM_fit_sol_7a.RData')
base_year              <- max(SAM_fit_sol_7a$data$years)
first_year             <- min(SAM_fit_sol_7a$data$years) 
nyears                 <- (base_year - first_year)+1


############
###Tables###
############
#catage (trim age) (INPUT)
catage <- read.taf("data/catage.csv")
catage <- as.data.frame(catage)
catage <-round(catage)
write.taf(catage, dir = "report")

#wcatch (trim age, round) (INPUT)
wcatch <- read.taf("data/wcatch.csv")
wcatch <- as.data.frame(wcatch)
wcatch <-round(wcatch,3)
write.taf(wcatch, dir = "report")

#wstock (trim age, round) (INPUT)
wstock <- read.taf("data/wstock.csv")
wstock <- as.data.frame(wstock)
wstock <-round(wstock,3)
write.taf(wstock, dir = "report")

#fatage (round)
fatage <- read.taf("output/fatage.csv")
fatage <- as.data.frame(fatage)
fatage  <-round(fatage ,3)
write.taf(fatage, dir = "report")

#natage (sum, round)
natage <- read.taf("output/natage.csv")
natage <- as.data.frame(natage)
natage <- round(natage)
write.taf(natage, dir = "report")


#ssbatage (sum, round)
ssbatage <- read.taf("output/ssbatage.csv")
ssbatage <- as.data.frame(ssbatage)
ssbatage <- round(ssbatage)
write.taf(ssbatage, dir = "report")

colSums(ssbatage)

#model details - fitting diagnostics
moddet <- read.taf("output/moddet.csv")
write.taf(moddet, dir="report")

#survey catchability
Diag_table_1 <- read.taf("output/sol7a_Diag_table_1.csv")
write.taf(Diag_table_1, dir = "report")
Diag_table_2 <- read.taf("output/sol7a_Diag_table_2.csv")
write.taf(Diag_table_2, dir = "report")




#############
###Figures###
#############

load('output/sol7a_catchn.Rdata')
load('output/sol7a_index.Rdata')
load('data/landings.Rdata')
load('data/totDis.Rdata')


##Catch, discards and landings
#Catch
taf.png("Catch.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
catchplot(SAM_fit_sol_7a)
dev.off()

#Catch/landings
catch<-as.data.frame(landings)
names(catch)[2]<-"data"
ggplot(catch,aes(x=Year, y=data)) + geom_line() +
  ylim(0,3000)+
  ylab("Landings (tonnes)") + xlab("Year") +
  theme(legend.title=element_blank(), legend.position="bottom")
ggsave('landings.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)



#Catch numbers-at-age
stock_data$cn
cn <- as.data.frame(stock_data$cn)
cn$Year <- first_year:base_year
head(cn)
cn <- cn %>% 
  gather("Age", "catchN", 1:10)
cn$Age <- as.factor(as.numeric(cn$Age))

taf.png("CNaA.png", width = 15, height = 15, units = "cm", res= 300)
ggplot(cn, aes(Year, Age, size = catchN)) +
  theme_bw() + geom_point(colour = "red4", fill = "red3", alpha = 0.5) +
  scale_size(range = c(1, 15)) +
  ylab("Age") + labs(size = "CATCH numbers-at-age")
dev.off()

#Catch proportions at age
ggplot(df_catchn, aes(x=year, y=as.factor(age))) +
  geom_point(aes(size=abs(cn)), shape=21, na.rm=TRUE) +
  scale_size(range = c(0.1, 17)) +
  ylab(paste0("catch proportions at age")) + xlab("Year") +
  theme(legend.title=element_blank(), legend.position="none") +
  guides(size = guide_legend(nrow = 1))
ggsave('Catchn.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)

#Standardised catch proportions at age
ggplot(df_catchn, aes(x=year, y=as.factor(age), size = abs(propstand), color = signs)) +
  geom_point(aes(size=abs(propstand)), shape=21, stroke=1.5,na.rm=TRUE) +
  scale_size(range = c(0.1, 17)) +
  ylab(paste0("standardised catch proportions at age")) + xlab("Year") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal") +
  guides(size = guide_legend(order=1),
         fill = guide_legend(order=2))  +
  labs(fill = NULL)
ggsave('Catchn_stand.png', path = "report", width = 15, height = 15, units = "cm", dpi=300, scale=2)


#Barplot landings and discards
landings$fraction <- "LAN"
totDis$fraction <- "DIS"

catches <- rbind(landings, totDis)

taf.png("catches.png", width = 15, height = 10, units = "cm", res= 300)
ggplot(catches, aes(Year, weight))+geom_bar(stat = "identity", position = "stack", colour = "black", aes(fill = fraction)) + theme_bw()+
  ylab("tonnes")+ labs(fill = "Catch fraction") 
dev.off()


#Barplot landings and discard numbers-at-age
stock_data$ln
ln <- as.data.frame(stock_data$ln)
ln$Year <- first_year:base_year
head(ln)
ln <- ln %>% 
  gather("Age", "landingN", 1:10)
ln$Age <- as.factor(as.numeric(ln$Age))

stock_data$dn
dn <- as.data.frame(stock_data$dn)
dn$Year <- first_year:base_year
head(dn)
dn <- dn %>% 
  gather("Age", "discardN", 1:10)
dn$Age <- as.factor(as.numeric(dn$Age))

head(ln)
ln <- select(ln, Year, Age, Numbers = landingN)
ln$fraction <- "LAN"

head(dn)
dn <- select(dn, Year, Age, Numbers = discardN)
dn$fraction <- "DIS"

ca <- rbind(dn, ln)

catchn_1<-ca %>% 
  filter(Year %in% c(1970:2002))
  
catchn_2<-ca %>% 
  filter(Year %in% c(2003:base_year))

  
my_colours <- viridis(5)

taf.png("NaAbarplot_1.png", width = 15, height = 15, units = "cm", res= 300)
ggplot(catchn_1, aes(Age, Numbers))+geom_bar(stat = "identity", position = "stack", colour = "black", aes(fill = fraction))+facet_wrap(~Year) + theme_bw()+
  scale_fill_manual(values = c(my_colours[5], my_colours[2]))+ylab("Numbers-at-age")+ labs(fill = "Catch fraction") 
dev.off()

taf.png("NaAbarplot_2.png", width = 15, height = 15, units = "cm", res= 300)
ggplot(catchn_2, aes(Age, Numbers))+geom_bar(stat = "identity", position = "stack", colour = "black", aes(fill = fraction))+facet_wrap(~Year) + theme_bw()+
  scale_fill_manual(values = c(my_colours[5], my_colours[2]))+ylab("Numbers-at-age")+ labs(fill = "Catch fraction") 
dev.off()


#Landings numbers-at-age
stock_data$ln
ln <- as.data.frame(stock_data$ln)
ln$Year <- first_year:base_year
head(ln)
ln <- ln %>% 
  gather("Age", "landingN", 1:10)
ln$Age <- as.factor(as.numeric(ln$Age))

taf.png("LNaA.png", width = 15, height = 15, units = "cm", res= 300)
ggplot(ln, aes(Year, Age, size = landingN)) +
  theme_bw() + geom_point(colour = "blue4", fill = "blue3", alpha = 0.5) +
  scale_size(range = c(1, 15)) +
  ylab("Age") + labs(size="LAN numbers-at-age")
dev.off()

#Discards numbers-at-age
stock_data$dn
dn <- as.data.frame(stock_data$dn)
dn$Year <- first_year:base_year
head(dn)
dn <- dn %>% 
  gather("Age", "discardN", 1:10)
dn$Age <- as.factor(as.numeric(dn$Age))

taf.png("DNaA.png",width = 15, height = 15, units = "cm", res= 300)
ggplot(dn, aes(Year, Age, size = discardN)) +
  theme_bw() + geom_point(colour = "yellow4", fill = "yellow3", alpha = 0.5) +
  scale_size(range = c(1, 15)) +
  ylab("Age") + labs(size="DIS numbers-at-age")
dev.off()


## Proportion discarded
pdis <- cn %>% 
  left_join(dn, by=c("Year", "Age")) %>% 
  mutate(propdis = discardN/catchN)
pdis$propdis <- ifelse(is.na(pdis$propdis),0,pdis$propdis)

head(pdis)
table(pdis$Age)
pdis$Age <- as.factor(as.numeric(pdis$Age))

ggplot(pdis, aes(Year, propdis))+geom_line(aes(colour = Age),size = 1)+theme_bw()+xlab("Year")+ylab("Proportion discarded (dn/cn)")+
geom_vline(xintercept = 2003, linetype = "dashed")+ geom_vline(xintercept = 2007, linetype = "dashed")+geom_text(label=pdis$Age, aes(colour=Age))+
theme(legend.position = "none")+
theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))
ggsave("Propdis.png", path = "report", width = 14, height = 14, units = "in", dpi=300)

pdis_2 <-pdis %>% 
  filter(Year %in% c(2003:base_year))

ggplot(pdis_2, aes(Age, propdis))+geom_boxplot()+theme_bw()+ylab("Proportion discarded (dn/cn)")+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))
ggsave("PropdisatAge.png", path = "report", width = 14, height = 14, units = "in", dpi=300)


#Catch weight-at-age
stock_data$cw
cw <- as.data.frame(stock_data$cw)
head(cw)
cw$Year <- first_year:base_year

cwplot <- cw %>% 
  gather("Age", "mWaA", 1:10)
head(cwplot)

cwplot$Age <- as.factor(as.numeric(cwplot$Age))
head(cwplot)

taf.png("cwplot.png", width = 15, height = 15, units = "cm", res= 300)
ggplot(cwplot, aes(Year, mWaA))+geom_line(aes(colour = Age),size = 1)+theme_bw()+ggtitle("Catch weight-at-age")+ylab("weight-at-age (kg)")
dev.off()

catchw<-melt(stock_data$cw)
names(catchw)<-c("year","age","cw")
cols <- viridis(n =length(unique(catchw$age)))

ggplot(catchw, aes(year, cw)) + geom_line() + facet_wrap(~age, scales='free') +
  theme_minimal() + labs(x="",y="Catch Mean Weight") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave('Catchw_sep.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)

#Discards weight-at-age
stock_data$dw
dw <- as.data.frame(stock_data$dw)
head(dw)
dw$Year <- first_year:base_year

dwplot <- dw %>% 
  gather("Age", "mWaA", 1:10)
head(dwplot)

dwplot$Age <- as.factor(as.numeric(dwplot$Age))
head(dwplot)

dwplot2 <- dwplot %>% 
  filter(Age %in% c(1:5))

taf.png("dwplotage1_5.png", width = 15, height = 15, units = "cm", res= 300)
ggplot(dwplot2, aes(Year, mWaA))+geom_line(aes(colour = Age),size = 1)+theme_bw()+ylab("weight-at-age (kg)")+ggtitle("Discard weight-at-age")
dev.off()

#Landings weight-at-age
stock_data$lw
lw <- as.data.frame(stock_data$lw)
head(lw)
lw$Year <- first_year:base_year

lwplot <- lw %>% 
  gather("Age", "mWaA", 1:10)
head(lwplot)

lwplot$Age <- as.factor(as.numeric(lwplot$Age))
head(lwplot)

taf.png("lwplot.png", width = 15, height = 15, units = "cm", res= 300)
ggplot(lwplot, aes(Year, mWaA))+geom_line(aes(colour = Age),size = 1)+theme_bw()+ggtitle("Landings weight-at-age")+ylab("weight-at-age (kg)")
dev.off()

#Mean weight-at-age in the stock
stock_data$sw
sw <- as.data.frame(stock_data$sw)
head(sw)
sw$Year <- first_year:base_year

swplot <- sw %>% 
  gather("Age", "mWaA", 1:10)
head(swplot)

swplot$Age <- as.factor(as.numeric(swplot$Age))
head(swplot)

taf.png("swplot.png", width = 15, height = 15, units = "cm", res= 300)
ggplot(swplot, aes(Year, mWaA))+geom_line(aes(colour = Age),size = 1)+theme_bw()+ggtitle("Stock weight-at-age")+ylab("weight-at-age (kg)")
dev.off()

stockw<-melt(stock_data$sw)
names(stockw)<-c("year","age","sw")
cols <- viridis(n =length(unique(stockw$age)))

ggplot(stockw, aes(year, sw)) + geom_line() + facet_wrap(~age, scales='free') +
  theme_minimal() + labs(x="",y="Stock Mean Weight") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave('Stockw_sep.png', path = "report", width = 20, height = 10, units = "cm", dpi=300, scale=2)


#Maturity
stock_data$mo
mo <- as.data.frame(stock_data$mo)
head(mo)
mo$Year <- first_year:base_year

moplot <- mo %>% 
  gather("Age", "Maturity", 1:10)
head(moplot)

moplot$Age <- as.factor(as.numeric(moplot$Age))
head(moplot)

taf.png("moplot.png", width = 15, height = 15, units = "cm", res= 300)
ggplot(moplot, aes(Year,Maturity))+geom_line(aes(colour = Age),size = 1)+theme_bw()+ylab("Maturity")
dev.off()


##Tuning indices
#Biomass indices
#SCALE = (index - mean timeseries)/standev(timeseries)
taf.png("Biomass_indices_scaled.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
plot(0,0, xlim = c(2004,2023),  
     xlab = "Year", ylim = c(-2.5,3),
     main = "", ylab = "scaled biomass index (kg/h)", yaxt = "n", xaxt = "n")
axis(1, at = seq(2004,2023,5))
axis(2, las = 2)
lines(as.numeric(attributes(stock_data$tun[[2]])$dimnames[[1]]),scale(stock_data$tun[[2]][,1]), type = "b", col = "black")
dev.off()

cv<-read.table(file = ("boot/initial/data/cv.txt"))  # !!! read.csv neemt de eerste CV als rowname
names(cv)[1]<-"CV"
cv$CV <- as.numeric(cv$CV)
cv$weight<-1/cv$CV
cv$year<-c(2004:2023)

taf.png("Biomass_indices_weights.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
plot(0,0, xlim = c(2004,2023),  
     xlab = "Year", ylim = c(0,10),
     main = "", ylab = "1/CV", yaxt = "n", xaxt = "n")
axis(1, at = seq(2004,2023,5))
axis(2, las = 2)
lines(data=cv, weight~year, type = "b", col = "black")
dev.off()

#Survey indices
#By year
ggplot(tun_bts, aes(x=year, y=log(index) , group=age, colour=factor(age))) + 
  geom_line() + geom_point() + ylab("log indices") + xlab("Year") + 
  scale_colour_discrete("Age") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
ggsave('Survey_indices_year.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)

ggplot(tun_bts, aes(x=year, y=log(index_stan) , group=age, colour=factor(age))) + 
  geom_line() + geom_point() + ylab("log standardised indices") + xlab("Year") + 
  scale_colour_discrete("Age") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
ggsave('Survey_standardised indices_year.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)

#By yearclass
ggplot(tun_bts, aes(x=yearclass, y=log(index) , group=age, colour=factor(age))) + 
  geom_line() + geom_point() + ylab("log indices") + xlab("Yearclass") + 
  scale_colour_discrete("Age") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
ggsave('Survey_indices_yearclass.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)

ggplot(tun_bts, aes(x=yearclass, y=log(index_stan) , group=age, colour=factor(age))) + 
  geom_line() + geom_point() + ylab("log standardised indices") + xlab("Yearclass") + 
  scale_colour_discrete("Age") + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
ggsave('Survey_standardised indices_yearclass.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)

ggplot(tun_bts, aes(yearclass, log(index_stan))) + geom_line() + facet_wrap(~age, scales='free') +
  theme_minimal() + labs(x="Yearclass",y="log standardised indices") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
ggsave('Survey_standardised indices_yearclass_sep.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)

#Internal consistency plots
BTStun <- as.data.frame(stock_data$tun$`UK (E&W)-BTS-Q3`)
nrows<-nrow(BTStun)

FLIndex_BTS <- FLIndex(index = FLQuant(c(t(BTStun)), dim=c(7,nrows),
                                          quant="age", units="numbers",dimnames=list(age=1:7, year=(first_year+18):base_year)))
taf.png("Survey_internal consistency", width = 15, height = 15, units = "cm", res= 300)
plot(FLIndex_BTS, type="internal", log.scales = T)
dev.off()

##SAM model
#Input
taf.png("dataplot.png", width = 15, height = 15, units = "cm", res= 300)
dataplot(SAM_fit_sol_7a)
dev.off()

#Summary
taf.png("Summary.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
plot(SAM_fit_sol_7a)
dev.off()

#SSB plot
taf.png("SSB.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
ssbplot(SAM_fit_sol_7a)
dev.off()

#Fbar plot
taf.png("Fbar.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
fbarplot(SAM_fit_sol_7a, partial=F)
dev.off()

#Rec plot
taf.png("Rec.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
recplot(SAM_fit_sol_7a)
dev.off()

###################################################################

taf.png("StockStatus.png", width = 25, height = 15, units = "cm", res= 300)
par(mfrow = c(2,2),
    mar = c(4,4,2,2))

catchplot(SAM_fit_sol_7a)

ssbplot(SAM_fit_sol_7a)
abline(h=3129,col="dodgerblue3",lwd=1.5)
abline(h=2235,col="red",lwd=1.5)

fbarplot(SAM_fit_sol_7a, partial=F, ylim=c(0,0.6))
abline(h=0.164,col="dodgerblue3",lwd=1.5)
abline(h=0.570,col="red",lwd=1.5)

recplot(SAM_fit_sol_7a)
dev.off()



#Stock recruitment plot
taf.png("SR.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
srplot(SAM_fit_sol_7a)
dev.off()

#Fishing mortality at age
FaA <- as.data.frame(faytable(SAM_fit_sol_7a))
FaA$Year <- first_year:base_year
FaA1 <- FaA %>% 
  gather("Age", "FatAge", 1:10)
head(FaA1)
FaA1$Age <- as.factor(as.numeric(FaA1$Age))

taf.png("FatAge.png",width = 15, height = 15, units = "cm", res= 300)
ggplot(FaA1, aes(Year, FatAge))+geom_line(aes(color = Age))+theme_bw()+geom_text(label=FaA1$Age, aes(colour = Age))+theme(legend.position = "none")
dev.off()


#Correlation plot
taf.png("Correlation.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
corplot(SAM_fit_sol_7a)
dev.off()

#Q plot
taf.png("Q.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
qtableplot(qtable(SAM_fit_sol_7a))
dev.off()

#Residuals
#OSA
load('output/sol7a_OSA_residuals.Rdata')
taf.png("OSA_res.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
plot(OSA_residuals)
dev.off()

taf.png("OSA_res_sum.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
plot(OSA_residuals, type = "summary")
dev.off()

#PRC
load('output/sol7a_PRC_residuals.Rdata')
taf.png("PRC_res.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
plot(PRC_residuals)
dev.off()

taf.png("PRC_res_sum.png", width = 15, height = 15, units = "cm", pointsize = 10, res= 300)
plot(PRC_residuals, type = "summary")
dev.off()

#Retro analysis
load('output/sol7a_retro.Rdata')
taf.png("Retro.png", width = 20, height = 20, units = "cm", pointsize = 10, res= 300)
plot(retrofits)
dev.off()

#Leave one out analysis
load('output/sol7a_leaveout_fit.Rdata')
taf.png("Leaveout.png", width = 20, height = 20, units = "cm", pointsize = 10, res= 300)
plot(leaveout_fit)
dev.off()

## Simulation analysis
load('output/sol7a_sim_fit.Rdata')
taf.png("Simulation.png", width = 20, height = 20, units = "cm", pointsize = 10, res= 300)
plot(sim_fit)
dev.off()

## jitter analysis
load('output/sol7a_jit_fit.Rdata')
taf.png("Jitter.png", width = 20, height = 20, units = "cm", pointsize = 10, res= 300)
plot(jit_fit)
dev.off()


# plot observational variance - observation error
sdplot<-function(fit){
  opar <- par()
  cf <- fit$conf$keyVarObs
  fn <- attr(fit$data, "fleetNames")
  ages <- fit$conf$minAge:fit$conf$maxAge
  pt <- partable(fit)
  sd <- unname(exp(pt[grep("logSdLogObs",rownames(pt)),1]))
  v<-cf
  v[] <- c(NA,sd)[cf+2]
  res<-data.frame(fleet=fn[as.vector(row(v))],name=paste0(fn[as.vector(row(v))]," age ",ages[as.vector(col(v))]), sd=as.vector(v))
  res<-res[complete.cases(res),]
  o<-order(res$sd)
  res<-res[o,]
  par(mar=c(13,6,2,1))
  barplot(res$sd, names.arg=res$name,las=2, col=colors()[as.integer(as.factor(res$fleet))*10], ylab="SD"); box()
  suppressWarnings(par(opar)) 
}
taf.png("sdplot_by_datastream.png", width = 20, height = 15, units = "cm", pointsize = 10, res= 300)
sdplot(SAM_fit_sol_7a)
dev.off()

getwd()



##Summary plot comparing with previous year
#Results from previous year for comparison
SAM_fit_sol_7a_PrevWG<-load("boot/initial/data/SAM_fit_sol_7a.RData") 
SAM_fit_sol_7a_PrevWG<-SAM_fit_sol_7a
load('model/SAM_fit_sol_7a.RData')

taf.png("Comparison.png", width = 25, height = 15, units = "cm", res= 300)
par(mfrow = c(2,2),
    mar = c(4,4,2,2))
y_max <- max(c(catchtable(SAM_fit_sol_7a_PrevWG)[,"Estimate"],catchtable(SAM_fit_sol_7a)[,"Estimate"]))
catchplot(SAM_fit_sol_7a_PrevWG, col = "black",cicol = alpha("black",0.3))
catchplot(SAM_fit_sol_7a, add = T, col = "red",cicol = alpha("red",0.3))
grid(col = "grey60", lty = 2)

y_max <- max(c(ssbtable(SAM_fit_sol_7a_PrevWG)[,"Estimate"],ssbtable(SAM_fit_sol_7a)[,"Estimate"]))
ssbplot(SAM_fit_sol_7a_PrevWG, col = "black",cicol = alpha("black",0.3))
ssbplot(SAM_fit_sol_7a, add = T, col = "red",cicol = alpha("red",0.3))
grid(col = "grey60", lty = 2)

y_max <- max(c(fbartable(SAM_fit_sol_7a_PrevWG)[,"Estimate"],fbartable(SAM_fit_sol_7a)[,"Estimate"]))
fbarplot(SAM_fit_sol_7a_PrevWG, col = "black", partial=F,cicol = alpha("black",0.3))
fbarplot(SAM_fit_sol_7a, add = T, col = "red", partial=F,cicol = alpha("red",0.3))
grid(col = "grey60", lty = 2)

y_max <- max(c(rectable(SAM_fit_sol_7a_PrevWG)[,"Estimate"],rectable(SAM_fit_sol_7a)[,"Estimate"]))
recplot(SAM_fit_sol_7a_PrevWG, col = "black",cicol = alpha("black",0.3))
recplot(SAM_fit_sol_7a, add = T, col = "red",cicol = alpha("red",0.3))
grid(col = "grey60", lty = 2)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend('top',c(paste0("2023WG_revisionWKBFLATFISH1"),paste0((base_year+1),"WG")),lty=c(1,1),lwd=2,col=c("black", "red"),bty='n',xpd = TRUE, horiz = TRUE)
dev.off()


#summary table
summary <- read.taf("output/summary_table.csv")
names(summary)<-c("year","R(age1)_low","R(age1)_value","R(age1)_high","TSB_low","TSB_value","TSB_high","SSB_low","SSB_value","SSB_high","Catch_low","Catch_value","Catch_high","Fbar(3-7)_low","Fbar(3-7)_value","Fbar(3-7)_high") 
write.taf(summary, dir = "report")

#STF options
summary <- read.taf("output/summary_table.csv")
sum<-summary[,c(1,3,9,12,15)]

sum$SSB_Fsq<-sum$SSB
sum$SSB_Flast<-sum$SSB
sum$Catch_Fsq<-sum$Catch
sum$Catch_Flast<-sum$Catch
sum$Fbar_Fsq<-sum$Fbar
sum$Fbar_Flast<-sum$Fbar
sum<-sum[,c(1,2,3,6,7,4,8,9,5,10,11)]

load('output/sol7a_tab_options.Rdata')

extra_1<-c((base_year+1),tab_options[2,2],tab_options[3,2],tab_options[3,2],tab_options[3,2],tab_options[4,2],tab_options[13,2],tab_options[22,2],tab_options[1,2],tab_options[10,2],tab_options[19,2])
extra_2<-c((base_year+2),tab_options[2,2],tab_options[3,3],tab_options[12,3],tab_options[21,3],tab_options[4,3],tab_options[13,3],tab_options[22,3],tab_options[1,3],tab_options[10,3],tab_options[19,3])
extra_3<-c((base_year+3),tab_options[2,2],tab_options[3,4],tab_options[12,4],tab_options[21,4],0,0,0,0,0,0)
sum<-rbind(sum,extra_1,extra_2,extra_3)
sum_short<-sum %>% 
  filter(year %in% c((base_year-5):(base_year+3)))

sum_R<-sum %>% 
  filter(year %in% c((base_year+1):(base_year+3)))

ggplot() + 
  geom_line(data=sum, aes(x=year,y = R), color = "black",lwd=1.2) +
  geom_line(data=sum_R, aes(x=year,y = R), color = "red",lwd=1.2)+
  ylab("") + xlab("Year") +
  ylim(2500,(max(sum$R)+1000)) +
  theme(legend.title=element_blank(), legend.position="bottom")+
  ggtitle("Recruitment age1 (thousands)")+
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.border=element_rect(colour = "black", fill=NA),legend.key = element_rect(fill = "white", colour = "white"),)
ggsave('R_IntermediateYear.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)

ggplot(sum_short,aes(x=year)) + 
  geom_line(aes(y = SSB_Fsq, color = "Faverage"), lwd=1.2) +
  geom_line(aes(y = SSB_Flast, color = "Flast"),  lwd=1.2) +
  geom_line(aes(y = SSB, color = "Catch"), lwd=1.2) +
  geom_hline(yintercept = 3129,size=1) +
  #geom_hline(yintercept = 3221,linetype="dotted", size=1) +
  geom_hline(yintercept = 2235, linetype="dashed",size=1) +
  geom_text(aes(x=2024,y = 3129), label = "MSY Btrigger", size = 3, vjust = 1.5) +
  #geom_text(aes(x=2024,y = 3221), label = "Bpa", size = 3, vjust = -0.5) +
  geom_text(aes(x=2024,y = 2235), label = "Blim", size = 3, vjust = -0.5) +
  ylab("") + xlab("Year") +
  xlim((base_year-5),(base_year+3)) +
  ylim(1000,(max(sum_short$SSB)+500)) +
  ggtitle("SSB (tonnes)") + 
  theme(plot.title = element_text(lineheight=2, face="bold")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.border=element_rect(colour = "black", fill=NA),legend.key = element_rect(fill = "white", colour = "white"),) +
  scale_color_manual(name="Options", values=c("Catch"="#619CFF", "Faverage"="#F8766D","Flast"="#00BA38")) 
ggsave('SSB_IntermediateYear.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)


ggplot(sum_short,aes(x=year)) + 
  geom_line(aes(y = Fbar_Fsq, color = "Faverage"), lwd=1.2)+
  geom_line(aes(y = Fbar_Flast, color = "Flast"),lwd=1.2)+
  geom_line(aes(y = Fbar, color = "Catch"), lwd=1.2) +
  geom_hline(yintercept = 0.164, size=1) +
  geom_hline(yintercept = 0.183, linetype="dotted", size=1) +
  geom_hline(yintercept = 0.570, linetype="dashed",size=1) +
  geom_text(aes(x=2024,y = 0.164), label = "FMSY", size = 3, vjust = -0.5) +
  geom_text(aes(x=2024,y = 0.183), label = "Fpa", size = 3, vjust = -0.5) +
  geom_text(aes(x=2024,y = 0.570), label = "Flim", size = 3, vjust = -0.5) +
  ylab("") + xlab("Year") +
  xlim((base_year-5),(base_year+2)) +
  ylim(0.0,(max(sum_short$Fbar)+0.5)) +
  ggtitle("Fbar (ages 3-7)") + 
  theme(plot.title = element_text(lineheight=2, face="bold")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.border=element_rect(colour = "black", fill=NA),legend.key = element_rect(fill = "white", colour = "white"),) +
  scale_color_manual(name="Options", values=c("Catch"="#619CFF", "Faverage"="#F8766D","Flast"="#00BA38")) 
ggsave('Fbar_IntermediateYear.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)

ggplot(sum_short,aes(x=year)) + 
  geom_line(aes(y = Catch_Fsq, color = "Faverage"), lwd=1.2) +
  geom_line(aes(y = Catch_Flast, color = "Flast"),  lwd=1.2) +
  geom_line(aes(y = Catch, color = "Catch"), lwd=1.2) +
  ylab("") + xlab("Year") +
  xlim((base_year-5),(base_year+2)) +
  ylim(0,(max(sum_short$Catch)+200)) +
  ggtitle("Catch (tonnes)") + 
  theme(plot.title = element_text(lineheight=2, face="bold")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.border=element_rect(colour = "black", fill=NA),legend.key = element_rect(fill = "white", colour = "white"),) +
  scale_color_manual(name="Options", values=c("Catch"="#619CFF", "Faverage"="#F8766D","Flast"="#00BA38")) 
ggsave('Catch_IntermediateYear.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)

#State of the stock/Ref points
ggplot(sum,aes(x=year)) + 
  geom_line(aes(y=SSB, color = "red"), lwd=1.2, show.legend = FALSE) +
  ylab("SSB (tonnes)") + xlab("Year") +
  geom_hline(yintercept = 3129,size=1) +
  #geom_hline(yintercept = 3221,linetype="dotted", size=1) +
  geom_hline(yintercept = 2235, linetype="dashed",size=1) +
  geom_text(aes(x=2022,y = 3129), label = "MSY Btrigger", size = 3, vjust = 1.5) +
  #geom_text(aes(x=2022,y = 3221), label = "Bpa", size = 3, vjust = -0.5) +
  geom_text(aes(x=2022,y = 2235), label = "Blim", size = 3, vjust = -0.5) +
  theme(plot.title = element_text(lineheight=2, face="bold")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.border=element_rect(colour = "black", fill=NA),legend.key = element_rect(fill = "white", colour = "white"),) 
ggsave('SSB_RefPoints.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)


ggplot(sum,aes(x=year)) + 
  geom_line(aes(y = Fbar, color = "red"), lwd=1.2, show.legend = FALSE) +
  ylab("Fbar(ages 3-7)") + xlab("Year") +
  geom_hline(yintercept = 0.164, size=1) +
  geom_hline(yintercept = 0.196, linetype="dotted", size=1) +
  geom_hline(yintercept = 0.57, linetype="dashed",size=1) +
  geom_text(aes(x=2022,y = 0.164), label = "FMSY", size = 3, vjust = -0.5) +
  geom_text(aes(x=2022,y = 0.196), label = "Fpa", size = 3, vjust = -0.5) +
  geom_text(aes(x=2022,y = 0.57), label = "Flim", size = 3, vjust = -0.5) +
  ylab("Fbar(ages 3-7)") + xlab("Year") +
  theme(plot.title = element_text(lineheight=2, face="bold")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.border=element_rect(colour = "black", fill=NA),legend.key = element_rect(fill = "white", colour = "white"),) 
ggsave('Fbar_RefPoints.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)

ggplot() + 
  geom_line(data=sum, aes(x=year,y = R), color = "red",lwd=1.2)+
  ylab("R age1 (thousands)") + xlab("Year") +
  ylim(2500,(max(sum$R)+1000)) +
  theme(legend.title=element_blank(), legend.position="bottom")+
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.border=element_rect(colour = "black", fill=NA),legend.key = element_rect(fill = "white", colour = "white"),)
ggsave('R.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)

ggplot(sum,aes(x=year)) + 
  geom_line(aes(y = Catch, color = "red"), lwd=1.2) +
  ylab("Catch (tonnes)") + xlab("Year") +
  ylim(0,(max(sum$Catch)+200)) +
  theme(plot.title = element_text(lineheight=2, face="bold")) +
  theme(panel.background = element_rect(fill = "white", colour = "white", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.5, linetype = 'solid', colour = "grey"),
        panel.border=element_rect(colour = "black", fill=NA),legend.key = element_rect(fill = "white", colour = "white"),)
ggsave('Catch.png', path = "report", width = 10, height = 6, units = "cm", dpi=300, scale=2)


load(paste0(file="output/sol7a_catchScenarios.Rdata"))
catchScenariosSplit
catchScenariosSplit[, 9] <- icesRound(catchScenariosSplit[, 9])
catchScenariosSplit[, 10] <- icesRound(catchScenariosSplit[, 10])
catchScenariosSplit[, 11] <- icesRound(catchScenariosSplit[, 11])
write.taf(catchScenariosSplit, dir = "report")


#Contribution plot SSBInt+1
load('output/sol7a_ssb_by_age.RData')
ssb_by_age<-as.data.frame(ssb_by_age)
ssb_total<-sum(ssb_by_age)
ssb_by_age$total<-ssb_total
ssb_by_age$perc<-ssb_by_age$ssb_by_age/ssb_by_age$total
#check
sum(ssb_by_age$perc)
ssb_by_age$perc2<-as.numeric(ifelse(ssb_by_age$perc==0, "", ssb_by_age$perc))
ssb_by_age$perc2<-as.numeric(ifelse(ssb_by_age$perc<0.02, "", ssb_by_age$perc))
ssb_by_age$Yearclass<-c(base_year+2,base_year+1,base_year,base_year-1,base_year-2,base_year-3,base_year-4,base_year-5,base_year-6,"plusgroup")
ssb_by_age$STF <-c(T,T,T,F,F,F,F,F,F,F)
g1 <- subset(ssb_by_age, is.na(perc2))

ggplot(data=ssb_by_age, aes(x=Yearclass, y=ssb_by_age)) +
  geom_bar(stat="identity",aes(fill=STF)) + ggtitle(paste("Share of SSB",base_year+3)) +
  coord_flip()+ geom_text(aes(label = scales::percent(perc2)),hjust = +1.1, vjust = 0.5, color="white", size = 3) + 
  geom_text(data=g1,label = "<2%",hjust = -1.1,vjust = 0.5, size = 3) +
  labs(x = "Yearclass", y = "SSB")
ggsave('yearclass_SSBInt+2.png',path = "report", width=8, height=5, dpi=300)


#Contribution plot CInt+1
load('output/sol7a_C_by_age.RData')
C_by_age<-as.data.frame(C_by_age)
C_total<-sum(C_by_age)
C_by_age$total<-C_total
C_by_age$perc<-C_by_age$C_by_age/C_by_age$total
#check
sum(C_by_age$perc)
C_by_age$perc2<-as.numeric(ifelse(C_by_age$perc==0, "", C_by_age$perc))
C_by_age$perc2<-as.numeric(ifelse(C_by_age$perc<0.01, "", C_by_age$perc))
C_by_age$Yearclass<-c(base_year+1,base_year,base_year-1,base_year-2,base_year-3,base_year-4,base_year-5,base_year-6,base_year-7,"plusgroup")
C_by_age$STF <-c(T,T,F,F,F,F,F,F,F,F)
g1 <- subset(C_by_age, is.na(perc2))

ggplot(data=C_by_age, aes(x=Yearclass, y=C_by_age), xlim=) +
  geom_bar(stat="identity",aes(fill=STF)) + ggtitle(paste("Share of Catch",base_year+2)) +
  coord_flip()+ geom_text(aes(label = scales::percent(perc2)),hjust = +1.1,vjust = 0.5, color="white", size = 3) + 
  geom_text(data=g1,label = "<1%",hjust = -1.1,vjust = 0.5, size = 3) +
  labs(x = "Yearclass", y = "Catch")
ggsave('yearclass_CatchInt+1.png',path = "report", width=8, height=5, dpi=300)


# extra stuff for ACOM

SAM_fit_sol_7a_this_yr<-get(load('model/SAM_fit_sol_7a.RData'))
Fmsy_forecast_this_yr<-get(load('output/sol7a_Fmsy_forecast.RData'))


# the numbers as estimated and forecast of this years WG and last year's
n_this_year_contr <- rbind(ntable(SAM_fit_sol_7a_this_yr),apply(exp(Fmsy_forecast_this_yr[[2]]$sim[,1:10]),2,median),
                           apply(exp(Fmsy_forecast_this_yr[[3]]$sim[,1:10]),2,median),apply(exp(Fmsy_forecast_this_yr[[4]]$sim[,1:10]),2,median))

rownames(n_this_year_contr) <- as.character(1970:2026)

n_this_year<- rbind(ntable(SAM_fit_sol_7a_this_yr),apply(exp(Fmsy_forecast_this_yr[[2]]$sim[,1:10]),2,median),
                    apply(exp(Fmsy_forecast_this_yr[[3]]$sim[,1:10]),2,median))

rownames(n_this_year) <- as.character(1970:2025)

setwd("C:/Users/snimmegeers/OneDrive - ILVO/Development/RStudio/D1SCI/VISBIO/ndgp.ices.assess.sol7a/WKFLATFISH 2024/SAMrun50/TAF/")
getwd()
SAM_fit_sol_7a_last_yr<-get(load('model/SAM_fit_sol_7a.RData'))
Fmsy_forecast_last_yr<-get(load('output/sol7a_Fmsy_forecast.RData'))

n_last_year <- rbind(ntable(SAM_fit_sol_7a_last_yr),
                     apply(exp(Fmsy_forecast_last_yr[[2]]$sim[,1:10]),2,median),
                     apply(exp(Fmsy_forecast_last_yr[[3]]$sim[,1:10]),2,median),
                     apply(exp(Fmsy_forecast_last_yr[[4]]$sim[,1:10]),2,median))
rownames(n_last_year) <- rownames(n_this_year)

n_this_year2    <- t(n_this_year[as.character(1970:2025),])
df_n_this_year <- melt(n_this_year2)
colnames(df_n_this_year) <- c("Age","Year","N")
df_n_this_year$"WG"<-"WGCSE 2024"
df_n_this_year$"Type"<-"Data"
df_n_this_year[df_n_this_year$Year==2024,]$"Type"<-"Intermediate year"
df_n_this_year[df_n_this_year$Year==2025,]$"Type"<-"Advice year"

head(df_n_this_year)
table(df_n_this_year$Year,df_n_this_year$Type)


n_last_year2    <- t(n_last_year[as.character(1970:2024),])
df_n_last_year <- melt(n_last_year2)
colnames(df_n_last_year) <- c("Age","Year","N")
df_n_last_year$"WG"<-"WGCSE 2023-revised"
df_n_last_year$"Type"<-"Data"
df_n_last_year[df_n_last_year$Year==2023,]$"Type"<-"Intermediate year"
df_n_last_year[df_n_last_year$Year==2024,]$"Type"<-"Advice year"

head(df_n_last_year)
table(df_n_last_year$Year,df_n_last_year$Type)

N_at_age_dat<-rbind(df_n_last_year,df_n_this_year)
dat<-N_at_age_dat[N_at_age_dat$Year>2021,]
dat$Type <- factor(dat$Type,levels=c("Data","Intermediate year","Advice year"))

taf.png("Compare inputs - Stock numbers-at-age.png",width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(dat,aes(x=as.factor(Age),y=N,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Abundance (thousands)",shape="") + xlab("Age") +
  facet_wrap(~Year,nrow=2)+theme_bw()+scale_shape_manual(values=c(16, 2, 0))
print(p1)
dev.off()

ncomp <- n_this_year/n_last_year
ncomp <- t(ncomp[as.character(2012:2025),])
write.taf(ncomp, dir = "report")


# biomass comparison
avg_years_this_yr              <- max(SAM_fit_sol_7a_this_yr$data$years) + (-2:0)
avg_years_last_yr              <- max(SAM_fit_sol_7a_last_yr$data$years) + (-2:0)

b_by_age_sims_1 <-sweep(exp(Fmsy_forecast_this_yr[[2]]$sim[,1:10]), MARGIN = 2, FUN = "*", STATS = apply(SAM_fit_sol_7a_this_yr$data$stockMeanWeight[as.character(avg_years_this_yr),],2,mean))
b_by_age_1<-b_by_age_sims_1[which.min(abs(rowSums(b_by_age_sims_1) - median(rowSums(b_by_age_sims_1)))),]
sum(b_by_age_1)

b_by_age_sims_2 <-sweep(exp(Fmsy_forecast_this_yr[[3]]$sim[,1:10]), MARGIN = 2, FUN = "*", STATS = apply(SAM_fit_sol_7a_this_yr$data$stockMeanWeight[as.character(avg_years_this_yr),],2,mean))
b_by_age_2<-b_by_age_sims_2[which.min(abs(rowSums(b_by_age_sims_2) - median(rowSums(b_by_age_sims_2)))),]
sum(b_by_age_2)

b_this_year <- rbind(ntable(SAM_fit_sol_7a_this_yr) * SAM_fit_sol_7a_this_yr$data$stockMeanWeight, b_by_age_1, b_by_age_2)

rownames(b_this_year) <- as.character(1970:2025)


b_by_age_sims_1 <-sweep(exp(Fmsy_forecast_last_yr[[2]]$sim[,1:10]), MARGIN = 2, FUN = "*", STATS = apply(SAM_fit_sol_7a_last_yr$data$stockMeanWeight[as.character(avg_years_last_yr),],2,mean))
b_by_age_1<-b_by_age_sims_1[which.min(abs(rowSums(b_by_age_sims_1) - median(rowSums(b_by_age_sims_1)))),]
sum(b_by_age_1)

b_by_age_sims_2 <-sweep(exp(Fmsy_forecast_last_yr[[3]]$sim[,1:10]), MARGIN = 2, FUN = "*", STATS = apply(SAM_fit_sol_7a_last_yr$data$stockMeanWeight[as.character(avg_years_last_yr),],2,mean))
b_by_age_2<-b_by_age_sims_2[which.min(abs(rowSums(b_by_age_sims_2) - median(rowSums(b_by_age_sims_2)))),]
sum(b_by_age_2)

b_by_age_sims_3 <-sweep(exp(Fmsy_forecast_last_yr[[4]]$sim[,1:10]), MARGIN = 2, FUN = "*", STATS = apply(SAM_fit_sol_7a_last_yr$data$stockMeanWeight[as.character(avg_years_last_yr),],2,mean))
b_by_age_3<-b_by_age_sims_3[which.min(abs(rowSums(b_by_age_sims_3) - median(rowSums(b_by_age_sims_3)))),]
sum(b_by_age_3)

b_last_year <- rbind(ntable(SAM_fit_sol_7a_last_yr) * SAM_fit_sol_7a_last_yr$data$stockMeanWeight,b_by_age_1,b_by_age_2,b_by_age_3)
rownames(b_last_year) <- rownames(b_this_year)


b_this_year2    <- t(b_this_year[as.character(1970:2025),])
df_b_this_year <- melt(b_this_year2)
colnames(df_b_this_year) <- c("Age","Year","B")
df_b_this_year$"WG"<-"WGCSE 2024"
df_b_this_year$"Type"<-"Data"
df_b_this_year[df_b_this_year$Year==2024,]$"Type"<-"Intermediate year"
df_b_this_year[df_b_this_year$Year==2025,]$"Type"<-"Advice year"

head(df_b_this_year)
table(df_b_this_year$Year,df_b_this_year$Type)


b_last_year2    <- t(b_last_year[as.character(1970:2024),])
df_b_last_year <- melt(b_last_year2)
colnames(df_b_last_year) <- c("Age","Year","B")
df_b_last_year$"WG"<-"WGCSE 2023-revised"
df_b_last_year$"Type"<-"Data"
df_b_last_year[df_b_last_year$Year==2023,]$"Type"<-"Intermediate year"
df_b_last_year[df_b_last_year$Year==2024,]$"Type"<-"Advice year"

head(df_b_last_year)
table(df_b_last_year$Year,df_b_last_year$Type)


B_at_age_dat<-rbind(df_b_last_year,df_b_this_year)
dat<-B_at_age_dat[B_at_age_dat$Year>2021,]
dat$Type <- factor(dat$Type,levels=c("Data","Intermediate year","Advice year"))

taf.png("Compare inputs - Stock biomass-at-age.png",width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(dat,aes(x=as.factor(Age),y=B,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Biomass (t)",shape="")+ xlab("Age") +
  facet_wrap(~Year,nrow=2)+theme_bw()+scale_shape_manual(values=c(16, 2, 0))
print(p1)
dev.off()


bcomp <- b_this_year/b_last_year
bcomp <- t(bcomp[as.character(2012:2025),])
write.taf(bcomp, dir = "report")


# compare forecast assumptions----------------------------------------------####

# read in data
dat <- read.csv("report/Compare_forecast_assumptions.csv")
dat$Type <- factor(dat$Type,levels=c("Data year","Intermediate year","Advice year"))

taf.png("Compare_forecast_assumptions.png",width = 11, height = 7, units = "in", res = 600)
p1 <- ggplot(dat,aes(x=Year,y=Value))+
  geom_line(aes(colour=WG))+
  geom_point(aes(colour=WG,shape=Type),size=2,fill='white')+
  scale_shape_manual(values=c(3,21,16)) +
  facet_wrap(~Variable,scales="free_y")+
  theme_bw()+labs(x="",y="",colour="",shape="")
print(p1)
dev.off()


# plot selectivity

sel__this_yr<-apply(faytable(SAM_fit_sol_7a_this_yr)[c("2021","2022","2023"),],2,mean)
sel__last_yr<-apply(faytable(SAM_fit_sol_7a_last_yr)[c("2020","2021","2022"),],2,mean)

sel__this_yr<-as.data.frame(sel__this_yr)
sel__last_yr<-as.data.frame(sel__last_yr)

sel__this_yr$label<-"mean(21-23)"
names(sel__this_yr)[1]<-"value"
sel__this_yr$Age<-rownames(sel__this_yr)
sel__last_yr$label<-"mean(20-22)"
names(sel__last_yr)[1]<-"value"
sel__last_yr$Age<-rownames(sel__last_yr)

com<-rbind(sel__this_yr,sel__last_yr)
com$Age <- as.factor(as.numeric(com$Age))
str(com)

taf.png("Compare mean F.png",width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=com, aes(Age, value, group=label)) + geom_line(aes(colour = label)) + ylab("mean F")
print(p1)
dev.off()


# plot stock weights

sw__last_yr<-apply(SAM_fit_sol_7a_last_yr$data$stockMeanWeight[as.character(avg_years_last_yr),],2,mean)
sw__this_yr<-apply(SAM_fit_sol_7a_this_yr$data$stockMeanWeight[as.character(avg_years_this_yr),],2,mean)

sw__this_yr<-as.data.frame(sw__this_yr)
sw__last_yr<-as.data.frame(sw__last_yr)

sw__this_yr$label<-"mean(21-23)"
names(sw__this_yr)[1]<-"value"
sw__this_yr$Age<-rownames(sw__this_yr)
sw__last_yr$label<-"mean(20-22)"
names(sw__last_yr)[1]<-"value"
sw__last_yr$Age<-rownames(sw__last_yr)

com<-rbind(sw__this_yr,sw__last_yr)
com$Age <- as.factor(as.numeric(com$Age))
str(com)

taf.png("Compare mean sw.png",width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(data=com, aes(Age, value, group=label)) + geom_line(aes(colour = label)) + ylab("mean stock weight")
print(p1)
dev.off()


