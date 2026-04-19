library("dplyr")
library("cluster")
library("ggplot2")
library("cluster")

setwd("TODO:change_path")
#nr <- 1535
colTypes=c(rep("factor",2),"character",rep("factor",2),"character","factor",rep("numeric",2014-1992+1))
Sales 	<-	read.csv("Sales.csv", header=TRUE, sep=";", dec=",",colClasses=colTypes)
Assets	<-	read.csv("Assets.csv", header=TRUE, sep=";", dec=",",colClasses=colTypes)
Depr 		<-	read.csv("Depr.csv", header=TRUE, sep=";", dec=",",colClasses=colTypes)
PPE 		<-	read.csv("PPE.csv", header=TRUE, sep=";", dec=",",colClasses=colTypes)
Interest<-	read.csv("Interest.csv", header=TRUE, sep=";", dec=",",colClasses=colTypes)
Profit	<-	read.csv("Profit.csv", header=TRUE, sep=";", dec=",",colClasses=colTypes)
Debt 		<-	read.csv("Debt.csv", header=TRUE, sep=";", dec=",",colClasses=colTypes)
Goodwill<-	read.csv("Goodwill.csv", header=TRUE, sep=";", dec=",",colClasses=colTypes)
Equity 	<-	read.csv("Equity.csv", header=TRUE, sep=";", dec=",",colClasses=colTypes)
Treasure<-	read.csv("Treasure.csv", header=TRUE, sep=";", dec=",",colClasses=colTypes)
NCA 		<-	read.csv("NCA.csv", header=TRUE, sep=";", dec=",",colClasses=colTypes)
OInc 		<-	read.csv("OInc.csv", header=TRUE, sep=";", dec=",",colClasses=colTypes)

#cSec<-c("") #cSec<-c("Energetics") Filter by Sector, if it needs 
#xComp<-c("Rural Electrification","Sichuan Chuantou Energy","Huaneng Power International") #Exclude financial companies
#ESales<-filter(Sales,Sector %in% cSec, !Company %in% xComp)

# Cluster analysis
ls<-c(10,3,3,1,1.5)  # Scaling ratios for data before clustering
ls<-c(1,1,1,1,1)  # Scaling ratios for data before clustering
year<-2007; xyear<-c(paste0("X",year))
df<-data.frame(Assets$Company,Assets$Sector,Assets$Region,
               Debt[xyear]/Equity[xyear]/ls[1],
               (Profit[xyear]+Interest[xyear])/(Debt[xyear]+Equity[xyear]),
               Goodwill[xyear]/Equity[xyear]/ls[2],
               (1-(PPE[xyear]-Depr[xyear]+Goodwill[xyear])/NCA[xyear])/ls[3],
               -Treasure[xyear]/(Equity[xyear]-Treasure[xyear])/ls[4],
               OInc[xyear]/Sales[xyear]/ls[5])

names(df)<-c("Company","Sector","Region","L","R","G","F","T","P")	
arNames<-c("Company","Sector","Region","Leverage","ROCE","Goodwill","Financial Assets","Treasures Share","ROS")
summary(df)
bnd<-c(-1/ls[1],10/ls[1],-0.3,0.6,-.5/ls[5],1.1/ls[5]); #names(bnd)<-("Lb","Lt","Rb","Rt","Pt")
o1 <- filter(df,L<bnd[1]|L>bnd[2]|R<bnd[3]|R>bnd[4]|P<bnd[5]|P>bnd[6])
o2 <- filter(df,is.na(R)|is.infinite(R)|is.na(F)|is.infinite(F)|is.na(P)|is.infinite(P)); 
dr <- filter(df,L>bnd[1],L<bnd[2],R>bnd[3],R<bnd[4],P>bnd[5],P<bnd[6],!is.na(R),!is.infinite(R),!is.na(F),!is.infinite(F),!is.na(P),!is.infinite(P))
d<-dr; d[4:9] <- scale (dr[4:9])
d[4] <- d[4]*1.5 # scale correction 
summary(d)
#boxplot(d[4:9])
arX<-4; arY<-8
#ck<-pam(data.frame(d[arX],d[arY]),6)
ck<-pam(d[4:9],6)
#plot(ck)
dr <- mutate(dr,cl=factor(ck$cluster))
#dr <- mutate(d,L=L*ls[1],G=G*ls[2],F=F*ls[3],T=T*ls[4],P=P*ls[5])
summary(dr)
prop.table(with(dr,table(cl,Region)),2)
(p<-qplot(x=dr[arX],y=dr[arY], data=dr, col=cl, alpha=2, main=year, xlab=arNames[arX], ylab=arNames[arY]) 
  + guides(col=guide_legend("Cluster"))) 
plot(dr[4:9],col=dr$cl)
p+facet_wrap( ~Region)
p+facet_wrap( ~Sector)

g<-filter(dr,P<(-1))

nyear<-2012; xyear<-c(paste0("X",nyear))
df<-data.frame(EROCE$Company,EROCE$Sector,EROCE$Region,ELeverage[xyear],EROCE[xyear])
names(df)<-c("Company", "Sector", "Region","L","ROCE")	
dc <- filter(df,!Company %in% o$Company)
dc <- mutate(dc,cl=factor(ck$cluster))
(p<-qplot(L,ROCE, data=dc, col=cl, main=paste(nyear,"on",year), xlab="Leverage", ylab="ROCE", xlim=c(-1,12), ylim = c(-0.2,0.6)) + guides(col=guide_legend("Cluster")))
p+facet_wrap( ~Region)

