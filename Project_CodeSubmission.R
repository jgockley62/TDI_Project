library(httr)
library(jsonlite)
library(lubridate)
library(data.table)
require(ggplot2)
require(reshape2)
#Pull for API:

URL<-"https://data.ct.gov/resource/bvbe-957i"
PULL <- GET(URL)
JSON_FILE_OBJ <- rawToChar(PULL$content)
AcPTI <- fromJSON(JSON_FILE_OBJ)

##Figure 1: 12 Most Frequent Offenses
#Get the offenses which are represented in each group
AfAm <- AcPTI[AcPTI[,10] == "BLACK", ]
Cau <- AcPTI[AcPTI[,10] == "WHITE", ]
Hisp <- AcPTI[AcPTI[,10] == "HISPANIC", ]

#All Shared Values:
#keys <- Reduce(intersect, list(AfAm[,9], Cau[,9], Hisp[,9]))
Top10 <- row.names(as.matrix(CrimeTabs[CrimeTabs[,1] > 6000,]))

AfAm_Top10 <- AfAm[AfAm[,9] %in% Top10, ]
Cau_Top10 <- Cau[Cau[,9] %in% Top10, ]
Hisp_Top10 <- Hisp[Hisp[,9] %in% Top10, ]

#AfAm_cleaned <- AfAm[AfAm[,9] %in% keys, ]
#Cau_cleaned <- Cau[Cau[,9] %in% keys, ]
#Hisp_cleaned <- Hisp[Hisp[,9] %in% keys, ]

#AcPTI_cleaned <- rbind(AfAm_cleaned,Cau_cleaned,Hisp_cleaned) 
AcPTI_Top10 <- rbind(AfAm_Top10,Cau_Top10,Hisp_Top10) 

#par(mar=c(20,3,2,0))
#boxplot(as.numeric(AcPTI_Top10[,2]) ~ interaction(AcPTI_Top10[,10], AcPTI_Top10[,9]), yaxt='n', ylim=c(0,3.5e6), las=2, cex.axis = .8, data = AcPTI_Top10, cex = .3, frame=F)
#axis(2, at = c(0,1e6,2e6,3e6), labels=c("0","1e6","2e6","3e6"), cex = 1.5, las=2)

#Plot Figure
pdf(file="Figure1.pdf")

  par(mar=c(22,3,2,0))
  boxplot(as.numeric(AcPTI_Top10[,2]) ~ interaction(AcPTI_Top10[,10], AcPTI_Top10[,9]), yaxt='n', ylim=c(0,2e6), las=2, main = "12 Most Frequent Offences", font.axis = 2, font = 2, col =c("grey33","grey75","gray87") , cex.axis = .8, data = AcPTI_Top10, cex = .3, frame=F)
  axis(2, at = c(0,1e6,2e6), labels=c("0","1e6","2e6"), cex = 4, las=2, font = 2)

dev.off()


#####Figure2: Violent versus Nonviolent Drug offenses
##Look at First Degree Assult (Large Amount of Samples + Violent Crime)
assult <- Top10[1]
Nv_Drug <- Top10[10]

##One sizable outlier, exclusion will make estimation more conservative 
Assult<-AcPTI_Top10[AcPTI_Top10[,9] == assult, ]
Assult_Mod<-Assult[as.numeric(Assult[,2]) < 1500000, ]
NV_Drug<-AcPTI_Top10[AcPTI_Top10[,9] == Nv_Drug, ]
Comb<-rbind(Assult_Mod, NV_Drug)


####Stats
##Data looks to be tailed and not quite normally distributed
##Use Wilcox Rank Sum

Ass_AFvW<-wilcox.test(as.numeric(Assult_Mod[Assult_Mod[,10] == "WHITE", 2]), as.numeric(Assult_Mod[Assult_Mod[,10] == "BLACK", 2]))
Ass_HisvW<-wilcox.test(as.numeric(Assult_Mod[Assult_Mod[,10] == "WHITE", 2]), as.numeric(Assult_Mod[Assult_Mod[,10] == "HISPANIC", 2]))

wilcox.test(as.numeric(Assult_Mod[Assult_Mod[,10] == "BLACK", 2]), as.numeric(Assult_Mod[Assult_Mod[,10] == "HISPANIC", 2]))

Nv_Drug_AFvW<-wilcox.test(as.numeric(NV_Drug[NV_Drug[,10] == "WHITE", 2]), as.numeric(NV_Drug[NV_Drug[,10] == "BLACK", 2]))
Nv_Drug_HispvW<-wilcox.test(as.numeric(NV_Drug[NV_Drug[,10] == "WHITE", 2]), as.numeric(NV_Drug[NV_Drug[,10] == "HISPANIC", 2]))

wilcox.test(as.numeric(NV_Drug[NV_Drug[,10] == "BLACK", 2]), as.numeric(NV_Drug[NV_Drug[,10] == "HISPANIC", 2]))


####Plot Figure2
NAMEs<-c("AfAm First Deg Assult (BF)", "Hisp First Deg Assult (BF)", "Cauc First Deg Assult (BF)","AfAmSale NARC/Amp (F)", "Hisp Sale NARC/Amp (F)", "Cauc Sale NARC/Amp (F)")

pdf(file="Figure2.pdf")
  par(mar=c(11,4,2,0))
  boxplot(as.numeric(Comb[,2]) ~ interaction(Comb[,10], Comb[,9]),  col =c("grey33","grey75","gray87"), yaxt='n' , pos=1, offset=.2, cex=.8, names = NAMEs, ylim=c(0,2e6), las = 2, frame=F, main = "Assult (Violent Crime) Vs Sale Narcotic (Non Violent)")
  axis(2, at = c(0,1e6,2e6), labels=c("0","1e6","2e6"), cex = 4, las=2, font = 2)
  #Assult
  ##AfAm v Cauc
  text(x=2, y= 1.82e6, "p = 1.65e-44" , pos=1, offset=.2, cex=1.75)
  segments(x0 = 1, y0= 1.7e6,x1 = 3, y1 = 1.7e6, col = "black", lwd = 2)
  
  ##Hisp v Cauc
  text(x=2.5, y= 1.57e6, paste("p", round(Ass_HisvW$p.value, digits =2 ), sep = " = ") , cex=1.75, pos=1, offset=.2)
  segments(x0 = 2, y0= 1.45e6,x1 = 3, y1 = 1.45e6, col = "black", lwd = 2)

  #Nv_Drug
  ##AfAm v Cauc
  text(x=5, y= 1.82e6, paste("p", round( Nv_Drug_AFvW$p.value, digits =2 ), sep = "=") , cex=1.75, pos=1, offset=.2)
  segments(x0 = 4, y0= 1.7e6,x1 = 6, y1 = 1.7e6, col = "black", lwd = 2)

  ##Hisp v Cauc
  text(x=5.5, y= 1.57e6,  "p = 4.75e-10" , pos=1, offset=.2, cex=1.75)
  segments(x0 = 5, y0= 1.45e6,x1 = 6, y1 = 1.45e6, col = "black", lwd = 2)
dev.off()

