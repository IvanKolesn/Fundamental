library(Quandl)
library(fpp2)
library(quantmod)
options(scipen=99)

Quandl.api_key('oVkyAxMcky_ZUy427s-6')

Tickers<-c("FB","SNAP","ZNGA","TWTR","GRPN","NTNX","RUN","FEYE","OKTA","COUP","BABA","TEAM",
           "PSTG","GPRO","SHOP","SQ","CLDR","LC","BOX","APRN","W","WDAY","MULE","VG","XRF",
           "TWLO","QUOT","NEWR")

Tickers<-c("VNET","ACIA","CRTO","ALRM","GDS","GSUM","INOV","INSE","JMU","ENPH","PSDO","QLYS","RESN","TRUE","VERI","YNDX","ZDGE",
           "WUBA","ATEN","HIVE","ECOM","NEWR","CARG","OOMA","SQNS","SSTK","TLRA","XNET")

Tickers<-c("ADBE","EPAY","BRKS","CERN","CSPI","DSPG","EGAN","HLIT","INTU","JCOM","KTCC","LOGI","MXIM","MIND","NVDA","PEGA",
           "POWI","QSII","RMBS","SGMA","SSYS","SYNT","TTWO","TSEM", "VECO","ZBRA")

Final<-matrix(NA,nrow = 1,ncol = length(Tickers))
colnames(Final)<-Tickers

for (i in 1:length(Tickers)) {
  
  Data<-Quandl.datatable('SHARADAR/SF1', ticker = Tickers[i])
  
  #Dividend
  Div<-Data$divyield
  Forecast<-forecast(ets(Div),h=4)
  Forecast<-Forecast$mean %>% as.vector()
  
  #ERP and Rf
  ERP<- 0.0508
  Rf<- 0.0297
  
  #Beta
  Return<-getSymbols(Tickers[i], from = "2015-06-20", auto.assign = FALSE) %>% dailyReturn()
  SP500 <- getSymbols("^GSPC",auto.assign = FALSE, from = min(index(Return)), to = (max(index(Return))+1)) %>% dailyReturn()
  
  #3year
  fit<-lm(Return~SP500)
  Beta_3<-fit$coefficients[2] %>% as.vector()
  
  #1year 
  Return <- tail(Return,n=52)
  SP500 <-tail(SP500,n=52)
  
  fit<-lm(Return~SP500)
  Beta_1<-fit$coefficients[2] %>% as.vector()
  
  #Unlever Betas (using the Hamada formula)
  # B_u<-B_l/(1+(1-Tc)*D/E)
  DE_3<-mean(tail(Data$de,3))
  DE_1<-mean(tail(Data$de,1))
  DE<-(DE_1+DE_3)/2
  #Corporate tax rate
  T_c <- 0.21                                
  
  Beta_u_1<-Beta_1/(1+(1-T_c)*DE_1)
  Beta_u_3<-Beta_3/(1+(1-T_c)*DE_3)
  Beta_u <- (Beta_u_1+Beta_u_3)/2
  #Relevering
  Beta_rl<-Beta_u
  Ke<-Rf+Beta_rl*ERP
  
  #PGR
  Delta_Wc<-c(0,diff(Data$workingcapital,1))
  PGR<-(Data$capex+Delta_Wc-Data$depamor)/Data$netinc
  PGR<-tail(PGR,1)*Ke
  
  TV<-tail(Forecast,1)*(1+PGR)/(Ke-PGR)
  
  V<-matrix(NA,nrow = (length(Forecast)+1), ncol=4) %>% as.data.frame()
  colnames(V)<-c("Time","FCFE","DF","Value")
  V$Time<-seq(1,(length(Forecast)+1),1)
  V$FCFE<-c(Forecast,TV)
  V$DF=1/(1+Ke)^V$Time
  V$Value<-V$FCFE*V$Time
  Price_Div<-sum(V$Value)/tail(Data$sharesbas,1)
  
  Final[1,i]<-Price_Div

}
