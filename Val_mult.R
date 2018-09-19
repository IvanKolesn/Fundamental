library(Quandl)
library(fpp2)
library(quantmod)
options(scipen=99)

Quandl.api_key('oVkyAxMcky_ZUy427s-6')

Tickers<-c("FB","SNAP","ZNGA","TWTR","GRPN","NTNX","RUN","FEYE","OKTA","COUP","BABA","TEAM",
           "PSTG","GPRO","SHOP","SQ","CLDR","BOX","APRN","W","WDAY","MULE","VG",
           "TWLO","QUOT","NEWR")

Tickers<-c("VNET","ACIA","CRTO","ALRM","GDS","GSUM","INOV","INSE","JMU","PSDO","QLYS","RESN","TRUE","VERI","ZDGE",
           "WUBA","ATEN","HIVE","ECOM","NEWR","CARG","OOMA","SQNS","SSTK","TLRA","XNET")

Tickers<-c("ADBE","EPAY","BRKS","CERN","CSPI","DSPG","EGAN","HLIT","INTU","JCOM","KTCC","LOGI","MXIM","MIND","NVDA","PEGA",
           "POWI","QSII","RMBS","SGMA","SSYS","SYNT","TTWO","TSEM", "VECO","ZBRA")

Final<-matrix(NA,nrow = 1,ncol = length(Tickers))
colnames(Final)<-Tickers

for (i in 1:length(Tickers)) {
  
  Data<-Quandl.datatable('SHARADAR/SF1', ticker = Tickers[i])
  Price_Ebit<-tail(Data$ebit,1)*13/tail(Data$sharesbas,1)
  Price_Ebitda<-tail(Data$ebitda,1)*16/tail(Data$sharesbas,1)
  Price<- (Price_Ebit+Price_Ebitda)/2
  Final[1,i]<-Price
  
}
