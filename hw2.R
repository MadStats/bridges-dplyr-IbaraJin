library(plyr);
library(choroplethr);
library(dplyr);
library(readr);
library(data.table);

#Get the abbreviation for all states
states=as.tbl(fread("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt"));
states=states[-(1:12),];
states[51,]=c("WashDC", "DC");
states[52,]=c("Puerto Rico", "PR");

#Get the filepath for each state
#filepath=rep("",52*25);
filepath2016=rep("",52);
filepathWI=rep("",7);
for (year in 2010:2016){
  filepathWI[(year-2009)]=paste("https://www.fhwa.dot.gov/bridge/nbi/",year,"/delimited/WI",year-2000,".txt",sep="");
}
for(i in 1:52){
  filepath2016[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/",states[i,2],"16.txt",sep="");
}
  
#Define the type of data
samplepath="https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt";
sample=as.tbl(fread(samplepath)); 
sampleclass=sapply(sample,class);

#Read data
dat16=ldply(filepath2016,fread,colClasses=sampleclass);
addYear=function(fp,...){
  dat=as.tbl(fread(fp));
  y=as.numeric(sub(pattern=".*(\\d{4})/delimited.*", replacement="\\1",x=fp));
  year=rep(y,length(dat[,1]));
  return(cbind(year,dat));
}
datWI=ldply(filepathWI,addYear,colClasses=sampleclass);

dat_use=dat16;
is.na(dat_use) %>% rowSums %>% hist;
#hist(rowSums(is.na(dat_use))) 
#Compute the num of missing values of each col and make a histogram
fun = function(x){ 
  return(which(x>20))
}
badrows=is.na(dat_use) %>% colSums %>% fun;
#fun(colSums(is.na(dat_use))) 
#The index of the cols that have more than 20 missing values
dat_use=dat_use[,-badrows];

#Delete the rows containing special chararcters 
justified=1; #The first j-1 rows are justified

for(j in justified:ncol(dat_use)){
  len=nchar(dat_use[,j], keepNA=T);
  #Returns Error if the variable contains special characters
}
print(j);
dat_use=dat_use[,-j];
justified=j;

#Choose variables
colnames(dat_use);
head(dat_use);

keep=c("STATE_CODE_001","COUNTY_CODE_003","LAT_016","LONG_017","YEAR_BUILT_027","ADT_029",
       "SERVICE_ON_042A","SERVICE_UND_042B","MAIN_UNIT_SPANS_045","DATE_LAST_UPDATE","SUFFICIENCY_RATING");
x=select(dat_use,one_of(keep));  # see chapter 5 (section 4) in r4ds.

dat_use=datWI;
keep=c("year","COUNTY_CODE_003","LAT_016","LONG_017","YEAR_BUILT_027","ADT_029",
       "SERVICE_ON_042A","SERVICE_UND_042B","MAIN_UNIT_SPANS_045","DATE_LAST_UPDATE","SUFFICIENCY_RATING");
y=select(dat_use,one_of(keep));

library(ggplot2);

state_code=x[,1];
state_code=state_code[!duplicated(state_code)];
CODE=rep(0,52);
LAT=rep(0,52);
LONG=rep(0,52);
ADT=rep(0,52);
SR=rep(0,52);
SO=rep(0,52);
SU=rep(0,52);
n=rep(0,52);
c=1;
for(i in state_code){
	dat_tmp=filter(x,STATE_CODE_001==i);
	dat_tmp=filter(dat_tmp,LAT_016>0);
	dat_tmp=filter(dat_tmp,LONG_017>0);
	dat_tmp=na.omit(dat_tmp);
	CODE[c]=i;
	n[c]=length(dat_tmp[,1]);
	LAT[c]=mean(dat_tmp$LONG_017);
	LONG[c]=mean(dat_tmp$LAT_016);
	ADT[c]=sum(dat_tmp$ADT_029);
	SR[c]=mean(dat_tmp$SUFFICIENCY_RATING);
	SO[c]=mean(dat_tmp$SERVICE_ON_042A);
	SU[c]=mean(dat_tmp$SERVICE_UND_042B);
	c=c+1;
}
dat_state=data.frame(CODE,LAT,LONG,ADT,SR,SO,SU,n);

ggplot(data=dat_state)+
	geom_point(mapping=aes(y=LAT,x=LONG,color=ADT,size=SR));

tmp=filter(dat_state,ADT<400000000);
ggplot(data=tmp)+
	geom_point(mapping=aes(y=SR,x=ADT))+
	geom_smooth(mapping=aes(y=SR,x=ADT));

code=as.factor(dat_state$CODE);
OUrate=dat_state$SO/(dat_state$SO+dat_state$SU);
ggplot()+ 
	geom_bar(mapping=aes(x=code,y=OUrate,fill=dat_state$SR),stat="identity")+
	coord_polar();

