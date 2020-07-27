###Create month variable
stocks=read.csv("d_logret_16stocks.csv")
date=as.Date(stocks[,1])
month=months(date,abbreviate=T)
month=match(month, month.abb)
stocks=cbind(stocks,month)
month_1=c(month,12)
###Construct portfolio

#sorted=sort(apply(stocks[1:120,2:17],2,sum))
#quartile1=sorted[1:4]
#quartile1=stocks[,c(1,7,15,13,9)]
#quartile4=sorted[13:16]
#quartile4=stocks[,c(1,14,6,2,10)]
return1=return4=rep(0,1480)
#return1[1]=sum(apply(quartile1,2,sum))
l=1
for(i in 141:(dim(stocks)[1]+1))
{
	if(month_1[i]!=month_1[i-1]){
		app=apply(stocks[(i-140):(i-21),2:17],2,sum)
		sorted=sort(app)
		for(k in 1:4){
		for(j in 1:16)
		{
			if(app[j]==sorted[k])
			quartile1[,k]=stocks[(i-20):(i-1),(j+1)]
			
		}
		}
		for(k in 13:16){
		for(j in 1:16)
		{
			if(app[j]==sorted[k])
			quartile4[,(k-12)]=stocks[(i-20):(i-1),(j+1)]
			
		}
		}
	 return1[l]=sum(apply(quartile1,2,mean))
	 return4[l]=sum(apply(quartile4,2,mean))
	 l=l+1

	}
	
}
return1=return1[1:(l-1)]
return4=return4[1:(l-1)]
##Q1.1
question1=cbind(return1,return4,return4-return1)
##Q1.2
question2.J=question1[c(7,19,31,43,55),]
question2.nonJ=question1[-c(7,19,31,43,55),]
##Q1.3
SP500=read.csv("SP500.csv",header=F)
SP500=SP500[which(SP500[,2]!="#N/A"),]
SP500=SP500[-1401,]
SP500=data.frame(lapply(SP500,as.character),stringsAsFactors=F)
sp500=as.numeric(as.vector(SP500[,2]))
Rm=diff(SP500[,2])
k=1
r=1
Rm.mean=rep(0,length(Rm))
for(i in 2:length(Rm.mean))
{
	if(month[i]!=month[i-1]){
		Rm.mean[r]=mean(Rm[k:(i-1)])
		k=i
		r=r+1}
		if(i==length(Rm)){Rm.mean[r]=mean(Rm[k:i])}
}
Rm=Rm[7:r]
rf=2/252
y=return4-return1-rf
y=(y-mean(y))/sd(y)
x=Rm-rf
x=(x-mean(x))/sd(x)
summary(lm(y~x-1))
###Q1.4
std.mean1=rep(0,length(return1))
std.mean1[1]=1
for(i in 2:length(return1))
{
	std.mean1[i]=(1+return1[i])/(1+return1[i-1])*std.mean1[i-1]
}

std.mean4=rep(0,length(return4))
std.mean4[1]=1
for(i in 2:length(return4))
{
	std.mean4[i]=(1+return4[i])/(1+return4[i-1])*std.mean4[i-1]
}
diff.std=std.mean1-std.mean4

####
##Q2.1
###
TY=read.csv("@TY# 30 Minutes.csv")
TY=data.frame(lapply(TY,as.character),stringsAsFactors=F)
#time=TY$X.Time
#time=as.POSIXct(strptime(time,"%H:%M:%S:"))
H=as.numeric(as.vector(TY$X.High))
L=as.numeric(as.vector(TY$X.Low))
C=as.numeric(as.vector(TY$X.Close))
O=as.numeric(as.vector(TY$X.Open))
date=TY$X.Date
date=as.Date(date,"%m/%d/%Y")
V=as.numeric(TY$X.Volume)
 day=as.POSIXlt(date)$mday 
 time=TY$X.Time
 rown=dim(TY)[1]
 coln=dim(TY)[2]
 k=1
 TY.day=TY[which(TY$X.Time=="16:00:00"),]
 H.day=as.numeric(as.vector(TY.day$X.High))
L.day=as.numeric(as.vector(TY.day$X.Low))
C.day=as.numeric(as.vector(TY.day$X.Close))
O.day=as.numeric(as.vector(TY.day$X.Open))
library(tseries)
year=as.numeric(format(date,"%Y"))
#find year
k=1
ind.y=rep(0,8)
for(i in 2: dim(TY)[1]){if(year[i]!=year[i-1]) {ind.y[k]=i-1
		k=k+1}
	if(i==dim(TY)[1]){ind.y[k]=i}}
ind.y=c(0,ind.y)
###estimate trend
#T=rep(0,(length(ind.y)-1))
#for(i in 1:(length(ind.y)-1))
#{
#	T[i]=mean(C[(ind.y[i]+1):ind.y[i+1]])
#}
#trend=rep(0,dim(TY)[1])
#for(i in 1:(length(ind.y)-1))
#{
#	trend[(ind.y[i]+1):ind.y[i+1]]=T[i]
#}
#new.C=C-trend
plot(V,type="l")
abline(v=ind.y,col="red")
acf(V)
pacf(V)
summary(arma(V,c(1,1)))
vol=as.numeric(as.vector(TY$X.Volume))
 Vol=rep(0,dim(TY.day)[1])
 k=1
 l=1
 for(i in 1:dim(TY)[1])
 {
 	if(time[i]=="16:00:00"){Vol[k]=sum(vol[(l+1):i])
 		k=k+1
 		l=i}
 }
 summary(arma(Vol,c(1,1),include.intercep=F))
####Q2.2
Val_micro=0.5*(log(H/L))^2-0.386*(log(C/O))^2
 plot(Val_micro,type="l")
 Val_macro=0.5*(log(H.day/L.day))^2-0.386*(log(C.day/O.day))^2
 plot(Val_micro,type="l")
 plot(Val_macro,type="l")
 ####Q2.3
  Val_macro_p=rep(0,(length(Val_macro)-22))
 for(i in 23:length(Val_macro))
 {
 	Val_macro_p[i-22]=sum(Vol[(i-22):(i-1)]*Val_macro[(i-22):(i-1)])/sum(Vol[(i-22):(i-1)])
 }
 
 Val_micro_p=rep(0,(length(Val_micro)-22))
 for(i in 23:length(Val_micro))
 {
 	Val_micro_p[i-22]=sum(vol[(i-22):(i-1)]*Val_micro[(i-22):(i-1)])/sum(vol[(i-22):(i-1)])
 }

###Q2.4
###price information only
L=20
computeMA<-function(x,L)
{
weights <- rep(1/L,L)
ma.series=as.vector(filter(x, weights, method="convolution",side=1))
D=cbind(as.vector(x),ma.series)
return(D)
}
maresult=computeMA(C,L)
buysignal=C<maresult[,2]
sellsignal=C>maresult[,2]
tradeindicator=rep(0,length(C))
tradeindicator[buysignal]=1
tradeindicator[sellsignal]=-1
n=length(C)
signal2value<-function(x,signal)
{
result=array(0,c(n,5))
longposition=which(signal==1)
shortposition=which(signal==-1)
if(length(longposition)==0) return(NA)
lastTrade = longposition[1]
i=longposition[1]+1
position = 1 #initial long position
value = 1 #initial total value is 1
#find te first long position
while(i<=length(x)){
if(signal[i]==position*(-1)){
# set new last trading
position = signal[i]#change position
if(signal[i]==-1){#liquidate
value = value * (x[i]/x[lastTrade])
return=x[i]/x[lastTrade]-1
result[i,]=c(i, x[i], x[lastTrade],value,return)
}
if(signal[i]==1){#buy order
result[i,]=c(i, x[i], x[lastTrade],value,0)
}
lastTrade = i
}else{#no trading
result[i,]=c(i, x[i], x[lastTrade],value,0)}
i = i+1
}
return(result)
}
value=signal2value(C,tradeindicator)
finalvalue=value[dim(value)[1],4]
plot(value[,4],type="l")
###daily data
maresult=computeMA(C.day,L)
buysignal=C.day<maresult[,2]
sellsignal=C.day>maresult[,2]
tradeindicator=rep(0,length(C.day))
tradeindicator[buysignal]=1
tradeindicator[sellsignal]=-1
n=length(C.day)
value=signal2value(C.day,tradeindicator)
finalvalue=value[dim(value)[1],4]
plot(value[,4],type="l")
######volume and price information
expsmooth<-function(x)
{
weights <- c(0.75,0.25)
ma.series=as.vector(filter(x, weights, method="convolution",side=1))
D=cbind(as.vector(x),ma.series)
return(D)
}
volume=log(as.numeric(TY$X.Volume))
vol_p=expsmooth(volume)[,2]
maresult=computeMA(C,L)
m=20
n=length(volume)
cum.vol=rep(0,n)
for (i in 1:n)
{
	k=min(c(m,i))
	cum.vol[i]=sum(volume[(i-k+1):i])
}
cum.vol.p=rep(0,n)
for (i in 1:n)
{
	k=min(c(m,i))
	cum.vol.p[i]=sum(vol_p[(i-k+1):i])
}
buysignal=(C<maresult[,2])&(cum.vol<cum.vol.p)
sellsignal=(C>maresult[,2])|(cum.vol>cum.vol.p)
tradeindicator=rep(0,length(volume))
tradeindicator[buysignal]=1
tradeindicator[sellsignal]=-1

value=signal2value(C,tradeindicator)
finalvalue=value[dim(value)[1],4]
plot(value[-c(1:36),4],type="l")
###daily
volume=log(Vol)
vol_p=expsmooth(volume)[,2]
maresult=computeMA(C.day,L)
m=20
n=length(volume)
cum.vol=rep(0,n)
for (i in 1:n)
{
	k=min(c(m,i))
	cum.vol[i]=sum(volume[(i-k+1):i])
}
cum.vol.p=rep(0,n)
for (i in 1:n)
{
	k=min(c(m,i))
	cum.vol.p[i]=sum(vol_p[(i-k+1):i])
}
buysignal=(C.day<maresult[,2])&(cum.vol<cum.vol.p)
sellsignal=(C.day>maresult[,2])|(cum.vol>cum.vol.p)
tradeindicator=rep(0,length(volume))
tradeindicator[buysignal]=1
tradeindicator[sellsignal]=-1
value=signal2value(C.day,tradeindicator)
finalvalue=value[dim(value)[1],4]
plot(value[-c(1:46),4],type="l")
###price,volume and volatility information
volume=log(as.numeric(TY$X.Volume))
vol_p=expsmooth(volume)[,2]
maresult=computeMA(C,L)
m=20
n=length(volume)
cum.vol=rep(0,n)
for (i in 1:n)
{
	k=min(c(m,i))
	cum.vol[i]=sum(volume[(i-k+1):i])
}
cum.vol.p=rep(0,n)
for (i in 1:n)
{
	k=min(c(m,i))
	cum.vol.p[i]=sum(vol_p[(i-k+1):i])
}
Val_mi_p=c(rep(NA,22),Val_micro_p)
buysignal=((C<maresult[,2])&(cum.vol<cum.vol.p))&(Val_micro<Val_mi_p)
sellsignal=((C>maresult[,2])|(cum.vol>cum.vol.p))|(Val_micro>Val_mi_p)
tradeindicator=rep(0,length(Val_micro))
tradeindicator[buysignal]=1
tradeindicator[sellsignal]=-1
n=length(Val_micro)
value=signal2value(C,tradeindicator)
finalvalue=value[dim(value)[1],4]
plot(value[-c(1:40),4],type="l")
###daily data
volume=log(Vol)
vol_p=expsmooth(volume)[,2]
m=20
n=length(volume)
cum.vol=rep(0,n)
for (i in 1:n)
{
	k=min(c(m,i))
	cum.vol[i]=sum(volume[(i-k+1):i])
}
cum.vol.p=rep(0,n)
for (i in 1:n)
{
	k=min(c(m,i))
	cum.vol.p[i]=sum(vol_p[(i-k+1):i])
}


maresult=computeMA(C.day,L)
Val_ma_p=c(rep(NA,22),Val_macro_p)
buysignal=(C.day<maresult[,2])&(Val_macro<Val_ma_p)&(cum.vol<cum.vol.p)
sellsignal=(C.day>maresult[,2])|(Val_macro>Val_ma_p)|(cum.vol>cum.vol.p)
tradeindicator=rep(0,length(Val_macro))
tradeindicator[buysignal]=1
tradeindicator[sellsignal]=-1
n=length(Val_macro)
value=signal2value(C.day,tradeindicator)
finalvalue=value[dim(value)[1],4]
plot(value[-c(1:46),4],type="l")