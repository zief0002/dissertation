# Load libraries
library(car)
library(foreign)
library(geepack)
library(gee)
library(lattice)
library(nlme)


#Read in Dissertation Data
test = read.spss("./data/FinalDissertationData.sav", use.value.labels=TRUE,to.data.frame=TRUE)
long = reshape(test, idvar=names(test)[1], varying=list(names(test)[4:7],names(test)[8:11]), direction="long")

long$linear=recode(long$time, "1=0;2=13;3=23;4=28")
long$quadratic=long$linear^2

levels(long$TEACHER)=c("A","B")
levels(long$INTERACT)=c("BA","BB","IA","IB")


thesis = groupedData(B1~linear+quadratic|ID, data=long)

# Read Long Data
long=read.spss("./data/FinalDissertationLong.sav", use.value.labels=TRUE,to.data.frame=TRUE)

long$linear=recode(long$INDEX1, "1=0;2=13;3=23;4=28")
long$quadratic=long$linear^2

levels(long$TEACHER)=c("A","B")
levels(long$INTERACT)=c("BA","BB","IA","IB")

thesis = groupedData(BIV~linear+quadratic|ID, data=long)

#Exploratory Plots

trellis.device(color=F)
xyplot(B1 ~ linear | SEQUENCE, groups=ID, long, type 
       ="l", aspect = 'xy', col="black", xlim=(seq(0,30,5)), 
       xlab="Day", ylab="Score on Bivariate Reasoning 
     Assessment", main="Figure 4.1", sub="Spaghetti Plots with Loess Smoother by Treatment", between=list(x=1), panel = function(...){
       panel.superpose.2(..., lty="dotted")
       panel.loess(..., lty="solid", lwd=2)})

xyplot(D1 ~ linear | SEQUENCE, groups=ID, long, type 
       ="l", aspect = 'xy', col="black", xlim=(seq(0,30,5)), 
       xlab="Day", ylab="Score on Distributional Reasoning 
     Assessment", main="Figure 4.1", sub="Spaghetti Plots with Loess Smoother by Treatment", between=list(x=1), panel = function(...){
       panel.superpose.2(..., lty="dotted")
       panel.loess(..., lty="solid", lwd=2)})

# Get Exploratory Spaghetti Plots for the 4 Treatments
trellis.device(color=F)

xyplot(B1 ~ time | TEACHER * SEQUENCE, groups=ID, long, type ="l", aspect = 'xy', col="black", xlim=(seq(1,5,1)), xlab="Assessment Period", ylab="Score on Bivariate Reasoning Assessment", main="Spaghetti Plots with Loess Smoother by Treatment", panel = function(...){
  panel.superpose.2(..., lty="dotted")
  panel.loess(..., lty="solid", lwd=2)})


#Obtain Means/Correlations for the 4 Assessment Periods
Bivariate=subset(test, select=B1:B4)
mean(Bivariate, na.rm=T)
cor(Bivariate, use="pairwise")

#Obtain Conditional Means for the 4 Assessment Periods
by(test[,4:7], test$TEACHER, summary, na.rm=T)
by(test[,4:7], test$TEACHER, sd, na.rm=T)
by(test[,4:7], test$SEQUENCE, summary, na.rm=T)
by(test[,4:7], test$SEQUENCE, sd, na.rm=T)
by(test[,4:7], test$INTERACT, summary, na.rm=T)
by(test[,4:7], test$INTERACT, sd, na.rm=T)

#Obtain Conditional Means for Complete Cases)
Bivariate2=subset(test, complete.cases(test))
by(Bivariate2[,4:7], Bivariate2$SEQUENCE, mean)
by(Bivariate2[,4:7], Bivariate2$SEQUENCE, sd)
by(Bivariate2[,4:7], Bivariate2$TEACHER, mean)
by(Bivariate2[,4:7], Bivariate2$TEACHER, sd)
by(Bivariate2[,4:7], Bivariate2$INTERACT, mean)
by(Bivariate2[,4:7], Bivariate2$INTERACT, sd)

model3=lme(h1~timeint+time2, test, random=~timeint|ID, correlation=corGaus(form=~timeint|ID, nugget=T), method="ML")


#Obtain Plot of Mean Distribution and Bivariate Score by Wave
All=subset(test, select=B1:D4)
means=mean(All, na.rm=T)
d=data.frame(means)
Biv=d[1:4,]
Dist=d[5:8,]
l=cbind(Biv,Dist)
d=data.frame(l)
d$wave=c(1,2,3,4)

plot(d$wave,d$Dist,type="b",pch="D", lty="dotted", lwd=1, ylim=c(0,8), xlim=c(0,5), xlab="Wave", ylab="Mean Score")
par(new=T)
plot(d$wave,d$Biv,type="b",pch="B", lty="dashed",lwd="2", ylim=c(0,8), xlim=c(0,5), axes=FALSE, ann=FALSE)
legend("top",inset=.05,legend=c("Bivariate Reasoning","Distributional Reasoning"),lty=c("dashed","dotted"),lwd=c("2","1"))





#Examine the Predictors
Predictor=subset(test, select=ID:ALGTEST)
Predictor$Interact=test$INTERACT

#Time-Varying Covariate
means=by(Predictor[,4:11], Predictor$SEQUENCE, mean, na.rm=T)
bb=c(0.9444444, 3.8800000, 5.0697674, 4.6086957)
bd=c(0.5000000, 4.2200000, 4.5116279, 4.3260870)
ib=c(0.6964286, 4.0877193, 4.6111111, 5.0200000)
id=c(1.2678571, 4.3684211, 4.3703704, 4.1600000)
l=cbind(bb,bd,ib,id)
d=data.frame(l)
d$wave=c(1,2,3,4)

par(mfrow=c(1,2))
plot(d$wave,d$bd,type="b",pch="D", lty="dotted", lwd=1, ylim=c(0,8), xlim=c(0,5), xlab="Wave", ylab="Mean Score", main="Sequence: Bivariate")
par(new=T)
plot(d$wave,d$bb,type="b",pch="B", lty="dashed",lwd="2", ylim=c(0,8), xlim=c(0,5), axes=FALSE, ann=FALSE)
legend("top",inset=.05,legend=c("Bivariate Reasoning","Distributional Reasoning"),lty=c("dashed","dotted"),lwd=c("2","1"))

plot(d$wave,d$id,type="b",pch="D", lty="dotted", lwd=1, ylim=c(0,8), xlim=c(0,5), xlab="Wave", ylab="Mean Score", main="Sequence: Inference")
par(new=T)
plot(d$wave,d$ib,type="b",pch="B", lty="dashed",lwd="2", ylim=c(0,8), xlim=c(0,5), axes=FALSE, ann=FALSE)
legend("top",inset=.05,legend=c("Bivariate Reasoning","Distributional Reasoning"),lty=c("dashed","dotted"),lwd=c("2","1"))


#Histograms of Predictors by Factors
attach(Predictor)
library(lattice)
trellis.device(color=F)
histogram(~CAOS|TEACHER, col="grey", xlim=c(0,25), xlab="Score on CAOS", main="Histograms by Teacher")
histogram(~CAOS|SEQUENCE, col="grey", xlim=c(0,25), xlab="Score on CAOS", main="Histograms by SEQUENCE")
histogram(~CAOS|Interact, col="grey", xlim=c(0,25), xlab="Score on CAOS", main="Histograms by Teacher and SEQUENCE")


#Summary Statistics of Predictors by Factors
by(Predictor[,12:13], Predictor$TEACHER, summary, na.rm=T)

#Boxplots of Predictors by Factors
bwplot(~CAOS|Interact, xlim=c(-1,25), xlab="Score on CAOS", main="Boxplots by Teacher and Sequence",layout=c(1,4), aspect="xy")
bwplot(~CAOS|SEQUENCE, xlim=c(-1,25), xlab="Score on CAOS", main="Boxplots by Sequence",layout=c(1,2), aspect="xy")
bwplot(~CAOS|TEACHER, xlim=c(-1,25), xlab="Score on CAOS", main="Boxplots by Teacher",layout=c(1,2), aspect="xy")

bwplot(~ALGTEST|Interact, xlim=c(0,13), xlab="Score on Algebra Test", main="Boxplots by Teacher and Sequence",layout=c(1,4), aspect="xy")
bwplot(~ALGTEST|TEACHER, xlim=c(0,13), xlab="Score on Algebra Test", main="Boxplots by Teacher",layout=c(1,2), aspect="xy")
bwplot(~ALGTEST|SEQUENCE, xlim=c(0,13), xlab="Score on Algebra Test", main="Boxplots by Sequence",layout=c(1,2), aspect="xy")


#Finding Correlation between Predictors
cor(CAOS,ALGTEST, use="pairwise")
cor(CAOS,ALGTEST, use="pairwise", method="spearman")


#Create Grouped Data Object
bivGrp=groupedData(B1~day|TEACHER/ID, data=long)


#Get fitted model equations
fm1Thesis.lis=lmList(B1~linear+quadratic, thesis, na.action=na.omit)
print(fm1Thesis)
summary(fm1Thesis)
pairs(fm1Thesis.lis, id=0.01, adj=-.5)
plot(intervals(fm1Thesis.lis))

#Mixed Effect Model

model1=lme(B1~linear+quadratic, thesis, random=list(TEACHER=pdDiag(~linear+quadratic),ID=pdDiag(~linear+quadratic)), method="REML",na.action=na.omit)
summary(model1)

#Check Within-group residuals for normality
trellis.device(color=F)
qqnorm(model1)


#GEE Testing

test=read.spss("./data/dissertationTest.sav", use.value.labels=TRUE,to.data.frame=TRUE)
test=read.spss("./data/check.sav", use.value.labels=TRUE,to.data.frame=TRUE)

long=reshape(test, idvar=names(test)[1], varying=list(names(test)[2:5]), direction="long")
long$time=long$time-1

gee3=geese(TIME~time, id=ID, data=long, family=poisson, sca.link="log")

long2=long[order(long$ID,long$time),]
gee8=geese(TIME~time+GENDER+time*GENDER, data=long2, id=ID, family=poisson, corstr="ar1", sca.link="log", cor.link="fisherz")


#Exploratory Plots
xyplot(B1 ~ linear | SEQUENCE, groups=ID, long, type 
       ="l", aspect = 'xy', col="black", xlim=(seq(0,30,5)), 
       xlab="Day", ylab="Score on Bivariate Reasoning 
     Assessment", main="Figure 4.1", sub="Spaghetti Plots with Loess Smoother by Treatment", between=list(x=1), panel = function(...){
       panel.superpose.2(..., lty="dotted")
       panel.loess(..., lty="solid", lwd=2)})

xyplot(D1 ~ linear | SEQUENCE, groups=ID, long, type 
       ="l", aspect = 'xy', col="black", xlim=(seq(0,30,5)), 
       xlab="Day", ylab="Score on Distributional Reasoning 
     Assessment", main="Figure 4.1", sub="Spaghetti Plots with Loess Smoother by Treatment", between=list(x=1), panel = function(...){
       panel.superpose.2(..., lty="dotted")
       panel.loess(..., lty="solid", lwd=2)})


#Choose Random Sample
sample25 <- sort(sample(113, 25))
ThesisSamp=test[sample25,]
long2=reshape(ThesisSamp, idvar=names(test)[1],
              varying=list(names(test)[4:7]), direction="long")
levels(long2$TEACHER)=c("A","B")
long2$linear=recode(long2$time, "1=0;2=13;3=23;4=28")
long2$quadratic=long2$linear^2
thesis2=groupedData(B1~linear+quadratic|TEACHER/ID, data=long2)
fm2Thesis.lis=lmList(B1~linear+quadratic, thesis2, 
                     na.action=na.omit)
summary(fm2Thesis.lis)
pairs(fm2Thesis.lis, id=0.01, adj=-.5)
plot(intervals(fm2Thesis.lis), layout=c(3,1), between=list(x=1), sub="Ninety-five percent confidence intervals on intercept, linear slope and quadratic estimates for a random sample of 25 students", cex.sub=.3, font.sub=1, main="Figure 4.2")



modelx=lme(BIV~linear+quadratic, thesis2, random=~linear+quadratic-1|ID, method="ML", control=list(maxIter=200,msMaxIter=200,msVerbose=T))

qqnorm(modelx, ~ranef(.,level=1), id=0.10, layout=c(2,1), between=list(x=1))
plot(modelx, resid(.)~linear|SEQUENCE, layout=c(2,1), between=list(x=1), abline=0, grid=F)
plot(modelx, ID~resid(.) | SEQUENCE, layout= c(2,1), between=list(x=1), grid=F, abline=0 )
plot(modelx, resid(., type="p")~fitted(.) | SEQUENCE, id=0.05, layout=c(2,1), between=list(x=1))


mat <- matrix(c(thesis$B1[thesis$time==1], thesis$B1[thesis$time==2], 
thesis$B1[thesis$time==3], thesis$B1[thesis$time==4]), ncol=4)
var(mat)
cor(mat)


#Graphical Presentation of ANOVAs (CAOS)
xbar=tapply(test$CAOS, test$INTERACT, mean, na.rm=T)
s=tapply(test$CAOS, test$INTERACT, sd, na.rm=T)
n=tapply(test$CAOS, test$INTERACT, length)
sem=s/sqrt(n)
stripchart(test$CAOS~test$INTERACT, "jitter", jit=.05, pch=16, vert=T)
arrows(1:4, xbar+2*sem, 1:4, xbar-2*sem, angle=90, code=3, length=.1)
lines(1:4, xbar, pch=4, type="b", cex=2)
title(xlab="Section", ylab="Score on CAOS")
mtext("Figure 4.2", line=2, adj=0, font=4)
mtext(expression(paste("Stripcharts of the CAOS scores for each section with ", bar(x), " ± 2SEM")), line=1, adj=0, font=1)


xbar=tapply(test$CAOS, test$SEQUENCE, mean, na.rm=T)
s=tapply(test$CAOS, test$SEQUENCE, sd, na.rm=T)
n=tapply(test$CAOS, test$SEQUENCE, length)
sem=s/sqrt(n)
stripchart(test$CAOS~test$SEQUENCE, "jitter", jit=.05, pch=16, vert=T)
arrows(1:2, xbar+2*sem, 1:2, xbar-2*sem, angle=90, code=3, length=.1)
lines(1:2, xbar, pch=4, type="b", cex=2)


xbar=tapply(test$CAOS, test$TEACHER, mean, na.rm=T)
s=tapply(test$CAOS, test$TEACHER, sd, na.rm=T)
n=tapply(test$CAOS, test$TEACHER, length)
sem=s/sqrt(n)
stripchart(test$CAOS~test$TEACHER, "jitter", jit=.05, pch=16, vert=T)
arrows(1:2, xbar+2*sem, 1:2, xbar-2*sem, angle=90, code=3, length=.1)
lines(1:2, xbar, pch=4, type="b", cex=2)


bartlett.test(test$CAOS~test$INTERACT)
anova(lm(test$CAOS~test$TEACHER))
pairwise.t.test(test$CAOS, test$INTERACT, p.adj="bonferroni")



#Graphical Presentation of ANOVAs (ALGTEST)
xbar=tapply(test$ALGTEST, test$INTERACT, mean, na.rm=T)
s=tapply(test$ ALGTEST, test$INTERACT, sd, na.rm=T)
n=tapply(test$ ALGTEST, test$INTERACT, length)
sem=s/sqrt(n)
stripchart(test$ALGTEST ~test$INTERACT, "jitter", jit=.05, pch=16, vert=T)
arrows(1:4, xbar+2*sem, 1:4, xbar-2*sem, angle=90, code=3, length=.1)
lines(1:4, xbar, pch=4, type="b", cex=2)
title(xlab="Section", ylab="Score on Algebra Test")
mtext("Figure 4.1", line=2, adj=0, font=4)
mtext(expression(paste("Stripcharts of the Algebra Test scores for each section with ", bar(x), " ± 2SEM")), line=1, adj=0, font=1)



xbar=tapply(test$ALGTEST, test$SEQUENCE, mean, na.rm=T)
s=tapply(test$ ALGTEST, test$ SEQUENCE, sd, na.rm=T)
n=tapply(test$ ALGTEST, test$ SEQUENCE, length)
sem=s/sqrt(n)
stripchart(test$ ALGTEST ~test$TEACHER, "jitter", jit=.05, pch=16, vert=T)
arrows(1:2, xbar+2*sem, 1:2, xbar-2*sem, angle=90, code=3, length=.1)
lines(1:2, xbar, pch=4, type="b", cex=2)


xbar=tapply(test$ALGTEST, test$TEACHER, mean, na.rm=T)
s=tapply(test$ ALGTEST, test$TEACHER, sd, na.rm=T)
n=tapply(test$ ALGTEST, test$TEACHER, length)
sem=s/sqrt(n)
stripchart(test$ ALGTEST ~test$TEACHER, "jitter", jit=.05, pch=16, vert=T)
arrows(1:2, xbar+2*sem, 1:2, xbar-2*sem, angle=90, code=3, length=.1)
lines(1:2, xbar, pch=4, type="b", cex=2)

bartlett.test(test$ALGTEST~test$INTERACT)
anova(lm(test$ALGTEST~test$INTERACT))





#Checking Section Consistency
test=read.spss("./data/FinalDissertationTest.sav", use.value.labels=TRUE,to.data.frame=TRUE)

levels(test$TEACHER)=c("A","B")
levels(test$INTERACT)=c("BA","BB","IA","IB")

survey=read.spss("./data/DissertationFirstDaySurvey.sav", use.value.labels=TRUE,to.data.frame=TRUE)
survey$Section2=factor(survey$SECTION)



#Table 4.2
summary(survey[,3:9])
sd(survey[,3:9], na.rm=T)
cor(survey, use="pairwise.complete.obs", method="pearson")
cor(survey, use="pairwise.complete.obs", method="spearman")
summary(test[,12:14])
sd(test[,12:14], na.rm=T)
cor(test[,12:14], use="pairwise.complete.obs", method="pearson")
cor(test[,12:14], use="pairwise.complete.obs", method="spearman")

#Table 4.1
by(survey[,3:9], survey$SEQUENCE, summary, na.rm=T)
by(survey[,3:9], survey$SEQUENCE, sd, na.rm=T)

#Table 4.3
bartlett.test(survey$AGE~ survey$SEQUENCE)
bartlett.test(survey$SEM_CRED~ survey$SEQUENCE)
bartlett.test(survey$CUM_CRED~ survey$SEQUENCE)
bartlett.test(survey$CUM_GPA~ survey$SEQUENCE)

t.test(survey$AGE~ survey$SEQUENCE, var.equal=T)
t.test(survey$CUM_GPA~ survey$SEQUENCE, var.equal=T)
t.test(survey$CUM_CRED~ survey$SEQUENCE)
t.test(survey$SEM_CRED~ survey$SEQUENCE)

#Table 4.6
bartlett.test(test$CAOS~test$SEQUENCE)
bartlett.test(test$ALGTEST~test$SEQUENCE)
bartlett.test(test$ACT~test$SEQUENCE)
t.test(test$CAOS~test$SEQUENCE, var.equal=T)
t.test(test$ALGTEST~test$SEQUENCE, var.equal=T)
t.test(test$ACT~test$SEQUENCE, var.equal=T)



#Table 4.7
summary(test[,4:7])
sd(test[,4:7], na.rm=T)
cor(test[,4:7], use="pairwise.complete.obs", method="pearson")
cor(test[,4:7], use="pairwise.complete.obs", method="spearman")

bartlett.test(survey$CUM_GPA~survey$Section2)
oneway.test(survey$CUM_GPA~survey$Section2)

math=read.spss("./data/Day1Covariates/MathSurvey.sav", use.value.labels=TRUE,to.data.frame=TRUE)
chisq.test(math$B, math$SECTION, simulate.p.value=T, B=10000)


#Testing Covariates
model.int=lme(BIV~linear+quadratic+SEQ_RECO+TEACH_RE+linear:SEQ_RECO+quadratic:SEQ_RECO+linear:TEACH_RE+quadratic:TEACH_RE+linear:SEQ_RECO:TEACH_RE+quadratic:SEQ_RECO:TEACH_RE, random=~linear+quadratic-1|ID, data=thesis, method="REML", control=list(maxIter=10000,msMaxIter=10000,niterEM=50,msVerbose=T))

model.main2=lmer(BIV~linear+quadratic+SEQ_RECO+TEACH_RE +(linear+quadratic-1|ID), data=thesis, method="REML", control=list(maxIter=10000,msMaxIter=10000,niterEM=50,msVerbose=T))


#Figure 4.1
xyplot(B1 ~ linear, groups=ID, long2, type="l", aspect = 'xy', col="black", xlim=(seq(0,30,5)), xlab="Day", ylab="Score on Bivariate Reasoning Assessment", main="\n\n", panel = function(...){
panel.superpose.2(..., lty="dotted")
panel.loess(..., lty="solid", lwd=2)})
new=F
mtext("Figure 4.1", line=2, adj=0, font=1)
mtext("Spaghetti plot of bivariate reasoning scores with a\nloess smoother", line=0, adj=0, font=3)

#Figure 4.2
plot(intervals(fm2Thesis.lis), layout=c(3,1), between=list(x=1), main="\n\n")
new=F
mtext("Figure 4.2", line=2, adj=-.1, font=2)
mtext("Ninety-five percent confidence intervals for the intercept, linear slope and quadratic rate of change", line=1, adj=3.2, font=3)
mtext("parameters for the randomly selected students", line=0, adj=-.18, font=3)

#[Old Figure 4.3]
plot(modelc, resid(., jit=0.05)~linear, grid=F,xlab="Day", main="\n")
new=F
mtext("Figure 4.3", line=2, adj=0, font=2)
mtext("Residual plot for the quadratic rate of change model", line=1, adj=0, font=3)

#Figure 4.3
plot(d$new,d$grow, type="l", xlab="Session", ylab="Score on BRA", xlim=c(0,30), ylim=c(0,8))
text(15,7, expression(hat(BRA)==0.89856+0.32046*(Session) -0.0064827*(Session^2)), pos=1)
arrows(5,6, .7, 1.5,code=2)
arrows(11,6,16,4.6,code=2)
arrows(21,6,17,4.7,code=2)
mtext("Figure 4.3", line=2, adj=0, font=2)
mtext("Predicted change in bivariate reasoning for an average student", line=1, adj=0, font=3)

#Figure 4.4 [460 x 375]
new=c(0:29)
d=data.frame(new)
d$grow=0.89856+0.32046*new-.0064827*new*new

d$grow2=.8564646+.1252870*0+.3229436*d$new-.0065801*d$new^2
d$grow4=.8564646+.1252870*-3+.3229436*d$new-.0065801*d$new^2
d$grow3=.8564646+.1252870*3+.3229436*d$new-.0065801*d$new^2

plot(d$new,d$grow2, type="l", xlab="Session", ylab="Score on BRA", xlim=c(0,30), ylim=c(0,8))
par(new=T)
plot(d$new,d$grow3, type="l", xlim=c(0,30), ylim=c(0,8), xlab="", ylab="")
par(new=T)
plot(d$new,d$grow4, type="l", xlim=c(0,30), ylim=c(0,8), xlab="", ylab="")
text(0,6, "Average change" , pos=4)
text(0.5,5.5, "in DRS score" , pos=4)
arrows(3.5,5.3, 3.5, 2.0,code=2)
text(18,8, "Above average change" , pos=4)
text(20,7.5, "in DRS score" , pos=4)
arrows(24,7.3, 24, 5.3,code=2)
text(13,1.5, "Below average change" , pos=4)
text(15,1, "in DRS score" , pos=4)
arrows(18,1.7, 18, 4.0,code=2)
mtext("Figure 4.4", line=2.5, adj=0, font=2)
mtext("Predicted average change in bivariate reasoning for students with small, ", line=1.5, adj=0, font=3)


#Figure 4.5 [840 x 640]
e$grow1=.8564646+.069379*-.21481*-1+.32444*e$new+.016207*-1*e$new-.0079945*e$new^2+.00020698*e$new^2*0
e$grow2=.8564646+.069379*0-.21481*1+.32444*e$new+.016207*1*e$new-.0079945*e$new^2+.00020698*e$new^2*0
e$grow3=.8564646+.069379*6.807-.21481*1+.32444*e$new+.016207*1*e$new-.0079945*e$new^2+.00020698*e$new^2*6.807
e$grow4=.8564646+.069379*6.807-.21481*-1+.32444*e$new+.016207*-1*e$new-.0079945*e$new^2+.00020698*e$new^2*6.807
e$grow5=.8564646+.069379*12-.21481*-1+.32444*e$new+.016207*-1*e$new-.0079945*e$new^2+.00020698*e$new^2*12
 e$grow6=.8564646+.069379*12-.21481*1+.32444*e$new+.016207*1*e$new-.0079945*e$new^2+.00020698*e$new^2*12

par(mfrow=c(2,2))
plot(e$new,e$grow1, type="l", xlab="Session", ylab="Score on BRA", xlim=c(0,30), ylim=c(0,8))
par(new=T)
plot(e$new,e$grow2, type="l",lty="dotted", xlim=c(0,30), ylim=c(0,8), xlab="", ylab="")
legend("top",inset=.05,legend=c("Teacher A","Teacher B"),lty=c("solid","dotted"), title="Below Average Change in DRS Score")
plot(e$new,e$grow4, type="l", xlab="Session", ylab="Score on BRA", xlim=c(0,30), ylim=c(0,8))
par(new=T)
plot(e$new,e$grow3, type="l",lty="dotted", xlim=c(0,30), ylim=c(0,8), xlab="", ylab="")
legend("top",inset=.05,legend=c("Teacher A","Teacher B"),lty=c("solid","dotted"), title="Average Change in DRS Score")
plot(e$new,e$grow5, type="l", xlab="Session", ylab="Score on BRA", xlim=c(0,30), ylim=c(0,8))
par(new=T)
plot(e$new,e$grow6, type="l",lty="dotted", xlim=c(0,30), ylim=c(0,8), xlab="", ylab="")
legend("top",inset=.05,legend=c("Teacher A","Teacher B"),lty=c("solid","dotted"), title="Above Average Change in DRS Score")
mtext("Figure 4.5", line=34, adj=0, font=2)
mtext("Predicted change in bivariate reasoning by teacher for students with below average,  average and above average changes in DRS score", line=32.5, adj=0, font=3)
