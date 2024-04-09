library(wesanderson)
library(AER)
library(stargazer)
library(ggplot2)

mydata1=read.csv(file="as3_crime.csv")
mydata1$rob2=mydata1$robbery_rate*mydata1$robbery_rate
mydata1$rob3=mydata1$robbery_rate*mydata1$robbery_rate*mydata1$robbery_rate

#Question 1

pdf("meo2.pdf")
ggplot(mydata1,aes(y=rob2, x=black)) +
  geom_point(alpha = .3, color = "#014d64") +
  stat_smooth(method = "lm", formula = y ~ poly(x,2), color = "#00887d") + 
  ggtitle("Relationship Between Robbery rate and Black") +  
  theme(plot.title = element_text(hjust = 0.5)) +      
  scale_x_continuous(name="Black") +
  scale_y_continuous(name="Robbery rate")
dev.off()

pdf("meo3.pdf")
ggplot(mydata1,aes(y=robbery_rate, x=black)) +
  geom_point(alpha = .3, color = "#E6A0C4") +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), color = "#C6CDF7") + 
  ggtitle("Relationship Between Robbery rate and Black") +  
  theme(plot.title = element_text(hjust = 0.5)) +      
  scale_x_continuous(name="Black") +
  scale_y_continuous(name="Robbery rate")
dev.off()

#Question 2
mydata1$income_scale=mydata1$income/10000
mydata1$black_sq=mydata1$black*mydata1$black
mydata1$black_cu=mydata1$black*mydata1$black*mydata1$black
mydata1$d2000=as.numeric(mydata1$year==2000)
mydata1$d2001=as.numeric(mydata1$year==2001)
mydata1$d2002=as.numeric(mydata1$year==2002)
mydata1$d2003=as.numeric(mydata1$year==2003)
mydata1$d2004=as.numeric(mydata1$year==2004)
mydata1$d2005=as.numeric(mydata1$year==2005)
mydata1$d2006=as.numeric(mydata1$year==2006)
mydata1$d2007=as.numeric(mydata1$year==2007)
mydata1$d2008=as.numeric(mydata1$year==2008)
mydata1$d2009=as.numeric(mydata1$year==2009)
mydata1$d2010=as.numeric(mydata1$year==2010)


reg1=lm(robbery_rate~black+black_sq+black_cu+income_scale+age+female+d2001+
        d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010,data=mydata1)
cov1=vcovHC(reg1, type = "HC1")    
se1=sqrt(diag(cov1))

reg2=lm(robbery_rate~black+black_sq+income_scale+age+female+d2001+
          d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010,data=mydata1)
cov2=vcovHC(reg2, type = "HC1")    
se2=sqrt(diag(cov2))

reg3=lm(robbery_rate~black+income_scale+age+female+d2001+
          d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010,data=mydata1)
cov3=vcovHC(reg3, type = "HC1")    
se3=sqrt(diag(cov3))


stargazer(reg1, reg2, reg3, type="text",
          se=list(se1, se2, se3),
          digits=2, 
          dep.var.labels=c("Robbery Rate"),
          covariate.labels=
            c("Black",
              "Black Squared",
              "Black Cube",
              "Income (in 000s)",
              "Age",
              "Female",
              "d2001",
              "d2002",
              "d2003",
              "d2004",
              "d2005",
              "d2006",
              "d2007",
              "d2008",
              "d2009",
              "d2010",
              "Constant"),
          out="reg_q2_output.txt")
              
#Question 3
#a)
newdata1=data.frame(black=0.05, black_sq=0.0025, black_cu=0.000125, 
                    income_scale=1, age=20, female=0.5, 
                    d2001=0, d2002=0, d2003=0, d2004=0, d2005=0,
                    d2006=0, d2007=0, d2008=0, d2009=0, d2010=1)

newdata2=data.frame(black=0.10, black_sq=0.01, black_cu=0.001, 
                    income_scale=1, age=20, female=0.5, 
                    d2001=0, d2002=0, d2003=0, d2004=0, d2005=0,
                    d2006=0, d2007=0, d2008=0, d2009=0, d2010=1)

robb1=predict(reg1, newdata = newdata1)          
robb2=predict(reg1, newdata = newdata2) 

drobb=robb2-robb1

Ftest=linearHypothesis(reg1,c("0.05*black+0.0075*black_sq+0.000875*black_cu=0")
                       ,vcov = vcovHC(reg1, "HC1"))
Fstat=Ftest[2,3]
se_drobb=abs(drobb)/sqrt(Fstat)

drobb_ci95L=drobb-se_drobb*1.96
drobb_ci95H=drobb+se_drobb*1.96
drobb_ciwidth=drobb_ci95H-drobb_ci95L

## Outputting results
sprintf("partial effect: %f", drobb)
sprintf("SE of partial effect: %f", se_drobb)
sprintf("95 CI lower bound for partial effect: %f", drobb_ci95L)
sprintf("95 CI upper bound for partial effect: %f", drobb_ci95H)
sprintf("Width of 95 CI for partial effect: %f", drobb_ciwidth)

#b)
newdata3=data.frame(black=0.15, black_sq=0.0225, black_cu=0.003375, 
                    income_scale=1, age=20, female=0.5, 
                    d2001=0, d2002=0, d2003=0, d2004=0, d2005=0,
                    d2006=0, d2007=0, d2008=0, d2009=0, d2010=1)



robb3=predict(reg1, newdata = newdata3)          


drobb1=robb3-robb2

Ftest1=linearHypothesis(reg1,c("0.05*black+0.0125*black_sq+0.002375*black_cu=0"),
                        vcov = vcovHC(reg1, "HC1"))
Fstat1=Ftest1[2,3]
se_drobb1=abs(drobb1)/sqrt(Fstat1)

drobb1_ci95L=drobb1-se_drobb1*1.96
drobb1_ci95H=drobb1+se_drobb1*1.96
drobb1_ciwidth=drobb1_ci95H-drobb1_ci95L

## Outputting results
sprintf("partial effect: %f", drobb1)
sprintf("SE of partial effect: %f", se_drobb1)
sprintf("95 CI lower bound for partial effect: %f", drobb1_ci95L)
sprintf("95 CI upper bound for partial effect: %f", drobb1_ci95H)
sprintf("Width of 95 CI for partial effect: %f", drobb1_ciwidth)

#Question 4
#Logarithmic variables
mydata1$log_robbery_rate = log(mydata1$robbery_rate)
mydata1$log_black = log(mydata1$black)

#Dummy variables
mydata1$start=1*(mydata1$year<=2003)
mydata1$middle=1*(mydata1$year>=2004&mydata1$year<=2007)
mydata1$end=1*(mydata1$year>=2008)

#Interactions variables
mydata1$log_black_income = mydata1$log_black*mydata1$income_scale
mydata1$log_black_start = mydata1$log_black*mydata1$start
mydata1$log_black_middle = mydata1$log_black*mydata1$middle
mydata1$log_black_end = mydata1$log_black*mydata1$end

#Regressions
reg1.4=lm(log_robbery_rate~black+income_scale+age+female+d2001+d2002+d2003+d2004+
          d2005+d2006+d2007+d2008+d2009+d2010, data=mydata1)
cov1.4=vcovHC(reg1.4, type = "HC1")    
se1.4=sqrt(diag(cov1.4))

reg2.4=lm(log_robbery_rate~log_black+income_scale+age+female+d2001+d2002+d2003+d2004+
            d2005+d2006+d2007+d2008+d2009+d2010, data=mydata1)
cov2.4=vcovHC(reg2.4, type = "HC1")    
se2.4=sqrt(diag(cov2.4))

reg3.4=lm(log_robbery_rate~log_black+log_black_middle+log_black_end+income_scale+age+
            female+d2001+d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+
            d2010, data=mydata1)
cov3.4=vcovHC(reg3.4, type = "HC1")    
se3.4=sqrt(diag(cov3.4))

reg4.4=lm(log_robbery_rate~log_black+log_black_income+income_scale+age+female+d2001+
          d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010, data=mydata1)
cov4.4=vcovHC(reg4.4, type = "HC1")    
se4.4=sqrt(diag(cov4.4))

## Polynomial regression output table
stargazer(reg1.4, reg2.4, reg3.4, reg4.4, type="text",
          se=list(se1.4, se2.4, se3.4, se4.4),
          align=TRUE, dep.var.labels=c("Log(Robbery Rate)"),
          covariate.labels=
            c("Share of Pop. that is Black",
              "Log of Share of Pop. that is Black",
              "Log of Share of Pop. that is Black x Years 2004-2007",   
              "Log of Share of Pop. that is Black x Years 2008-2010",    
              "Log of Share of Pop. that is Black x Avg. Household Inc.",              
              "Avg. Household Inc. (ten thousands)",
              "Avg. Age",
              "Share of Pop. that is Female",
              "2001",
              "2002",
              "2003",
              "2004",              
              "2005",
              "2006",
              "2007",
              "2008",
              "2009",
              "2010",
              "Constant"),
          out="q4_reg_output.txt")
            
#Question 7
q7=linearHypothesis(reg3.4,c("log_black_middle=log_black_end"),
                 vcov = vcovHC (reg3.4, "HC1"))
#Question 8
#B0 + B1 log(black) + B2 log(black)*income_scale
summary8=summary(reg4.4)
summarycf=summary8[["coefficients"]]

#Elasticity at income=3
b1=summarycf[2,1]
b2=summarycf[3,1]
E3=b1+3*b2

Ftest8.1=linearHypothesis(reg4.4, c("log_black+3*log_black_income=0"),
                          vcov = vcovHC (reg4.4, "HC1"))
Fstat8.1=Ftest8.1[2,3]
se_e3=E3/sqrt(Fstat8.1)

E3_95ci_l=E3-1.96*se_e3
E3_95ci_h=E3+1.96*se_e3
E3_95ci_w=E3_95ci_h-E3_95ci_l

#Result
sprintf("elasticity at income = 3: %f", E3)
sprintf("SE of elasticity at income = 3: %f", se_e3)
sprintf("95 CI lower bound E3: %f", E3_95ci_l)
sprintf("95 CI upper bound for E3: %f", E3_95ci_h)
sprintf("Width of 95 CI for E3: %f", E3_95ci_w)

#Elasticity at income=5
b1=summarycf[2,1]
b2=summarycf[3,1]
E5=b1+5*b2

Ftest8.2=linearHypothesis(reg4.4, c("log_black+5*log_black_income=0"),
                          vcov = vcovHC (reg4.4, "HC1"))      
Fstat8.2=Ftest8.2[2,3]
se_e5=E5/sqrt(Fstat8.2)

E5_95ci_l=E5-1.96*se_e5
E5_95ci_h=E5+1.96*se_e5
E5_95ci_w=E5_95ci_h-E5_95ci_l

#Result
sprintf("elasticity at income = 5: %f", E5)
sprintf("SE of elasticity at income = 5: %f", se_e5)
sprintf("95 CI lower bound E5: %f", E5_95ci_l)
sprintf("95 CI upper bound for E5: %f", E5_95ci_h)
sprintf("Width of 95 CI for E5: %f", E5_95ci_w)