#assignment 2
#Amy Ly
#***************************************************************************
library(AER)
library(stargazer)

#Question 1
as2_crime=read.csv(file="as2_crime.csv")
summary(as2_crime)
#rescale variables
as2_crime$robberyrate_rescale=as2_crime$robbery_rate/100
as2_crime$burglary_rescale=as2_crime$burglary_rate/100
as2_crime$assaultrate_rescale=as2_crime$assault_rate/100
as2_crime$income_rescale=as2_crime$income/1000

#Question2
#scatterplot
#robbery_rate vs black



pdf("as2_robbery_black1.pdf")
plot(as2_crime$black,as2_crime$robbery_rate,
     main = "Number of Robberies and Percentage of African American 
     across the states",
     xlab = "Percentage of African American",
     ylab = "Number of robberies per 100,000 people",
     col="pink",
     pch=16)
abline(reg1, col="#ca3767", lwd=2)
dev.off()


#robberyrate_rescale vs income_rescale
reg2=lm(robbery_rate~income_rescale,data=as2_crime)
summary(reg2)
coeftest(reg2, vcov = vcovHC(reg1,"HC1" ))
summary(reg2)$adj.r.squared

pdf("as2_robberyraterescale_income.pdf")
plot(as2_crime$income_rescale,as2_crime$robbery_rate,
     main = "Number of Robberies and Average household income (in 000s)
     across the states",
     xlab = "Average household income (in 000s)",
     ylab = "Number of robberies per 100,000 people",
     col="#93dfb8",
     pch=16)
abline(reg2, col="#158078", lwd=2)
dev.off()


#black vs income
reg3=lm(black~income_rescale,data=as2_crime)
summary(reg3)
coeftest(reg3, vcov = vcovHC(reg1,"HC1" ))
summary(reg3)$adj.r.squared


pdf("as2_black_income.pdf")
plot(as2_crime$income_rescale,as2_crime$black,
main = "Average household income and Percentage of African America 
  in the population across the states",
xlab = "Average household income (in 000s)",
ylab = "Percentage of African American",
col="#cda4de",
pch=16)
abline(reg3, col="#8e4585", lwd=2)
dev.off()

#Question 3


#Question 4 
#construct dummy variables for all years in the dataset 2000-2010
as2_crime$d2000=as.numeric(as2_crime$year==2000)
as2_crime$d2001=as.numeric(as2_crime$year==2001)
as2_crime$d2002=as.numeric(as2_crime$year==2002)
as2_crime$d2003=as.numeric(as2_crime$year==2003)
as2_crime$d2004=as.numeric(as2_crime$year==2004)
as2_crime$d2005=as.numeric(as2_crime$year==2005)
as2_crime$d2006=as.numeric(as2_crime$year==2006)
as2_crime$d2007=as.numeric(as2_crime$year==2007)
as2_crime$d2008=as.numeric(as2_crime$year==2008)
as2_crime$d2009=as.numeric(as2_crime$year==2009)
as2_crime$d2010=as.numeric(as2_crime$year==2010)


regdummy=lm(robbery_rate~d2000+d2001+d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010,data=as2_crime)
summary(regdummy)
coeftest(regdummy, vcov = vcovHC(regdummy,"HC1" ))
summary(regdummy)$adj.r.squared

#Question 5
# Single linear regression of robbery_rate on black
reg4=lm(robbery_rate~black,data=as2_crime)
cov1=vcovHC(reg4, type = "HC1")                        
se1=sqrt(diag(cov1))

#Controlling for income
reg5=lm(robbery_rate~black+income_rescale,data=as2_crime)
cov2=vcovHC(reg5, type = "HC1")                        
se2=sqrt(diag(cov2))

#Controlling for income, age 
reg6=lm(robbery_rate~black+income_rescale+age,data=as2_crime)
cov3=vcovHC(reg6, type = "HC1")                        
se3=sqrt(diag(cov3))

#Controlling for income, age, female 
reg7=lm(robbery_rate~black+income_rescale+age+female,data=as2_crime)
cov4=vcovHC(reg7, type = "HC1")                        
se4=sqrt(diag(cov4))

#Controlling for income, age, female, years
reg8=lm(robbery_rate~black+income+age+female+d2001+d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010,data=as2_crime) 
cov5=vcovHC(reg8, type = "HC1")                        
se5=sqrt(diag(cov5))
coeftest(reg8, vcov = vcovHC(reg8,"HC1" ))

reg8=lm(robbery_rate~black+income_rescale+age+female+d2001+d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010,data=as2_crime) 
cov5=vcovHC(reg8, type = "HC1")                        
se5=sqrt(diag(cov5))




#regression output table for the 5 regressions reg4, reg5, reg6, reg7, reg8
stargazer(reg4, reg5, reg6, reg7, reg8, type="text",
          se=list(se1, se2, se3, se4, se5),
          digits=2, 
          dep.var.labels=c("Number of Robberies Per 100,000 People"), 
          covariate.labels=
            c("Percentage of Population that is African American",
              "Average Household Income (in 000s)",
              "Average Individual Age (Years)",
              "Percentage of Population that is Female",
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
          out="Q5_output.txt") 

#Question 7 
#Robbery_rate regression controlling on income, age, female, year
reg9=lm(robbery_rate~black+income_rescale+age+female+d2001+d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010,data=as2_crime) 
cov6=vcovHC(reg9, type = "HC1")                        
se6=sqrt(diag(cov6))

#Assault_rate regression controlling on income, age, female, year
reg10=lm(assault_rate~black+income_rescale+age+female+d2001+d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010,data=as2_crime) 
cov7=vcovHC(reg10, type = "HC1")                        
se7=sqrt(diag(cov7))

#Burglary_rate regression controlling on income, age, female, year
reg11=lm(burglary_rate~black+income_rescale+age+female+d2001+d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010,data=as2_crime) 
cov8=vcovHC(reg11, type = "HC1")                        
se8=sqrt(diag(cov8))

#Constructing regression table for robbery_rate, assault_rate, burglary_rate
stargazer(reg9, reg10, reg11, type="text",
          se=list(se6, se7, se8),
          digits=2, 
          dep.var.labels=c("Robbery Rate Per 100,000 People","Assault Rate Per 100,000 People","Burglary Rate Per 100,000 People"), 
          covariate.labels=
            c("Percentage of population that is black",
              "Average household income (in 000s)",
              "Average individual age",
              "Percentage of population that is female",
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
          out="Q7_output.txt") 


#Question 7 
#Robbery_rate regression controlling on income, age, female, year
reg9=lm(robbery_rate~black+income_rescale+age+female+d2001+d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010,data=as2_crime) 
cov6=vcovHC(reg9, type = "HC1")                        
se6=sqrt(diag(cov6))

#Assault_rate regression controlling on income, age, female, year
reg10=lm(assault_rate~black+income_rescale+age+female+d2001+d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010,data=as2_crime) 
cov7=vcovHC(reg10, type = "HC1")                        
se7=sqrt(diag(cov7))

#Burglary_rate regression controlling on income, age, female, year
reg11=lm(burglary_rate~black+income_rescale+age+female+d2001+d2002+d2003+d2004+d2005+d2006+d2007+d2008+d2009+d2010,data=as2_crime) 
cov8=vcovHC(reg11, type = "HC1")                        
se8=sqrt(diag(cov8))

#Constructing regression table for robbery_rate, assault_rate, burglary_rate
stargazer(reg9, reg10, reg11, type="text",
          se=list(se6, se7, se8),
          digits=2, 
          dep.var.labels=c("Robbery Rate Per 100,000 People","Assault Rate Per 100,000 People","Burglary Rate Per 100,000 People"), 
          covariate.labels=
            c("Percentage of population that is black",
              "Average household income (in 000s)",
              "Average individual age",
              "Percentage of population that is female",
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
          out="Q7_output.txt") 


#Question 8
linearHypothesis(reg9,c("black=0", "income_rescale=0", "age=0", "female=0", 
                         "d2001=0", "d2002=0", "d2003=0", "d2004=0",
                         "d2005=0", "d2006=0", "d2007=0", "d2008=0" ,"d2009=0", "d2010=0"), vcov = vcovHC(reg1a, "HC1"))

linearHypothesis(reg10,c("black=0", "income_rescale=0", "age=0", "female=0", 
                         "d2001=0", "d2002=0", "d2003=0", "d2004=0",
                         "d2005=0", "d2006=0", "d2007=0", "d2008=0" ,
                         "d2009=0", "d2010=0"), vcov = vcovHC(reg2a, "HC1"))

linearHypothesis(reg11,c("black=0", "income_rescale=0", "age=0", "female=0", 
                         "d2001=0", "d2002=0", "d2003=0", "d2004=0",
                         "d2005=0", "d2006=0", "d2007=0", "d2008=0" ,
                         "d2009=0", "d2010=0"), vcov = vcovHC(reg3a, "HC1"))

