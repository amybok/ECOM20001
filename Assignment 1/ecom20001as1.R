as1_obama$young=1*(as1_obama$share_under25>0.5)

#question1
summary(as1_obama)
sd(as1_obama$amount)
sd(as1_obama$young)
sd(as1_obama$share_under25)

#question2 - 95% confidence interval of amount
t.test(as1_obama$amount,mu=319.2)
amount_mu=mean(as1_obama$amount)
amount_nobs=length(as1_obama$amount)
amount_sd=sd(as1_obama$amount)
amount_se=amount_sd/sqrt(amount_nobs) 
amount_CI95_low=amount_mu-1.96*amount_se
amount_CI95_high=amount_mu+1.96*amount_se

paste("95% CI Lower Bound:",amount_CI95_low)
paste("95% CI Upper Bound:",amount_CI95_high)

#question2 - 95% confidence interval of share_under25
t.test(as1_obama$share_under25,mu=0.4734)
share_mu=mean(as1_obama$share_under25)
share_nobs=length(as1_obama$share_under25)
share_sd=sd(as1_obama$share_under25)
share_se=share_sd/sqrt(share_nobs) 
share_CI95_low=share_mu-1.96*share_se
share_CI95_high=share_mu+1.96*share_se

paste("95% CI Lower Bound:",share_CI95_low)
paste("95% CI Upper Bound:",share_CI95_high)

#question2 - 95% confidence interval of young
t.test(as1_obama$young,mu=0.3012)
young_mu=mean(as1_obama$young)
young_nobs=length(as1_obama$young)
young_sd=sd(as1_obama$young)
young_se=young_sd/sqrt(young_nobs) 
young_CI95_low=young_mu-1.96*young_se
young_CI95_high=young_mu+1.96*young_se

paste("95% CI Lower Bound:",young_CI95_low)
paste("95% CI Upper Bound:",young_CI95_high)

#question3 
pdf("bbb.pdf")
plot(density(as1_obama$amount[as1_obama$young==1]),main = "Amount donated to the Democratic Party in the United States during
the 2008 and 2012 elections and Youthful demographic areas",ylim=c(0, 0.0045),xlab="Amount donated",
     ylab="Density",col='darkslateblue')
lines(density(as1_obama$amount[as1_obama$young==0]),col='pink3')
legend(1000, 0.003, legend=c("Young 1", "Young 0"),
       col=c("darkslateblue", "pink3"), lty=1:1, cex=0.8,
        text.font=1, bg='white')
dev.off()

#question5
#create binary variable amount_zero
as1_obama$amount_zero=1*(as1_obama$amount==0)
summary(as1_obama$amount_zero)
#mean of amount_zero given young =1
mean(as1_obama$amount_zero[as1_obama$young==1])
#mean of amount_zero given young =0
mean(as1_obama$amount_zero[as1_obama$young==0])
#mean difference
mean(as1_obama$amount_zero[as1_obama$young==0])-mean(as1_obama$amount_zero[as1_obama$young==1])
#t-test
t.test(as1_obama$amount_zero[as1_obama$young==1],as1_obama$amount_zero[as1_obama$young==0]) 

#question6
pdf("abc.pdf")
plot(as1_obama$share_under25,as1_obama$amount,pch=20,col="cadetblue4")
dev.off()

#question7
share_reg=lm(amount~share_under25,data=as1_obama)
summary(share_reg)
sd(as1_obama$share_under25)

pdf("777.pdf")
plot(as1_obama$share_under25,as1_obama$amount,
     main="Relationship Between Youthful demographic areas 
     and Amount donated",
     xlab="Ratio of people under 25",
     ylab="Amount",
     col="cadetblue4",
     pch=20)
dev.off()
