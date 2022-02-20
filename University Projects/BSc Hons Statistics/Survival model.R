# here will attempt to fit a survival model to model the time to graduation


library(readxl)

stem_data=read_excel(file.choose())
str(stem_data)

# we start by fitting a non parametric model to our data to get an idea of what survival parametric model we should use

df_new<-stem_data%>%filter(status=="Graduated")
df_new2<-df_new%>%filter(MINREGYEARS>=0)
str(df_new)
View(df_new2)
surv_obj=Surv(df_new2$MINREGYEARS,df_new2$event==1)
surv_obj

# KM estimate of survival function

fit1=survfit(surv_obj~1)
fit1
plot(fit1)
fit1$time
fit1$n.risk
fit1$n.event
plot(fit1$time,-log(fit1$surv),ylab = "-log(S(T))",xlab = "time")
# from this plot we seem to have a straight line indicating an exponential model may be useful

plot(log(fit1$time),log(-log(fit1$surv)),xlab="log(Time)",ylab="log(-log(S(T)))")

# here we do not get a straight line indicating the a weibull model is not a good fit
plot(log(fit1$time),logit(fit1$surv))

# here the graph looks like an approximate straight line which suggests that loglogistic model may possibly work

# we shall try 3 different types of parametric models 

# loglogistic,exponential,peicewise exponential,coxph model
fit1$surv
# exponential model fit
df_new2$time=df_new2$MINREGYEARS+1
View(df_new2)
fit2=survreg(Surv(time,event)~1+GENDER+MATRICPOINTS+NSFASYN+PERCENTAGE+ENG+degree+QUINTILE,data=df_new2,dist = "exponential")
fit2
summary(fit2)

# loglogistic fit

fit3=survreg(Surv(time,event)~1+GENDER+MATRICPOINTS+NSFASYN+PERCENTAGE+ENG+degree+QUINTILE,data=df_new2,dist = "loglogistic")
fit3
summary(fit3)

# coxph model

# we will first fit the coxph model and then test the PH assumption

cox_model<-coxph(Surv(time,event)~GENDER+MATRICPOINTS+NSFASYN+PERCENTAGE+ENG+degree+QUINTILE,data = df_new2)
cox_model

# if the cox ph model fits the data then the PH assumption should be satisified

summary(cox_model)
cox.zph(cox_model)

# here we can see that the variables matricpoints and percentage violate the cox-ph assumption

