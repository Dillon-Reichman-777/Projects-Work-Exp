# here will attempt to fit a survival model to model the time to graduation


library(readxl)

stem_data=read_excel(file.choose())
str(stem_data)
d<-stem_data%>%filter(GENDER=="F")
q<-stem_data%>%filter(GENDER=="M")
# we start by fitting a non parametric model to our data to get an idea of what survival parametric model we should use

df_new<-stem_data
str(df_new)
str(df_new)
View(df_new2)
surv_obj=Surv(df_new$REGYEARSALL,df_new$event==1)
head(surv_obj)

df_new3<-df_new2%>%filter(GENDER=="F")
df_new4<-df_new3%>%filter(degree=="Data_Sci")
View(df_new4)
# KM estimate of survival function
str(df_new4)
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
fit2=survreg(Surv(REGYEARSALL,event)~1+GENDER+MATRICPOINTS+RACE+NSFASYN+PERCENTAGE+ENG+degree+QUINTILE,data=df_new,dist = "exponential")
fit2
summary(fit2)

# loglogistic fit

fit3=survreg(Surv(REGYEARSALL,event)~1+MATRICPOINTS+NSFASYN+PERCENTAGE+ENG+degree+RACE+QUINTILE,data=df_new,dist = "loglogistic")
fit3
summary(fit3)

# coxph model
# we will first fit the coxph model and then test the PH assumption
cox_model<-coxph(Surv(REGYEARSALL,event)~GENDER+degree+NSFASYN+MATRICPOINTS+PERCENTAGE+RACE+ENG+QUINTILE,data=df_new)
cox_model
View(df_new4)
# if the cox ph model fits the data then the PH assumption should be satisified

summary(cox_model)
cox.zph(cox_model)

# we stratify the variables which fail the ph assumption to solve the problem

cox_model2<-coxph(Surv(time,event)~GENDER+strata(degree)+NSFASYN+matric_aps+math_new+ENG+new_quintile,data=df_new2)
cox_model2
cox.zph(cox_model2)
df_new2$matric_aps<-df_new2$MATRICPOINTS

df_new$matric_aps[df_new$MATRICPOINTS<33]<-"bad"
df_new$matric_aps[df_new$MATRICPOINTS>33 & df_new$MATRICPOINTS<41]<-"good"
df_new$matric_aps[df_new$MATRICPOINTS>40 ]<-"very_good"

df_new$math_new<-df_new$PERCENTAGE
df_new$math_new[df_new$PERCENTAGE<=60]<-"bad"
df_new$math_new[df_new$PERCENTAGE>60 & df_new$PERCENTAGE<75]<-"good"
df_new$math_new[df_new$PERCENTAGE>=75]<-"very_good"
View(df_new)
df_new$new_quintile<-df_new$QUINTILE
df_new$new_quintile[df_new$QUINTILE==1]<-"lower"
df_new$new_quintile[df_new$QUINTILE==2]<-"lower"
df_new$new_quintile[df_new$QUINTILE==3]<-"lower"
df_new$new_quintile[df_new$QUINTILE==4]<-"upper"
df_new$new_quintile[df_new$QUINTILE==5]<-"upper"

# we try recoding continious variables to categorcal variables to see if we get a better cox ph model

# here we can see that the variables matricpoints and percentage violate the cox-ph assumption

# ALL HAS NOT WORKED WITH COXPH MODELS BECAUSE IT ASSUMES A CONTINIOUS DEPENDENT VARIABLE FOR TIME

# we move onto discrete time based models and in particular the gompertz regression model
str(df_new2)
event_1=df_new2$event
sample_size=nrow(df_new2)
haz=event_1/sample_size
str(haz)
# we create a test and training set to evaluate model performance on the testing set for classifiction purposes

split=sample.split(energy_2$Total_bill,SplitRatio =0.8)
training_set=subset(energy_2,split==TRUE)

y_test=sample.split(y$event,SplitRatio =0.8)
y_train=subset(y,split==TRUE)
str(y_train)
str(y_test)
head(y_train)
dim(y_train)
dim(y_test)





z_new$event<-as.numeric(z_new$event)
str(z_new)
z_new$event<-z_new$event-1
table(z_new$event)
event1=sum(z_new$event)
str(z_new)

event1
head(z_new$event)
z_new%>%
  group_by(REGYEARSALL) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = REGYEARSALL, y = log(-log(1-hazard)))) +
  geom_point() +
  geom_smooth()

# clearly minregyears is constant implies intercept term in model must be a constant

Gompertz_Model_Baseline <- glm(formula = y ~ time1+time2+time3+time4+time5+time6,
                               family = binomial(link = "cloglog"),
                               data = d)

summary(Gompertz_Model_Baseline)
summ(Gompertz_Model_Baseline, exp = T)

# here we get significant postive relationship between allregyears and hazard

# for a one unit increase in allregyears the prob of a person graduating given that he or she survived last time period increases by 66 %

# now we fit full gompertz regression model

# To answer the research question (i.e. What are the effects of gender, ENG,RACE,NSFAS,MATRICPOINTS,PERCENTAGE,QUINTILE and degree on a  person’s conditional probability of graduating during a given time interval?)

# Before we specify the model, it might be interesting to also visually examine the relationship between these variables and cloglog(hazard).


# gender vs cloglog(hazard)

str(df_new)
View(df_new)

y<-df_new%>%filter(status==c("Graduated","Currently"))
view(y)

z<-y%>%filter(GENDER=="F")

g<-y%>%filter(GENDER=="M")

z_new %>%
  group_by(REGYEARSALL, GENDER) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = REGYEARSALL, 
             y = log(-log(1-hazard)),
             col = GENDER)) +
  geom_point() +
  geom_smooth()

# ENG vs cloglog(hazard)

z_new %>%
  group_by(REGYEARSALL, ENG) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = REGYEARSALL, 
             y = log(-log(1-hazard)),
             col = ENG)) +
  geom_point() +
  geom_smooth()

# RACE vs cloglog(hazard)

z_new%>%
  group_by(REGYEARSALL, RACE) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = REGYEARSALL, 
             y = log(-log(1-hazard)),
             col = RACE)) +
  geom_point() +
  geom_smooth()

# NSFAS vs cloglog(hazard)

z %>%
  group_by(REGYEARSALL, NSFASYN) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = REGYEARSALL, 
             y = log(-log(1-hazard)),
             col = NSFASYN)) +
  geom_point() +
  geom_smooth()


# MATRICPOINTS VS cloglog(hazard)

z %>%filter(matric_aps!="NA")%>%
  group_by(REGYEARSALL, matric_aps) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = REGYEARSALL, 
             y = log(-log(1-hazard)),
             col = matric_aps)) +
  geom_point() +
  geom_smooth()

# PERCENTAGE vs cloglog(hazard)

z_new %>%
  group_by(REGYEARSALL, math_new) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = REGYEARSALL, 
             y = log(-log(1-hazard)),
             col = math_new)) +
  geom_point() +
  geom_smooth()

# QUINTILE vs cloglog(hazard)

z %>%
  group_by(REGYEARSALL, new_quintile) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = REGYEARSALL, 
             y = log(-log(1-hazard)),
             col = new_quintile)) +
  geom_point() +
  geom_smooth()

# DEGREE vs cloglog(hazard)

  group_by(REGYEARSALL, degree) %>%
  summarise(event = sum(event),
            total = n()) %>%
  mutate(hazard = event/total) %>%
  ggplot(aes(x = REGYEARSALL, 
             y = log(-log(1-hazard)),
             col = degree)) +
  geom_point() +
  geom_smooth()

# now we fit the full model with all the covariatesz
z$event<-as.factor(z$event)
z$QUINTILE<-as.factor(z$QUINTILE)
table(z$ENG)
table(y$ENG)

control =glm.control(maxit=50)
# MODEL FOR FEMALES ONLY IN CIVIL ENG AND DATASCIENCE
stem_data$RACE=as.factor(stem_data$RACE)
Gompertz_Model_Full <- glm(formula = y ~time1+time2+time3+time4+time5+time6-1+RACE+MATRICPOINTS+NSFASYN+PERCENTAGE+new_quintile+degree+ENG,
                           family = binomial(link = "cloglog"),
                           data =d)
str(d)
View(d)

str(d)
d%>%rename(ADD_YRS=MINREGYEARS)
view(stem_data)

j<-Gompertz_Model_Full$fitted.values
view(j)
head(j)
hazard<--exp(-exp(j))+1
head(hazard)
view(hazard)
d$haz=hazard
View(d)

logistic_data=read_excel(file.choose())

str(logistic_data)
c1<-logistic_data
c2<-logistic_data%>%select(p,GENDER,MINREGYEARS)
c3<-c2%>%group_by(MINREGYEARS,GENDER)%>%summarise(mean_prob=mean(p))

c3%>%ggplot(aes(MINREGYEARS,mean_prob))+geom_point(aes(color=factor(GENDER)))+geom_line(aes(color=factor(GENDER))) +ylab("mean predicted prob")+xlab("additional years taken")+ggtitle("mean predicted prob vs time stratified by Gender")+scale_color_discrete(name="Gender",labels=c("Female","Male"))+scale_x_continuous(breaks = seq(min(c3$MINREGYEARS),max(c3$MINREGYEARS),by=1)) 
j
x<-stem_data
x%>%ggplot(aes(years,haz))+geom_point(aes(color=factor(GENDER)))

f<-stem_data%>%select(GENDER,haz,years)

j<-f%>%group_by(years,GENDER)%>%summarise(mean_haz=mean(haz))
j%>%ggplot(aes(years,mean_haz))+geom_point(aes(color=factor(GENDER)))+geom_line(aes(color=factor(GENDER))) +ylab("mean hazard")+xlab("additional reg years")+ggtitle("mean hazard vs time sratified by Gender")+scale_color_discrete(name="Gender",labels=c("Female","Male"))+scale_x_continuous(breaks = seq(min(c$years),max(c$years),by=1)) 
j
d%>%ggplot(aes(years,mean_haz))+geom_point()+geom_line()

# some more explorotory plots for females only model where data store in variable name d 
str(d)
w<-d%>%select(RACE,haz,years,new_quintile,degree,ENG,NSFASYN,matric_new,math_new)
c<-w%>%group_by(years,RACE)%>%summarise(mean_haz=mean(haz))
c%>%filter(RACE !="O")%>%ggplot(aes(years,mean_haz))+geom_point(aes(color=factor(RACE)))+geom_line(aes(color=factor(RACE)))+scale_x_continuous(breaks = seq(min(c$years),max(c$years),by=1))+scale_color_discrete(name="Race",labels=c("African","Coloured","Indian","White"))+ylab("mean hazard")+xlab("additional reg years")+ggtitle("mean hazard vs time sratified by Race")

r<-w%>%group_by(years,NSFASYN)%>%summarise(mean_haz=mean(haz))
r%>%ggplot(aes(years,mean_haz))+geom_point(aes(color=factor(NSFASYN)))+geom_line(aes(color=factor(NSFASYN)))+scale_x_continuous(breaks = seq(min(c$years),max(c$years),by=1))+ylab("mean hazard")+xlab("additional reg years")+scale_color_discrete(name="NSFAS",labels=c("No","Yes"))+ggtitle("mean hazard vs time sratified by NSFAS")

e<-w%>%group_by(years,ENG)%>%summarise(mean_haz=mean(haz))
e%>%ggplot(aes(years,mean_haz))+geom_point(aes(color=factor(ENG)))+geom_line(aes(color=factor(ENG)))+scale_x_continuous(breaks = seq(min(c$years),max(c$years),by=1))+ylab("mean hazard")+xlab("additional reg years")+scale_color_discrete(name="level of Engish",labels=c("English 1st add","Eng 1N","Eng 2nd add","Eng home lang"))+ggtitle("mean hazard vs time stratified by English level")

t<-w%>%group_by(years,degree)%>%summarise(mean_haz=mean(haz))
t%>%ggplot(aes(years,mean_haz))+geom_point(aes(color=factor(degree)))+geom_line(aes(color=factor(degree)))+scale_x_continuous(breaks = seq(min(c$years),max(c$years),by=1))+ylab("mean hazard")+xlab("additional reg years")+scale_color_discrete(name="Type of degree",labels=c("Civl Eng","Mathematical Sci"))+ggtitle("mean hazard vs time sratified by Degree type")

# now let us stratify mathematics marks and matricpoints to gain understanding of its relationship with time for females in civil eng and data science
o1<-d%>%filter(ENG!="ENG1N")
str(o1)
d$matric_new=d$MATRICPOINTS
d$matric_new[d$MATRICPOINTS<33]<-"not good"
d$matric_new[d$MATRICPOINTS>=33 & d$MATRICPOINTS<40]<-"good"
d$matric_new[d$MATRICPOINTS>=40]<-"very good"

d$math_new=d$PERCENTAGE
d$math_new[d$PERCENTAGE<60]<-"not good"
d$math_new[d$PERCENTAGE>=60 & d$PERCENTAGE<75]<-"good"
d$math_new[d$PERCENTAGE>=75]<-"very good"

i<-w%>%group_by(years,matric_new)%>%summarise(mean_haz=mean(haz))
i%>%ggplot(aes(years,mean_haz))+geom_point(aes(color=factor(matric_new)))+geom_line(aes(color=factor(matric_new)))+scale_x_continuous(breaks = seq(min(c$years),max(c$years),by=1))+ylab("mean hazard")+xlab("additional reg years")+scale_color_discrete(name="Matric points ranking",labels=c("good:(33,40)","not good:<33","very good:>=40"))+ggtitle("mean hazard vs time sratified by Matric points ranking")

u<-w%>%group_by(years,math_new)%>%summarise(mean_haz=mean(haz))
u%>%ggplot(aes(years,mean_haz))+geom_point(aes(color=factor(math_new)))+geom_line(aes(color=factor(math_new)))+scale_x_continuous(breaks = seq(min(c$years),max(c$years),by=1))+ylab("mean hazard")+xlab("additional reg years")+scale_color_discrete(name="Maths ranking",labels=c("good:[60,75)","not good:<60","very good:>=75"))+ggtitle("mean hazard vs time sratified by Math score ranking")

p_1<-w%>%group_by(years,new_quintile)%>%summarise(mean_haz=mean(haz))
p_1%>%ggplot(aes(years,mean_haz))+geom_point(aes(color=factor(new_quintile)))+geom_line(aes(color=factor(new_quintile)))+scale_x_continuous(breaks = seq(min(c$years),max(c$years),by=1))+ylab("mean hazard")+xlab("additional reg years")+scale_color_discrete(name="Quintile",labels=c("Lower(1,2,3)","other","Upper(4,5)"))+ggtitle("mean hazard vs time sratified by Quintile")
str(d)










stem_data$mean_haz<-stem_data$years
stem_data$mean_haz[stem_data$years==1]<-0.810
stem_data$mean_haz[stem_data$years==2]<-0.816
stem_data$mean_haz[stem_data$years==3]<-0.828
stem_data$mean_haz[stem_data$years==4]<-0.838
stem_data$mean_haz[stem_data$years==5]<-0.851
stem_data$mean_haz[stem_data$years==6]<-0.804
stem_data$mean_haz[stem_data$years==7]<-0.845
str(stem_data)
stem_data%>%filter(GENDER=="M")%>%ggplot(aes(years,mean_haz))+geom_point()+geom_smooth()
stem_data%>%filter(GENDER=="F")%>%ggplot(aes(years,mean_haz))+geom_point()+

str(f)
agg=aggregate(f,by=list(f$haz),FUN = mean)

?aggregate
str(males)

str(stem_data)
des<-svydesign(ids=~psu, strata = ~strata , weights=~weight, data=z_final)
table(z_final$RACE)
# MODEL FOR MALES ONLY IN CIVIL ENG AND DATASCIENCE
Gompertz_Model_Full1 <- glm(formula = event ~REGYEARSALL+NSFASYN+MATRICPOINTS+PERCENTAGE+RACE+new_quintile+degree,
                           family = binomial(link = "cloglog"),confint,
                           data =g_new)
add_ci(d,Gompertz_Model_Full,alpha=0.05)

str(d)
?add_ci
table(df_final$new_quintile)
df_final<-stem_data%>%filter(new_quintile!="missi")
summary(Gompertz_Model_Full)
str(stem_data)
summ(Gompertz_Model_Full,exp = T)


plot_coefs(Gompertz_Model_Full,coefs = c("RACE(AFRICAN):(WHITE)"="RACEA","RACE(COLOURED):(WHITE)"="RACEC","RACE(INDIAN):(WHITE)"="RACEI","NSFAS(NO FUNDING)"="NSFASYNY","MATRIC APS POINTS"="MATRICPOINTS","MATHEMATICS MARK"="PERCENTAGE","QUINTILE(OTHER):(LOWER)"="new_quintilemissi","QUNTILE(UPPER):(LOWER)"="new_quintileupper","Mathematical Science degree(Civil ENG)"="degreeData_Sci","ENG1N:(ENG 1st additional)"="ENGENG1N","ENG 2nd additional(ENG 1st additional"="ENGENG2","ENG home language(ENG 1st additional)"="ENGENGHN"),exp = F,ci_level =0.95,colors = "#FF6633")+xlab("Parameter estimates")+ggtitle("Parameter effects")





?plot_coefs


class(k3)   
k3
k3%>%ggplot()+scale_color_brewer("set1")
                                                                                                                                                                                                                               

                                                                                                                                                                                                                                                                                                     
                                                                                                                                                                                                                           
                                                                                                                                                 


plot_summs(Gompertz_Model_Full,exp = T,scale=T)
?glm
plot_model(Gompertz_Model_Full,vline.color = "red",)
?plot_model
k1<-summ(Gompertz_Model_Full)
k1$coeftable
?plot_coefs
head(df_new$QUINTILE)

plot(allEffects(Gompertz_Model_Full))
plot_model( Gompertz_Model_Full)
?plot_model
anova(Gompertz_Model_Baseline, Gompertz_Model_Full, test ="Chisq")

Gompertz_Model_Full$aic

Gompertz_Model_Baseline$aic

view(z)

# note our analysis is completely thrash since we did use the correct data structure  

id <- rownames(stem_data)
d <- cbind(id=id, stem_data)
View(d)
d$MINREGYEARS<-d$MINREGYEARS+1
# converting data to a person period format

z_new <- dataLong(dataSet= d,timeColumn ="MINREGYEARS",censColumn = "event")

# we also create time dummy variables for our data set


?dataLong
View(z_new)
str(d)
z_new$MINREGYEARS<-z_new$MINREGYEARS-1
# exploratory analysis for females time to graduation in data science and civil eng

z%>%filter(event==1)%>%ggplot(aes(factor(REGYEARSALL)))+geom_bar(position ="dodge",aes(y= (..count..)/sum(..count..),fill=factor(NSFASYN)))+scale_y_continuous(labels = scales::percent)+facet_wrap(vars(factor(new_quintile)))

# here is seems that students in upper quintile take much longer time to graduate than students in lower quintiles

z%>%filter(event==1)%>%ggplot(aes(factor(REGYEARSALL)))+geom_bar(position ="dodge",aes(y= (..count..)/sum(..count..),fill=factor(NSFASYN)))+scale_y_continuous(labels = scales::percent)

z%>%filter(event==1)%>%ggplot(aes(factor(NSFASYN),REGYEARSALL))+geom_point()+geom_smooth()

z%>%filter(event==1)%>%ggplot(aes(factor(REGYEARSALL)))+geom_bar(position ="dodge",aes(y= (..count..)/sum(..count..),fill=factor(ENG)))+scale_y_continuous(labels = scales::percent)


z%>%filter(event==1)%>%ggplot(aes(MATRICPOINTS,REGYEARSALL))+geom_point(alpha=0.001)+geom_smooth()+ggtitle("Average line of matric_aps points vs Regyears for females who graduated from Data Sci and Civil Eng")

# on average it seems as the matric points increases that the regyears at university also decrease for females in civil eng and data sci

z%>%filter(event==1)%>%ggplot(aes(PERCENTAGE,REGYEARSALL))+geom_point(alpha=0.001)+geom_smooth()+ggtitle("Average line of Math% vs Regyears for females who graduated from Data Sci and Civil Eng")



# on average it seems as the percentage in maths increases so does the reg years at uni

z%>%filter(event==1)%>%ggplot( aes(x=NSFASYN, y=new_quintile, fill=REGYEARSALL)) + 
  geom_tile() +ggtitle("Heat map: Females in data science and civil ENG degrees who have graduated")+scale_fill_gradient2(low = "blue", high = "red", mid = "white")
                                                                                                       

# we can see that upper quintile students take shorted amount of time to graduate than lower quitnile students with no nsfas and with nsfas

# lower quintile students seem to graduate faster without no nsfas than with nsfas 
z%>%filter(event==1)%>%ggplot( aes(x=NSFASYN, y=degree, fill=REGYEARSALL)) + geom_tile() +ggtitle("Heat map: Females in data science and civil ENG degrees who have graduated")+scale_fill_gradient2(low = "purple", high = "white", mid = "purple")


z%>%filter(event==1)%>%ggplot( aes(x=NSFASYN, y=ENG, fill=REGYEARSALL)) + geom_tile() +ggtitle("Heat map: Females in data science and civil ENG degrees who have graduated")+scale_fill_gradient2(low = "purple", high = "green", mid = "purple")

# data scince degree students with nsfas seems to take much longer to graduated than civil eng students with nsfas 

# civil eng with and without nsfas seem to graduate in a decent amount of time

# data science students with no nsfas seem to graduate in a decent amount of time




# we need to exclude those with neggative min duration and no qualification since they are not eligble to graduate

# exclude those with neggative duration and who are qualified since we do not know how long they take to qualify , they could of come from different univeristy etc...




