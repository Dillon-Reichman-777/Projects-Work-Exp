stem_data=read_excel(file.choose())
View(stem_data)
str(stem_data)
unique(stem_data)

# we can convert our plots to interactive plots with plotly
sum(duplicated(stem_data))

table(duplicated(stem_data))
str(stem_data)

# this indicates we have no duplicates in the data

# let us test the duplicate function

x<-c(1,1,1,1)
y<-c(1,1,1,1)
df<-data.frame(x,y)
df1<-cbind(x,y)
df
class(df)
class(df1)
table(duplicated(df))
View(df)
# need to rename some cols in the stem_data df

# essentially it seems that we do not have any duplicates in the data

# note dplyr select and mass select can clash

ggplot(stem_data,aes(PERCENTAGE,PERCENTAGE_2,color=factor(HONSREGYN)))+geom_point()+xlab("Maths mark %")+ylab("English Percentage%")
ggplot(stem_data%>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC"),aes(PERCENTAGE, PERCENTAGE_2,color=factor(QUINTILE)))+geom_point(alpha=0.5)+xlab("Maths mark %")+ylab("English Percentage%")
#  HERE WE CAN SEE THAT QUINTILE 5 STUDENTS HAVE HIGHER MATHS AND SCIENCE MARKS
X=stem_data %>% select(CURRREGIND,YEAR,status,DEPTNAME,GRADIND,MATRICPOINTS,NSFASYN,PERCENTAGE,PERCENTAGE_2,ENG,HONSREGYN,GENDER,MINREGYEARS,EXCLIND,RACE,QUINTILE,DEPTNAME)
?select
table(X$ status)
table(stem_data$DEPTNAME)
View(x)
X %>% filter(GRADIND==1,CURRREGIND!=1,MATRICPOINTS>0,QUINTILE!="NA") %>% ggplot(aes(PERCENTAGE,MATRICPOINTS,color=factor(QUINTILE)))+geom_point(alpha=0.5)+
  xlab("Maths marks %")+ylab("Matric Aps points") +ggtitle("Plot english vs maths marks grouped by quintile for students who have graduated") +scale_color_discrete(name="QUINTILE")
# here we can see that students mainly coming from Quintile 5 schools not have higher and english maths marks but also have way more number of succesful grads in comparison to other quintiles
ggplotly(y)
X %>% filter(GRADIND==0,CURRREGIND!=1,MATRICPOINTS>25) %>% ggplot(aes(PERCENTAGE,MATRICPOINTS,color=factor(QUINTILE)))+geom_point(alpha=0.5)+
  xlab("Maths marks %")+ylab("English marks %") +ggtitle("Plot english vs maths marks grouped by quintile for students who have not graduated")
# here we can see that majority of students who do not graduate come from lower quintile schools

X %>% filter(QUINTILE!="NA") %>%  ggplot(aes(factor(QUINTILE)))+geom_bar(aes(y= (..count..)/sum(..count..),fill=factor(GRADIND)),position = "dodge")+scale_y_continuous(labels = scales::percent)+ geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y =(..count..)/sum(..count..),group=factor(GRADIND)),vjust = 1.5,position = position_dodge(width = 0.9),stat='count')+ xlab("QUINTILE") +labs(y="Percentage %") + 
  ggtitle("Quintile grouped by graduated vs did not graduate") +scale_fill_discrete(name="Graduation",labels=c("No","Yes"))
#  HERE WE CAN SEE THAT QUINTILE 5 SCHOOLS HAVE WAY MORE NUMBER OF GRADS THAN OTHER QUINTILE SCHOOL HENCE QUINTLE PLAYS A ROLE






X %>%filter(ENG!="NA",ENG!="ENG2N",ENG!="ENG1N") %>%ggplot(aes(factor(ENG)))+geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y =(..count..)/sum(..count..),group=factor(GRADIND)),vjust = 1.5,position = position_dodge(width = 0.9),stat='count')+scale_y_continuous(labels = scales::percent) +
xlab("English proficiency")+ylab("Percent %") +ggtitle("Quintile|English proficiency grouped by graduated vs did not graduate") + geom_bar(position="dodge",aes(y=(..count..)/sum(..count..),factor(ENG),fill=factor(GRADIND))) + geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y =(..count..)/sum(..count..),group=factor(GRADIND)),vjust = 1.5,position = position_dodge(width = 0.9),stat='count') +theme_gray()+scale_fill_discrete(name="Graduation",labels=c("No","Yes")) 

# HERE WE SEE THAT ENGLISH AS FIRST LANGUAGE HAS MORE NUMBER OF SUCCESSFUL GRADS THAN OTHER OPTIONS OF ENGLISH OFFERED AT HIGHSCHOOL LEVEL





X  %>% filter(HONSREGYN!="NA",QUINTILE!="NA")%>%ggplot(aes(factor(QUINTILE)))+geom_bar(aes(y= (..count..)/sum(..count..),fill=factor(HONSREGYN))) + xlab("QUINTILE") +scale_fill_discrete(name="HONS REGISTRATION") + 
  scale_y_continuous(labels = scales::percent) + ylab("Percent %")
# here we see that we have more number of students from quintile 5 schools registering for honours degrees
# quintile seems to be a driver for postgrad registration



X  %>% filter(HONSREGYN!="NA")%>%ggplot(aes(factor(QUINTILE)))+geom_bar(aes(fill=factor(RACE)))+xlab("QUINTILE")+ggtitle("QUINTILE GROUPED BY RACE")
# HERE WE SEE THAT WHITE AND INDIAN STUDENTS IN QUINTILE 5 SCHOOLS , CLEARLY WE HAVE INEQUALITY IN THE SCHOOLING SYSTEM IN WHICH AFRICAN STUDENTS ARE NOT GETTING EQUAL OPPERTUNITY TO DECENT EDUCATION


X  %>% filter(HONSREGYN!="NA",QUINTILE!="NA")%>%ggplot(aes(MATRICPOINTS,factor(HONSREGYN)))+geom_violin(trim =FALSE,aes(fill=GENDER)) +geom_boxplot(width=0.01) +xlab("matric aps points")+ylab("HONOURS REGISTRATION")
# WE SEE THAT THE MEDIAN MATRIC APS SCORE FOR STUDENTS WHO REGISTER FOR HONS is generally higher than those who do not register
# in particular for males and females that register for postgrad , the median aps score females is higher than that of males


X  %>% filter(HONSREGYN!="NA")%>%ggplot(aes(MATRICPOINTS,factor(HONSREGYN)))+geom_boxplot(aes(fill=RACE)) +xlab("matric aps points")+ylab("HONOURS REGISTRATION")
# HERE WE SEE THAT FOR INDIAN AND WHITE STUDENTS THAT DO ENROLL FOR HONS , THEY HAVE A HIGHER MEDIAN APS SCORE IN GRADE 12 COMPARED WITH AFRICAN STUDENTS. THE SAME APPLYS TO STUDENTS WHO DO NOT REGISTER FOR HONS



Y %>% filter(QUINTILE!="NA",DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC",MATRICPOINTS>25) %>% ggplot(aes(PERCENTAGE,MATRICPOINTS))+geom_point(aes(shape=factor(GENDER),color=factor(NSFASYN))) +scale_colour_discrete(name="NSFAS",labels=c("No","Yes"))+facet_grid(factor(status)~factor(QUINTILE)) + 
  xlab("Maths percentage %") + ylab("Matric aps points")  +ggtitle("Data sciecne degrees- Maths, Stats,and Comp Sci") +scale_shape_discrete(name="Gender",labels=c("Female","Male"))
# new plot for data science degrees
# here we can also see we have more graduates from quintile 5 schools that other quintile schools
# for quintile 4 schools we can see that majority of students that graduate are funded by NSFAS 
# for quintile 1,2,3 we have quite a few students who do not graduate which are not funded by NSFAS 
# we see that even for quintile 5 that majority which drop out have no nsfas while quite alot who are still registered have nsfas ,and you can see for those who get excluded you have very few which are in 80+ for maths and above 40 aps score for grade 12 but for those who drop out for quintile 5 we have many students with good marks but no funding
Y %>% filter(QUINTILE!="NA",DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC",MATRICPOINTS>25) %>% ggplot(aes(PERCENTAGE,MATRICPOINTS))+geom_point(aes(color=factor(NSFASYN))) +scale_colour_discrete(name="NSFAS",labels=c("No","Yes"))+facet_grid(factor(status)~factor(GENDER)) + 
  xlab("Maths percentage %") + ylab("Matric aps points")  +ggtitle("Data sciecne degrees- Maths, Stats,and Comp Sci") 
# HERE AGAIN FEMALES WHO DROP OUT MOST AND WHO HAVE ABOVE 80 PERCENT FOR MATHS AND AN APS SCORE OF ATLEAST 40 ARE NOT FUNDED
# while students who are excluded do really fall into the above 8o in math and more that 40 aps score in grade 12
X%>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC")%>%ggplot(aes(MATRICPOINTS,factor(HONSREGYN)))+geom_boxplot(aes(fill=GENDER)) +ggtitle("Data Science related degrees-Maths,Stats and Comp Sci") + xlab("Matric APS score") + ylab ("Honors registration")
#  FOR DATA SCIENCE RELATED DEGREES WE OBSERVE THAT THE MEDIAN APS SCORES FOR STUDENTS WHO DO HONS ARE HIGHER THAN THAT OF THOSE WHO DO NOT DO HONS , IN PARTICULAR FEMALES WHO DO HONS HAVE HIGHER APS MEDIAN SCORE THAN THAT OF MALES


X%>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC",MATRICPOINTS>25,QUINTILE!="NA")%>%ggplot(aes(MATRICPOINTS,factor(QUINTILE)))+geom_boxplot(aes(fill=GENDER))+facet_wrap(vars(factor(status)))+xlab("matric aps points")+ylab("Quintile")+ggtitle("Data Science related degrees-Maths,Stats and Comp Sci")

#  for students who graduated from the data science related degree we can see that the median aps score is higher than for students who do not graduated, in particular females who graduate and do not graduate have higher median aps score than males

X%>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC",MATRICPOINTS>25,QUINTILE!="NA")%>%ggplot(aes(factor(QUINTILE),MATRICPOINTS))+geom_jitter(aes(color=factor(GENDER))) +facet_wrap(vars(factor(status)))


X%>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC",CURRREGIND!=1)%>%ggplot(aes(PERCENTAGE ,factor(GRADIND)))+geom_boxplot(aes(fill=GENDER))+xlab("Maths marks %")+ylab("graduated vs not graduated")+ggtitle("Data Science related degrees-Maths,Stats,and Comp Sci for students not currently registered")
# we see here that for data science related degrees the median maths percentage marks for students who graduate is higher than that of students who do not graduate, females seem to have a higher median math marks than that of males for students who graduate


X%>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC")%>%ggplot(aes(PERCENTAGE_2,factor(GRADIND)))+geom_boxplot(aes(fill=GENDER))+xlab("English marks %")+ylab("graduated vs not graduated")+ggtitle("MATHS,STATS,AND COMPSCI(DATA SCIENCE DEGREES)")
# we see here that for data science related degrees the median english percentage marks for students who graduate is higher than that of students who do not graduate, females seem to have a higher median math marks than that of males for students who graduate


X%>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC",QUINTILE!="NA",CURRREGIND!=1)%>%ggplot(aes(factor(QUINTILE)))+geom_bar(position ="dodge",aes(y= (..count..)/sum(..count..),fill=factor(GRADIND)))+facet_wrap(vars(factor(GENDER)))+xlab("Quintile")+ylab("Percent")+
  ggtitle("Data Science degrees-Maths,Stats,Comp Sci for students not currently registered")+scale_fill_discrete(name="Graduation",labels=c("Yes","No")) +  scale_y_continuous(labels = scales::percent)+geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y =(..count..)/sum(..count..),group=factor(GRADIND)),vjust = 1.5,position = position_dodge(width = 0.9),stat='count')
# we see that for both males and females that we have more succesful graduates coming from quintile 5 schools relative to the other quintiles, we have more males than females


?facet_wrap
X%>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC",QUINTILE!="NA",CURRREGIND!=1)%>%ggplot(aes(factor(QUINTILE)))+geom_bar(position = "dodge",aes(y= (..count..)/sum(..count..),fill=factor(GRADIND)))+facet_grid(factor(NSFASYN)~factor(GENDER))+xlab("Quintile")+ylab("Percent")+
  ggtitle("NSFAS FUNDING:Data Science degrees:MATH,STATS,COMP for students currently not registered")+scale_fill_discrete(name="Graduation",labels=c("Yes","No")) + scale_y_continuous(labels = scales::percent)+ geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y =(..count..)/sum(..count..),group=factor(GRADIND)),vjust = 1.5,position = position_dodge(width = 0.9),stat='count')
# in data science related degrees we can see that we have more succesful grads from lower quintile schools than quintile 5 when they are funded by nsfas
# in data science related degrees we can see that we have more succesful grads coming from quintile 5 schools when they are not funded by nsfas.
# That in comparison of males and females, we have more males graduates than females grads


X%>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC",ENG!="NA",ENG!="ENG1N",MINREGYEARS!=-2,MINREGYEARS!=-1,CURRREGIND!=1) %>%ggplot(aes(factor(MINREGYEARS)))+geom_bar(aes(y= (..count..)/sum(..count..),fill=factor(GRADIND)))+facet_grid(factor(ENG)~factor(GENDER))+xlab("ADDITIONAL YEARS TAKEN TO COMPLETE DEGREE")+ylab("Percent")+
  ggtitle("English proffeciency:Data Science degrees-MATH,STATS,COMP for students not currently registered")+scale_fill_discrete(name="Graduation",labels=c("Yes","No")) +scale_y_continuous(labels = scales::percent)
# in data science related degrees we have more number succesful grads in both male and female in which their choice of english is 1st additional english language in comparison to 2nd additional language
# in data science related degrees we have more number of succesful grads for both females and males who complete the degree in minimum time and who take 1 additional year in comparison to those students which take english 2nd additional language
# It seems that choice of english is a driver for success in data science related degrees for both males and female


X%>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC")%>%ggplot(aes(factor(RACE)))+geom_bar(position = "dodge",aes(y= (..count..)/sum(..count..),fill=factor(GENDER)))+facet_grid(factor(GRADIND)~factor(NSFASYN))+ggtitle("MATHS,STATS,AND COMP-SCI (DATA SCIENCE RELATED DEGREES)")+scale_fill_discrete(name="Gender",labels=c("Female","Males"))+xlab("Race") + 
  scale_y_continuous(labels = scales::percent) + ylab("Percent %") 
# we seem to have more number of students who are funded by nsfas that do not graduate compared with students who graduate


X%>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC")%>%ggplot(aes(factor(DROPOUTIND)))+geom_bar(aes(fill=factor(GENDER)))+facet_wrap(vars(factor(NSFASYN)))+ggtitle("MATHS,STATS,AND COMPSCI(DATA SCIENCE RELATED DEGREES)")+scale_fill_discrete(name="Gender")+xlab("DROPOUT VS NOT DROPOUT")
# for data science related degrees we have more dropouts for students who are not funded by nsfas compared with those who are not funded 


X%>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC")%>%ggplot(aes(factor(EXCLIND)))+geom_bar(aes(fill=factor(GENDER)))+facet_wrap(vars(factor(NSFASYN)))+ggtitle("MATHS,STATS,AND COMPSCI(DATA SCIENCE RELATED DEGREES)")+scale_fill_discrete(name="Gender")+xlab("EXCLUDED VS NOT EXCLUDED")
# the number of students which are excluded for funded and not funded seem to be similar which implies funding is not an issue in academic exclution or so it seems
# we do however notice we have more females which are not funded that are excluded in comparison to females which are funded and who are excluded
# we need to conclude this with civil engineering


X%>% filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC") %>% ggplot(aes(factor(GRADIND),MATRICPOINTS)) +geom_boxplot() + facet_wrap(vars(factor(GENDER))) + 
  xlab("1=Graduated 0= Did not graduate") + ggtitle("DATA SCIENCE DEGREES: MATHS,STATS,COMPSCI") +ylab("Matric aps score")
#  We see that the median aps score for both females and males who graduate is higher than that of those who dont graduate and females have a higher median aps score than males

X%>% filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC") %>% ggplot(aes(factor(GRADIND),PERCENTAGE)) +geom_boxplot() + facet_wrap(vars(factor(GENDER))) + 
  xlab("1=Graduated 0= Did not graduate") + ggtitle("Maths percentage VS GRADUATION grouped by gender : DATA SCIENCE DEGREES: MATHS,STATS,COMPSCI") + ylab("Maths marks %")

# here it is also clear that the median math score for those who graduated is higher than for those who do not graduate for both males and females
X %>% filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC",MINREGYEARS !="7",MINREGYEARS!="6",MINREGYEARS!= "-1",MINREGYEARS!="-2",MINREGYEARS!="5",MATRICPOINTS > 20,CURRREGIND!=1) %>% ggplot(aes(factor(MINREGYEARS),MATRICPOINTS))+geom_boxplot(aes(fill=factor(GRADIND))) +facet_wrap(vars(factor(GENDER))) + 
  xlab(" Additional Registration years") + scale_fill_discrete(name="Graduation") + ylab("Matric aps score") +
  ggtitle("Data Science related degrees-Maths,Stats,and Comp Sci for student currently not registered")

# here we see that those who graduated in min time have higher median aps compared with students who took longer to graduate for both males and females 

str(X)

str(stem_data)
Y<-stem_data

Y %>% filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)",MINREGYEARS !="7",MINREGYEARS!="6",MINREGYEARS!= "-1",MINREGYEARS!="-2",MINREGYEARS!="5",MATRICPOINTS > 20,CURRREGIND!=1) %>% ggplot(aes(factor(MINREGYEARS),MATRICPOINTS))+geom_boxplot(aes(fill=factor(GRADIND))) +facet_wrap(vars(factor(GENDER))) + 
  xlab(" Additional Registration years") + scale_fill_discrete(name="Graduation") + ylab("Matric aps score") +
  ggtitle("Civil ENG for student currently not registered")



Y%>%filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)",QUINTILE!="NA",CURRREGIND!=1)%>%ggplot(aes(factor(QUINTILE)))+geom_bar(position = "dodge",aes(y= (..count..)/sum(..count..),fill=factor(GRADIND)))+facet_grid(factor(NSFASYN)~factor(GENDER))+xlab("Quintile")+ylab("Count")+
  ggtitle("NSFAS FUNDING:Civil ENG for students currently not registered")+scale_fill_discrete(name="Graduation",labels=c("Yes","No")) + scale_y_continuous(labels = scales::percent)+ geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y =(..count..)/sum(..count..),group=factor(GRADIND)),vjust = 1.5,position = position_dodge(width = 0.9),stat='count')

Y%>%filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)",PERCENTAGE>0,CURRREGIND!=1)%>%ggplot(aes(PERCENTAGE,factor(GRADIND)))+geom_boxplot(aes(fill=GENDER))+xlab("Maths marks %")+ylab("graduated vs not graduated")+ggtitle("Civil ENG for students not currently registered")

Y%>%filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)",ENG!="NA",ENG!="ENG1N",MINREGYEARS!=-2,MINREGYEARS!=-1,MINREGYEARS!=-3,CURRREGIND!=1) %>%ggplot(aes(factor(MINREGYEARS)))+geom_bar(aes(y= (..count..)/sum(..count..),fill=factor(GRADIND)))+facet_grid(factor(ENG)~factor(GENDER))+xlab("ADDITIONAL YEARS TAKEN TO COMPLETE DEGREE")+ylab("Percent")+
  ggtitle("English proffeciency:Civil ENG for students not currently registered") +scale_fill_discrete(name="Graduation",labels=c("No","Yes"))+scale_y_continuous(labels = scales::percent)


Y%>%filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)",QUINTILE!="NA",CURRREGIND!=1)%>%ggplot(aes(factor(QUINTILE)))+geom_bar(position ="dodge",aes(y= (..count..)/sum(..count..),fill=factor(GRADIND)))+facet_wrap(vars(factor(GENDER)))+xlab("Quintile")+ylab("Percent")+
  ggtitle("Civil ENG for students not currently registered")+scale_fill_discrete(name="Graduation",labels=c("Yes","No")) +  scale_y_continuous(labels = scales::percent)+geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y =(..count..)/sum(..count..),group=factor(GRADIND)),vjust = 1.5,position = position_dodge(width = 0.9),stat='count')

Y%>%filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)",MATRICPOINTS>0,QUINTILE!="NA")%>%ggplot(aes(MATRICPOINTS,factor(QUINTILE)))+geom_boxplot(aes(fill=GENDER))+facet_wrap(vars(factor(status)))+xlab("matric aps points")+ylab("Quintile")+ggtitle("Civil ENG")


Y %>% filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)",YEAR != "2017",YEAR !="2018",YEAR !="2019") %>% ggplot(aes(factor(YEAR))) +geom_bar(aes(fill=factor(GRADIND))) + facet_wrap(vars(factor(GENDER))) + 
  xlab("YEAR") + ggtitle("YEAR VS GRADUATION grouped by gender : Civil Engineering") + scale_fill_discrete(name="Legend", labels=c("Did not graduate","graduated"))

X %>% filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC",YEAR != "2017",YEAR !="2018",YEAR !="2019")%>% ggplot(aes(factor(YEAR))) +geom_bar(aes(fill=factor(GRADIND))) + facet_wrap(vars(factor(GENDER))) +  xlab("YEAR") + ggtitle("DATA SCIENCE DEGREES:MATHS,STATS,COMPSCI") + scale_fill_discrete(name="Legend", labels=c("Did not graduate","graduated"))
# we can see from this plot that the number of students graduating over the years is decreasing aswell as the number of enrollments for both males and females

Y %>% filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)",QUINTILE!="NA") %>% ggplot(aes(factor(QUINTILE))) +geom_bar(aes(fill=factor(GRADIND))) + facet_wrap(vars(factor(GENDER))) + 
  xlab("QUINTILE") + ggtitle("QUINTILE VS GRADUATION grouped by gender : Civil Eng") + scale_fill_discrete(name="Legend", labels=c("Did not graduate","graduated"))

# we have the most number of succesful graduates coming from quintile 5 schools for both males and females thus quintile seems to play a big role

Y %>% filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)") %>% ggplot(aes(factor(NSFASYN))) +geom_bar(aes(fill=factor(GRADIND))) + facet_wrap(vars(factor(GENDER))) + 
  xlab("NSFAS YES/NO") + ggtitle("NSFAS FUNDING VS GRADUATION grouped by gender : Civil Eng") + scale_fill_discrete(name="Legend", labels=c("Did not graduate","graduated"))

# here we can see that most students who do graduate are not funded by nsfas for both males and females 

Y %>% filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)") %>% ggplot(aes(factor(GRADIND),MATRICPOINTS)) +geom_boxplot() + facet_wrap(vars(factor(GENDER))) + 
  xlab("1=Graduated 0= Did not graduate") + ggtitle("Matric aps score VS GRADUATION grouped by gender : Civil Engineering") 

# here we can see that the meadian aps points for females and males who graduate is higher than that of those who do not graduate, we can also see the females who graduate and do not graduate have a higher median aps score than males

Y %>% filter(MAJOR1_FIRSTYRDESC=="Civil Engineering") %>% ggplot(aes(factor(GRADIND),PERCENTAGE)) +geom_boxplot() + facet_wrap(vars(factor(GENDER))) + 
  xlab("1=Graduated 0= Did not graduate") + ggtitle("Maths mark percentage VS GRADUATION grouped by gender : Civil Engineering") + ylab("Maths mark %")

# here we can also see that median maths marks for males and females is higher for those who graduate than for those who do not graduate

# The median Maths marks for females who do not graduate is lower than for males who do not graduate, while the median maths marks for females who graduate is higher than that of males

Y %>% filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)") %>% ggplot(aes(factor(GRADIND),PERCENTAGE)) +geom_boxplot() + facet_wrap(vars(factor(GENDER))) + 
  xlab("1=Graduated 0= Did not graduate") + ggtitle("Maths mark percentage VS GRADUATION grouped by gender : Civil Engineering") + ylab("Maths mark %")

Y %>% filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)",ENG!="NA",ENG!="ENG2N",ENG!="ENG1N") %>% ggplot(aes(factor(ENG)))+geom_bar(aes(fill=factor(GRADIND))) +facet_wrap(vars(factor(GENDER))) + 
  xlab(" ENG1:ENGLISH 1ST ADDITIONAL ENG2:ENGLISH 2nd ADDITIONAL") + scale_fill_discrete(name="Legend") + 
  ggtitle("Type of English in Grade 12 grouped by graduation and gender:Civil Engineering")
Y<-new_df
Y %>% ggplot(aes(QUINTILE))+geom_bar(aes(fill=factor(ENG)))
str(Y)
# it is clear that english 1st additional students have more number of graduates than the other choices of english for both males and females

Y %>% filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)",MINREGYEARS !="7",MINREGYEARS!="6",MINREGYEARS!="-3",MINREGYEARS!="-2",MINREGYEARS!="-1",MINREGYEARS!="4",MINREGYEARS!="5",MATRICPOINTS>0) %>% ggplot(aes(factor(MINREGYEARS),MATRICPOINTS))+geom_boxplot(aes(fill=factor(GRADIND))) +facet_wrap(vars(factor(GENDER))) + 
  xlab(" Additional Registration years") + scale_fill_discrete(name="Legend") + ylab("Matric aps score") +
  ggtitle("Number of additional Reg years vs matric aps score grouped by gender:Civil Engineering")


# Median aps score for females who take 1 additional year to complete the degree is higher than for females who finish in min time, for males 
# we see that median aps score who finish in min time is higher than for those who take longer than 4 years. it is also clear the median score for those who graduate is higher than for those who do not for both males and females 

# now let us do an overview of the data


# new addition plot for civil engineering

Y %>% filter(QUINTILE!="NA",QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)",MATRICPOINTS>25) %>% ggplot(aes(PERCENTAGE,MATRICPOINTS))+geom_point(aes(color=factor(QUINTILE))) +facet_grid(factor(status)~factor(GENDER)) + 
  xlab("Maths percentage %") + ylab("Matric aps points") +scale_colour_discrete(name="Quintile") +ggtitle("Civil Eng")
# here we see that most females in engineering are from quintile 5 schools mainly , is ukzn marketing to lower quintile schools in which they aim to develop untouched talent within females
Y %>% filter(QUINTILE!="NA",QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)",MATRICPOINTS>25) %>% ggplot(aes(PERCENTAGE,MATRICPOINTS))+geom_point(aes(shape=factor(GENDER),color=factor(NSFASYN))) +facet_grid(factor(status)~factor(QUINTILE)) + 
  xlab("Maths percentage %") + ylab("Matric aps points") +scale_colour_discrete(name="NSFAS",labels=c("Yes","No")) +ggtitle("Civil Eng") +scale_shape_discrete(name="Gender",labels=c("Female","Male"))
# it seems the threshold roughly for succesful graduation for maths is about 80 percent , as majority of students that graduate from engineering regardless of quintiles has 80 and above for maths while majority who do not graduate have less than that
# majority of students who graduate from quintile 4 and 5 schools have aps points greater than an eqaul to 40 while majority of those who do not graduatate have less than that
# for those who are currently registerted we can sort of gain an idea on the liklihood of success in the degree based on these thresholds 
# alot of the students for both males and females are funded by nsfas in comparison to those who dropped out which did not have that many funded by nsfas and for those who were excluded for both males and females

?facet_wrap

?scale_fill_discrete

?ca

# now we try do some correpondance analysis plots to visually see relationships of categorical variables

z<-stem_data %>% filter(MAJOR1_FIRSTYRDESC=="Civil Engineering")


p<-z %>%select(ENG,GRADIND)
View(p)
class(p)
str(p)
?MCA
k<-na.omit(p)
View(k)
class(k)
k$ENG<-factor(k$ENG)
k$GRADIND<-factor(k$GRADIND)
p$ENG<-factor(p$ENG)
p$GRADIND<-factor(p$GRADIND)
str(k)
str(p)
res.mca <- MCA(k,graph = TRUE)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45)) +ggtitle("scree plot: civil engineering")
fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal()) +ggtitle("Correpondance analysis of choice of english vs graduation(yes=1,no=0): Civil Engineering") 
#  we seem to have an association between no graduation in civil engineering and ENGHN, ENG1N and an association between graduation and ENG1

s<-z %>%select(GRADIND,QUINTILE)
str(s)
s$GRADIND<-factor(s$GRADIND)
s$QUINTILE<-factor(s$QUINTILE)
str(s)

res.mca <- MCA(s,graph = FALSE)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45)) +ggtitle("scree plot: civil engineering QUINTILE vs Graduation")
fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal()) +ggtitle("Correpondance analysis of choice of english vs graduation(yes=1,no=0): Civil Engineering")
?fviz_mca_var
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

# overview of the data
C %>% filter(CURRREGIND!=1,MATRICPOINTS>0)%>%ggplot(aes(PERCENTAGE,MATRICPOINTS)) +geom_point(aes(color=factor(GRADIND))) +facet_grid(factor(GENDER)~factor(ACCESSYN))  + 
  scale_color_discrete(name="Graduation",labels=c("No","Yes")) +  xlab("Maths percentage %") + ylab("Matric aps points") + ggtitle("Maths marks vs matric aps points partitioned by access program and gender in which students are not currently registered")

C %>% filter(CURRREGIND!=1,MATRICPOINTS>0)%>%ggplot(aes(PERCENTAGE,MATRICPOINTS)) +geom_point(aes(color=factor(GRADIND))) +facet_grid(factor(NSFASYN)~factor(ACCESSYN))  + 
  scale_color_discrete(name="Graduation",labels=c("No","Yes")) +  xlab("Maths percentage %") + ylab("Matric aps points") + ggtitle("Maths marks vs matric aps points partitioned by access program and NSFAS in which students are not currently registered")




C %>% filter(CURRREGIND!=1,MATRICPOINTS>0,QUINTILE!="NA")%>%ggplot(aes(PERCENTAGE,MATRICPOINTS)) +geom_point(aes(color=factor(GRADIND))) +facet_grid(factor(QUINTILE)~factor(ACCESSYN))  + 
  scale_color_discrete(name="Graduation",labels=c("No","Yes")) +  xlab("Maths percentage %") + ylab("Matric aps points") + ggtitle("Maths marks vs matric aps points partitioned by access program and QUINTILE in which students are not currently registered")


C=stem_data
C %>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC",QUINTILE!="NA") %>% ggplot(aes(factor(QUINTILE))) +geom_bar(aes(fill=(RACE)))+facet_wrap(vars(factor(GENDER))) + 
  xlab("QUINTILE") +ggtitle("DATA SCIENCE RELATED DEGREES: MATHS,STATS,COMP SCI")

# here we can that majority of indian and white students are coming from quintile 5 schools and these are the students which are most succesful in terms of graduation for both females and males

C %>%filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC",QUINTILE!="NA") %>% ggplot(aes(factor(QUINTILE))) +geom_bar(aes(fill=(RACE)))+facet_wrap(vars(factor(NSFASYN))) + 
  xlab("QUINTILE") +ggtitle("DATA SCIENCE RELATED DEGREES: MATHS,STATS,COMP SCI Grouped by NSFAS funding,
                            Race and Quintile")

# most of the students funded by nsfas are african and come from mostly non quintile 5 schools

# it seems most students in the DATA Science related diciplines are not funded by nsfas , in particular Quintile 5 students

C %>%filter(MAJOR1_FIRSTYRDESC=="Civil Engineering",QUINTILE!="NA") %>% ggplot(aes(factor(QUINTILE))) +geom_bar(aes(fill=(RACE)))+facet_wrap(vars(factor(GENDER))) + 
  xlab("QUINTILE") +ggtitle("Civil Engineering degrees Grouped by QUINTILE,
                            Race and GENDER")

# it seems that most students in civil engineering come from quintile 5 schools for both male and female

# in which majority are indian , white and coloured students from quintile 5 schools

C %>%filter(QUALNAME=="Bachelor of Science in Engineering (Civil Engineering)",QUINTILE!="NA") %>% ggplot(aes(factor(QUINTILE))) +geom_bar(aes(fill=(RACE)))+facet_wrap(vars(factor(NSFASYN))) + 
  xlab("QUINTILE") +ggtitle("Civil Engineering degrees Grouped by QUINTILE,
                            Race and NSFAS funding")

# here we see that most students who come from quintile 5 are not funded by nsfas and mainly african students coming from non quintile 5 schools are funded by nsfas

# now we want to create a new column in the stem df called status

nrow(new_df)
for (i in 1:nrow(new_df)) {
  if (stem_data$GRADIND[i]==1){
    stem_data$status[i]<-"Graduated"
  }else if (stem_data$EXCLIND[i]==1){
    stem_data$status[i]<-"Excluded"
  }else if (stem_data$DROPOUTIND[i]==1){
    stem_data$status[i]<-"Dropped out"
  }else {
    stem_data$status[i]<-"Still registered"
  }
  
}

X %>% filter(QUINTILE != "NA",MATRICPOINTS>0) %>% ggplot(aes(PERCENTAGE,MATRICPOINTS,color=factor(QUINTILE)))+geom_point(alpha=0.5)+ facet_wrap(vars(factor(status))) + xlab("Maths marks %") + 
  ylab("Matric Aps score") + ggtitle("Plot of english vs maths marks grouped by Quintile ") +scale_color_discrete(name="Quintile")

summary(stem_data$GRADIND)
table(stem_data$GRADIND)
nrow(stem_data)
table(stem_data$DROPOUTIND)
table(stem_data$EXCLIND)
table(stem_data$CURRREGIND)

# here we see that we have no missing values for gradind,dropoutind,exclind and currregind

table(stem_data$status)
warnings()


# some additional new plots to enhance the presentation

X %>% ggplot(aes(factor(RACE)))+geom_bar(aes(fill=factor(NSFASYN))) + facet_grid(factor(GRADIND)~factor(GENDER)) + 
  xlab("RACE") + scale_fill_discrete(name="nsfas funding") + ggtitle("Race grouped by graduation,gender and nsfas funding")

X %>% filter(DEPTNAME==c("SCHOOL OF ENGINEERING","SCHOOL OF MATHS,STATS &COMP SC")) %>% ggplot(aes(factor(DEPTNAME)))+geom_bar(aes(fill=factor(NSFASYN))) + facet_grid(factor(status)~factor(GENDER)) + 
  xlab("Department name") +scale_fill_discrete(name="NSFAS funding") +ggtitle("Type of school in ukzn group by status of registration")
  

# we now start doing statistical tests for the varaibles we want to model 

# we quantify the effects they have on each other using the chi square test for 2 categorical variables 

#  For 2 continious variables we use pearsons test if variables normally distributed (parametric)

#  use spearmans test for non normally distributed data (non parametric)

ggdensity(X$PERCENTAGE...53, fill = "lightgray")
ggqqplot(X$PERCENTAGE...53)

# maths marks do not seem to be normally distributed

ggdensity(X$MATRICPOINTS, fill = "lightgray")
ggqqplot(X$MATRICPOINTS)

# matric points do not seems to be normally distributed
df<-data.frame(X$MATRICPOINTS,X$PERCENTAGE...53)
dim(df)
str(df)
scaled_dat<-scale(df)
# scaled_dat is a matrix object and need to be converted to a data frame
scaled_df<-as.data.frame(scaled_dat)
class(scaled_df)
str(scaled_df)
View(scaled_df)
ggqqplot(scaled_df$X.PERCENTAGE...53) + ggtitle("Maths marks QQ-PLOT")

# now we do the correlation test ]

cor(scaled_df$X.MATRICPOINTS,scaled_df$X.PERCENTAGE...53, method = c("pearson", "kendall", "spearman"),use = "complete.obs")

cor.test(scaled_df$X.MATRICPOINTS,scaled_df$X.PERCENTAGE...53, method=c( "kendall"))

tb1=table(stem_data$NSFASYN,stem_data$GRADIND)
tb1

chisq.test(tb1)
x<-stem_data%>%filter(ENG!="ENG2N")
tb2=table(x$ENG,x$GRADIND)
tb2

chisq.test(tb2)

tb3=table(stem_data$QUINTILE,stem_data$GRADIND)
tb3
chisq.test(tb3)

tb4=table(stem_data$QUINTILE,stem_data$ENG)
tb4
chisq.test(tb4)

tb5=table(stem_data$QUINTILE,stem_data$NSFASYN)
chisq.test(tb5)

tb6=table(stem_data$HONSREGYN,stem_data$QUINTILE)
chisq.test(tb6)

tb7=table(stem_data$ENG,stem_data$NSFASYN)

chisq.test(tb7)

# new discovery for nsfas funding

X %>% filter(QUINTILE < 5)%>% ggplot(aes(factor(status))) + geom_bar(aes(fill=factor(GENDER))) +facet_wrap(vars(factor(NSFASYN))) + xlab("Registration outcome") + 
  scale_fill_discrete(name="Gender") + ggtitle("students from lower quintiles grouped by reg outcome and funding")

# we will now have to clean our data as we have alot of duplicates and then we need redo our EDA , tables and statistical tests

sum(duplicated(stem_data$STUID))

# we have 9316 duplicates in the data

table(stem_data$GRADIND)

new_df<-stem_data[!duplicated(stem_data$STUID), ]
class(new_df)
table(new_df$GRADIND)

sum(duplicated(new_df$STUID))

# now lets check how many missing values we have for quintile in our data set in particular for civil eng and data science related degrees

y=X%>% filter(DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC") 
table(y$QUINTILE,exclude = NULL)

# for data science related degrees we only 620 missing observations for QUINTILE

z=Y%>%filter(QUALNAME== "Bachelor of Science in Engineering (Civil Engineering)")
table(z$QUINTILE,exclude = NULL)

# for civil engineering degrees we only have 280 missing values for quintile
View(Y)
View(Y%>%filter(QUINTILE=="NA"))
k<-subset(Y,QUALNAME== "Bachelor of Science in Engineering (Civil Engineering)"& is.na(Y$QUINTILE))
View(k)

n<-subset(Y,DEPTNAME=="SCHOOL OF MATHS,STATS &COMP SC" & is.na(Y$QUINTILE))
View(n)

summary(n$QUINTILE)
table(n$ENG)
# MOST FREQUENT ENG for all NA for data science degree is ENG1 and ENGHN
n$ENG=as.factor(n$ENG)
n$ACCESSYN=as.factor(n$ACCESSYN)
table(n$ENG,n$ACCESSYN)

class(n)
