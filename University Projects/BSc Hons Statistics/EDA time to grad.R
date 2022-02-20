stem_data=read_excel(file.choose())
str(stem_data)
df_new<-stem_data%>%filter(status=="Graduated")
df_new2<-df_new%>%filter(MINREGYEARS>=0)
View(df_new2)
X<-df_new2
X%>%ggplot(aes(factor(MINREGYEARS)))+geom_bar(aes(fill=factor(GENDER)))+facet_wrap(vars(factor(QUINTILE)))
X%>%ggplot(aes(factor(MINREGYEARS)))+geom_bar(aes(fill=factor(GENDER)))+facet_wrap(vars(factor(NSFASYN)))

# from doing EDA we see that we have more student graduating in min time and in 1 additonal year with no nsfas 

# we also see that we have more students graduating in min time and 1 additional year from quintile 4 and 5 schools respectively than from lower quintiles

