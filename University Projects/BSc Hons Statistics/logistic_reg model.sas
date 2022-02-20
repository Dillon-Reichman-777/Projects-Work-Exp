/* here we will now build a logistic regression model */

PROC IMPORT DATAFILE="/home/u49229476/Stats project school data/civil.xlsx"
		    OUT=civil
		    DBMS=XLSX
		    REPLACE;
RUN;

proc print data=civil(obs=400);
run;


PROC IMPORT DATAFILE="/home/u49229476/Stats project school data/data_science.xlsx"
		    OUT=data_sci
		    DBMS=XLSX
		    REPLACE;
RUN;

proc print data=data_sci(obs=400);
run;

data new;
	set work.civil;
	if status NE "john" then degree="civil";
run;

proc print data=new(obs=200);
run;

data new1;
	set work.data_sci;
	if status NE "john" then degree="Data_Sci";
run;

proc freq data=new1;
tables degree;
run;


proc append base=new1 data=new;
run;

proc freq data=new1;
tables degree;
run;

data model2;
	set work.new1;
run;

data new1;
	set work.model2;
	if QUINTILE in (1 2 3) then new_quintile="lower";
	else if QUINTILE in (4 5) then new_quintile="upper";
	where GENDER="F";
run;

data new2;
	set work.model2;
	if QUINTILE in (1 2 3) then new_quintile="lower";
	else if QUINTILE in (4 5) then new_quintile="upper";
	where GENDER="M";
run;
data female;
	set work.model2;
	where GENDER="F";
run;

proc print data=female;
run;

data male;
	set work.model2;
	where GENDER="M";
run;
proc print data=male;
run;


proc export 
  data=work.model2
  dbms=xlsx 
  outfile="/home/u49229476/model_data2" 
  replace;
run;

/* we will build model for data science degrees and for civil engineering degrees */

/* data sci degrees */

ods graphics on;
proc logistic data=data_sci
plots(only)=(effect (clband x=(PERCENTAGE MATRICPOINTS))
oddsratio (type=horizontalstat range=clip));
class GENDER QUINTILE NSFASYN ENG GRADIND
/param=ref ref=first;
model GRADIND(event='1')=GENDER QUINTILE NSFASYN ENG PERCENTAGE MATRICPOINTS
/clodds=pl stb parmlabel expb lackfit scale=none rsq aggregate ctable;
ROC "ALL VARIABLES INCLUDED"
GENDER QUINTILE NSFASYN ENG PERCENTAGE MATRICPOINTS;
oddsratio QUINTILE/ diff=all cl=pl;
oddsratio ENG/ diff=all cl=pl;
title 'full model';
run;

ods graphics on;
proc logistic data=data_sci
plots(only)=(effect (clband x=(PERCENTAGE MATRICPOINTS))
oddsratio (type=horizontalstat range=clip));
class QUINTILE ENG GRADIND
/param=ref ref=first;
model GRADIND(event='1')= QUINTILE ENG PERCENTAGE MATRICPOINTS
/clodds=pl stb parmlabel expb lackfit scale=none rsq aggregate ctable;
ROC "Excluding Gender and Nsfas"
QUINTILE ENG PERCENTAGE MATRICPOINTS;
oddsratio QUINTILE/ diff=all cl=pl;
oddsratio ENG/ diff=all cl=pl;
title 'excluding GENDER and NSFASYN';
run;


ods graphics on;
proc logistic data=data_sci
plots(only)=(effect (clband x=(PERCENTAGE MATRICPOINTS))
oddsratio (type=horizontalstat range=clip));
class ENG GRADIND
/param=ref ref=first;
model GRADIND(event='1')=  ENG PERCENTAGE MATRICPOINTS
/clodds=pl stb parmlabel expb lackfit scale=none rsq aggregate ctable;
ROC "Excluding Gender,Nsfas and quintile"
ENG PERCENTAGE MATRICPOINTS;
oddsratio ENG/ diff=all cl=pl;
title 'excluding GENDER ,NSFASYN and Quintile';
run;

/* civil degrees */

ods graphics on;
proc logistic data=civil
plots(only)=(effect (clband x=(PERCENTAGE MATRICPOINTS))
oddsratio (type=horizontalstat range=clip));
class GENDER QUINTILE NSFASYN ENG GRADIND
/param=ref ref=first;
model GRADIND(event='1')=GENDER QUINTILE NSFASYN ENG PERCENTAGE MATRICPOINTS
/clodds=pl stb parmlabel expb lackfit scale=deviance rsq aggregate ctable;
roc "main effects"
GENDER QUINTILE NSFASYN ENG PERCENTAGE MATRICPOINTS;
oddsratio QUINTILE/ diff=all cl=pl;
oddsratio ENG/ diff=all cl=pl;
title 'full model';
run;

/* model for both degrees */

ods graphics on;
proc logistic data=new1
plots(only)=(roc);
class  new_quintile NSFASYN ENG GRADIND degree
/param=ref ref=first;
model GRADIND(event='1')= new_quintile NSFASYN ENG PERCENTAGE MATRICPOINTS degree  NSFASYN*new_quintile ENG*degree/ clodds=pl stb parmlabel expb lackfit scale=deviance rsq aggregate ctable;
roc "main effects"
 new_quintile NSFASYN ENG PERCENTAGE MATRICPOINTS degree;
oddsratio ENG/ diff=all cl=pl;
effectplot fit(x=PERCENTAGE );
effectplot fit(x=MATRICPOINTS);
title 'full model for females only';
run;


ods graphics on;
proc logistic data=new2
plots(only)=(roc);
class  new_quintile NSFASYN ENG GRADIND degree
/param=ref ref=first;
model GRADIND(event='1')= new_quintile NSFASYN ENG PERCENTAGE MATRICPOINTS degree  NSFASYN*new_quintile ENG*degree/ clodds=pl stb parmlabel expb lackfit scale=deviance rsq aggregate ctable;
roc "main effects"
 new_quintile NSFASYN ENG PERCENTAGE MATRICPOINTS degree;
oddsratio ENG/ diff=all cl=pl;
effectplot fit(x=PERCENTAGE );
effectplot fit(x=MATRICPOINTS);
title 'full model for Males only';
run;

/* we now check for interaction effects */

/* GENDER and new_quintile interaction effects are not statistically significant */

/* GENDER AND NSFAS interaction effects are not statistically significant */

/* GENDER and ENG interaction effects are not statistically significant */

/* new_quintile and NSFASYN interaction effect is statistically significant */

/* new_quintile and ENG interaction effect is not statistically significant */

/* NSFASYN and ENG interaction effect is statistically significant but makes the model fit worse */

/* The inclution of both interaction effects does not improve the model fit */




/* females only */

ods graphics on;
proc logistic data=female
plots(only)=(effect (clband x=(PERCENTAGE MATRICPOINTS))
oddsratio (type=horizontalstat range=clip));
class  QUINTILE NSFASYN ENG GRADIND degree
/param=ref ref=first;
model GRADIND(event='1')= QUINTILE NSFASYN ENG PERCENTAGE MATRICPOINTS degree
/clodds=pl stb parmlabel expb lackfit scale=deviance rsq aggregate ctable;
roc "main effects"
 QUINTILE NSFASYN ENG PERCENTAGE MATRICPOINTS degree;
oddsratio QUINTILE/ diff=all cl=pl;
oddsratio ENG/ diff=all cl=pl;
title 'full model';
run;


/* males only */

ods graphics on;
proc logistic data=male
plots(only)=(effect (clband x=(PERCENTAGE MATRICPOINTS))
oddsratio (type=horizontalstat range=clip));
class  QUINTILE NSFASYN ENG GRADIND degree
/param=ref ref=first;
model GRADIND(event='1')= QUINTILE NSFASYN ENG PERCENTAGE MATRICPOINTS degree
/clodds=pl stb parmlabel expb lackfit scale=deviance rsq aggregate ctable;
roc "main effects"
 QUINTILE NSFASYN ENG PERCENTAGE MATRICPOINTS degree;
oddsratio QUINTILE/ diff=all cl=pl;
oddsratio ENG/ diff=all cl=pl;
title 'full model';
run;

/* logistic regression models does not seem to fit the data */





