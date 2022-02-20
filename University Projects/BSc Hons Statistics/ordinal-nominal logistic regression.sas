PROC IMPORT DATAFILE="/home/u49229476/CLEANED_NEW.xlsx"
		    OUT=model_2
		    DBMS=XLSX
		    REPLACE;
RUN;

proc print data=model_2(obs=400);
run;

proc freq data=model_2;
tables status;
where MINREGYEARS>=0;
RUN;


data new14;
set model_2;
if status="Excluded" then new_status="No graduation";
else if status="Currently" then new_status="No graduation";
else if status="Dropped o" then new_status="No graduation";
else if status="Graduated" then new_status="Graduated";
run;

data new15;
	set work.new14;
	if QUINTILE in (1 2 3) then new_quintile="lower";
	else if QUINTILE in (4 5) then new_quintile="upper";
	else if QUINTILE=6 then new_quintile="Missing";
run;

data new16;
set work.new15;
where MINREGYEARS GE 0;
run;

data Females_only (RENAME=(MINREGYEARS=ADD_YRS));
set work.new16;
WHERE RACE NE "O";
run;
DATA dsname (RENAME = (o1=n1 o2=n2 ...));
data Females_only1 ;
set work.females_only;

run;


ods graphics on ;
proc logistic data=Females_only plots=roc outmodel=pred;
class new_quintile NSFASYN GENDER ENG  degree RACE/param=ref;
model new_status(order=data ref="No graduation")= GENDER RACE ADD_YRS new_quintile NSFASYN ENG  degree PERCENTAGE MATRICPOINTS /LINK=logit
clodds=pl stb parmlabel expb lackfit scale=pearson rsq aggregate;
roccontrast;
ODDSRATIO ENG/diff=all cl=pl;
store nomilogmodel;
/*  effectplot fit(x=MINREGYEARS plotby=GENDER); */
/* effectplot interaction(x= plotby=NSFASYN)/LINK; */
title "MALES AND FEMALES";
run;
title;

proc print data=pred1;
run;

proc export 
  data=work.pred1
  dbms=xlsx 
  outfile="/home/u49229476/logistic_predprob" 
  replace;
run;





ods graphics on ;
proc logistic data=new16 plots=all;
class new_quintile NSFASYN ENG GENDER degree/param=ref;
model new_status(order=data ref="No graduation")= GENDER MINREGYEARS new_quintile NSFASYN ENG  degree PERCENTAGE MATRICPOINTS /LINK=logit
clodds=pl stb parmlabel expb lackfit scale=pearson rsq aggregate;
roccontrast;
ODDSRATIO ENG/diff=all cl=pl;
store nomilogmodel;
/* effectplot fit(x=MATRICPOINTS)/at(new_quintile="lower"); */
/* effectplot interaction(x=new_quintile plotby=NSFASYN)/LINK; */
title "MALES AND FEMALES";
run;
title;





proc freq data=model_2;

/* we will fit generalised logit model in which the response is nominal */

/* the response in this instance is dropped out,excluded,graduated */

data new;
	set work.model_2;
	where status not like "Currently";
run;

data new2;
	set work.new;
	if QUINTILE in (1 2 3) then new_quintile="lower";
	else if QUINTILE in (4 5) then new_quintile="upper";
	where GENDER = "F";
run;

data new3;
	set work.new;
	if QUINTILE in (1 2 3) then new_quintile="lower";
	else if QUINTILE in (4 5) then new_quintile="upper";
	where GENDER="M";
run;


proc print data=new2(obs=200);
run;


proc print data=new(obs=200);
run;

ods graphics on ;
proc logistic data=new2 plots=all;
class new_quintile NSFASYN ENG  degree/param=ref;
model status(order=data ref="Dropped o")= new_quintile NSFASYN ENG  degree PERCENTAGE MATRICPOINTS /LINK=glogit
clodds=pl stb parmlabel expb lackfit scale=pearson rsq aggregate;
roccontrast;
ODDSRATIO ENG/diff=all cl=pl;
store nomilogmodel;
effectplot fit(x=MATRICPOINTS)/at(new_quintile="lower");
effectplot interaction(x=new_quintile plotby=NSFASYN)/LINK;
title "females only";
run;
title;

ods graphics on ;
proc logistic data=new3 plots=all;
class new_quintile NSFASYN ENG  degree/param=ref;
model status(order=data ref="Dropped o")= new_quintile NSFASYN ENG  degree PERCENTAGE MATRICPOINTS /LINK=glogit
clodds=pl stb parmlabel expb lackfit scale=pearson rsq aggregate;
roccontrast;
ODDSRATIO ENG/diff=all cl=pl;
store nomilogmodel;
effectplot fit(x=MATRICPOINTS)/at(new_quintile="lower");
effectplot interaction(x=new_quintile plotby=NSFASYN)/LINK;
title "males only";
run;
title;


proc plm source=nomilogmodel;
	effectplot fit(x=MATRICPOINTS plotby=GENDER); 
run;

proc plm source=nomilogmodel;
   effectplot slicefit(x=PERCENTAGE  plotby=GENDER)/at(new_quintile="lower" NSFASYN="N" degree="Data_Sci" "civil" ENG= "ENG1" "ENG2" "ENGHN" "ENG1N" );
run;

proc plm source=nomilogmodel;
   effectplot slicefit(x=PERCENTAGE  plotby=GENDER)/at(new_quintile="lower" NSFASYN="Y" degree="Data_Sci" "civil" ENG= "ENG1" "ENG2" "ENGHN" "ENG1N" );
run;

proc plm source=nomilogmodel;
   effectplot slicefit(x=PERCENTAGE  plotby=GENDER)/at(MATRICPOINTS=20 25 30 35 40 45 50 new_quintile="upper" NSFASYN="Y" degree="Data_Sci" "civil" ENG= "ENG1" "ENG2" "ENGHN" "ENG1N" );
run;


/* the interaction between gender and nsfas is not statistically significant */

/* the interaction with gender and new_quintile is not statistically significant */

/* ENG and gender interaction term is statistically significant */

/* gender and degree interaction term is not statistically significant */

/* percentage and gender interaction effects are not statistically significant */

/* matricpoints and gender interactions effects are not statistically significant */

/* new_quintile and NSFAS interaction effects is significant */

/* new_quintile and ENG interaction effects is not statistically significant */

/* new_quintile and GENDER interaction effects is not statistically significant */

/* interaction between NSFAS AND ENG is statistically significant */

/* NSFAS AND GENDER interaction effects are not statistically significant */

/* NSFAS and degree interaction effects are statistically significant  */





