PROC IMPORT DATAFILE="/home/u49229476/Stats project school data/model2.xlsx"
		    OUT=model_2
		    DBMS=XLSX
		    REPLACE;
RUN;

proc print data=model_2(obs=400);
where GRADIND ne 0;
run;

/* now we shall model the time to graduation with ordinal logistic regression */

data new2;
	set work.model_2;
	where status not like "Currently";
run;

data new3;
	set work.new2;
	if QUINTILE in (1 2 3) then new_quintile="lower";
	else if QUINTILE in (4 5) then new_quintile="upper";
run;

data new4;
	set work.new3;
	where GRADIND ne 0;
run;

data new5;
	set work.new4;
	where MINREGYEARS GE 0;
run;


/* now we fit ordinal logistic regression model */

ods graphics on;
proc logistic data=new5 PLOTS=ALL;
class GENDER new_quintile NSFASYN ENG degree/param=ref;
model MINREGYEARS=GENDER new_quintile NSFASYN ENG degree MATRICPOINTS PERCENTAGE  / equalslopes  clodds=pl stb parmlabel expb lackfit scale=none rsq aggregate ctable ;
ROCCONTRAST;
oddsratio ENG/diff=all cl=pl;
effectplot fit(x=MATRICPOINTS);
run;
ods graphics off;

















