PROC IMPORT DATAFILE="/home/u49229476/Stats project school data/cleaned_3.xlsx"
		    OUT=school_data
		    DBMS=XLSX
		    REPLACE;
RUN;

proc print data=school_data (obs=10);
run;


/* let us check for duplicates in the data */

proc sort data=school_data out=deduped;
   by STUID ;
run;

/* 9316 obs were removed as duplicated */
proc sort data=deduped nodupkey out=Cleaned_1;
   by STUID;
run;

/* Cleaned_1 is data set with removed duplicates */

proc print data=Cleaned_1 (obs=100);
run;


proc freq data=Cleaned_2;
	tables QUINTILE;
	where DEPTNAME like "SCHOOL OF MATHS,STATS &COMP SC";
run;

/* we have 620 missing values for data science-related degrees for QUINTILE */

proc freq data=Cleaned_1;
	tables QUINTILE;
	where QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)";
run;

/* we have 280 missing observations for quintile in civil engineering degrees */

proc print data=Cleaned_1 ;
	var ENG 
	where QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)" and QUINTILE is null;
run;

proc freq data=Cleaned_1;
	tables QUINTILE;
	where ACCESSYN EQ "Y";
run;

proc freq data=Cleaned_1;
	tables QUINTILE;
	where NSFASYN EQ "Y" and ACCESSYN="Y" AND MATRICPOINTS<30;
run;

proc freq data=Cleaned_1;
	tables QUINTILE;
	where  ACCESSYN="Y" and NSFASYN="Y" AND RACE in ("A","I");
run;

/* here the modal quintile is 3 */
proc freq data=Cleaned_1;
	tables QUINTILE;
	where  ACCESSYN="N" and NSFASYN="N" AND RACE in ("A","I");
run;

/* here the modal quintile is 5 and 4 */

proc freq data=Cleaned_1;
	tables QUINTILE;
	where  ACCESSYN="N" and NSFASYN="Y" AND RACE in ("A","I");
run;	

/* here modal quintile is 3 and 5 */

proc print data=Cleaned_1 ; 
	where QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)" and ACCESSYN="N" and NSFASYN="Y" and QUINTILE is null;
run;

/* these 20 observations can been given quintile 5 */

proc print data=Cleaned_1 ; 
	where QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)" and ACCESSYN="Y" and NSFASYN="Y" and QUINTILE is null;
run;

proc freq data=Cleaned_1;
	tables ENG NSFASYN ACCESSYN;
WHERE QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)" AND QUINTILE is null;
RUN;

proc freq data=Cleaned_1;
	tables ENG*QUINTILE;
RUN;

/* ENG1N is for the lower quintiles and ENG2,eng1 and eng hn for higher quintiles */

proc freq data=Cleaned_1;
	tables QUINTILE ;
RUN;

/* now we start imputing for missing quintiles for data science related degrees and civil engineering degrees */

proc freq data=Cleaned_1;
	tables QUINTILE ;
WHERE ACCESSYN="Y" and NSFASYN="Y" and ENG in ("ENG1N","ENG2");
RUN;

/* MODE IS 3 */

proc freq data=Cleaned_1;
	tables QUINTILE ;
WHERE ACCESSYN="Y" and NSFASYN="N" and ENG in ("ENG1N","ENG2");
RUN;

/* mode is 3 */

proc freq data=Cleaned_1;
	tables QUINTILE ;
WHERE ACCESSYN="N" and NSFASYN="Y" and ENG in ("ENG1N","ENG2");
RUN;

/* mode is 3 */

proc freq data=Cleaned_1;
	tables QUINTILE ;
WHERE ACCESSYN="N" and NSFASYN="N" and ENG in ("ENG1N","ENG2");
RUN;

/* mode is 4 */

proc freq data=Cleaned_1;
	tables QUINTILE ;
WHERE ACCESSYN="N" and NSFASYN="N" and ENG in ("ENG1","ENGHN");
RUN;

/* mode is 5 */

proc freq data=Cleaned_1;
	tables QUINTILE ;
WHERE ACCESSYN="N" and NSFASYN="Y" and ENG in ("ENG1","ENGHN");
RUN;

/* mode is 5 */

proc freq data=Cleaned_1;
	tables QUINTILE ;
WHERE ACCESSYN="Y" and NSFASYN="N" and ENG in ("ENG1","ENGHN");
RUN;

/* mode is 5  */

proc freq data=Cleaned_1;
	tables QUINTILE ;
WHERE ACCESSYN="Y" and NSFASYN="Y" and ENG in ("ENG1","ENGHN");
RUN;

/* mode is 5 */

data cleaned_2;
set work.cleaned_1;
if missing(QUINTILE) AND ACCESSYN="Y" and NSFASYN="Y" and ENG in ("ENG1N","ENG2") then QUINTILE=3 ;

else if missing(QUINTILE) and ACCESSYN="Y" and NSFASYN="N" and ENG in ("ENG1N","ENG2") then QUINTILE=3;

else if missing(QUINTILE) and ACCESSYN="N" and NSFASYN="Y" and ENG in ("ENG1N","ENG2") then QUINTILE=3;

else if missing(QUINTILE) and  ACCESSYN="Y" and NSFASYN="N" and ENG in ("ENG1N","ENG2") then QUINTILE=4;

else if missing(QUINTILE) and  ACCESSYN="N" and NSFASYN="N" and ENG in ("ENG1","ENGHN") then QUINTILE=5;

else if missing(QUINTILE) and ACCESSYN="N" and NSFASYN="Y" and ENG in ("ENG1","ENGHN") then QUINTILE=5;

else if missing(QUINTILE)and  ACCESSYN="Y" and NSFASYN="N" and ENG in ("ENG1","ENGHN") then QUINTILE=5;

else if missing(QUINTILE)and  ACCESSYN="Y" and NSFASYN="Y" and ENG in ("ENG1","ENGHN") then QUINTILE=5;
;
 
proc freq data=Cleaned_2;
	tables QUINTILE;
	where DEPTNAME like "SCHOOL OF MATHS,STATS &COMP SC";
run;
/* now have 147 observations from 620 for data science degrees */

proc freq data=Cleaned_2;
	tables QUINTILE;
	where QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)";
run;

/* now have 59 missing obs from 280 for civil eng*/

data cleaned_3;
set work.cleaned_2;
if GRADIND=1 then status="Graduated";

else if DROPOUTIND=1 then status="Dropped out";

else if EXCLIND=1 then status="Excluded";

else status="Currently registered";

run;

proc freq data=cleaned_3;
tables status;
run;

/* now we export the cleaned_3 data set to excel and import into R for data visualisation */

proc export 
  data=work.cleaned_3 
  dbms=xlsx 
  outfile="/home/u49229476/cleaned_3.xlsx" 
  replace;
run;

proc freq data=cleaned_3;
tables GENDER*GRADIND/chisq;
where QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)" or DEPTNAME like "SCHOOL OF MATHS,STATS &COMP SC";
run;


/* we now proceed to clean the data to start doing modeling for further analysis */

proc freq data=school_data;
	tables GENDER;
where DEPTNAME like "SCHOOL OF MATHS,STATS &COMP SC";
run;

/* no missing values */
 
proc freq data=school_data;
	tables NSFASYN;
where DEPTNAME like "SCHOOL OF MATHS,STATS &COMP SC";
run;

/* no missing values */

proc freq data=new1;
	tables ENG;
where DEPTNAME like "SCHOOL OF MATHS,STATS &COMP SC";
run;

/* with missing quintiles removed and matric points>20 only have 6 missing obs */

proc freq data=new1;
	tables PERCENTAGE;
where DEPTNAME like "SCHOOL OF MATHS,STATS &COMP SC";
run;

/* with missing quintiles removed and matric points>20 only have 93 missing obs */

proc freq data=new1;
	tables MATRICPOINTS;
where DEPTNAME like "SCHOOL OF MATHS,STATS &COMP SC";
run;

/* no missing observations*/

proc freq data=new1;
	tables QUINTILE;
where DEPTNAME like "SCHOOL OF MATHS,STATS &COMP SC";
run;

/* no missing observations*/

/* The above is for data science related degrees only */

proc freq data=school_data;
	tables GENDER;
where QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)";
run;

/* no missing values */

proc freq data=school_data;
	tables NSFASYN;
where QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)";
run;

/* no missing values */

proc freq data=new1;
	tables ENG;
where QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)";
run;

/* with missing quintiles removed and matric points>20 only have 1 missing obs */

proc freq data=new1;
	tables PERCENTAGE;
where QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)";
run;

/* with missing quintiles removed and matric points>20 only have 24 missing obs */

proc freq data=new1;
	tables MATRICPOINTS;
where QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)";
run;

/* with missing quintiles removed and matric points>20 only have 0 missing obs */


proc freq data=new1;
	tables QUINTILE;
where QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)";
run;

/* with missing quintiles removed and matric points>20 only have 0 missing obs */

data new;
	set work.school_data;
	where not missing(QUINTILE);
run;

data new1;
	set work.new;
	where MATRICPOINTS>20;
run;

data new3;
	set work.new1;
	where not missing(PERCENTAGE);
run;

data new4;
	set work.new3;
	where not missing(ENG);
run;

data civil;
	set work.new4;
	where QUALNAME like "Bachelor of Science in Engineering (Civil Engineering)";
	keep NSFASYN ENG GENDER PERCENTAGE MATRICPOINTS QUINTILE GRANDIND status MINREGYEARS;
run;

data data_sci;
	set work.new4;
	where DEPTNAME like "SCHOOL OF MATHS,STATS &COMP SC";
	keep NSFASYN ENG GENDER PERCENTAGE MATRICPOINTS QUINTILE GRANDIND status MINREGYEARS;
run;

proc append base=civil data=data_sci;
run;

data model_data;
	set work.civil;
	keep NSFASYN ENG GENDER PERCENTAGE MATRICPOINTS QUINTILE GRANDIND status MINREGYEARS;
run;

proc print data=model_data (obs=200);
run;

/* now let us export the data for modelling */

proc export 
  data=work.model_data
  dbms=xlsx 
  outfile="/home/u49229476/Stats project school data" 
  replace;
run;

proc export 
  data=work.civil
  dbms=xlsx 
  outfile="/home/u49229476/civil" 
  replace;
run;

proc export 
  data=work.data_sci
  dbms=xlsx 
  outfile="/home/u49229476/data_science" 
  replace;
run;






