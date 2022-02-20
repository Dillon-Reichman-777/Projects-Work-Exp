/* HERE WE CREATE DATA FOR SURVIVAL MODEL */

PROC IMPORT DATAFILE="/home/u49229476/CLEANED_NEW.xlsx"
		    OUT=cleaned
		    DBMS=XLSX
		    REPLACE;
RUN;

proc print data=cleaned (obs=100);
run;

data new10;
set work.cleaned;
where status in ("Graduated" "Currently");
run;

data new11;
set work.new10;
if status="Graduated" then event=1;
else if status ne "Graduated" then event=0;
run;

data new12;
set work.new11;
if QUINTILE in (1 2 3) then new_quintile="lower";
else if QUINTILE in(4 5) then new_quintile="upper";
else if QUINTILE=6 then new_quintile="missing quintile";
run;

proc freq data=new12;
tables new_quintile;
run;

data new13;
set work.new12;
where MINREGYEARS GE 0;
run;
data new14;
set work.new13;
years=MINREGYEARS+1;
RUN;

PROC FREQ DATA=new14;
tables years;
run;


proc export 
  data=work.new13
  dbms=xlsx 
  outfile="/home/u49229476/FINAL_SURVIVALDATA" 
  replace;
run;

PROC FREQ DATA=new13;
TABLES MINREGYEARS;
RUN;

/* we now convert the data to long format(person-period format) */


data time;
retain time1-time6 0;
array dd[6] time1-time6;
set work.new14;
if years=7 then do time=1 to 6;
	y=0;dd[time]=1;
	output;
	dd[time]=0;
end;
else do time=1 to years;
if time=years THEN y=1;
	else y=0;
	dd[time]=1;
	output;
	dd[time]=0;
end;
run;

proc export 
  data=work.time
  dbms=xlsx 
  outfile="/home/u49229476/FINAL_SURVIVALDATA3" 
  replace;
run;



