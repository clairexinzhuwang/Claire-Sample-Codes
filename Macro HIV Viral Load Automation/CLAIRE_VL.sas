********************IMPORT previous VL data**************************;

%let path=H:\SOM Work;*change this to your own data path";
%let file=2005_07_2020VLWF.csv;*change this to your previous data file";
    data WORK.VL_PREVIOUS;
      %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
      infile "&path.\&file" delimiter = ',' MISSOVER DSD
 lrecl=32767 firstobs=2 ;
         informat SID best32. ;
         informat DESCRIPTION $27. ;
         informat LAB $19. ;
         informat RESULT_ORIGINAL $20. ;
         informat Viralload_coded best32. ;
         informat NewdeIDlabdate mmddyy10. ;
         informat year best32. ;
         informat month best32. ;
         informat quarter best32. ;
         format SID best12. ;
         format DESCRIPTION $27. ;
         format LAB $19. ;
         format RESULT_ORIGINAL $20. ;
         format Viralload_coded best12. ;
         format NewdeIDlabdate mmddyy10. ;
         format year best12. ;
         format month best12. ;
         format quarter best12. ;
      input
                  SID
                  DESCRIPTION  $
                  LAB  $
                  RESULT_ORIGINAL  $
                  Viralload_coded
                  NewdeIDlabdate
                  year
                  month
                  quarter
      ;
      if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
      run;
proc print data=WORK.VL_previous (obs=20);
run;

*********** Import newest data ***********;


PROC IMPORT OUT= WORK.VL_new                        
                        DATAFILE= "H:\SOM Work\LOGVL_AUG_DEC2020.xlsx" 
                        DBMS=EXCEL REPLACE;
                 GETNAMES=yes;
                 MIXED=no;
                 SCANTEXT=YES;
                 USEDATE=YES;
                 SCANTIME=YES;
				 
            RUN;
	
	
PROC EXPORT DATA= WORK.VL_new
            OUTFILE= "H:\SOM Work\LOGVL_AUG_DEC2020.csv" 
            DBMS=CSV REPLACE;
		
     PUTNAMES=YES;
RUN;


proc print data =WORK.VL_new(obs=20);
run;




data vl_new;
set WORK.VL_new;
if RESULT_ORIGINAL="Not detected" or RESULT_ORIGINAL="Not Detected" or RESULT_ORIGINAL="not detected" then Viralload_coded=0.001; *recoded;
	else if RESULT_ORIGINAL="<1.60" or RESULT_ORIGINAL="< 1.60" then Viralload_coded=1.59;
		else if RESULT_ORIGINAL="> 7.00" then Viralload_coded=7.01;
		else Viralload_coded=RESULT_ORIGINAL;
run;
data vl_new;
set WORK.VL_new;
if month=12 then month=11;
run;

proc print data =vl_new(obs=20);
run;

**************************************** check and clean variables *******************************;
title "QC: whether every records has viral load";
proc means data=vl_new nmiss mean std min max;
vars Viralload_coded ;
run;


title "QC: which viral loads are missing";
proc print  data=vl_new;
where Viralload_coded=. or SID=.;
run;

title "QC: viral loads are from what time period";
proc sql;
select min(NewdeIDlabdate) as from_dt format mmddyy8.,max(NewdeIDlabdate) as to_dt format mmddyy8. from vl_new;
quit;
title;


title "Viralload_coded greater than 7.01";
proc print data=vl_new;
where Viralload_coded gt 7.01;
run;
title;


*print out if Viralload_coded greater than 0.001 less than 1.59; 
title "Viralload_coded greater than 0.001 less than 1.59";
proc print data=vl_new;
where Viralload_coded > 0.001 and Viralload_coded <1.59;
run;
title;

*delete the incorrect records;
data vl_new;
set vl_new;
if Viralload_coded gt 7.01 or Viralload_coded=. or SID=. then delete;
run;
                                                                   
*check year and month;
title "check whether year and month are correct";
proc freq data=vl_new;
tables year*month;
run;
title;


*check recoded match with the original;
data work.match;
set vl_new;
if Viralload_coded ne RESULT_ORIGINAL;
run;
proc freq data=work.match;
tables Viralload_coded*RESULT_ORIGINAL;
run;


************************************** DUPLICATES PROCESS ****************************;
*create a dataset where Viralload_coded are not missing;
data vl_new_1;
set vl_new;
	if Viralload_coded~=.;
run;


********remove same patient with same VL on the same date************;
proc sort nodupkey data=vl_new_1 dupout=dup1;
by SID NewdeIDlabdate Viralload_coded;
run;

title "QC: same patient with same VL on the same date";
	proc print data=dup1;
	run;
title;

************** output different Vl for the same pt on the same date ***************;
proc sort data=vl_new_1;
by SID NewdeIDlabdate;
run;

data vl_new_2;
set vl_new_1;
by SID NewdeIDlabdate;
retain count;
if first.SID or first.NewdeIDlabdate then count=0;
count+1;
run;


title "QC: WARNING!!!!!same patient with greater than 2 VL on the same date";
	proc print data=vl_new_2;
	WHERE COUNT ge 2;
	run;

	data qc;
	set vl_new_2;
	where COUNT ge 2;
	run;
title;

*need to output the samller value of the VL if there are multiple Vl on the same date;

proc sort data=vl_new_2;
by SID NewdeIDlabdate descending Viralload_coded;
run;

proc sort data=vl_new_2 nouniquekeys uniqueout=singles_vl out=dups_vl;
by SID NewdeIDlabdate ;
run;

title "check dups";
proc print data=dups_vl;
run;
title;

data dups_vl_diff;
set dups_vl;
by SID;
diff=lag(Viralload_coded)-Viralload_coded;
if first.SID then do;diff=.;end;
if diff ne . and diff le 0.02 then delete;
run;

title "output smaller value of VL";
proc sort nodupkey data=dups_vl_diff dupout=dup2;
by SID NewdeIDlabdate;
run;
proc print data=dup2;
run;
title;

title "QC:check if the smaller values are output (1 is smallest value/3 is largest)";
proc freq data=dup2;
tables count;
run;
title;

data dup3;
set dup2(drop=year month quarter);
	informat NewdeIDlabdate1 mmddyy10.;
	format NewdeIDlabdate1 mmddyy10.;
	NewdeIDlabdate1=intnx('day',NewdeIDlabdate,-1); 
	year=year(NewdeIDlabdate1);
	month=month(NewdeIDlabdate1);
	if month in (1,2,3) then quarter=1;
	if month in (4,5,6) then quarter=2;
	if month in (7,8,9) then quarter=3;
	if month in (10,11,12) then quarter=4;
	
	drop NewdeIDlabdate;
run;

title "check dup3 if date is moved one day prior";
proc print data=dup3;
run;
title;

data vlmerge;
set singles_vl dups_vl_diff dup3(rename=NewdeIDlabdate1=NewdeIDlabdate);
drop count diff;
run;

*******check lastly to see if there is duplicates***********;
data vlmerge_check;
set vlmerge;
run;

proc sort nodupkey data=vlmerge_check dupout=qc;
by SID NewdeIDlabdate ;
run;
proc print data=qc;
run;

proc sort nodupkey data=vlmerge dupout=dup4;
by SID NewdeIDlabdate Viralload_coded;
run;


title "QC: same patient with same VL on the same date";
	proc print data=dup4;
	run;
title;

************** output different Vl for the same pt on the same date ***************;
proc sort data=vlmerge;
by SID NewdeIDlabdate;
run;

data vl_merge_2;
set vlmerge;
by SID NewdeIDlabdate;
retain count;
if first.SID or first.NewdeIDlabdate then count=0;
count+1;
run;


title "QC: WARNING!!!!!same patient with > 2 VL on the same date";
	proc print data=vl_merge_2;
	WHERE COUNT > 2;
	run;

	data qc;
	set vl_merge_2;
	where COUNT > 2;
	run;
title;

*need to output the samller value of the VL if there are multiple Vl on the same date;

proc sort data=vl_merge_2;
by SID NewdeIDlabdate descending Viralload_coded;
run;

proc sort data=vl_merge_2 nouniquekeys uniqueout=singles_vl out=dups_vl_2;
by SID NewdeIDlabdate ;
run;

title "check duplicate records";
proc print data=dups_vl_2;
run;
title;

data dups_vl_diff_2;
set dups_vl_2;
by SID;
diff=lag(Viralload_coded)-Viralload_coded;
if first.SID then do;diff=.;end;
if diff ne . and diff le 0.02 then delete;
run;

proc sort nodupkey data=dups_vl_diff dupout=dup5;
by SID NewdeIDlabdate;
run;


title "QC:check if the smaller values are output (1 is smallest value/3 is largest)";
proc freq data=dup5;
tables count;
run;
title;

data dup6;
set dup5(drop=year month quarter);
	informat NewdeIDlabdate1 mmddyy10.;
	format NewdeIDlabdate1 mmddyy10.;
	NewdeIDlabdate1=intnx('day',NewdeIDlabdate,-1); 
	year=year(NewdeIDlabdate1);
	month=month(NewdeIDlabdate1);
	if month in (1,2,3) then quarter=1;
	if month in (4,5,6) then quarter=2;
	if month in (7,8,9) then quarter=3;
	if month in (10,11,12) then quarter=4;
	
	drop NewdeIDlabdate;
run;

title "check dup3 if date is moved one day prior";
proc print data=dup6;
run;
title;

data vlmerge;
set singles_vl dups_vl_diff_2 dup6(rename=NewdeIDlabdate1=NewdeIDlabdate);
drop count diff;
run;


*************************** combine the files and please check the numbers of combined ***************************;
data vlfinal;
set WORK.VL_previous vlmerge;
run;
%let path=H:\SOM Work;*change this to your own data path";
%let year=2020; *change to the year of data";
%let month=12;  *change to the season of data";
proc export data=vlfinal
dbms=csv
outfile="&path.\2005_&month._&year.VLWF.csv"; *change the path to whatever you like;
run;


********************************************************************************************************
*********************************frequency tables of VL for recent three years**************************;

%let year1=2018;*change to 2 years before;
%let year2=2019;*change to 1 year before;
%let year3=2020;*change to the year of the data;
*generate data for each month of each year;
%macro Viralload (year);
	data a; *filter to certain year and month;
	set vlfinal;
	if year=&year;
	run;

	data a;
	set a;
	if Viralload_coded~=.; *removing missing viral load;
	month_name=put(NewdeIDlabdate, monname.);
	run;

	proc sort data=a ; *check if there is any duplicate records for the same pts on the same date;
	by SID NewdeIDlabdate;
	run;

	proc sql;
    create table freq&year as
	select month, month_name, count(SID) as total_vl&year
    from a
	group by month, month_name
    order by month, month_name; 
	quit;

**************************vl le 2.3 **********************;
data vl_le;
set a;
if Viralload_coded le 2.3;
run;

	proc sql;
    create table freq_vl_le&year as
	select month, month_name, count(SID) as vl_le&year
    from vl_le
	group by month, month_name
    order by month, month_name; 
	quit;
title"vl less or equal to 2.3 for year &year";
proc print data=freq_vl_le&year ;
run;
title; 

**************************vl gt 2.3 **********************;
data vl_gt;
set a;
if Viralload_coded gt 2.3;
run;

	proc sql;
    create table freq_vl_gt&year as
	select month, month_name, count(SID) as vl_gt&year
    from vl_gt
	group by month, month_name
    order by month, month_name; 
	quit;
title"vl greater than 2.3 for year &year";
proc print data=freq_vl_gt&year ;
run;
title;

***************************vl le 2.3 in total in percent ***************;

data pct&year;
merge freq&year freq_vl_le&year freq_vl_gt&year;
format pct_le_&year pct_gt_&year percent8.2;
pct_le_&year=vl_le&year/total_vl&year;
pct_gt_&year=vl_gt&year/total_vl&year;
run;
title "percentage of VL in total";
proc print;
run;
title;

**************** transpose ***************;

proc transpose data=pct&year(drop=month)
				out=vl&year
				name=vl_year 
;
var total_vl&year vl_le&year pct_le_&year vl_gt&year pct_gt_&year;
id month_name;
run;



%mend Viralload;
%Viralload(&year1.);
%Viralload(&year2.);
%Viralload(&year3.);


************** output frequency tables ****************;

data frequency_vertical;
merge pct&year1. pct&year2. pct&year3.;

run;
proc print data=frequency_vertical;
run;


proc export data=frequency_vertical
dbms=csv
outfile="&path.\2005_&month._&year.VL_frequency.csv"; *change the path name to your path;
run;
proc import datafile = "&path.\2005_&month._&year.VL_frequency1.csv" out = frequency_new
DBMS =  csv replace;
run;

data frequency_vertical1;
set frequency_vertical;
format pct_le_2020 best.;
run;
data frequency_vertical2;
set frequency_vertical1;
pct_le_2020_exclude=pct_le_2020;
if pct_le_2020_exclude<0.73 then pct_le_2020_exclude=.;
run;
data frequency_vertical3;
set frequency_vertical2;
format pct_le_2020 percent8.2;
format pct_le_2020_exclude percent8.2;
run;


*******************************************  graphs **************************************************;

**********************last three years graph with the corresponding months of the year***********************;
**********************in the graph, display total num of vl and vl gt 2.3, and vl le 2.3  for last three years **********;

ods rtf file="&path.\VL graphic summary report_&month.&year..rtf";

title "Overall log10(VL) Number Over Year &year1., &year2., and &year3.";
ods startpage=no;
proc sgplot data=frequency_vertical;
xaxis type=discrete;
  series x=month_name y=total_vl&year1.;
  series x=month_name y=total_vl&year2.;
  series x=month_name y=total_vl&year3.;
  yaxis label="Overall log10(VL) Number";
 run;
 proc print data=frequency_vertical;
 var total_vl&year1. total_vl&year2. total_vl&year3.;
 run;
 title;
ods rtf startpage=now;
ods startpage=no;
 title  "log10(VL) Numbers Less or Equal to 2.3 Over Year &year1., &year2., and &year3.";
proc sgplot data=frequency_vertical;
xaxis type=discrete;
  series x=month_name y=vl_le&year1.;
  series x=month_name y=vl_le&year2.;
  series x=month_name y=vl_le&year3.;
  yaxis label="log10(VL) Numbers Less or Equal to 2.3";
 run;
 proc print data=frequency_vertical;
 var vl_le&year1. vl_le&year2. vl_le&year3.;
 run;
 title;
ods rtf startpage=now;
ods startpage=no;
  title  "log10(VL) Numbers Greater Than 2.3 Over Year &year1., &year2., and &year3.";
proc sgplot data=frequency_vertical;
xaxis type=discrete;
  series x=month_name y=vl_gt&year1.;
  series x=month_name y=vl_gt&year2.;
  series x=month_name y=vl_gt&year3.;
  yaxis label="log10(VL) Numbers Greater Than 2.3";
 run;
 proc print data=frequency_vertical;
 var vl_gt&year1. vl_gt&year2. vl_gt&year3.;
 run;
 title;
 ods rtf startpage=now;
 ods startpage=no;
  title  "log10(VL) Numbers Over Year &year1., &year2., and &year3.";
proc sgplot data=frequency_vertical;
xaxis type=discrete;
 series x=month_name y=vl_le&year1.;
  series x=month_name y=vl_le&year2.;
  series x=month_name y=vl_le&year3.;
  series x=month_name y=vl_gt&year1.;
  series x=month_name y=vl_gt&year2.;
  series x=month_name y=vl_gt&year3.;
  yaxis label="log10(VL) Numbers";
 run;
 proc print data=frequency_vertical;
 var vl_le&year1. vl_le&year2. vl_le&year3. vl_gt&year1. vl_gt&year2. vl_gt&year3.;
 run;
 title;
ods rtf startpage=now;
ods startpage=no;

  title  "Percent of log10(VL) Numbers less or equal to 2.3 Over Year &year1., &year2., and &year3.";
proc sgplot data=frequency_vertical;
xaxis type=discrete;
series x=month_name y=pct_le_&year1.;
  series x=month_name y=pct_le_&year2.;
  series x=month_name y=pct_le_&year3.;
yaxis label="Percent of log10(VL) Numbers less or equal to 2.3";
 run;
 proc print data=frequency_vertical;
 var pct_le_&year1. pct_le_&year2. pct_le_&year3.;
 run;
 title;
ods rtf startpage=now;
ods startpage=no;

title  "Linear Regression of Percent of log10(VL) Numbers less or equal to 2.3 Over Year &year1., &year2., and &year3. (April 2020 and May 2020 Outliers Included)" ;
proc sgplot data=frequency_vertical3;
xaxis label='Month' values=(1 to 12);

reg x=month y=pct_le_&year1./legendlabel='pct_le_&year1.';
  reg x=month y=pct_le_&year2./legendlabel='pct_le_&year2.';
  reg x=month y=pct_le_&year3./legendlabel='pct_le_&year3.';
  yaxis label="Linear Regression of Percent of log10(VL) Numbers less or equal to 2.3 (including outliers)";
 run;
 proc print data=frequency_vertical3;
 var pct_le_&year1. pct_le_&year2. pct_le_&year3.;
 run;
 title;
ods rtf startpage=now;
ods startpage=no;

 title  "Linear Regression of Percent log10(VL) Numbers less or equal to 2.3 Over Year &year1., &year2., and &year3. (April 2020 and May 2020 Outliers Excluded)";
proc sgplot data=frequency_vertical3;
xaxis label='Month_name' values=(1 to 12);

reg x=month y=pct_le_&year1./legendlabel='pct_le_&year1.';
  reg x=month y=pct_le_&year2./legendlabel='pct_le_&year2.';
  reg x=month y=pct_le_&year3./legendlabel='pct_le_&year3.';
  reg x=month y=pct_le_&year3._exclude/legendlabel='pct_le_&year3._Outliers_Deleted';
  yaxis label="Linear Regression of Percent of log10(VL) Numbers less or equal to 2.3 (excluding outliers)";
 run;
 proc print data=frequency_vertical3;
 var pct_le_&year1. pct_le_&year2. pct_le_&year3. pct_le_&year3._exclude ;
 run;

 title;


ods rtf close;


 


