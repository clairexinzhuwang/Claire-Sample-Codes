*Read Data from your directory and Labeling;
DATA whas; 
  set 'C:\Users\china\Downloads\whas500.sas7bdat'; 
RUN;

*rename av3 to av since proc format doesn't allow the variable name end with a number;
data whas1;
set whas;
rename av3=av;
run;

*label variable names;
data whas1;
set whas1;
label gender = "Gender"
cvd = "History of CVD" 
afb="Atrial Fibrillation"
sho = "Cardiogenic Shock"
chf = "Congestive Heart Complications"
av = "Complete Heart Block"
miord = "MI Order"
mitype = "MI Type"
age = "Age (year)"
hr = "Heart Rate (bpm)"
sysbp = "Systolic Blood Pressure (mmHg)"
diasbp = "Diastolic Blood Pressure (mmHg)"
bmi = "Body Mass Index (kg/m^2)"
;
run;

*convert survival time to years;
data t51; set whas1;
lf = lenfol / 365.25; 
run;

*Start the Macro; 
*Call the macro with whas500 data as a demonstration;
*Continuous variables is divided into tertiles (3 groups), if you would like to change it to other groups, modify the group in proc rank;
*Categorical variables in this macro has two levels;
%macro table1 (data=t51, time=lf, censor=fstat, catevar=gender cvd afb sho chf av miord mitype, contvar=age hr sysbp diasbp bmi, datout=demo);

proc datasets; delete &datout.;run; quit;

%if &catevar.^= %then %do;
%let nvarlist_cat=%sysfunc(countw(&catevar.));
%do i=1 %to &nvarlist_cat.;
%let var&i.=%scan(&catevar.,&i.);

proc sql;
create table dat_&i. as
   select distinct &&var&i..
   from &data.;
quit;
 
proc transpose data=dat_&i. out=dat_&i._1;
run;

data dat_&i._2(drop=_NAME_);
set dat_&i._1;
run;

proc append base=&datout. data=dat_&i._2; run;
%end;

proc sort data=&datout. out=&datout._5;
by _LABEL_;
run;

proc transpose data=&datout._5
   out=&datout._1 (rename=(_label_=Variable col1=Categories) drop=_NAME_);
   var COL:;
  by _label_;
run;

data &datout._2;
set &datout._1;
if categories=. then delete;
run;



%let nvarlist_cat=%sysfunc(countw(&catevar.));
%do i= 1 %to &nvarlist_cat.;
%let var&i.=%scan(&catevar.,&i.);

ODS TRACE ON;
ODS OUTPUT CensoredSummary = _censorsum&i.
 HomTests=_homtest&i.;
proc lifetest data = &data.;
strata  &&var&i.. / test=(logrank) nodetail;
time &time.*&censor.(0);
run;
ODS OUTPUT CLOSE;
ODS TRACE OFF; 


data censorsum&i.;
	set _censorsum&i. end=last;
	if not last then output;
run;

proc sql;
  create table dat_&i._1 as
  select   
  catx(', ',failed,total) as events 
  from censorsum&i.;
quit;



proc sql;
  create table dat_&i._2 as
  select   
  ProbChiSq as log_rank_p
  from _homtest&i.;
quit;

data dat_&i._3;
merge dat_&i._1 dat_&i._2;
run;


%if &i.=1 %then %do;
data append_dt;
set dat_&i._3;
run;
%end;
%else %do;
data append_dt;set append_dt dat_&i._3;
run;
%end;

data &datout._6(keep=_label_ rename=(_label_=Variable));
set &datout.;
do i=1 to 2;
output;
end;
run;

data &datout._7;
merge &datout._6 append_dt;
run;

proc sort data=&datout._7;
by variable;
run;

proc sort data=&datout._2;
by variable;
run;

data middle;
merge &datout._2 &datout._7;
run;

%end;


%let nvarlist_cat=%sysfunc(countw(&catevar.));
%do i=1 %to &nvarlist_cat.;
%let var&i.=%scan(&catevar.,&i.);

proc sort data = &data.;
       by &&var&i..;
     run;


 ods listing close;
     ods output quartiles = _q_&i. (where = (percent=50));
     proc lifetest data = &data.;
       time &time.*&censor.(0);
	   by &&var&i..;
	   
     run;



data _q_&i.;
     set _q_&i.;
	 Estimate=round(Estimate, 0.01);
     LowerLimit=round(LowerLimit,0.01);
	 UpperLimit=round(UpperLimit, 0.01);
     run;


   proc sql;
   create table out_&i. as
   select
   cat(estimate,' (',cat(LowerLimit, ', ', UpperLimit),')') as Median_Survival_Time 
   from _q_&i.;
   quit;
   

   %if &i.=1 %then %do;
data append_median;
set out_&i.;
run;
%end;
%else %do;
data append_median;set append_median out_&i.;
run;
%end;



   data &datout._9;
   merge &datout._6 append_median;
   run;



proc sort data=&datout._9;
   by variable;
   run;

   
   data &datout._3;
   merge middle &datout._9;
   run;
 
   
  %end;

%end;



%if &contvar.^= %then %do;

proc rank data=&data. out=&data._1
GROUPS=3;
var &contvar.;
ranks &contvar.;
run;


%let nvarlist_cat=%sysfunc(countw(&contvar.));
%do i=1 %to &nvarlist_cat.;
%let var&i.=%scan(&contvar.,&i.);

proc sql;
create table dat_&i. as
   select distinct &&var&i..
   from &data._1;
quit;
 
proc transpose data=dat_&i. out=dat_&i._1;
run;

data dat_&i._2(drop=_NAME_);
set dat_&i._1;
run;


%if &i.=1 %then %do;
data con1;
set dat_&i._2;
run;
%end;
%else %do;
data con1;set con1 dat_&i._2;
run;
%end;



%end;
proc sort data=con1 out=con2;
by _LABEL_;
run;

proc transpose data=con2
   out=&datout._cont_1 (rename=(_label_=Variable col1=Categories) drop=_NAME_);
   var COL:;
  by _label_;
run;

data &datout._cont_2;
set &datout._cont_1;
if categories=. then delete;
run;


%let nvarlist_cat=%sysfunc(countw(&contvar.));
%do i=1 %to &nvarlist_cat.;
%let var&i.=%scan(&contvar.,&i.);

ODS TRACE ON;
ODS OUTPUT CensoredSummary =_censorsum&i.
 HomTests=_homtest&i.;
proc lifetest data = &data._1;
strata  &&var&i.. / test=(logrank) nodetail;
time &time.*&censor.(0);
run;
ODS OUTPUT CLOSE;
ODS TRACE OFF; 


data censorsum&i.;
	set _censorsum&i. end=last;
	if not last then output;
run;


proc sql;
  create table dat_&i._1 as
  select   
  catx(', ',failed,total) as events 
  from censorsum&i.;
quit;



proc sql;
  create table dat_&i._2 as
  select   
  ProbChiSq as log_rank_p
  from _homtest&i.;
quit;

data dat_&i._3;
merge dat_&i._1 dat_&i._2;
run;


%if &i.=1 %then %do;
data &datout._cont_1;
set dat_&i._3;
run;
%end;
%else %do;
data &datout._cont_1;set &datout._cont_1 dat_&i._3;
run;
%end;

data &datout._cont_6(keep=_label_ rename=(_label_=Variable));
set con1;
do i=1 to 3;
output;
end;
run;

data &datout._cont_7;
merge &datout._cont_6 &datout._cont_1;
run;

proc sort data=&datout._cont_7;
by variable;
run;

proc sort data=&datout._cont_2;
by variable;
run;

data &datout._cont_4;
merge &datout._cont_2 &datout._cont_7;
run;


%end;

%let nvarlist_cat=%sysfunc(countw(&contvar.));
%do i=1 %to &nvarlist_cat.;
%let var&i.=%scan(&contvar.,&i.);

 proc sort data = &data._1;
       by &&var&i..;
     run;
   
     ods listing close;
     ods output quartiles = _q_&i. (where = (percent=50));
     proc lifetest data = &data._1;
       time &time.*&censor.(0);
	   by &&var&i..;
	   survival stderr out = _s_&i. 
       (where = ( 0< survival <1 & sdf_stderr ~=. )) ;
     run;

	 data _q_&i.;
     set _q_&i.;
	 Estimate=round(Estimate, 0.01);
     LowerLimit=round(LowerLimit,0.01);
	 UpperLimit=round(UpperLimit, 0.01);
     run;


   proc sql;
   create table out_&i. as
   select
   cat(estimate,' (',cat(LowerLimit, ', ', UpperLimit),')') as Median_Survival_Time
   from _q_&i.;
   quit;
      
  %if &i.=1 %then %do;
data &datout._cont_8;
set out_&i.;
run;
%end;
%else %do;
data &datout._cont_8;set &datout._cont_8 out_&i.;
run;
%end;

   data &datout._cont_9;
   merge &datout._cont_6 &datout._cont_8;
   run;

   proc sort data=&datout._cont_9;
   by variable;
   run;

   proc sort data=&datout._cont_4;
   by variable;
   run;

   data &datout._cont_3;
   merge &datout._cont_4 &datout._cont_9;
   run;

  %end;

%end;

proc sort data=&datout._3;
by Variable;
run;

proc sort data=&datout._cont_3;
by Variable;
run;

data final;
set &datout._3 &datout._cont_3;
run;

proc print data=final;
run;

%mend;
%table1; /*call the macros*/

