*Part A-Simple;

data summary1(keep=game gamescore halfscores: holescores:);
p1 = 1/12; p2 = 3/12; p3 = 2/12; p4 = 2/12; p5 = 3/12; p6 = 1/12;
  array scores(6) (-1,0,1,2,3,4);
  array p (6) p1-p6;
  

  do game = 1 to 1000;


    length gamescore 8;
    array halfscores(2);     
    array holescores(2, 9);   

    call missing (of holescores(*));
    call missing (of halfscores(*));

    do half = 1, 2;
      do hole = 1 to 9;

        minscore = 5;*make sure it is larger than 4;

        do die = 1 to 10-hole;
          score = scores(rantbl(123456, of p(*)));
          minscore = min(minscore, score);
        end;

        holescores(half,hole) = minscore;
        halfscores(half) + minscore;
      end;
    end;
    gamescore = sum (of halfscores(*));
    output;
  end;
run;

proc means data=work.summary1;
var gamescore;
output out=work.simple mean=simple;
run;


*Part A-Modification1;

data work.summary2 (keep=game gamescore halfscores: holescores:);
	do game = 1 to 1000;
		gamescore=0;
		array halfscores(2) halfscores1-halfscores2;
		do d = 1 to 2;
				array holescores (9) holescores1-holescores9;
				do i = 1 to 9;
					score=rantbl(123456,1/12,3/12,2/12,2/12,3/12,1/12)-2;
					holescores(i)=score;
					gamescore=gamescore+score;
				end;
			halfscores(d)=sum(of holescores1-holescores9);
		end;
	     gamescore=halfscores1+halfscores2;
		output;
	end;
run;
proc means data=work.summary2;
	var gamescore;
	output out=work.modification1 mean=modification1;
run;

*Part A-Modification2;


 data summary3(keep=game gamescore halfscores: smallest:);
p1 = 1/12; p2 = 3/12; p3 = 2/12; p4 = 2/12; p5 = 3/12; p6 = 1/12;
array die(9) die1-die9;
array smallest(9) smallest9-smallest1;
array halfscores(2) halfscores1-halfscores2;
do game=1 to 1000;
do s=1 to 2;
rollnum=9;

do q=1 to 9;
smallest(q)=.;
end;
do while (rollnum>0);
count=0;
do i=1 to rollnum;
die_score=rantbl(123456,p1,p2,p3,p4,p5,p6)-2;

die(i)=die_score;
if die(i)=-1 then count=count+1;
if count ne 0 then smallest(rollnum)=-1*count;
 else smallest(rollnum)=min(of die1-die9);
 end;
 if min(of die1-die9)=-1 then rollnum=rollnum-count;
 else rollnum=rollnum-1;
 do z=1 to 9;
die(z)=.;
end;
end;
 halfscores(s)=sum(of smallest1-smallest9);
 end;
gamescore=halfscores1+halfscores2;
output;
end;
run;


proc means data=work.summary3;
	var gamescore;
	output out=work.modification2 mean=modification2;
run;




***========================================================***;
* PART B;

%macro GOLO (strategy,numgame,seed,lib, dsn, output);
data &lib..&dsn;
%if &strategy=simple %then %do;
p1 = 1/12; p2 = 3/12; p3 = 2/12; p4 = 2/12; p5 = 3/12; p6 = 1/12;
  array scores(6) (-1,0,1,2,3,4);
  array p (6) p1-p6;

  do game =1 to &numgame;
    length gamescore 8;
    array halfscores(2);     
    array holescores(2, 9);   

    call missing (of holescores(*));
    call missing (of halfscores(*));

    do half = 1, 2;
      do hole = 1 to 9;

        minscore = max (of scores(*)) + 1;*make sure it is larger than 5;

        do die = 1 to 10-hole;
          score = scores(rantbl(&seed, of p(*)));
          minscore = min(minscore, score);
        end;

        holescores(half,hole) = minscore;
        halfscores(half) + minscore;
      end;
    end;
    gamescore = sum (of halfscores(*));
    output;
  end;
run;
%end;
%if &strategy=modification1 %then %do;
	do game =1 to &numgame;
		gamescore=0;
		array halfscores(2) halfscores1-halfscores2;
		do d = 1 to 2;
				array holescores (9) holescores1-holescores9;
				do i = 1 to 9;
					score=rantbl(&seed,1/12,3/12,2/12,2/12,3/12,1/12)-2;
					holescores(i)=score;
					gamescore=gamescore+score;
				end;
			halfscores(d)=sum(of holescores1-holescores9);
		end;
	     gamescore=halfscores1+halfscores2;
		output;
	end;
run;
%end;

%if &strategy=modification2 %then %do;
p1 = 1/12; p2 = 3/12; p3 = 2/12; p4 = 2/12; p5 = 3/12; p6 = 1/12;
array die(9) die1-die9;
array smallest(9) smallest9-smallest1;
array halfscores(2) halfscores1-halfscores2;
do game=1 to &numgame;
do s=1 to 2;
rollnum=9;

do q=1 to 9;
smallest(q)=.;
end;
do while (rollnum>0);
count=0;
do i=1 to rollnum;
die_score=rantbl(&seed,p1,p2,p3,p4,p5,p6)-2;

die(i)=die_score;
if die(i)=-1 then count=count+1;
if count ne 0 then smallest(rollnum)=-1*count;
 else smallest(rollnum)=min(of die1-die9);
 end;
 if min(of die1-die9)=-1 then rollnum=rollnum-count;
 else rollnum=rollnum-1;
 do z=1 to 9;
die(z)=.;
end;
end;
 halfscores(s)=sum(of smallest1-smallest9);
 end;
gamescore=halfscores1+halfscores2;
output;
end;
run;


%end;
title "Macro running Dataset &dsn with &seed seed,&strategy strategy, &numgame times of game and a &output output. ";
%if &output=Yes %then %do;
proc means data=&lib..&dsn ;
var gamescore;
run;
%end;
%mend GOLO;
options mprint symbolgen mlogic;
%GOLO (simple,1000,123456,work,GOLO,Yes);
%GOLO (modification1,1000,123456,work,GOLO,Yes);
%GOLO (modification2,1000,123456,work,GOLO,Yes);



