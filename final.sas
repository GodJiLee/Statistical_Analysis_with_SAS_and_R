proc import datafile = "C://Users//leejiwon//Desktop//train.csv" dbms = csv out = train;
run;
proc print data = train (obs = 10);
run;
proc means data = train N mean std var sum min max maxdec = 2;
	var staff -- ect;
run;

proc corr data = train cov;
run;
 
proc princomp data = train out = pcscores covariance;
	var staff -- ect;
	title 'raw data and the sample covariance matrix';
run;

proc print data = pcscores;
	var Prin1 -- Prin10;
run;

proc univariate data = pcscores plot;
	var Prin1--Prin2;
run;

proc gplot data = pcscores;
	plot Prin2 * Prin1;
run;

proc g3d data = pcscores;
	scatter Prin1 * Prin2 = Prin3;
	scatter Prin1 * Prin2 = Prin3 / rotate = 30;
run;








proc import datafile = "C://Users//leejiwon//Desktop//train.csv" dbms = csv out = train;
run;
proc print data = train (obs = 10);
run;
proc factor data = train method = prin nfactors = 3 scree preplot rotate = promax reorder;
	var staff -- ect;
run;




proc import datafile = "C://Users//leejiwon//Desktop//train.csv" dbms = csv out = train;
run;
proc print data = train (obs = 10);
run;

title1 'Cluster Analysis using Single Linkage';
proc cluster data=train method=sin outtree=tree1;
	id language;
run;
proc tree data=tree1;
run;

title 'Cluster Analysis using Complete Linkage';
proc cluster data=lang method=com outtree=tree2 ;
	id language;
run;

proc tree data=tree2;
run; 
 
title 'Cluster Analysis using Average Linkage';
proc cluster data=lang method=ave outtree=tree3;
	id language;
run;
proc tree data=tree3;
run; 
 
 




proc factor data = train nfact = 3 score out = scoreout outstat = statout rotate = varimax reorder score scree;
	var staff -- ect;
run;
