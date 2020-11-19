/* 실습 예제 #5 - 정준상관분석 */

/* 예제 1 - 강의자료 */
title 'Canonical Correlation Analysis';
data skull (type = corr);
	_type_ = 'CORR';
	input _name_$ x1 x2 y1 y2;
	cards;
	x1		1.0		.			.			.   
	x2		0.505	1.0		.			.   
	y1 	0.569 	0.422 	1.0 		.   
	y2		0.602	0.467	0.926 	1.0
;
proc cancorr data = skull vprefix = head wprefix = leg;
	var x1 x2;
	with y1 y2;
run;


/* 예제 2 - [프로그램 9.1] */
proc import datafile = 'C:\Users\samsung\Desktop\chem.csv' out = chem dbms = csv replace;
run;

proc cancorr data = chem out = chem1
	vprefix = u vname = 'conditions'
	wprefix = v wname = 'products';
	var x1 x2 x3;
	with y1 y2;
run;
proc print data = chem1;
run; 



/* 예제 3 - diabetes data */
proc import datafile = 'C:\Users\samsung\Desktop\diabetes.csv' out = diabetes dbms = csv replace;
run;
proc print data = diabetes (obs = 10);
run;

proc corr data = diabetes out = diabetes_corr;
run;
proc print data = diabetes_corr;
run;

data diabetes_corr_data;
	set diabetes_corr;
	if _type_ = 'CORR';
	drop _type_;
run;
proc print data = diabetes_corr_data;
run;

proc cancorr data = diabetes_corr_data out = diabetes_cancorr 
vprefix = u wprefix = v;
	var x1 - x3;
	with y1 y2;
run;
