/*실습 예제 #3 - 주성분 분석*/

/*1. 주성분 분석*/
/*1.1 예제 1*/

proc import datafile = 'C:\Users\uos\Desktop\applicant.csv' dbms = csv out = applicant replace;
run;
proc print data = applicant (obs = 10);
run;

proc means data = applicant N mean std var sum min max maxdec = 2;
	var fl -- suit;
run;

proc corr data = applicant cov;
run;

proc princomp data = applicant out = pcscores covariance;
	var fl -- suit;
	title 'PCon raw data and the sample covariance matrix';
run;

proc print data = pcscores;
	var Prin1 -- Prin15;
run;

proc univariate data = pcscores plot;
	var Prin1 -- Prin4;
run;

proc gplot data = pcscores;
	plot Prin2 * Prin1;
run;

proc g3d data = pcscores;
	scatter Prin1 * Prin2 = Prin3;
	scatter Prin1 * Prin2 = Prin3 / rotate = 30;
run;


/*1.2 예제2 [프로그램 7.1]*/
proc import datafile = 'C:\Users\uos\Desktop\headsize.csv' dbms = csv out = headsize;
run;
proc print data = headsize (obs = 10);
run;

proc means data = headsize N mean std var sum min max maxdec = 2;
run;
proc corr data = headsize;
run;

proc princomp data = headsize out = headsize_comp covariance;
run;
/* sorted by Prin1*/
proc sort data = headsize_comp;
	by Prin1;
run;
proc print data = headsize_comp;
	var Prin1 Prin2 first_son second_son;
run;
proc univariate data = headsize_comp plot;
run;

proc gplot data = headsize_comp;
	plot second_son * first_son;
run;
/*principal component graph*/
proc gplot data = headsize_comp;
	plot Prin2 * Prin1;
run;

/*principal component analysis with Covariance matrix*/
proc princomp data = headsize out = headsize_cov COV;
	var first_son second_son;
run;


/* [프로그램 7.2] 심리자료에 대한 주성분 분석 (예제 7.5)*/
data pschy;
	infile 'C:\Users\uos\Desktop\pschy.csv' dlm = ',' firstobs = 2;
	input gender x1 x2 x3 x4;

proc princomp out = pschy_out COV;
	by gender;
	var x1 x2 x3 x4;
run;

proc princomp out = pschy_all COV;
	var x1 x2 x3 x4;
run;

data male;
	set pschy_out;
	if gender = 1;
run;

%plotit(data = male, labelvar = gender, plotvars = Prin2 Prin1, color = black, colors = blue)
run;

data female;
	set pschy_out;
	if gender = 2;
run;

%plotit(data = female, labelvar = gender, plotvars = Prin2 Prin1, color = black, colors = blue)
run;
