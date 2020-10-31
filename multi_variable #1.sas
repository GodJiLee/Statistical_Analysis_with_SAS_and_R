/*1*/
proc import datafile = "C:\Users\samsung\Desktop\sweat.csv" dbms =csv
	replace out = sweat; 
	getnames = yes; 
run; 

proc print data = sweat; 
run;


proc means data= sweat; 
	var sweatrate; 
run;


proc univariate data = sweat mu0=4; 
	var sweatrate; 
run; 

proc ttest data = sweat h0 = 4; 
	var sweatrate; 
run;


/*2*/
proc means data= sweat; 
	var sweatrate sodium potassium; 
run; 


proc corr data= sweat cov outp = sweat_corr; 
	var sweatrate sodium potassium; 
run;


data sweat_cov_mat; 
	set sweat_corr;
	if _type_ = 'COV'; 
	drop _character_; 
run; 


proc iml; 
	mu = {0.64, -4.6, -0.035}; 
	use sweat_cov_mat; 
	read all into cov_mat; 
	mu_t = mu`;
	inv_cov = inv(cov_mat); 
	T = 20 * mu_t * inv_cov * mu; 
	q1 = 19 * 3 / (20 - 3) * finv(.95, 3, 17); 
	q2 = 19 * 3 / (20 - 3) * finv(.90, 3, 17);
	print cov_mat, inv_cov, mu, T, q1, q2; 
run;


data sweat_h; 
	set sweat;
	nx1 = sweatrate -4; 
	nx2 = sodium - 50;
	nx3 = potassium - 10; 
run; 

proc print data = sweat_h; 
run; 


proc glm data = sweat_h; 
	model nx1 nx2 nx3 = / ; 
	manova h = intercept; 
run; 


proc iml; 
	T = 19 * (1-0.66112774)/0.66112774; 
	print T; 
run;


proc G3D data=sweat;
	scatter sodium * sweatrate = potassium;
run;


/*3*/
proc import datafile = 'C:\Users\samsung\Desktop\engineer.csv' 
out = engineer replace; 

proc import datafile = 'C:\Users\samsung\Desktop\pilot.csv' 
out = pilot replace; 
run;


proc corr data = engineer cov outp = out_engineer;
proc corr data = pilot cov outp = out_pilot; 
run; 

data out_engineer_pilot ; 
	set out_engineer out_pilot; 
run;


proc iml ; 
	use out_engineer_pilot ; 
	p = 1:2 ; 
	q = 8 : 9 ; 
	r = 3 ;
	s = 10 ; 
	read point p into s_engineer; 
	read point q into s_pilot ; 
	read point r into x_engineer ; 
	read point s into x_pilot ;
	print s_engineer, s_pilot, x_engineer, x_pilot; 
run;
