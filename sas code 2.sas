libname database "C://Users//leejiwon//Desktop";

data database one;
	input a b $;
cards;
1 a
2 b
;
run;

proc print data = database.one;
run;

proc sort data = database.iris out = database sorted_iris1;
by Sepal_Length;
run; quit;
