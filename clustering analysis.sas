
Data crime;
	input id city $ murder rape @@;
	datalines;

1	Atlanta	16.5	24.8 
2	Boston	4.2	13.3 
3	Chicago	11.6	24.7
4	Dallas	18.9	34.2
5	Denver	6.9	41.5 
6	Detroit	13	35.7
7	Hartford	2.5	8.8
8	Honolulu	3.6	12.7
9	Houston	16.8	26.6 
10	KanasCity	10.8	43.2
11	LosAngeles	9.7	51.8
12	NewOrleans	10.3	39.7
13	NewYork	9.4	19.4
14	Portland	5.9	23
15	Tucson	5.1	22.9
16	Washington	12.5	27.6

run;

/* Method = single */
proc cluster data = crime method = single outtree = out1 standard;
	var murder rape;
	id city;
	copy murder rape;
proc tree data = out1 ncl = 2 out = cluster_1; id city;
proc print data = cluster_1;
run;

/* Method = complete */
proc cluster data = crime method = complete outtree = out2 standard;
	var murder rape;
	id city;
proc tree data = out2 ncl = 2 out = cluster_2 ; id city;
run;

proc print data = cluster_2;
run;

/* Method = ward */
proc cluster data = crime method = ward outtree = out4 standard psedu rsq;
	var murder rape;
	id city;
proc tree data = out4 ncl = 2 out = cluster_4;
id city;
run;


/* Method = ward */
proc cluster data = crime method = ward outtree = out4 standard ;
	var murder rape;
	id city;
proc tree data = out4 ncl = 2 out = cluster_4; id city;
run;

proc print data = cluster_4; run;
proc sort data = cluster_4; by city ; run;
proc sort data = crime ; by city; run;
data cluster_4d;
	merge cluster_4 crime;
	by city;
run;

proc print data = cluster_4d; run;
proc sort data = cluster_4d ; by cluster ; run;
proc means data = cluster_4d; by cluster ; run; /* clusterwise mean*/

/* K-means method with 2 groups */
proc fastclus data = crime maxc = 2 list summary distance out = out5 ;
	var murder rape;
	id city;
run;

/* K-means method with 3 groups*/

proc fastclus data = crime maxc = 3 list summary distance out = out6;
	var murder rape;
	id city;
run;

