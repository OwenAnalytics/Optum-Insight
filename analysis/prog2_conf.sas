/*
THIS PROGRAM PREPARES DATA SETS FOR CALCULATING TABLE 1 STATISTICS.
1. READ SURGERY AND ADMISSION INFORMATION FROM CONF_DVT_20XX DATA SETS,
  THAT OCCURRED WITHIN FOUR WEEKS BEFORE INDEX VTE
*/

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname dmdata ".";
libname conf "X:\DVT\Inp_vte";
libname member "X:\DVT\Member_vte";

*** EXTRACT DISTINCT PATIDS ***;
proc sql;
	create table patids as
	select distinct *
	from dmdata.subset_v4_nodup (keep=patid index_dt);
quit;

%macro create_conf(yr=);
	select strip(put(patid,20.)) as patid, admit_date format=YYMMDD10. as admit_dt label="Admit Date", 
		diag1, diag2, diag3, diag4, diag5, disch_date format=YYMMDD10. as disch_dt label="Discharge Date"
	from conf.conf_dvt_&yr(keep=patid admit_date diag1-diag5 disch_date)
	where calculated patid in (select distinct strip(patid) from patids)
%mend;

proc sql noprint;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
	create table confs as
	%create_conf(yr=2007)
	outer union corr
	%create_conf(yr=2008)
	outer union corr
	%create_conf(yr=2009)
	outer union corr
	%create_conf(yr=2010)
	outer union corr
	%create_conf(yr=2011)
	outer union corr
	%create_conf(yr=2012)
	outer union corr
	%create_conf(yr=2013)
	outer union corr
	%create_conf(yr=2014)
	outer union corr
	%create_conf(yr=2015)
	outer union corr
	%create_conf(yr=2016);

*** ADD INDEX VTE DATE ***;
	create table conf_vte as
	select a.*, b.index_dt
	from confs a left join patids b
	on a.patid = b.patid;
quit;


proc export data=conf_vte
   outfile='conf_all.csv'
   dbms=csv
   replace;
run;


proc export data=conf_vte
	outfile='C:\Users\mengbing\Box Sync\Optum Insight - Data Management\conf_all.txt' 
	dbms=tab replace;
putnames=yes;
run;


proc datasets library=user nolist;
   delete confs conf_vte; 
quit; run;
