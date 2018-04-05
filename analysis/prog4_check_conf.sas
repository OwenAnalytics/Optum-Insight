/*
THIS PROGRAM CHECKS WHETHER ALL PATIENTS APPEARING IN CONFINEMENT DATA SETS ARE ALSO IN MEDICAL
DATA SETS TO CONFIRM UNDERSTANDING OF THE DOCUMENTS.
*/

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname dmdata ".";
libname med "X:\DVT\Medical_vte";
libname conf "X:\DVT\Inp_vte";

*** EXTRACT DISTINCT PATIDS FROM CONFINEMENT ***;
%macro create_conf(yr=);
	select distinct strip(put(patid,20.)) as patid, &yr as year_src_conf
	from conf.conf_dvt_&yr(keep=patid)
%mend;

proc sql noprint inobs=5000;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
	create table conf_all as
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
	%create_conf(yr=2016)
	order by patid, year_src_conf;

*** KEEP ONLY DISTINCT PATID OVER ALL YEARS ***;
	create table conf_id as
	select distinct patid
	from conf_all
	order by patid;
quit;





*** EXTRACT DISTINCT PATIDS FROM MEDICAL ***;
%macro create_med(yr=);
	select distinct strip(put(patid,20.)) as patid, &yr as year_src_med
	from med.med_dvt_&yr(keep=patid)
%mend;

proc sql noprint inobs=10000;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
	create table med_all as
	%create_med(yr=2007)
	outer union corr
	%create_med(yr=2008)
	outer union corr
	%create_med(yr=2009)
	outer union corr
	%create_med(yr=2010)
	outer union corr
	%create_med(yr=2011)
	outer union corr
	%create_med(yr=2012)
	outer union corr
	%create_med(yr=2013)
	outer union corr
	%create_med(yr=2014)
	outer union corr
	%create_med(yr=2015)
	outer union corr
	%create_med(yr=2016);

*** KEEP ONLY DISTINCT PATID OVER ALL YEARS ***;
	create table med_id as
	select distinct patid
	from med_all
	order by patid;
quit;


proc sql;
*** CHECK IF ALL PATID'S FROM CONFINEMENT ARE IN MEDICAL. IF NOT, THEN OUTPUT THE PATID ***;
	create table conf_only_id as
	select distinct patid
	from conf_patid
	where patid not in (select patid from med_id)
	order by patid;

***  ***;

quit;





/**/
/*proc export data=labs4*/
/*	outfile='C:\Users\mengbing\Box Sync\Optum Insight - Data Management\labs_he_pl_gfr.txt' */
/*	dbms=tab replace;*/
/*putnames=yes;*/
/*run;*/
/**/
/*proc datasets library=user nolist;*/
/*   delete patids labs1-labs4; */
/*quit; run;*/

