/*
THIS PROGRAM EXTRACT LAB INFORMATION ON THREE LAB TESTS THAT WILL BE INCLUDED IN
  TABLE 1: HEMOGLOBIN, PLATELETS, GFR
*/

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;
proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


libname dmdata ".";
libname lab "X:\DVT\Lab_vte";
libname member "X:\DVT\Member_vte";

*** EXTRACT DISTINCT PATIDS ***;
proc sql;
	create table patids as
	select distinct *
	from dmdata.subset_v4_nodup (keep=patid index_dt);
quit;

%macro create_lab(yr=);
	select strip(put(patid,20.)) as patid, tst_desc , fst_dt format=YYMMDD10. as lab_dt label="Lab Test Date", rslt_nbr,
			low_nrml, hi_nrml, rslt_unit_nm
	from lab.lab_dvt_&yr(keep=patid fst_dt tst_desc rslt_nbr low_nrml hi_nrml rslt_unit_nm)
/*						where=(upcase(strip(tst_desc)) like trim('%HEMOGLOBIN%')))*/
	where calculated patid in (select distinct strip(patid) from patids)

%mend;

proc sql noprint;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
	create table labs1 as
	%create_lab(yr=2007)
	outer union corr
	%create_lab(yr=2008)
	outer union corr
	%create_lab(yr=2009)
	outer union corr
	%create_lab(yr=2010)
	outer union corr
	%create_lab(yr=2011)
	outer union corr
	%create_lab(yr=2012)
	outer union corr
	%create_lab(yr=2013)
	outer union corr
	%create_lab(yr=2014)
	outer union corr
	%create_lab(yr=2015)
	outer union corr
	%create_lab(yr=2016);

*** KEEP ONLY WANTED LAB TESTS ***;
	create table labs2 as
	select *
	from labs1
	where (upcase(strip(tst_desc)) like trim('%PLATELETS%'))
		or (upcase(strip(tst_desc)) like trim('%HEMOGLOBIN%') )
		or (upcase(strip(tst_desc)) like trim('%GFR%') );

*** ADD INDEX VTE DATE ***;
	create table labs3 as
	select a.patid, b.index_dt, a.tst_desc, a.lab_dt, a.rslt_nbr, a.low_nrml, a.hi_nrml, a.rslt_unit_nm
	from labs2 a left join patids b
	on a.patid = b.patid;

	create table labs4 as
	select *
	from labs3
	where index_dt - 30 <= lab_dt < index_dt
	order by patid, lab_dt;
quit;


proc export data=labs4
   outfile='labs_he_pl_gfr.csv'
   dbms=csv
   replace;
run;


proc export data=labs4
	outfile='C:\Users\mengbing\Box Sync\Optum Insight - Data Management\labs_he_pl_gfr.txt' 
	dbms=tab replace;
putnames=yes;
run;

proc datasets library=user nolist;
   delete patids labs1-labs4; 
quit; run;

