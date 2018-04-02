/*
THIS PROGRAM ADDS ADDITIONAL INFORMATION FROM RAW DATA TO THE EXISTING SUBSET DATA SUBSET_V4_SAS7BDAT.
1. ADD INR TEST INFORMATION.
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
	select strip(put(patid,20.)) as patid, tst_desc , fst_dt format=YYMMDD10. as inr_dt label="Test Date", rslt_nbr,
			Low_Nrml, Hi_Nrml, Rslt_Unit_Nm
	from lab.lab_dvt_&yr(keep=patid fst_dt tst_desc rslt_nbr low_nrml hi_nrml Rslt_Unit_Nm)
/*							("INR", "PROTIME INR", "PROTHROMBIN TIME/INR", "INTER. NORMALIZED RATIO")))*/
	where calculated patid in (select distinct strip(patid) from patids)
%mend;

proc sql noprint inobs=5000;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
	create table lab_inr as
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

*** KEEP ONLY INR TESTS ***;
	create table lab_inr2 as
	select *
	from lab_inr
	where ((upcase(strip(tst_desc)) like trim('%PROTHROMBIN TIME%') )
    	or ( upcase(strip(tst_desc)) like trim('%I.N.R%') )
	    or ( upcase(strip(tst_desc)) like trim('%INR%') )
	    or ( upcase(strip(tst_desc)) like trim('%NORMALIZED RATIO%') )
	    or ( upcase(strip(tst_desc)) like trim('%INTR%NORM%RATIO%') ));


*** ADD INDEX VTE DATE ***;
	create table lab_inr3 as
	select a.patid, b.index_dt, a.tst_desc, a.inr_dt, a.rslt_nbr, a.low_nrml, a.hi_nrml, a.Rslt_Unit_Nm
	from lab_inr_test a left join patids b
	on a.patid = b.patid;

	create table lab_inr4 as
	select *
	from lab_inr3
	where index_dt <= inr_dt
	order by patid, inr_dt;
quit;


proc export data=lab_inr4
   outfile='lab_inr_added.csv'
   dbms=csv
   replace;
run;

proc datasets library=user nolist;
   delete patids lab_inr lab_inr2 lab_inr3 lab_inr4; 
quit; run;




/*proc export data=member.mem_detail_dvt */
/*	outfile='C:\Users\mengbing\Box Sync\Optum Insight - Data Management\mem_details.txt' */
/*	dbms=tab replace;*/
/*putnames=yes;*/
/*run;*/
