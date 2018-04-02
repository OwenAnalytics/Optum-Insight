/* 
THIS PROGRAM IMPLEMENTS THE INCLUSION AND EXCLUSION CRITERIA.
1. EXCLUDE PATIENTS EVER GOT ANTI-COAGULANT WITHIN 90-5 DAYS PRIOR TO INDEX VTE DATE 
  (THIS PROGRAM CORRECTS THE PREVIOUS ALGORITHM BY EXCLUDING ALL ROWS OF SUCH A PATIENT)
2. FIND THE INDEX CANCER: THE FIRST CANCER OCCURRENCE AMONG ALL CANCER DIAGNOSIS;
3. EXCLUDE PATIENTS WHO DO NOT HAVE AN ACTIVE CANCER: NO HCPCS/NDC AFTER THE CANCER 
  DIAGNOSIS AND PRIOR TO INDEX VTE. HCPCS CODES COME FROM MEDICAL. NDC CODES COME 
  FROM MEDICAL AND PHARMA.
4. EXCLUDE PATIENTS WHO DO NOT GET AN OUTPATIENT AC LONG AFTER INDEX VTE:
  1) IF THE PATIENT WAS HOSPITALIZED, WHICH IS IDENTIFIED FROM CONFINEMENT, DURING INDEX
  VTE, THEN SHOULD HAVE AN OUTPATIENT AC WITHIN 60 DAYS AFTER INDEX VTE
  2) IF THE PATIENT WAS NOT HOSPITALIZED DURING INDEX VTE, THEN SHOULD HAVE AN OUTPATIENT
  AC WITHIN 14 DAYS AFTER INDEX VTE

ADDITIONAL MANIPULATION:
2. INCLUDE PLANID TO THE FINAL OUTPUT DATASET
3. CHECK FOR DUPLICATES: 
  1) IF A PATIENT HAD TWO RECORDS ON THE SAME DATE FOR THE SAME AC, THEN KEEY ONLY
  ONE OF THEM.
  2) IF A PATIENT GOT WARFARIN AND LMWH ON THE SAME DATE, THEN ONLY
  KEEP THE ROW FOR WARFARIN AND REMOVE THE ROW FOR LMWH. OUTPUT THESE TO A NO_DUP 
  DATA SET.
*/;

options symbolgen user="X:\DVT\dm";
/*options symbolgen user=work;*/
options fullstimer msglevel=i ;

libname dmdata ".";
libname member "X:\DVT\Member_vte";
libname medical "X:\DVT\Medical_vte";
libname others ".\others";
libname pharma "X:\DVT\Pharma_vte";


proc options group=memory; run;
proc options option=utilloc; run;
proc options option=threads; run;


%macro create_pharm(yr=);
	select strip(put(patid,20.)) as patid, strip(put(pat_planid,19.)) as planid,
		copay, fill_dt format=YYMMDD10. as fst_dt, days_sup, quantity, strength, ndc
	from pharma.pharm_dvt_&yr(keep=patid pat_planid copay fill_dt days_sup quantity strength ndc)
%mend;

proc sql;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
	create table pharm as
	%create_pharm(yr=2007)
	outer union corr
	%create_pharm(yr=2008)
	outer union corr
	%create_pharm(yr=2009)
	outer union corr
	%create_pharm(yr=2010)
	outer union corr
	%create_pharm(yr=2011)
	outer union corr
	%create_pharm(yr=2012)
	outer union corr
	%create_pharm(yr=2013)
	outer union corr
	%create_pharm(yr=2014)
	outer union corr
	%create_pharm(yr=2015)
	outer union corr
	%create_pharm(yr=2016);
quit;

proc sql;
*** READ IN MEMBER DATASET FOR INDEX VTE ***;
	create table member as
	select distinct strip(put(patid,20.)) as patid, index_dt format=YYMMDD10.
	from member.member(keep = patid index_dt);
quit;


/* 
EXCLUDE PATIENTS EVER GOT ANTI-COAGULANT WITHIN 30 DAYS PRIOR TO INDEX VTE DATE
*/;

proc sql noprint;
*** KEEP ONLY PATIENTS IN THE MEMBER DATASET ***;
	create table patinfo1 as
	select b.*, a.index_dt
	from member a, pharm b 
	where a.patid = b.patid;

*** SELECT PATIENTS WHO GOT ANTI-COAGULANTS AND KEEP RECORDS WITH NO
	FILL_DT WITHIN 90 ~ 5 DAYS PRIOR TO INDEX VTE ***;
	create table final1 (drop=pre_days) as
	select *, (index_dt - fst_dt) as pre_days
	from patinfo1
	where ndc in (select code from others.ndc_ac) and
		(calculated pre_days > 90 or calculated pre_days <= 5);

	select count(*), count(distinct patid)
		into :numobs_noac, :numsubjs_noac
	from final1;
quit;

%put &numobs_noac &numsubjs_noac;

proc datasets library=user nolist;
   delete patinfo1;
quit; run;



/* 
FIND INDEX CANCER DATE AND TYPE 
*/;

*** THIS MACRO READS ONLY MED INFORMATION OF PATIENTS APPEARING IN FINAL1 ***;
%macro create_med(yr=);
	select strip(put(patid2,20.)) as patid, *
	from medical.med_dvt_&yr(keep=patid diag1-diag25 fst_dt ndc proc_cd
		rename=(patid=patid2)) 
	where calculated patid in (select distinct patid from final1)
%mend;

proc sql;
*** CREATE TABLE WITH INFORMATION ON PRESCRIPTION OVER ALL YEARS ***;
	create table med(drop=patid2) as
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
quit;


proc sql noprint;
*** READ IN ICD9 CODES FOR CANCERS ***;
	create table cancer_codes as
	select strip(put(icd_code,5.)) as icd_code, type
	from others.cancer_codes;

	select quote(strip(icd_code)) into :cancers separated by ', '
	from cancer_codes;
quit;


*** CHECK WHETHER PATIENTS EVER HAD A CANCER DIAGNOSIS AND 
	CREATE AN OBSERVATION NUMBER FOR EACH ROW ***;
data patinfo2;
/*	retain obsid;*/
	set med;
/*	obsid = _N_;*/
	length cancer_diag $30 cancer_ct 3;
	cancer_ct = 0;
	array diag{25} diag1-diag25;
	do i=1 to 25;
		if diag[i] in (&cancers) then do;
			cancer_diag=catx(', ', strip(cancer_diag), diag[i]);
			cancer_ct +1;
		end;
	end;
	drop i diag1-diag25;
run;


proc datasets library=user nolist;
   delete med;
quit; run;
%symdel cancers;


proc sql;
*** SELECT PATIENTS WHO EVER HAD A CANCER DIAGNOSIS ***;
	create table patinfo3 (drop=cancer_total) as
	select *, sum(cancer_ct) as cancer_total
	from patinfo2 
	group by patid
	having calculated cancer_total >= 1;
quit;

proc datasets library=user nolist;
   delete patinfo2;
quit; run;

proc sql;
*** SELECT OBSERVATIONS HAVING A CANCER DIAGNOSIS ***;
	create table cancerobs (drop=cancer_ct) as
	select *
	from patinfo3
	where cancer_ct >= 1;

*** IDENTIFY THE INDEX CANCER DATE FOR EACH PATIENT ***;
	create table cancerobs2 as
	select *
	from cancerobs
	group by patid
	having fst_dt = min(fst_dt);
quit;

proc datasets library=user nolist;
   delete cancerobs;
quit; run;


*** REPLACE ICD CODES WITH CANCER TYPES ***;
*** SELECT DISTINCT ROWS OF CANCER DIAGNOSIS OF EACH PATIENT ***;
proc sql;
	create table cancerobs3 as
	select distinct patid, fst_dt as index_cancer_dt label="Index Cancer Date", cancer_diag
	from cancerobs2;
quit;


proc datasets library=user nolist;
   delete cancerobs2;
quit; run;


data cancerobs3_long;	
	set cancerobs3;
	length diag_code $30;
	do i=1 to (count(cancer_diag, ', ')+1);
		diag_code = strip(scan(cancer_diag,i,','));
		output;
	end;
	drop i;
run;

proc sql;
	create table cancerobs4 as 
	select distinct patid, diag_code
	from cancerobs3_long;
quit;

proc datasets library=user nolist;
   delete cancerobs3_long;
quit; run;

*** ADD CANCER TYPES MATCHING CANCER CODES ***;
proc sql;
	create table cancerobs4_type as
	select distinct a.patid, b.type
	from cancerobs4 a left join cancer_codes b
	on strip(a.diag_code) = strip(b.icd_code)
	order by patid;
quit;

proc datasets library=user nolist;
   delete cancerobs4 cancer_codes;
quit; run;

*** TRANSFORM SEMI INTO WIDE FORMAT BASED ON INDEX CANCER TYPES:
	CANCEROBS5 CONTAINS ONLY PATID AND INDEX CANCER TYPES ***;
data cancerobs5;
	length cancer_type $62;
	do until (last.patid);
		set cancerobs4_type;
		by patid;
		cancer_type = catx(', ', strip(cancer_type), type);
	end;
	drop type;
run;

proc datasets library=user nolist;
   delete cancerobs4_type;
quit; run;

proc sql;
*** INCLUDE EXCLUSIVELY INDEX VTE, INDEX CANCER DATE FOR EACH DISTINCT PATIENT ***;
	create table cancerobs6 as
	select distinct a.patid, a.cancer_type, b.index_cancer_dt, c.index_dt
	from cancerobs5 a,
		cancerobs3 (keep = patid index_cancer_dt) b,
		member c
	where a.patid = b.patid = c.patid
		and c.index_dt >= b.index_cancer_dt;
quit;


proc datasets library=user nolist;
   delete cancerobs5 cancerobs3 member;
quit; run;

/*
EXCLUDE PATIENTS WHOSE CANCERS WERE NOT ACTIVE
*/;

proc sql noprint;
*** MERGE INFORMATION ON CHEMO DRUG AND PROCEDURES FROM MED AND PHARM ***;
	create table ctinfo as
	select *
	from patinfo3 (keep = patid ndc proc_cd fst_dt)
		where patid in (select distinct patid from cancerobs6)
		outer union corr
	select *
	from pharm (drop=copay)
	where patid in (select distinct patid from cancerobs6);

***	ADD INDEX VTE AND INDEX CANCER DATE TO CTINFO ***;
	create table ctinfo2 as
	select a.*, b.index_dt, b.index_cancer_dt, b.cancer_type
	from ctinfo a, cancerobs6 b
	where a.patid = b.patid;
quit;

proc datasets library=user nolist;
   delete ctinfo cancerobs6;
quit; run;


proc sql noprint;
*** READ IN HCPCS CODES ***;
	create table hcpcs_codes(drop=px2 code_type) as
	select strip(px2) as hcpcs, *
	from others.hcpcs_codes 
	where code_type = "HCPC"
	order by hcpcs;

*** READ IN NDC CODES ***;
	create table chemo_codes(drop=ndc2) as
	select strip(put(ndc2,12.)) as ndc, *
	from others.chemo_codes;

*** SELECT PATIENTS WHOSE CANCER WAS ACTIVE ***;
	create table patinfo4 as 
	select *
	from ctinfo2 
	where index_cancer_dt <= fst_dt <= index_dt
		and ((ndc in (select ndc from chemo_codes)) or (proc_cd in (select hcpcs from hcpcs_codes)));
quit;

proc datasets library=user nolist;
   delete hcpcs_codes chemo_codes ctinfo2;
quit; run;


proc sql noprint;
	create table final2 as
	select distinct patid, index_cancer_dt, cancer_type
	from patinfo4;

	select count(patid)
		into :numsubjs_active
	from final2;

*** COMBINE INFORMATION FROM FINAL1 AND FINAL 2 AND KEEP ONLY PATIENTS APPEARING IN BOTH DATASETS ***;
	create table semifinal as
	select a.*, b.index_cancer_dt, b.cancer_type
	from final1 a, final2 b
	where a.patid = b.patid;

*** READ IN ANTICOAGULANT CODES ***;
	create table ac as 
	select *
	from others.ndc_ac;

*** ADD AC TYPE, REORDER COLUMNS AND LABEL VARIABLES IN THE FINAL DATASET ***;
	create table final as
	select a.patid, a.planid, a.fst_dt, b.category label="AC Category", b.gen_name label"AC Generic Name",
		copay, a.index_dt label="Index VTE Date", a.index_cancer_dt,
		a.cancer_type label="Index Cancer Types", days_sup, quantity, strength
	from semifinal a left join ac b
	on a.ndc = b.code
	order by patid, fst_dt;

	select count(*), count(distinct patid)
		into :numobs_final, :numsubjs_final
	from final;
quit;

%put The number of patients with active cancer before VTE is &numsubjs_active;
%put The number of rows in the final dataset is &numobs_final;
%put The number of patients in the final dataset is &numsubjs_final;

*** DELETE ALL TEMPORARY DATASETS EXCEPT FOR FINAL***;
proc datasets library=user nolist;
   save final;
quit; run;


*** MERGE ROWS WITH THE SAME AC ON THE SAME DATE BUT ONLY DIFFER IN PLANID ***;
proc sql noprint;
	create table subset_v4_nodup as
	select distinct patid, fst_dt, category, gen_name, copay, index_dt, index_cancer_dt,
		cancer_type, days_sup, quantity, strength
	from final
	order by patid, fst_dt;

	select count(*), count(distinct patid)
		into :numobs_nodup, :numsubjs_nodup
	from subset_v4_nodup;
quit;

%put The number of rows in the final dataset with no duplication is &numobs_nodup;
%put The number of patients in the final dataset with no duplication is &numsubjs_nodup;


data dmdata.subset_v4;
	set final;
run;

data dmdata.subset_v4_nodup;
	set subset_v4_nodup;
run;


proc export data=dmdata.subset_v4_nodup
   outfile='subset_v4.csv'
   dbms=csv
   replace;
run;

proc export data=dmdata.subset_v4_nodup
   outfile='subset_v4_nodup.csv'
   dbms=csv
   replace;
run;



proc datasets library=user nolist;
   delete final subset_v4_nodup; 
quit; run;


