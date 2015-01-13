/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 1:35:25 PM
PROJECT: PoulterR_SAS_project_011315_sas
PROJECT PATH: P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp
---------------------------------------- */

/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\rpoulter\Assignments' ;
/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\rpoulter\Assignments' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (MYDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MYDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA  "P:\QAC\qac200\students\rpoulter\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:06 PM
   By task: Data Set Attributes

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(MYDATA.FULLYR__0000_SUBSET);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA MYDATA.FULLYR__0000_SUBSET(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM MYDATA.FULLYR__0000_SUBSET
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=MYDATA.FULLYR__0000_SUBSET OUT=MYDATA.FULLYR__0000_SUBSET;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM MYDATA.FULLYR__0000_SUBSET
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Subset and Observations Filter for MRI and Xray   */
%LET _CLIENTTASKLABEL='Subset and Observations Filter for MRI and Xray';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MEPS_FILTER_FULLYR_2012);

PROC SQL;
   CREATE TABLE MYDATA.MEPS_FILTER_FULLYR_2012(label="MEPS_FILTER_FULLYR_2012") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.ACTDTY31, 
          t1.ACTDTY42, 
          t1.ACTDTY53, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.CANCERDX, 
          t1.DVGEN12, 
          t1.DVGEXP12, 
          t1.DVGMCD12, 
          t1.DVGMCR12, 
          t1.DVGOFD12, 
          t1.DVGOPR12, 
          t1.DVGOPU12, 
          t1.DVGOSR12, 
          t1.DVGOTH12, 
          t1.DVGPRV12, 
          t1.DVGPTR12, 
          t1.DVGSLF12, 
          t1.DVGSTL12, 
          t1.DVGTCH12, 
          t1.DVGTRI12, 
          t1.DVGVA12, 
          t1.DVGWCP12, 
          t1.DVOEXP12, 
          t1.DVOMCD12, 
          t1.DVOMCR12, 
          t1.DVOOFD12, 
          t1.DVOOPR12, 
          t1.DVOOPU12, 
          t1.DVOOSR12, 
          t1.DVOOTH12, 
          t1.DVOPRV12, 
          t1.DVOPTR12, 
          t1.DVORTH12, 
          t1.DVOSLF12, 
          t1.DVOSTL12, 
          t1.DVOTCH12, 
          t1.DVOTRI12, 
          t1.DVOVA12, 
          t1.DVOWCP12, 
          t1.DVTEXP12, 
          t1.DVTMCD12, 
          t1.DVTMCR12, 
          t1.DVTOFD12, 
          t1.DVTOPR12, 
          t1.DVTOPU12, 
          t1.DVTOSR12, 
          t1.DVTOT12, 
          t1.DVTOTH12, 
          t1.DVTPRV12, 
          t1.DVTPTR12, 
          t1.DVTSLF12, 
          t1.DVTSTL12, 
          t1.DVTTCH12, 
          t1.DVTTRI12, 
          t1.DVTVA12, 
          t1.DVTWCP12, 
          t1.EMPHDX, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.OFREMP31, 
          t1.OFREMP42, 
          t1.OFREMP53, 
          t1.ERDOPR12, 
          t1.ERDOPU12, 
          t1.ERDTCH12, 
          t1.ERTTCH12, 
          t1.ERFTCH12, 
          t1.ERTOTH12, 
          t1.ERDMCD12, 
          t1.ERFMCD12, 
          t1.ERTMCD12, 
          t1.ERDMCR12, 
          t1.ERFMCR12, 
          t1.ERTMCR12, 
          t1.ERDOTH12, 
          t1.ERFOTH12, 
          t1.ERTOPR12, 
          t1.ERTOPU12, 
          t1.ERDSTL12, 
          t1.ERFSTL12, 
          t1.ERTSTL12, 
          t1.ERDOSR12, 
          t1.ERFOSR12, 
          t1.ERTOSR12, 
          t1.ERDOFD12, 
          t1.ERTOFD12, 
          t1.ERFOFD12, 
          t1.ERFOPR12, 
          t1.ERFOPU12, 
          t1.ERTPRV12, 
          t1.ERDPRV12, 
          t1.ERFPRV12, 
          t1.ERDPTR12, 
          t1.ERFPTR12, 
          t1.ERTPTR12, 
          t1.ERDSLF12, 
          t1.ERFSLF12, 
          t1.ERTSLF12, 
          t1.ERDTRI12, 
          t1.ERFTRI12, 
          t1.ERTTRI12, 
          t1.ERDVA12, 
          t1.ERFVA12, 
          t1.ERTVA12, 
          t1.ERDWCP12, 
          t1.ERFWCP12, 
          t1.ERTWCP12, 
          t1.MIDX, 
          t1.ERTOT12, 
          t1.ARTHDX, 
          t1.ENGCMF42, 
          t1.ENGSPK42, 
          t1.LANGHM42, 
          t1.STRKDX, 
          t1.PHQ242, 
          t1.K6SUM42, 
          t1.SFFLAG42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.JTPAIN31, 
          t1.JTPAIN53, 
          t1.INS12X, 
          t1.INS53X, 
          t1.INS31X, 
          t1.INS42X, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.USBORN42, 
          t1.EDRECODE
      FROM EC100031.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Code For Data Set Filtered   */
%LET SYSLAST=MYDATA.MEPS_FILTER_FULLYR_2012;
%LET _CLIENTTASKLABEL='Code For Data Set Filtered';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';
%LET _SASPROGRAMFILE=;

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 10:59:06 AM
   By task: Data Set Filtered

   Input Data: Local:MYDATA.MEPS_FILTER_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTENTSFORFILTER_FOR_MEPS);
TITLE "Data set attributes for subset data set";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.MEPS_FILTER_FULLYR_2012;

RUN;



GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies for X-rays and MRI's for Adults 2012   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for X-rays and MRI''s for Adults 2012';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:07 PM
   By task: One-Way Frequencies for X-rays and MRI's for Adults 2012

   Input Data: Local:MYDATA.MEPS_FILTER_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.MEPS_FILTER_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.AGE12X, T.SEX, T.EDUCYR, T.EDUYRDEG, T.REGION12, T.RACETHX, T.MARRY12X, T.SAQELIG, T.SAQWT12F, T.ACTDTY31, T.ACTDTY42, T.ACTDTY53, T.ADAPPT42, T.ADCAPE42, T.ADCLIM42, T.ADCMPD42, T.ADCMPM42, T.ADCMPY42, T.ADDAYA42, T.ADDOWN42
		     , T.ADDPRS42, T.ADDRBP42, T.ADEFRT42, T.ADEGMC42, T.ADEXPL42, T.ADEZUN42, T.ADFFRM42, T.ADFHLP42, T.ADGENH42, T.ADHECR42, T.ADHOPE42, T.ADILCR42, T.ADILWW42, T.ADINSA42, T.ADINSB42, T.ADINST42, T.ADINTR42, T.ADLANG42
		     , T.ADLIST42, T.ADMALS42, T.ADMWLM42, T.ADNDCR42, T.ADNERV42, T.ADNRGY42, T.ADNSMK42, T.ADOVER42, T.ADPAIN42, T.ADPALS42, T.ADPRTM42, T.ADPRX42, T.ADPWLM42, T.ADRESP42, T.ADREST42, T.ADRISK42, T.ADRTCR42, T.ADRTWW42, T.ADSAD42
		     , T.ADSMOK42, T.ADSOCA42, T.ADSPEC42, T.ADSPRF42, T.ADTLHW42, T.ADWRTH42, T.CANCERDX, T.DVGEN12, T.DVGEXP12, T.DVGMCD12, T.DVGMCR12, T.DVGOFD12, T.DVGOPR12, T.DVGOPU12, T.DVGOSR12, T.DVGOTH12, T.DVGPRV12, T.DVGPTR12, T.DVGSLF12
		     , T.DVGSTL12, T.DVGTCH12, T.DVGTRI12, T.DVGVA12, T.DVGWCP12, T.DVOEXP12, T.DVOMCD12, T.DVOMCR12, T.DVOOFD12, T.DVOOPR12, T.DVOOPU12, T.DVOOSR12, T.DVOOTH12, T.DVOPRV12, T.DVOPTR12, T.DVORTH12, T.DVOSLF12, T.DVOSTL12, T.DVOTCH12
		     , T.DVOTRI12, T.DVOVA12, T.DVOWCP12, T.DVTEXP12, T.DVTMCD12, T.DVTMCR12, T.DVTOFD12, T.DVTOPR12, T.DVTOPU12, T.DVTOSR12, T.DVTOT12, T.DVTOTH12, T.DVTPRV12, T.DVTPTR12, T.DVTSLF12, T.DVTSTL12, T.DVTTCH12, T.DVTTRI12
		     , T.DVTVA12, T.DVTWCP12, T.EMPHDX, T.EMPST31, T.EMPST42, T.EMPST53, T.OFREMP31, T.OFREMP42, T.OFREMP53, T.ERDOPR12, T.ERDOPU12, T.ERDTCH12, T.ERTTCH12, T.ERFTCH12, T.ERTOTH12, T.ERDMCD12, T.ERFMCD12, T.ERTMCD12, T.ERDMCR12
		     , T.ERFMCR12, T.ERTMCR12, T.ERDOTH12, T.ERFOTH12, T.ERTOPR12, T.ERTOPU12, T.ERDSTL12, T.ERFSTL12, T.ERTSTL12, T.ERDOSR12, T.ERFOSR12, T.ERTOSR12, T.ERDOFD12, T.ERTOFD12, T.ERFOFD12, T.ERFOPR12, T.ERFOPU12, T.ERTPRV12, T.ERDPRV12
		     , T.ERFPRV12, T.ERDPTR12, T.ERFPTR12, T.ERTPTR12, T.ERDSLF12, T.ERFSLF12, T.ERTSLF12, T.ERDTRI12, T.ERFTRI12, T.ERTTRI12, T.ERDVA12, T.ERFVA12, T.ERTVA12, T.ERDWCP12, T.ERFWCP12, T.ERTWCP12, T.MIDX, T.ERTOT12, T.ARTHDX
		     , T.ENGCMF42, T.ENGSPK42, T.LANGHM42, T.STRKDX, T.PHQ242, T.K6SUM42, T.SFFLAG42, T.MCS42, T.PCS42, T.JTPAIN31, T.JTPAIN53, T.INS12X, T.INS53X, T.INS31X, T.USBORN42, T.INS42X, T.INSAT31X, T.INSAT42X, T.INSAT53X, T.INSAT12X, T.INSCOV12
	FROM MYDATA.MEPS_FILTER_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for 2012 of 18 years and older who received MRI's and X-rays following ER visits";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES AGE12X /  SCORES=TABLE;
	TABLES SEX /  SCORES=TABLE;
	TABLES EDUCYR /  SCORES=TABLE;
	TABLES EDUYRDEG /  SCORES=TABLE;
	TABLES REGION12 /  SCORES=TABLE;
	TABLES RACETHX /  SCORES=TABLE;
	TABLES MARRY12X /  SCORES=TABLE;
	TABLES SAQELIG /  SCORES=TABLE;
	TABLES SAQWT12F /  SCORES=TABLE;
	TABLES ACTDTY31 /  SCORES=TABLE;
	TABLES ACTDTY42 /  SCORES=TABLE;
	TABLES ACTDTY53 /  SCORES=TABLE;
	TABLES ADAPPT42 /  SCORES=TABLE;
	TABLES ADCAPE42 /  SCORES=TABLE;
	TABLES ADCLIM42 /  SCORES=TABLE;
	TABLES ADCMPD42 /  SCORES=TABLE;
	TABLES ADCMPM42 /  SCORES=TABLE;
	TABLES ADCMPY42 /  SCORES=TABLE;
	TABLES ADDAYA42 /  SCORES=TABLE;
	TABLES ADDOWN42 /  SCORES=TABLE;
	TABLES ADDPRS42 /  SCORES=TABLE;
	TABLES ADDRBP42 /  SCORES=TABLE;
	TABLES ADEFRT42 /  SCORES=TABLE;
	TABLES ADEGMC42 /  SCORES=TABLE;
	TABLES ADEXPL42 /  SCORES=TABLE;
	TABLES ADEZUN42 /  SCORES=TABLE;
	TABLES ADFFRM42 /  SCORES=TABLE;
	TABLES ADFHLP42 /  SCORES=TABLE;
	TABLES ADGENH42 /  SCORES=TABLE;
	TABLES ADHECR42 /  SCORES=TABLE;
	TABLES ADHOPE42 /  SCORES=TABLE;
	TABLES ADILCR42 /  SCORES=TABLE;
	TABLES ADILWW42 /  SCORES=TABLE;
	TABLES ADINSA42 /  SCORES=TABLE;
	TABLES ADINSB42 /  SCORES=TABLE;
	TABLES ADINST42 /  SCORES=TABLE;
	TABLES ADINTR42 /  SCORES=TABLE;
	TABLES ADLANG42 /  SCORES=TABLE;
	TABLES ADLIST42 /  SCORES=TABLE;
	TABLES ADMALS42 /  SCORES=TABLE;
	TABLES ADMWLM42 /  SCORES=TABLE;
	TABLES ADNDCR42 /  SCORES=TABLE;
	TABLES ADNERV42 /  SCORES=TABLE;
	TABLES ADNRGY42 /  SCORES=TABLE;
	TABLES ADNSMK42 /  SCORES=TABLE;
	TABLES ADOVER42 /  SCORES=TABLE;
	TABLES ADPAIN42 /  SCORES=TABLE;
	TABLES ADPALS42 /  SCORES=TABLE;
	TABLES ADPRTM42 /  SCORES=TABLE;
	TABLES ADPRX42 /  SCORES=TABLE;
	TABLES ADPWLM42 /  SCORES=TABLE;
	TABLES ADRESP42 /  SCORES=TABLE;
	TABLES ADREST42 /  SCORES=TABLE;
	TABLES ADRISK42 /  SCORES=TABLE;
	TABLES ADRTCR42 /  SCORES=TABLE;
	TABLES ADRTWW42 /  SCORES=TABLE;
	TABLES ADSAD42 /  SCORES=TABLE;
	TABLES ADSMOK42 /  SCORES=TABLE;
	TABLES ADSOCA42 /  SCORES=TABLE;
	TABLES ADSPEC42 /  SCORES=TABLE;
	TABLES ADSPRF42 /  SCORES=TABLE;
	TABLES ADTLHW42 /  SCORES=TABLE;
	TABLES ADWRTH42 /  SCORES=TABLE;
	TABLES CANCERDX /  SCORES=TABLE;
	TABLES DVGEN12 /  SCORES=TABLE;
	TABLES DVGEXP12 /  SCORES=TABLE;
	TABLES DVGMCD12 /  SCORES=TABLE;
	TABLES DVGMCR12 /  SCORES=TABLE;
	TABLES DVGOFD12 /  SCORES=TABLE;
	TABLES DVGOPR12 /  SCORES=TABLE;
	TABLES DVGOPU12 /  SCORES=TABLE;
	TABLES DVGOSR12 /  SCORES=TABLE;
	TABLES DVGOTH12 /  SCORES=TABLE;
	TABLES DVGPRV12 /  SCORES=TABLE;
	TABLES DVGPTR12 /  SCORES=TABLE;
	TABLES DVGSLF12 /  SCORES=TABLE;
	TABLES DVGSTL12 /  SCORES=TABLE;
	TABLES DVGTCH12 /  SCORES=TABLE;
	TABLES DVGTRI12 /  SCORES=TABLE;
	TABLES DVGVA12 /  SCORES=TABLE;
	TABLES DVGWCP12 /  SCORES=TABLE;
	TABLES DVOEXP12 /  SCORES=TABLE;
	TABLES DVOMCD12 /  SCORES=TABLE;
	TABLES DVOMCR12 /  SCORES=TABLE;
	TABLES DVOOFD12 /  SCORES=TABLE;
	TABLES DVOOPR12 /  SCORES=TABLE;
	TABLES DVOOPU12 /  SCORES=TABLE;
	TABLES DVOOSR12 /  SCORES=TABLE;
	TABLES DVOOTH12 /  SCORES=TABLE;
	TABLES DVOPRV12 /  SCORES=TABLE;
	TABLES DVOPTR12 /  SCORES=TABLE;
	TABLES DVORTH12 /  SCORES=TABLE;
	TABLES DVOSLF12 /  SCORES=TABLE;
	TABLES DVOSTL12 /  SCORES=TABLE;
	TABLES DVOTCH12 /  SCORES=TABLE;
	TABLES DVOTRI12 /  SCORES=TABLE;
	TABLES DVOVA12 /  SCORES=TABLE;
	TABLES DVOWCP12 /  SCORES=TABLE;
	TABLES DVTEXP12 /  SCORES=TABLE;
	TABLES DVTMCD12 /  SCORES=TABLE;
	TABLES DVTMCR12 /  SCORES=TABLE;
	TABLES DVTOFD12 /  SCORES=TABLE;
	TABLES DVTOPR12 /  SCORES=TABLE;
	TABLES DVTOPU12 /  SCORES=TABLE;
	TABLES DVTOSR12 /  SCORES=TABLE;
	TABLES DVTOT12 /  SCORES=TABLE;
	TABLES DVTOTH12 /  SCORES=TABLE;
	TABLES DVTPRV12 /  SCORES=TABLE;
	TABLES DVTPTR12 /  SCORES=TABLE;
	TABLES DVTSLF12 /  SCORES=TABLE;
	TABLES DVTSTL12 /  SCORES=TABLE;
	TABLES DVTTCH12 /  SCORES=TABLE;
	TABLES DVTTRI12 /  SCORES=TABLE;
	TABLES DVTVA12 /  SCORES=TABLE;
	TABLES DVTWCP12 /  SCORES=TABLE;
	TABLES EMPHDX /  SCORES=TABLE;
	TABLES EMPST31 /  SCORES=TABLE;
	TABLES EMPST42 /  SCORES=TABLE;
	TABLES EMPST53 /  SCORES=TABLE;
	TABLES OFREMP31 /  SCORES=TABLE;
	TABLES OFREMP42 /  SCORES=TABLE;
	TABLES OFREMP53 /  SCORES=TABLE;
	TABLES ERDOPR12 /  SCORES=TABLE;
	TABLES ERDOPU12 /  SCORES=TABLE;
	TABLES ERDTCH12 /  SCORES=TABLE;
	TABLES ERTTCH12 /  SCORES=TABLE;
	TABLES ERFTCH12 /  SCORES=TABLE;
	TABLES ERTOTH12 /  SCORES=TABLE;
	TABLES ERDMCD12 /  SCORES=TABLE;
	TABLES ERFMCD12 /  SCORES=TABLE;
	TABLES ERTMCD12 /  SCORES=TABLE;
	TABLES ERDMCR12 /  SCORES=TABLE;
	TABLES ERFMCR12 /  SCORES=TABLE;
	TABLES ERTMCR12 /  SCORES=TABLE;
	TABLES ERDOTH12 /  SCORES=TABLE;
	TABLES ERFOTH12 /  SCORES=TABLE;
	TABLES ERTOPR12 /  SCORES=TABLE;
	TABLES ERTOPU12 /  SCORES=TABLE;
	TABLES ERDSTL12 /  SCORES=TABLE;
	TABLES ERFSTL12 /  SCORES=TABLE;
	TABLES ERTSTL12 /  SCORES=TABLE;
	TABLES ERDOSR12 /  SCORES=TABLE;
	TABLES ERFOSR12 /  SCORES=TABLE;
	TABLES ERTOSR12 /  SCORES=TABLE;
	TABLES ERDOFD12 /  SCORES=TABLE;
	TABLES ERTOFD12 /  SCORES=TABLE;
	TABLES ERFOFD12 /  SCORES=TABLE;
	TABLES ERFOPR12 /  SCORES=TABLE;
	TABLES ERFOPU12 /  SCORES=TABLE;
	TABLES ERTPRV12 /  SCORES=TABLE;
	TABLES ERDPRV12 /  SCORES=TABLE;
	TABLES ERFPRV12 /  SCORES=TABLE;
	TABLES ERDPTR12 /  SCORES=TABLE;
	TABLES ERFPTR12 /  SCORES=TABLE;
	TABLES ERTPTR12 /  SCORES=TABLE;
	TABLES ERDSLF12 /  SCORES=TABLE;
	TABLES ERFSLF12 /  SCORES=TABLE;
	TABLES ERTSLF12 /  SCORES=TABLE;
	TABLES ERDTRI12 /  SCORES=TABLE;
	TABLES ERFTRI12 /  SCORES=TABLE;
	TABLES ERTTRI12 /  SCORES=TABLE;
	TABLES ERDVA12 /  SCORES=TABLE;
	TABLES ERFVA12 /  SCORES=TABLE;
	TABLES ERTVA12 /  SCORES=TABLE;
	TABLES ERDWCP12 /  SCORES=TABLE;
	TABLES ERFWCP12 /  SCORES=TABLE;
	TABLES ERTWCP12 /  SCORES=TABLE;
	TABLES MIDX /  SCORES=TABLE;
	TABLES ERTOT12 /  SCORES=TABLE;
	TABLES ARTHDX /  SCORES=TABLE;
	TABLES ENGCMF42 /  SCORES=TABLE;
	TABLES ENGSPK42 /  SCORES=TABLE;
	TABLES LANGHM42 /  SCORES=TABLE;
	TABLES STRKDX /  SCORES=TABLE;
	TABLES PHQ242 /  SCORES=TABLE;
	TABLES K6SUM42 /  SCORES=TABLE;
	TABLES SFFLAG42 /  SCORES=TABLE;
	TABLES MCS42 /  SCORES=TABLE;
	TABLES PCS42 /  SCORES=TABLE;
	TABLES JTPAIN31 /  SCORES=TABLE;
	TABLES JTPAIN53 /  SCORES=TABLE;
	TABLES INS12X /  SCORES=TABLE;
	TABLES INS53X /  SCORES=TABLE;
	TABLES INS31X /  SCORES=TABLE;
	TABLES USBORN42 /  SCORES=TABLE;
	TABLES INS42X /  SCORES=TABLE;
	TABLES INSAT31X /  SCORES=TABLE;
	TABLES INSAT42X /  SCORES=TABLE;
	TABLES INSAT53X /  SCORES=TABLE;
	TABLES INSAT12X /  SCORES=TABLE;
	TABLES INSCOV12 /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode Variables   */
%LET _CLIENTTASKLABEL='Recode Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FILTER_FULLYR);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FILTER_FULLYR AS 
   SELECT t1.DUPERSID AS DUPERSID1, 
          t1.AGE12X AS AGE12X1, 
          t1.SEX AS SEX1, 
          t1.EDUCYR AS EDUCYR1, 
          t1.EDUYRDEG AS EDUYRDEG1, 
          t1.REGION12 AS REGION121, 
          t1.RACETHX AS RACETHX1, 
          t1.MARRY12X AS MARRY12X1, 
          t1.SAQELIG AS SAQELIG1, 
          t1.SAQWT12F AS SAQWT12F1, 
          t1.ACTDTY31 AS ACTDTY311, 
          t1.EDRECODE, 
          t1.ACTDTY42 AS ACTDTY421, 
          t1.ACTDTY53 AS ACTDTY531, 
          t1.ADAPPT42 AS ADAPPT421, 
          t1.ADCAPE42 AS ADCAPE421, 
          t1.ADCLIM42 AS ADCLIM421, 
          t1.ADCMPD42 AS ADCMPD421, 
          t1.ADCMPM42 AS ADCMPM421, 
          t1.ADCMPY42 AS ADCMPY421, 
          t1.ADDAYA42 AS ADDAYA421, 
          t1.ADDOWN42 AS ADDOWN421, 
          t1.ADDPRS42 AS ADDPRS421, 
          t1.ADDRBP42 AS ADDRBP421, 
          t1.ADEFRT42 AS ADEFRT421, 
          t1.ADEGMC42 AS ADEGMC421, 
          t1.ADEXPL42 AS ADEXPL421, 
          t1.ADEZUN42 AS ADEZUN421, 
          t1.ADFFRM42 AS ADFFRM421, 
          t1.ADFHLP42 AS ADFHLP421, 
          t1.ADGENH42 AS ADGENH421, 
          t1.ADHECR42 AS ADHECR421, 
          t1.ADHOPE42 AS ADHOPE421, 
          t1.ADILCR42 AS ADILCR421, 
          t1.ADILWW42 AS ADILWW421, 
          t1.ADINSA42 AS ADINSA421, 
          t1.ADINSB42 AS ADINSB421, 
          t1.ADINST42 AS ADINST421, 
          t1.ADINTR42 AS ADINTR421, 
          t1.ADLANG42 AS ADLANG421, 
          t1.ADLIST42 AS ADLIST421, 
          t1.ADMALS42 AS ADMALS421, 
          t1.ADMWLM42 AS ADMWLM421, 
          t1.ADNDCR42 AS ADNDCR421, 
          t1.ADNERV42 AS ADNERV421, 
          t1.ADNRGY42 AS ADNRGY421, 
          t1.ADNSMK42 AS ADNSMK421, 
          t1.ADOVER42 AS ADOVER421, 
          t1.ADPAIN42 AS ADPAIN421, 
          t1.ADPALS42 AS ADPALS421, 
          t1.ADPRTM42 AS ADPRTM421, 
          t1.ADPRX42 AS ADPRX421, 
          t1.ADPWLM42 AS ADPWLM421, 
          t1.ADRESP42 AS ADRESP421, 
          t1.ADREST42 AS ADREST421, 
          t1.ADRISK42 AS ADRISK421, 
          t1.ADRTCR42 AS ADRTCR421, 
          t1.ADRTWW42 AS ADRTWW421, 
          t1.ADSAD42 AS ADSAD421, 
          t1.ADSMOK42 AS ADSMOK421, 
          t1.ADSOCA42 AS ADSOCA421, 
          t1.ADSPEC42 AS ADSPEC421, 
          t1.ADSPRF42 AS ADSPRF421, 
          t1.ADTLHW42 AS ADTLHW421, 
          t1.ADWRTH42 AS ADWRTH421, 
          t1.CANCERDX AS CANCERDX1, 
          t1.DVGEN12 AS DVGEN121, 
          t1.DVGEXP12 AS DVGEXP121, 
          t1.DVGMCD12 AS DVGMCD121, 
          t1.DVGMCR12 AS DVGMCR121, 
          t1.DVGOFD12 AS DVGOFD121, 
          t1.DVGOPR12 AS DVGOPR121, 
          t1.DVGOPU12 AS DVGOPU121, 
          t1.DVGOSR12 AS DVGOSR121, 
          t1.DVGOTH12 AS DVGOTH121, 
          t1.DVGPRV12 AS DVGPRV121, 
          t1.DVGPTR12 AS DVGPTR121, 
          t1.DVGSLF12 AS DVGSLF121, 
          t1.DVGSTL12 AS DVGSTL121, 
          t1.DVGTCH12 AS DVGTCH121, 
          t1.DVGTRI12 AS DVGTRI121, 
          t1.DVGVA12 AS DVGVA121, 
          t1.DVGWCP12 AS DVGWCP121, 
          t1.DVOEXP12 AS DVOEXP121, 
          t1.DVOMCD12 AS DVOMCD121, 
          t1.DVOMCR12 AS DVOMCR121, 
          t1.DVOOFD12 AS DVOOFD121, 
          t1.DVOOPR12 AS DVOOPR121, 
          t1.DVOOPU12 AS DVOOPU121, 
          t1.DVOOSR12 AS DVOOSR121, 
          t1.DVOOTH12 AS DVOOTH121, 
          t1.DVOPRV12 AS DVOPRV121, 
          t1.DVOPTR12 AS DVOPTR121, 
          t1.DVORTH12 AS DVORTH121, 
          t1.DVOSLF12 AS DVOSLF121, 
          t1.DVOSTL12 AS DVOSTL121, 
          t1.DVOTCH12 AS DVOTCH121, 
          t1.DVOTRI12 AS DVOTRI121, 
          t1.DVOVA12 AS DVOVA121, 
          t1.DVOWCP12 AS DVOWCP121, 
          t1.DVTEXP12 AS DVTEXP121, 
          t1.DVTMCD12 AS DVTMCD121, 
          t1.DVTMCR12 AS DVTMCR121, 
          t1.DVTOFD12 AS DVTOFD121, 
          t1.DVTOPR12 AS DVTOPR121, 
          t1.DVTOPU12 AS DVTOPU121, 
          t1.DVTOSR12 AS DVTOSR121, 
          t1.DVTOT12 AS DVTOT121, 
          t1.DVTOTH12 AS DVTOTH121, 
          t1.DVTPRV12 AS DVTPRV121, 
          t1.DVTPTR12 AS DVTPTR121, 
          t1.DVTSLF12 AS DVTSLF121, 
          t1.DVTSTL12 AS DVTSTL121, 
          t1.DVTTCH12 AS DVTTCH121, 
          t1.DVTTRI12 AS DVTTRI121, 
          t1.DVTVA12 AS DVTVA121, 
          t1.DVTWCP12 AS DVTWCP121, 
          t1.EMPHDX AS EMPHDX1, 
          t1.EMPST31 AS EMPST311, 
          t1.EMPST42 AS EMPST421, 
          t1.EMPST53 AS EMPST531, 
          t1.OFREMP31 AS OFREMP311, 
          t1.OFREMP42 AS OFREMP421, 
          t1.OFREMP53 AS OFREMP531, 
          t1.ERDOPR12 AS ERDOPR121, 
          t1.ERDOPU12 AS ERDOPU121, 
          t1.ERDTCH12 AS ERDTCH121, 
          t1.ERTTCH12 AS ERTTCH121, 
          t1.ERFTCH12 AS ERFTCH121, 
          t1.ERTOTH12 AS ERTOTH121, 
          t1.ERDMCD12 AS ERDMCD121, 
          t1.ERFMCD12 AS ERFMCD121, 
          t1.ERTMCD12 AS ERTMCD121, 
          t1.ERDMCR12 AS ERDMCR121, 
          t1.ERFMCR12 AS ERFMCR121, 
          t1.ERTMCR12 AS ERTMCR121, 
          t1.ERDOTH12 AS ERDOTH121, 
          t1.ERFOTH12 AS ERFOTH121, 
          t1.ERTOPR12 AS ERTOPR121, 
          t1.ERTOPU12 AS ERTOPU121, 
          t1.ERDSTL12 AS ERDSTL121, 
          t1.ERFSTL12 AS ERFSTL121, 
          t1.ERTSTL12 AS ERTSTL121, 
          t1.ERDOSR12 AS ERDOSR121, 
          t1.ERFOSR12 AS ERFOSR121, 
          t1.ERTOSR12 AS ERTOSR121, 
          t1.ERDOFD12 AS ERDOFD121, 
          t1.ERTOFD12 AS ERTOFD121, 
          t1.ERFOFD12 AS ERFOFD121, 
          t1.ERFOPR12 AS ERFOPR121, 
          t1.ERFOPU12 AS ERFOPU121, 
          t1.ERTPRV12 AS ERTPRV121, 
          t1.ERDPRV12 AS ERDPRV121, 
          t1.ERFPRV12 AS ERFPRV121, 
          t1.ERDPTR12 AS ERDPTR121, 
          t1.ERFPTR12 AS ERFPTR121, 
          t1.ERTPTR12 AS ERTPTR121, 
          t1.ERDSLF12 AS ERDSLF121, 
          t1.ERFSLF12 AS ERFSLF121, 
          t1.ERTSLF12 AS ERTSLF121, 
          t1.ERDTRI12 AS ERDTRI121, 
          t1.ERFTRI12 AS ERFTRI121, 
          t1.ERTTRI12 AS ERTTRI121, 
          t1.ERDVA12 AS ERDVA121, 
          t1.ERFVA12 AS ERFVA121, 
          t1.ERTVA12 AS ERTVA121, 
          t1.ERDWCP12 AS ERDWCP121, 
          t1.ERFWCP12 AS ERFWCP121, 
          t1.ERTWCP12 AS ERTWCP121, 
          t1.MIDX AS MIDX1, 
          t1.ERTOT12 AS ERTOT121, 
          t1.ARTHDX AS ARTHDX1, 
          t1.ENGCMF42 AS ENGCMF421, 
          t1.ENGSPK42 AS ENGSPK421, 
          t1.LANGHM42 AS LANGHM421, 
          t1.STRKDX AS STRKDX1, 
          t1.PHQ242 AS PHQ2421, 
          t1.K6SUM42 AS K6SUM421, 
          t1.SFFLAG42 AS SFFLAG421, 
          t1.MCS42 AS MCS421, 
          t1.PCS42 AS PCS421, 
          t1.JTPAIN31 AS JTPAIN311, 
          t1.JTPAIN53 AS JTPAIN531, 
          t1.INS12X AS INS12X1, 
          t1.INS53X AS INS53X1, 
          t1.INS31X AS INS31X1, 
          t1.INS42X AS INS42X1, 
          t1.INSAT31X AS INSAT31X1, 
          t1.INSAT42X AS INSAT42X1, 
          t1.INSAT53X AS INSAT53X1, 
          t1.INSAT12X AS INSAT12X1, 
          t1.INSCOV12 AS INSCOV121, 
          t1.USBORN42 AS USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          /* EDUCYR_RECODE */
            (CASE 
               WHEN -1 = t1.EDUCYR THEN .
               WHEN -7 = t1.EDUCYR THEN .
               WHEN -8 = t1.EDUCYR THEN .
               WHEN -9 = t1.EDUCYR THEN .
               ELSE t1.EDUCYR
            END) LABEL="Education level recoded missing Recoded" AS EDUCYR_RECODE, 
          /* EDUYRDEG_RECODE */
            (CASE 
               WHEN -1 = t1.EDUYRDEG THEN .
               WHEN -7 = t1.EDUYRDEG THEN .
               WHEN -8 = t1.EDUYRDEG THEN .
               WHEN -9 = t1.EDUYRDEG THEN .
               ELSE t1.EDUYRDEG
            END) LABEL="Education or Highest Degree Recoded" AS EDUYRDEG_RECODE, 
          /* ADGENH42_RECODE */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="ADGENH42 Recoded" AS ADGENH42_RECODE, 
          /* ADDAYA42_RECODE */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="HLTH LIMITS MOD Recoded" AS ADDAYA42_RECODE, 
          /* ADCLIM42_RECODE */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="HLTH LIMITS CLIMBING STAIRS 2012 Recoded" AS ADCLIM42_RECODE, 
          /* ADPALS42_RECODE */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="SAQ 4WKS ACCMP LESS PHY PRBS Recoded" AS ADPALS42_RECODE, 
          /* ADPWLM42_RECODE */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="SAQ 4WKS WORK LIMT PHY PROBS Recoded" AS ADPWLM42_RECODE, 
          /* ADMALS42_RECODE */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="SAQ 4WKS ACCMP LESS MNT PRBS Recoded" AS ADMALS42_RECODE, 
          /* ADPAIN42_RECODE */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="SAW 4WKS PAIN LIMITS NORMAL WORK Recode" AS ADPAIN42_RECODE, 
          /* AGE12X_RECODED */
            (CASE 
               WHEN -1 = t1.AGE12X THEN .
               ELSE t1.AGE12X
            END) LABEL="AGE AS OF 123112 Recode" AS AGE12X_RECODED, 
          /* ADCAPE42_RECODE */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="SAQ 4WKS FELT CALM/PEACEFUL Recode" AS ADCAPE42_RECODE, 
          /* REGION12_RECODE */
            (CASE 
               WHEN -1 = t1.REGION12 THEN .
               ELSE t1.REGION12
            END) LABEL="CENSUS REGION AS OF 123112 Recode" AS REGION12_RECODE, 
          /* ADNRGY42_RECODE */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="SAQ 4WKS HAD A LOT OF ENERGY Recode" AS ADNRGY42_RECODE, 
          /* MARRY12X_RECODE */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="MARITAL STATUS 123112 Recoded" AS MARRY12X_RECODE, 
          /* ADMWLM42_RECODE */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="SAQ 4WKS WORK LIMT MNT PROBS Recode" AS ADMWLM42_RECODE, 
          /* ADDOWN42_RECODE */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="SAW 4WKS FELT DOWNHEARTED/DEPR Recode" AS ADDOWN42_RECODE, 
          /* ADSOCA42_RECODE */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="SAQ 4WKS HLTH STOPPED SOC ACTIV Recoded" AS ADSOCA42_RECODE, 
          /* ADHECR42_RECODED */
            (CASE 
               WHEN -1 = t1.ADHECR42 THEN .
               WHEN -9 = t1.ADHECR42 THEN .
               ELSE t1.ADHECR42
            END) LABEL="SAQ 12 MOS RATING OF HEALTH CARE Recoded" AS ADHECR42_RECODED, 
          /* ADHOPE42_RECODE */
            (CASE 
               WHEN -1 = t1.ADHOPE42 THEN .
               WHEN -7 = t1.ADHOPE42 THEN .
               WHEN -8 = t1.ADHOPE42 THEN .
               WHEN -9 = t1.ADHOPE42 THEN .
               ELSE t1.ADHOPE42
            END) LABEL="SAQ 30 DAYS: HOW OFTEN FELT HOPELESS Recode" AS ADHOPE42_RECODE, 
          /* ADREST42_RECODE */
            (CASE 
               WHEN -1 = t1.ADREST42 THEN .
               WHEN -7 = t1.ADREST42 THEN .
               WHEN -8 = t1.ADREST42 THEN .
               WHEN -9 = t1.ADREST42 THEN .
               ELSE t1.ADREST42
            END) LABEL="SAQ 30 DAYS: HOW OFTEN FELT RESTLESS Recoded" AS ADREST42_RECODE, 
          /* ADSAD42_RECODE */
            (CASE 
               WHEN -1 = t1.ADSAD42 THEN .
               WHEN -7 = t1.ADSAD42 THEN .
               WHEN -8 = t1.ADSAD42 THEN .
               WHEN -9 = t1.ADSAD42 THEN .
               ELSE t1.ADSAD42
            END) LABEL="SAQ 30 DAYS: HOW OFTEN FELT SAD Recoded" AS ADSAD42_RECODE, 
          /* ADWRTH42_RECODE */
            (CASE 
               WHEN -1 = t1.ADWRTH42 THEN .
               WHEN -7 = t1.ADWRTH42 THEN .
               WHEN -8 = t1.ADWRTH42 THEN .
               WHEN -9 = t1.ADWRTH42 THEN .
               ELSE t1.ADWRTH42
            END) LABEL="SAQ 30 DAYS: HOW OFTEN FELT WORTHLESS Recoded" AS ADWRTH42_RECODE, 
          /* K6SUM42_RECODE */
            (CASE 
               WHEN -1 = t1.K6SUM42 THEN .
               WHEN -9 = t1.K6SUM42 THEN .
               ELSE t1.K6SUM42
            END) LABEL="SAQ 30 DAYS: OVERALL RATING OF FEELINGS Recoded" AS K6SUM42_RECODE, 
          /* ADINSA42_RECODE */
            (CASE 
               WHEN -1 = t1.ADINSA42 THEN .
               WHEN -7 = t1.ADINSA42 THEN .
               WHEN -8 = t1.ADINSA42 THEN .
               WHEN -9 = t1.ADINSA42 THEN .
               ELSE t1.ADINSA42
            END) LABEL="SAQ: DO NOT NEED HEALTH INSURANCE Recoded" AS ADINSA42_RECODE, 
          /* ADINSB42_RECODE */
            (CASE 
               WHEN -1 = t1.ADINSB42 THEN .
               WHEN -7 = t1.ADINSB42 THEN .
               WHEN -8 = t1.ADINSB42 THEN .
               WHEN -9 = t1.ADINSB42 THEN .
               ELSE t1.ADINSB42
            END) LABEL="SAQ: HEALTH INSURANCE NOT WORTH COST Recoded" AS ADINSB42_RECODE, 
          /* PHQ242_ RECODE */
            (CASE 
               WHEN -1 = t1.PHQ242 THEN .
               WHEN -9 = t1.PHQ242 THEN .
               ELSE t1.PHQ242
            END) LABEL="SAQ 2 WKS: OVERALL RATING OF FEELINGS Recoded" AS 'PHQ242_ RECODE'n, 
          /* ADNERV42_RECODE */
            (CASE 
               WHEN -1 = t1.ADNERV42 THEN .
               WHEN -7 = t1.ADNERV42 THEN .
               WHEN -8 = t1.ADNERV42 THEN .
               WHEN -9 = t1.ADNERV42 THEN .
               ELSE t1.ADNERV42
            END) LABEL="SAQ 30 DAYS: HOW OFTEN FELT NERVOUS Recoded" AS ADNERV42_RECODE, 
          /* CANCERDX_RECODE */
            (CASE 
               WHEN -1 = t1.CANCERDX THEN .
               WHEN -7 = t1.CANCERDX THEN .
               WHEN -8 = t1.CANCERDX THEN .
               WHEN -9 = t1.CANCERDX THEN .
               ELSE t1.CANCERDX
            END) LABEL="CANCER DIAGNOSIS Recoded" AS CANCERDX_RECODE, 
          /* INS12X_RECODE */
            (CASE 
               WHEN -1 = t1.INS12X THEN .
               ELSE t1.INS12X
            END) LABEL="INSURED 123112 Recoded" AS INS12X_RECODE, 
          /* MIDX_RECODE */
            (CASE 
               WHEN -1 = t1.MIDX THEN .
               WHEN -7 = t1.MIDX THEN .
               WHEN -8 = t1.MIDX THEN .
               WHEN -9 = t1.MIDX THEN .
               ELSE t1.MIDX
            END) LABEL="HEART ATTACK DIAG Recoded" AS MIDX_RECODE, 
          /* ARTHDX_RECODE */
            (CASE 
               WHEN -1 = t1.ARTHDX THEN .
               WHEN -7 = t1.ARTHDX THEN .
               WHEN -8 = t1.ARTHDX THEN .
               WHEN -9 = t1.ARTHDX THEN .
               ELSE t1.ARTHDX
            END) LABEL="ARTHRITIS DIAGNOSIS Recoded" AS ARTHDX_RECODE, 
          /* STRKDX_RECODE */
            (CASE 
               WHEN -1 = t1.STRKDX THEN .
               WHEN -7 = t1.STRKDX THEN .
               WHEN -8 = t1.STRKDX THEN .
               WHEN -9 = t1.STRKDX THEN .
               ELSE t1.STRKDX
            END) LABEL="STROKE DIAGNOSIS Recoded" AS STRKDX_RECODE, 
          /* EMPHDX_RECODE */
            (CASE 
               WHEN -1 = t1.EMPHDX THEN .
               WHEN -7 = t1.EMPHDX THEN .
               WHEN -8 = t1.EMPHDX THEN .
               WHEN -9 = t1.EMPHDX THEN .
               ELSE t1.EMPHDX
            END) LABEL="EMPHYSEMA DIAGNOSIS Recoded" AS EMPHDX_RECODE, 
          /* USBORN42_RECODE */
            (CASE 
               WHEN -1 = t1.USBORN42 THEN .
               WHEN -7 = t1.USBORN42 THEN .
               WHEN -8 = t1.USBORN42 THEN .
               WHEN -9 = t1.USBORN42 THEN .
               ELSE t1.USBORN42
            END) LABEL="AC03 WAS PERSON BORN IN US Recoded" AS USBORN42_RECODE, 
          /* ENGCMF42_RECODE */
            (CASE 
               WHEN -1 = t1.ENGCMF42 THEN .
               WHEN -7 = t1.ENGCMF42 THEN .
               WHEN -8 = t1.ENGCMF42 THEN .
               WHEN -9 = t1.ENGCMF42 THEN .
               ELSE t1.ENGCMF42
            END) LABEL="AC02 WHOLE HH COMFRTBLE SPEAKNG ENG Recoded" AS ENGCMF42_RECODE, 
          /* ENGSPK42_RECODE */
            (CASE 
               WHEN -1 = t1.ENGSPK42 THEN .
               WHEN -9 = t1.ENGSPK42 THEN .
               ELSE t1.ENGSPK42
            END) LABEL="AC02A NOT COMFRTBLE SPEAKNG ENGLISH Recoded" AS ENGSPK42_RECODE, 
          /* JTPAIN31_RECODE */
            (CASE 
               WHEN -1 = t1.JTPAIN31 THEN .
               WHEN -7 = t1.JTPAIN31 THEN .
               WHEN -8 = t1.JTPAIN31 THEN .
               WHEN -9 = t1.JTPAIN31 THEN .
               ELSE t1.JTPAIN31
            END) LABEL="JOINT PAIN LAST 12 MONTHS 3/1 Recoded" AS JTPAIN31_RECODE, 
          /* JTPAIN53_RECODE */
            (CASE 
               WHEN -1 = t1.JTPAIN53 THEN .
               WHEN -7 = t1.JTPAIN53 THEN .
               WHEN -8 = t1.JTPAIN53 THEN .
               WHEN -9 = t1.JTPAIN53 THEN .
               ELSE t1.JTPAIN53
            END) LABEL="JOINT PAIN LAST 12 MONTHS 5/3 Recoded" AS JTPAIN53_RECODE, 
          /* ERECODE_RECODE */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="Educational Recode Recoded" AS ERECODE_RECODE
      FROM MYDATA.MEPS_FILTER_FULLYR_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: RERECODE_DX   */
%LET _CLIENTTASKLABEL='RERECODE_DX';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FILTER_FULLY_0000);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FILTER_FULLY_0000 AS 
   SELECT t1.DUPERSID1, 
          t1.AGE12X1, 
          t1.SEX1, 
          t1.EDUCYR1, 
          t1.EDUYRDEG1, 
          t1.REGION121, 
          t1.RACETHX1, 
          t1.MARRY12X1, 
          t1.SAQELIG1, 
          t1.SAQWT12F1, 
          t1.ACTDTY311, 
          t1.ACTDTY421, 
          t1.ACTDTY531, 
          t1.ADAPPT421, 
          t1.ADCAPE421, 
          t1.ADCLIM421, 
          t1.ADCMPD421, 
          t1.ADCMPM421, 
          t1.ADCMPY421, 
          t1.ADDAYA421, 
          t1.ADDOWN421, 
          t1.ADDPRS421, 
          t1.ADDRBP421, 
          t1.ADEFRT421, 
          t1.ADEGMC421, 
          t1.ADEXPL421, 
          t1.ADEZUN421, 
          t1.ADFFRM421, 
          t1.ADFHLP421, 
          t1.ADGENH421, 
          t1.ADHECR421, 
          t1.ADHOPE421, 
          t1.ADILCR421, 
          t1.ADILWW421, 
          t1.ADINSA421, 
          t1.ADINSB421, 
          t1.ADINST421, 
          t1.ADINTR421, 
          t1.ADLANG421, 
          t1.ADLIST421, 
          t1.ADMALS421, 
          t1.ADMWLM421, 
          t1.ADNDCR421, 
          t1.ADNERV421, 
          t1.ADNRGY421, 
          t1.ADNSMK421, 
          t1.ADOVER421, 
          t1.ADPAIN421, 
          t1.ADPALS421, 
          t1.ADPRTM421, 
          t1.ADPRX421, 
          t1.ADPWLM421, 
          t1.ADRESP421, 
          t1.ADREST421, 
          t1.ADRISK421, 
          t1.ADRTCR421, 
          t1.ADRTWW421, 
          t1.ADSAD421, 
          t1.ADSMOK421, 
          t1.ADSOCA421, 
          t1.ADSPEC421, 
          t1.ADSPRF421, 
          t1.ADTLHW421, 
          t1.ADWRTH421, 
          t1.CANCERDX1, 
          t1.DVGEN121, 
          t1.DVGEXP121, 
          t1.DVGMCD121, 
          t1.DVGMCR121, 
          t1.DVGOFD121, 
          t1.DVGOPR121, 
          t1.DVGOPU121, 
          t1.DVGOSR121, 
          t1.DVGOTH121, 
          t1.DVGPRV121, 
          t1.DVGPTR121, 
          t1.DVGSLF121, 
          t1.DVGSTL121, 
          t1.DVGTCH121, 
          t1.DVGTRI121, 
          t1.DVGVA121, 
          t1.DVGWCP121, 
          t1.DVOEXP121, 
          t1.DVOMCD121, 
          t1.DVOMCR121, 
          t1.DVOOFD121, 
          t1.DVOOPR121, 
          t1.DVOOPU121, 
          t1.DVOOSR121, 
          t1.DVOOTH121, 
          t1.DVOPRV121, 
          t1.DVOPTR121, 
          t1.DVORTH121, 
          t1.DVOSLF121, 
          t1.DVOSTL121, 
          t1.DVOTCH121, 
          t1.DVOTRI121, 
          t1.DVOVA121, 
          t1.DVOWCP121, 
          t1.DVTEXP121, 
          t1.DVTMCD121, 
          t1.DVTMCR121, 
          t1.DVTOFD121, 
          t1.DVTOPR121, 
          t1.DVTOPU121, 
          t1.DVTOSR121, 
          t1.DVTOT121, 
          t1.DVTOTH121, 
          t1.DVTPRV121, 
          t1.DVTPTR121, 
          t1.DVTSLF121, 
          t1.DVTSTL121, 
          t1.DVTTCH121, 
          t1.DVTTRI121, 
          t1.DVTVA121, 
          t1.DVTWCP121, 
          t1.EMPHDX1, 
          t1.EMPST311, 
          t1.EMPST421, 
          t1.EMPST531, 
          t1.OFREMP311, 
          t1.OFREMP421, 
          t1.OFREMP531, 
          t1.ERDOPR121, 
          t1.ERDOPU121, 
          t1.ERDTCH121, 
          t1.ERTTCH121, 
          t1.ERFTCH121, 
          t1.ERTOTH121, 
          t1.ERDMCD121, 
          t1.ERFMCD121, 
          t1.ERTMCD121, 
          t1.ERDMCR121, 
          t1.ERFMCR121, 
          t1.ERTMCR121, 
          t1.ERDOTH121, 
          t1.ERFOTH121, 
          t1.ERTOPR121, 
          t1.ERTOPU121, 
          t1.ERDSTL121, 
          t1.ERFSTL121, 
          t1.ERTSTL121, 
          t1.ERDOSR121, 
          t1.ERFOSR121, 
          t1.ERTOSR121, 
          t1.ERDOFD121, 
          t1.ERTOFD121, 
          t1.ERFOFD121, 
          t1.ERFOPR121, 
          t1.ERFOPU121, 
          t1.ERTPRV121, 
          t1.ERDPRV121, 
          t1.ERFPRV121, 
          t1.ERDPTR121, 
          t1.ERFPTR121, 
          t1.ERTPTR121, 
          t1.ERDSLF121, 
          t1.ERFSLF121, 
          t1.ERTSLF121, 
          t1.ERDTRI121, 
          t1.ERFTRI121, 
          t1.ERTTRI121, 
          t1.ERDVA121, 
          t1.ERFVA121, 
          t1.ERTVA121, 
          t1.ERDWCP121, 
          t1.ERFWCP121, 
          t1.ERTWCP121, 
          t1.MIDX1, 
          t1.ERTOT121, 
          t1.ARTHDX1, 
          t1.ENGCMF421, 
          t1.ENGSPK421, 
          t1.LANGHM421, 
          t1.STRKDX1, 
          t1.PHQ2421, 
          t1.K6SUM421, 
          t1.SFFLAG421, 
          t1.MCS421, 
          t1.PCS421, 
          t1.JTPAIN311, 
          t1.JTPAIN531, 
          t1.INS12X1, 
          t1.INS53X1, 
          t1.INS31X1, 
          t1.INS42X1, 
          t1.INSAT31X1, 
          t1.INSAT42X1, 
          t1.INSAT53X1, 
          t1.INSAT12X1, 
          t1.INSCOV121, 
          t1.USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.EDUCYR_RECODE, 
          t1.EDUYRDEG_RECODE, 
          t1.ADGENH42_RECODE, 
          t1.ADDAYA42_RECODE, 
          t1.ADCLIM42_RECODE, 
          t1.ADPALS42_RECODE, 
          t1.ADPWLM42_RECODE, 
          t1.ADMALS42_RECODE, 
          t1.ADPAIN42_RECODE, 
          t1.AGE12X_RECODED, 
          t1.ADCAPE42_RECODE, 
          t1.REGION12_RECODE, 
          t1.ADNRGY42_RECODE, 
          t1.MARRY12X_RECODE, 
          t1.ADMWLM42_RECODE, 
          t1.ADDOWN42_RECODE, 
          t1.ADSOCA42_RECODE, 
          t1.ADHECR42_RECODED, 
          t1.ADHOPE42_RECODE, 
          t1.ADREST42_RECODE, 
          t1.ADSAD42_RECODE, 
          t1.ADWRTH42_RECODE, 
          t1.K6SUM42_RECODE, 
          t1.ADINSA42_RECODE, 
          t1.ADINSB42_RECODE, 
          t1.'PHQ242_ RECODE'n, 
          t1.ADNERV42_RECODE, 
          t1.CANCERDX_RECODE, 
          t1.INS12X_RECODE, 
          t1.MIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.STRKDX_RECODE, 
          t1.EMPHDX_RECODE, 
          t1.USBORN42_RECODE, 
          t1.ENGCMF42_RECODE, 
          t1.ENGSPK42_RECODE, 
          t1.JTPAIN31_RECODE, 
          t1.JTPAIN53_RECODE, 
          /* Aggregate_diagnosis */
            (SUM(t1.CANCERDX_RECODE,t1.MIDX_RECODE,t1.ARTHDX_RECODE,t1.STRKDX_RECODE,t1.EMPHDX_RECODE)) LABEL=
            "Aggregate of DX for various ailments" AS Aggregate_diagnosis, 
          /* CANCERDX_RERECODE */
            (CASE 
               WHEN 2 = t1.CANCERDX_RECODE THEN 0
               ELSE t1.CANCERDX_RECODE
            END) LABEL="Cancer diagnosis recoded for categorical" AS CANCERDX_RERECODE, 
          /* MIDX_RERECODE */
            (CASE 
               WHEN 2 = t1.MIDX_RECODE THEN 0
               ELSE t1.MIDX_RECODE
            END) LABEL="Heart attack diagnosis recoded for categorical" AS MIDX_RERECODE, 
          /* ARTHDX_RERECODE */
            (CASE 
               WHEN 2 = t1.ARTHDX_RECODE THEN 0
               ELSE t1.ARTHDX_RECODE
            END) LABEL="Arthritis diagnosis recoded for categorical" AS ARTHDX_RERECODE, 
          /* STRKDX_RERECODE */
            (CASE 
               WHEN 2 = t1.STRKDX_RECODE THEN 0
               ELSE t1.STRKDX_RECODE
            END) LABEL="Stroke diagnosis recoded for categorical" AS STRKDX_RERECODE, 
          /* EMPHDX_RERECODE */
            (CASE 
               WHEN 2 = t1.EMPHDX_RECODE THEN 0
               ELSE t1.EMPHDX_RECODE
            END) LABEL="Emphyzima diagnosis recoded for categorical" AS EMPHDX_RERECODE
      FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: AGGSUM_DX   */
%LET _CLIENTTASKLABEL='AGGSUM_DX';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FILTER_FULLY);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FILTER_FULLY AS 
   SELECT t1.DUPERSID1, 
          t1.AGE12X1, 
          t1.SEX1, 
          t1.EDUCYR1, 
          t1.EDUYRDEG1, 
          t1.REGION121, 
          t1.RACETHX1, 
          t1.MARRY12X1, 
          t1.SAQELIG1, 
          t1.SAQWT12F1, 
          t1.ACTDTY311, 
          t1.ACTDTY421, 
          t1.ACTDTY531, 
          t1.ADAPPT421, 
          t1.ADCAPE421, 
          t1.ADCLIM421, 
          t1.ADCMPD421, 
          t1.ADCMPM421, 
          t1.ADCMPY421, 
          t1.ADDAYA421, 
          t1.ADDOWN421, 
          t1.ADDPRS421, 
          t1.ADDRBP421, 
          t1.ADEFRT421, 
          t1.ADEGMC421, 
          t1.ADEXPL421, 
          t1.ADEZUN421, 
          t1.ADFFRM421, 
          t1.ADFHLP421, 
          t1.ADGENH421, 
          t1.ADHECR421, 
          t1.ADHOPE421, 
          t1.ADILCR421, 
          t1.ADILWW421, 
          t1.ADINSA421, 
          t1.ADINSB421, 
          t1.ADINST421, 
          t1.ADINTR421, 
          t1.ADLANG421, 
          t1.ADLIST421, 
          t1.ADMALS421, 
          t1.ADMWLM421, 
          t1.ADNDCR421, 
          t1.ADNERV421, 
          t1.ADNRGY421, 
          t1.ADNSMK421, 
          t1.ADOVER421, 
          t1.ADPAIN421, 
          t1.ADPALS421, 
          t1.ADPRTM421, 
          t1.ADPRX421, 
          t1.ADPWLM421, 
          t1.ADRESP421, 
          t1.ADREST421, 
          t1.ADRISK421, 
          t1.ADRTCR421, 
          t1.ADRTWW421, 
          t1.ADSAD421, 
          t1.ADSMOK421, 
          t1.ADSOCA421, 
          t1.ADSPEC421, 
          t1.ADSPRF421, 
          t1.ADTLHW421, 
          t1.ADWRTH421, 
          t1.CANCERDX1, 
          t1.DVGEN121, 
          t1.DVGEXP121, 
          t1.DVGMCD121, 
          t1.DVGMCR121, 
          t1.DVGOFD121, 
          t1.DVGOPR121, 
          t1.DVGOPU121, 
          t1.DVGOSR121, 
          t1.DVGOTH121, 
          t1.DVGPRV121, 
          t1.DVGPTR121, 
          t1.DVGSLF121, 
          t1.DVGSTL121, 
          t1.DVGTCH121, 
          t1.DVGTRI121, 
          t1.DVGVA121, 
          t1.DVGWCP121, 
          t1.DVOEXP121, 
          t1.DVOMCD121, 
          t1.DVOMCR121, 
          t1.DVOOFD121, 
          t1.DVOOPR121, 
          t1.DVOOPU121, 
          t1.DVOOSR121, 
          t1.DVOOTH121, 
          t1.DVOPRV121, 
          t1.DVOPTR121, 
          t1.DVORTH121, 
          t1.DVOSLF121, 
          t1.DVOSTL121, 
          t1.DVOTCH121, 
          t1.DVOTRI121, 
          t1.DVOVA121, 
          t1.DVOWCP121, 
          t1.DVTEXP121, 
          t1.DVTMCD121, 
          t1.DVTMCR121, 
          t1.DVTOFD121, 
          t1.DVTOPR121, 
          t1.DVTOPU121, 
          t1.DVTOSR121, 
          t1.DVTOT121, 
          t1.DVTOTH121, 
          t1.DVTPRV121, 
          t1.DVTPTR121, 
          t1.DVTSLF121, 
          t1.DVTSTL121, 
          t1.DVTTCH121, 
          t1.DVTTRI121, 
          t1.DVTVA121, 
          t1.DVTWCP121, 
          t1.EMPHDX1, 
          t1.EMPST311, 
          t1.EMPST421, 
          t1.EMPST531, 
          t1.OFREMP311, 
          t1.OFREMP421, 
          t1.OFREMP531, 
          t1.ERDOPR121, 
          t1.ERDOPU121, 
          t1.ERDTCH121, 
          t1.ERTTCH121, 
          t1.ERFTCH121, 
          t1.ERTOTH121, 
          t1.ERDMCD121, 
          t1.ERFMCD121, 
          t1.ERTMCD121, 
          t1.ERDMCR121, 
          t1.ERFMCR121, 
          t1.ERTMCR121, 
          t1.ERDOTH121, 
          t1.ERFOTH121, 
          t1.ERTOPR121, 
          t1.ERTOPU121, 
          t1.ERDSTL121, 
          t1.ERFSTL121, 
          t1.ERTSTL121, 
          t1.ERDOSR121, 
          t1.ERFOSR121, 
          t1.ERTOSR121, 
          t1.ERDOFD121, 
          t1.ERTOFD121, 
          t1.ERFOFD121, 
          t1.ERFOPR121, 
          t1.ERFOPU121, 
          t1.ERTPRV121, 
          t1.ERDPRV121, 
          t1.ERFPRV121, 
          t1.ERDPTR121, 
          t1.ERFPTR121, 
          t1.ERTPTR121, 
          t1.ERDSLF121, 
          t1.ERFSLF121, 
          t1.ERTSLF121, 
          t1.ERDTRI121, 
          t1.ERFTRI121, 
          t1.ERTTRI121, 
          t1.ERDVA121, 
          t1.ERFVA121, 
          t1.ERTVA121, 
          t1.ERDWCP121, 
          t1.ERFWCP121, 
          t1.ERTWCP121, 
          t1.MIDX1, 
          t1.ERTOT121, 
          t1.ARTHDX1, 
          t1.ENGCMF421, 
          t1.ENGSPK421, 
          t1.LANGHM421, 
          t1.STRKDX1, 
          t1.PHQ2421, 
          t1.K6SUM421, 
          t1.SFFLAG421, 
          t1.MCS421, 
          t1.PCS421, 
          t1.JTPAIN311, 
          t1.JTPAIN531, 
          t1.INS12X1, 
          t1.INS53X1, 
          t1.INS31X1, 
          t1.INS42X1, 
          t1.INSAT31X1, 
          t1.INSAT42X1, 
          t1.INSAT53X1, 
          t1.INSAT12X1, 
          t1.INSCOV121, 
          t1.USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.EDUCYR_RECODE, 
          t1.EDUYRDEG_RECODE, 
          t1.ADGENH42_RECODE, 
          t1.ADDAYA42_RECODE, 
          t1.ADCLIM42_RECODE, 
          t1.ADPALS42_RECODE, 
          t1.ADPWLM42_RECODE, 
          t1.ADMALS42_RECODE, 
          t1.ADPAIN42_RECODE, 
          t1.AGE12X_RECODED, 
          t1.ADCAPE42_RECODE, 
          t1.REGION12_RECODE, 
          t1.ADNRGY42_RECODE, 
          t1.MARRY12X_RECODE, 
          t1.ADMWLM42_RECODE, 
          t1.ADDOWN42_RECODE, 
          t1.ADSOCA42_RECODE, 
          t1.ADHECR42_RECODED, 
          t1.ADHOPE42_RECODE, 
          t1.ADREST42_RECODE, 
          t1.ADSAD42_RECODE, 
          t1.ADWRTH42_RECODE, 
          t1.K6SUM42_RECODE, 
          t1.ADINSA42_RECODE, 
          t1.ADINSB42_RECODE, 
          t1.'PHQ242_ RECODE'n, 
          t1.ADNERV42_RECODE, 
          t1.CANCERDX_RECODE, 
          t1.INS12X_RECODE, 
          t1.MIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.STRKDX_RECODE, 
          t1.EMPHDX_RECODE, 
          t1.USBORN42_RECODE, 
          t1.ENGCMF42_RECODE, 
          t1.ENGSPK42_RECODE, 
          t1.JTPAIN31_RECODE, 
          t1.JTPAIN53_RECODE, 
          t1.Aggregate_diagnosis, 
          t1.CANCERDX_RERECODE, 
          t1.MIDX_RERECODE, 
          t1.ARTHDX_RERECODE, 
          t1.STRKDX_RERECODE, 
          t1.EMPHDX_RERECODE, 
          /* Aggregate_Sum_of_Diagnosis */
            (SUM(t1.CANCERDX_RERECODE,t1.MIDX_RERECODE,t1.ARTHDX_RERECODE,t1.STRKDX_RERECODE,t1.EMPHDX_RERECODE)) LABEL=
            "Diagnosis aggregate sum" AS Aggregate_Sum_of_Diagnosis
      FROM WORK.QUERY_FOR_MEPS_FILTER_FULLY_0000 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:09 PM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Aggregate_Sum_of_Diagnosis
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLY as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES Aggregate_Sum_of_Diagnosis /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:10 PM
   By task: Summary Statistics

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Aggregate_Sum_of_Diagnosis
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLY as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR Aggregate_Sum_of_Diagnosis;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for DX 2012   */
%LET _CLIENTTASKLABEL='Distribution Analysis for DX 2012';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:10 PM
   By task: Distribution Analysis for DX 2012

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Aggregate_Sum_of_Diagnosis
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLY(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Aggregate_Sum_of_Diagnosis";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS QUANTILES;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR Aggregate_Sum_of_Diagnosis;
	HISTOGRAM   Aggregate_Sum_of_Diagnosis / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: CATAG_DX   */
%LET _CLIENTTASKLABEL='CATAG_DX';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FILTER_FULLY_0001);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FILTER_FULLY_0001 AS 
   SELECT t1.DUPERSID1, 
          t1.AGE12X1, 
          t1.SEX1, 
          t1.EDUCYR1, 
          t1.EDUYRDEG1, 
          t1.REGION121, 
          t1.RACETHX1, 
          t1.MARRY12X1, 
          t1.SAQELIG1, 
          t1.SAQWT12F1, 
          t1.ACTDTY311, 
          t1.ACTDTY421, 
          t1.ACTDTY531, 
          t1.ADAPPT421, 
          t1.ADCAPE421, 
          t1.ADCLIM421, 
          t1.ADCMPD421, 
          t1.ADCMPM421, 
          t1.ADCMPY421, 
          t1.ADDAYA421, 
          t1.ADDOWN421, 
          t1.ADDPRS421, 
          t1.ADDRBP421, 
          t1.ADEFRT421, 
          t1.ADEGMC421, 
          t1.ADEXPL421, 
          t1.ADEZUN421, 
          t1.ADFFRM421, 
          t1.ADFHLP421, 
          t1.ADGENH421, 
          t1.ADHECR421, 
          t1.ADHOPE421, 
          t1.ADILCR421, 
          t1.ADILWW421, 
          t1.ADINSA421, 
          t1.ADINSB421, 
          t1.ADINST421, 
          t1.ADINTR421, 
          t1.ADLANG421, 
          t1.ADLIST421, 
          t1.ADMALS421, 
          t1.ADMWLM421, 
          t1.ADNDCR421, 
          t1.ADNERV421, 
          t1.ADNRGY421, 
          t1.ADNSMK421, 
          t1.ADOVER421, 
          t1.ADPAIN421, 
          t1.ADPALS421, 
          t1.ADPRTM421, 
          t1.ADPRX421, 
          t1.ADPWLM421, 
          t1.ADRESP421, 
          t1.ADREST421, 
          t1.ADRISK421, 
          t1.ADRTCR421, 
          t1.ADRTWW421, 
          t1.ADSAD421, 
          t1.ADSMOK421, 
          t1.ADSOCA421, 
          t1.ADSPEC421, 
          t1.ADSPRF421, 
          t1.ADTLHW421, 
          t1.ADWRTH421, 
          t1.CANCERDX1, 
          t1.DVGEN121, 
          t1.DVGEXP121, 
          t1.DVGMCD121, 
          t1.DVGMCR121, 
          t1.DVGOFD121, 
          t1.DVGOPR121, 
          t1.DVGOPU121, 
          t1.DVGOSR121, 
          t1.DVGOTH121, 
          t1.DVGPRV121, 
          t1.DVGPTR121, 
          t1.DVGSLF121, 
          t1.DVGSTL121, 
          t1.DVGTCH121, 
          t1.DVGTRI121, 
          t1.DVGVA121, 
          t1.DVGWCP121, 
          t1.DVOEXP121, 
          t1.DVOMCD121, 
          t1.DVOMCR121, 
          t1.DVOOFD121, 
          t1.DVOOPR121, 
          t1.DVOOPU121, 
          t1.DVOOSR121, 
          t1.DVOOTH121, 
          t1.DVOPRV121, 
          t1.DVOPTR121, 
          t1.DVORTH121, 
          t1.DVOSLF121, 
          t1.DVOSTL121, 
          t1.DVOTCH121, 
          t1.DVOTRI121, 
          t1.DVOVA121, 
          t1.DVOWCP121, 
          t1.DVTEXP121, 
          t1.DVTMCD121, 
          t1.DVTMCR121, 
          t1.DVTOFD121, 
          t1.DVTOPR121, 
          t1.DVTOPU121, 
          t1.DVTOSR121, 
          t1.DVTOT121, 
          t1.DVTOTH121, 
          t1.DVTPRV121, 
          t1.DVTPTR121, 
          t1.DVTSLF121, 
          t1.DVTSTL121, 
          t1.DVTTCH121, 
          t1.DVTTRI121, 
          t1.DVTVA121, 
          t1.DVTWCP121, 
          t1.EMPHDX1, 
          t1.EMPST311, 
          t1.EMPST421, 
          t1.EMPST531, 
          t1.OFREMP311, 
          t1.OFREMP421, 
          t1.OFREMP531, 
          t1.ERDOPR121, 
          t1.ERDOPU121, 
          t1.ERDTCH121, 
          t1.ERTTCH121, 
          t1.ERFTCH121, 
          t1.ERTOTH121, 
          t1.ERDMCD121, 
          t1.ERFMCD121, 
          t1.ERTMCD121, 
          t1.ERDMCR121, 
          t1.ERFMCR121, 
          t1.ERTMCR121, 
          t1.ERDOTH121, 
          t1.ERFOTH121, 
          t1.ERTOPR121, 
          t1.ERTOPU121, 
          t1.ERDSTL121, 
          t1.ERFSTL121, 
          t1.ERTSTL121, 
          t1.ERDOSR121, 
          t1.ERFOSR121, 
          t1.ERTOSR121, 
          t1.ERDOFD121, 
          t1.ERTOFD121, 
          t1.ERFOFD121, 
          t1.ERFOPR121, 
          t1.ERFOPU121, 
          t1.ERTPRV121, 
          t1.ERDPRV121, 
          t1.ERFPRV121, 
          t1.ERDPTR121, 
          t1.ERFPTR121, 
          t1.ERTPTR121, 
          t1.ERDSLF121, 
          t1.ERFSLF121, 
          t1.ERTSLF121, 
          t1.ERDTRI121, 
          t1.ERFTRI121, 
          t1.ERTTRI121, 
          t1.ERDVA121, 
          t1.ERFVA121, 
          t1.ERTVA121, 
          t1.ERDWCP121, 
          t1.ERFWCP121, 
          t1.ERTWCP121, 
          t1.MIDX1, 
          t1.ERTOT121, 
          t1.ARTHDX1, 
          t1.ENGCMF421, 
          t1.ENGSPK421, 
          t1.LANGHM421, 
          t1.STRKDX1, 
          t1.PHQ2421, 
          t1.K6SUM421, 
          t1.SFFLAG421, 
          t1.MCS421, 
          t1.PCS421, 
          t1.JTPAIN311, 
          t1.JTPAIN531, 
          t1.INS12X1, 
          t1.INS53X1, 
          t1.INS31X1, 
          t1.INS42X1, 
          t1.INSAT31X1, 
          t1.INSAT42X1, 
          t1.INSAT53X1, 
          t1.INSAT12X1, 
          t1.INSCOV121, 
          t1.USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.EDUCYR_RECODE, 
          t1.EDUYRDEG_RECODE, 
          t1.ADGENH42_RECODE, 
          t1.ADDAYA42_RECODE, 
          t1.ADCLIM42_RECODE, 
          t1.ADPALS42_RECODE, 
          t1.ADPWLM42_RECODE, 
          t1.ADMALS42_RECODE, 
          t1.ADPAIN42_RECODE, 
          t1.AGE12X_RECODED, 
          t1.ADCAPE42_RECODE, 
          t1.REGION12_RECODE, 
          t1.ADNRGY42_RECODE, 
          t1.MARRY12X_RECODE, 
          t1.ADMWLM42_RECODE, 
          t1.ADDOWN42_RECODE, 
          t1.ADSOCA42_RECODE, 
          t1.ADHECR42_RECODED, 
          t1.ADHOPE42_RECODE, 
          t1.ADREST42_RECODE, 
          t1.ADSAD42_RECODE, 
          t1.ADWRTH42_RECODE, 
          t1.K6SUM42_RECODE, 
          t1.ADINSA42_RECODE, 
          t1.ADINSB42_RECODE, 
          t1.'PHQ242_ RECODE'n, 
          t1.ADNERV42_RECODE, 
          t1.CANCERDX_RECODE, 
          t1.INS12X_RECODE, 
          t1.MIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.STRKDX_RECODE, 
          t1.EMPHDX_RECODE, 
          t1.USBORN42_RECODE, 
          t1.ENGCMF42_RECODE, 
          t1.ENGSPK42_RECODE, 
          t1.JTPAIN31_RECODE, 
          t1.JTPAIN53_RECODE, 
          t1.Aggregate_diagnosis, 
          t1.CANCERDX_RERECODE, 
          t1.MIDX_RERECODE, 
          t1.ARTHDX_RERECODE, 
          t1.STRKDX_RERECODE, 
          t1.EMPHDX_RERECODE, 
          t1.Aggregate_Sum_of_Diagnosis, 
          /* Categorical_of_DX */
            (CASE  
               WHEN t1.Aggregate_Sum_of_Diagnosis = 0
               THEN 0
               WHEN t1.Aggregate_Sum_of_Diagnosis = 1
               THEN 1
               WHEN t1.Aggregate_Sum_of_Diagnosis = 2
               THEN 2
               ELSE 3
            END) LABEL="Categorical of DX" AS Categorical_of_DX
      FROM WORK.QUERY_FOR_MEPS_FILTER_FULLY t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Categorical_VS_Qantitative   */
%LET _CLIENTTASKLABEL='Categorical_VS_Qantitative';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:12 PM
   By task: Categorical_VS_Qantitative

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Aggregate_Sum_of_Diagnosis, T.Categorical_of_DX
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLY_0001(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for Quant and Cat DX 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES Aggregate_Sum_of_Diagnosis * Categorical_of_DX /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: MARRY12X_EDRECODE   */
%LET _CLIENTTASKLABEL='MARRY12X_EDRECODE';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FILTER_FULLY_0002);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FILTER_FULLY_0002 AS 
   SELECT t1.DUPERSID1, 
          t1.AGE12X1, 
          t1.SEX1, 
          t1.EDUCYR1, 
          t1.EDUYRDEG1, 
          t1.REGION121, 
          t1.RACETHX1, 
          t1.MARRY12X1, 
          t1.SAQELIG1, 
          t1.SAQWT12F1, 
          t1.ACTDTY311, 
          t1.EDRECODE, 
          t1.ACTDTY421, 
          t1.ACTDTY531, 
          t1.ADAPPT421, 
          t1.ADCAPE421, 
          t1.ADCLIM421, 
          t1.ADCMPD421, 
          t1.ADCMPM421, 
          t1.ADCMPY421, 
          t1.ADDAYA421, 
          t1.ADDOWN421, 
          t1.ADDPRS421, 
          t1.ADDRBP421, 
          t1.ADEFRT421, 
          t1.ADEGMC421, 
          t1.ADEXPL421, 
          t1.ADEZUN421, 
          t1.ADFFRM421, 
          t1.ADFHLP421, 
          t1.ADGENH421, 
          t1.ADHECR421, 
          t1.ADHOPE421, 
          t1.ADILCR421, 
          t1.ADILWW421, 
          t1.ADINSA421, 
          t1.ADINSB421, 
          t1.ADINST421, 
          t1.ADINTR421, 
          t1.ADLANG421, 
          t1.ADLIST421, 
          t1.ADMALS421, 
          t1.ADMWLM421, 
          t1.ADNDCR421, 
          t1.ADNERV421, 
          t1.ADNRGY421, 
          t1.ADNSMK421, 
          t1.ADOVER421, 
          t1.ADPAIN421, 
          t1.ADPALS421, 
          t1.ADPRTM421, 
          t1.ADPRX421, 
          t1.ADPWLM421, 
          t1.ADRESP421, 
          t1.ADREST421, 
          t1.ADRISK421, 
          t1.ADRTCR421, 
          t1.ADRTWW421, 
          t1.ADSAD421, 
          t1.ADSMOK421, 
          t1.ADSOCA421, 
          t1.ADSPEC421, 
          t1.ADSPRF421, 
          t1.ADTLHW421, 
          t1.ADWRTH421, 
          t1.CANCERDX1, 
          t1.DVGEN121, 
          t1.DVGEXP121, 
          t1.DVGMCD121, 
          t1.DVGMCR121, 
          t1.DVGOFD121, 
          t1.DVGOPR121, 
          t1.DVGOPU121, 
          t1.DVGOSR121, 
          t1.DVGOTH121, 
          t1.DVGPRV121, 
          t1.DVGPTR121, 
          t1.DVGSLF121, 
          t1.DVGSTL121, 
          t1.DVGTCH121, 
          t1.DVGTRI121, 
          t1.DVGVA121, 
          t1.DVGWCP121, 
          t1.DVOEXP121, 
          t1.DVOMCD121, 
          t1.DVOMCR121, 
          t1.DVOOFD121, 
          t1.DVOOPR121, 
          t1.DVOOPU121, 
          t1.DVOOSR121, 
          t1.DVOOTH121, 
          t1.DVOPRV121, 
          t1.DVOPTR121, 
          t1.DVORTH121, 
          t1.DVOSLF121, 
          t1.DVOSTL121, 
          t1.DVOTCH121, 
          t1.DVOTRI121, 
          t1.DVOVA121, 
          t1.DVOWCP121, 
          t1.DVTEXP121, 
          t1.DVTMCD121, 
          t1.DVTMCR121, 
          t1.DVTOFD121, 
          t1.DVTOPR121, 
          t1.DVTOPU121, 
          t1.DVTOSR121, 
          t1.DVTOT121, 
          t1.DVTOTH121, 
          t1.DVTPRV121, 
          t1.DVTPTR121, 
          t1.DVTSLF121, 
          t1.DVTSTL121, 
          t1.DVTTCH121, 
          t1.DVTTRI121, 
          t1.DVTVA121, 
          t1.DVTWCP121, 
          t1.EMPHDX1, 
          t1.EMPST311, 
          t1.EMPST421, 
          t1.EMPST531, 
          t1.OFREMP311, 
          t1.OFREMP421, 
          t1.OFREMP531, 
          t1.ERDOPR121, 
          t1.ERDOPU121, 
          t1.ERDTCH121, 
          t1.ERTTCH121, 
          t1.ERFTCH121, 
          t1.ERTOTH121, 
          t1.ERDMCD121, 
          t1.ERFMCD121, 
          t1.ERTMCD121, 
          t1.ERDMCR121, 
          t1.ERFMCR121, 
          t1.ERTMCR121, 
          t1.ERDOTH121, 
          t1.ERFOTH121, 
          t1.ERTOPR121, 
          t1.ERTOPU121, 
          t1.ERDSTL121, 
          t1.ERFSTL121, 
          t1.ERTSTL121, 
          t1.ERDOSR121, 
          t1.ERFOSR121, 
          t1.ERTOSR121, 
          t1.ERDOFD121, 
          t1.ERTOFD121, 
          t1.ERFOFD121, 
          t1.ERFOPR121, 
          t1.ERFOPU121, 
          t1.ERTPRV121, 
          t1.ERDPRV121, 
          t1.ERFPRV121, 
          t1.ERDPTR121, 
          t1.ERFPTR121, 
          t1.ERTPTR121, 
          t1.ERDSLF121, 
          t1.ERFSLF121, 
          t1.ERTSLF121, 
          t1.ERDTRI121, 
          t1.ERFTRI121, 
          t1.ERTTRI121, 
          t1.ERDVA121, 
          t1.ERFVA121, 
          t1.ERTVA121, 
          t1.ERDWCP121, 
          t1.ERFWCP121, 
          t1.ERTWCP121, 
          t1.MIDX1, 
          t1.ERTOT121, 
          t1.ARTHDX1, 
          t1.ENGCMF421, 
          t1.ENGSPK421, 
          t1.LANGHM421, 
          t1.STRKDX1, 
          t1.PHQ2421, 
          t1.K6SUM421, 
          t1.SFFLAG421, 
          t1.MCS421, 
          t1.PCS421, 
          t1.JTPAIN311, 
          t1.JTPAIN531, 
          t1.INS12X1, 
          t1.INS53X1, 
          t1.INS31X1, 
          t1.INS42X1, 
          t1.INSAT31X1, 
          t1.INSAT42X1, 
          t1.INSAT53X1, 
          t1.INSAT12X1, 
          t1.INSCOV121, 
          t1.USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.EDUCYR_RECODE, 
          t1.EDUYRDEG_RECODE, 
          t1.ADGENH42_RECODE, 
          t1.ADDAYA42_RECODE, 
          t1.ADCLIM42_RECODE, 
          t1.ADPALS42_RECODE, 
          t1.ADPWLM42_RECODE, 
          t1.ADMALS42_RECODE, 
          t1.ADPAIN42_RECODE, 
          t1.AGE12X_RECODED, 
          t1.ADCAPE42_RECODE, 
          t1.REGION12_RECODE, 
          t1.ADNRGY42_RECODE, 
          t1.MARRY12X_RECODE, 
          t1.ADMWLM42_RECODE, 
          t1.ADDOWN42_RECODE, 
          t1.ADSOCA42_RECODE, 
          t1.ADHECR42_RECODED, 
          t1.ADHOPE42_RECODE, 
          t1.ADREST42_RECODE, 
          t1.ADSAD42_RECODE, 
          t1.ADWRTH42_RECODE, 
          t1.K6SUM42_RECODE, 
          t1.ADINSA42_RECODE, 
          t1.ADINSB42_RECODE, 
          t1.'PHQ242_ RECODE'n, 
          t1.ADNERV42_RECODE, 
          t1.CANCERDX_RECODE, 
          t1.INS12X_RECODE, 
          t1.MIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.STRKDX_RECODE, 
          t1.EMPHDX_RECODE, 
          t1.USBORN42_RECODE, 
          t1.ENGCMF42_RECODE, 
          t1.ENGSPK42_RECODE, 
          t1.JTPAIN31_RECODE, 
          t1.JTPAIN53_RECODE, 
          /* ERECODE_RECODE */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="Education recode recoded" AS ERECODE_RECODE
      FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: MARRY12X_Freq   */
%LET _CLIENTTASKLABEL='MARRY12X_Freq';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:12 PM
   By task: MARRY12X_Freq

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY_0002
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY_0002
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.MARRY12X_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLY_0002(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MARRY12X_RECODE /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ERECODE_Frequencies   */
%LET _CLIENTTASKLABEL='ERECODE_Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:13 PM
   By task: ERECODE_Frequencies

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY_0002
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY_0002
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ERECODE_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLY_0002(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ERECODE_RECODE /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: MARRY12X_CAT   */
%LET _CLIENTTASKLABEL='MARRY12X_CAT';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FILTER_FULLY_0003);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FILTER_FULLY_0003 AS 
   SELECT t1.DUPERSID1, 
          t1.AGE12X1, 
          t1.SEX1, 
          t1.EDUCYR1, 
          t1.EDUYRDEG1, 
          t1.REGION121, 
          t1.RACETHX1, 
          t1.MARRY12X1, 
          t1.SAQELIG1, 
          t1.SAQWT12F1, 
          t1.ACTDTY311, 
          t1.EDRECODE, 
          t1.ACTDTY421, 
          t1.ACTDTY531, 
          t1.ADAPPT421, 
          t1.ADCAPE421, 
          t1.ADCLIM421, 
          t1.ADCMPD421, 
          t1.ADCMPM421, 
          t1.ADCMPY421, 
          t1.ADDAYA421, 
          t1.ADDOWN421, 
          t1.ADDPRS421, 
          t1.ADDRBP421, 
          t1.ADEFRT421, 
          t1.ADEGMC421, 
          t1.ADEXPL421, 
          t1.ADEZUN421, 
          t1.ADFFRM421, 
          t1.ADFHLP421, 
          t1.ADGENH421, 
          t1.ADHECR421, 
          t1.ADHOPE421, 
          t1.ADILCR421, 
          t1.ADILWW421, 
          t1.ADINSA421, 
          t1.ADINSB421, 
          t1.ADINST421, 
          t1.ADINTR421, 
          t1.ADLANG421, 
          t1.ADLIST421, 
          t1.ADMALS421, 
          t1.ADMWLM421, 
          t1.ADNDCR421, 
          t1.ADNERV421, 
          t1.ADNRGY421, 
          t1.ADNSMK421, 
          t1.ADOVER421, 
          t1.ADPAIN421, 
          t1.ADPALS421, 
          t1.ADPRTM421, 
          t1.ADPRX421, 
          t1.ADPWLM421, 
          t1.ADRESP421, 
          t1.ADREST421, 
          t1.ADRISK421, 
          t1.ADRTCR421, 
          t1.ADRTWW421, 
          t1.ADSAD421, 
          t1.ADSMOK421, 
          t1.ADSOCA421, 
          t1.ADSPEC421, 
          t1.ADSPRF421, 
          t1.ADTLHW421, 
          t1.ADWRTH421, 
          t1.CANCERDX1, 
          t1.DVGEN121, 
          t1.DVGEXP121, 
          t1.DVGMCD121, 
          t1.DVGMCR121, 
          t1.DVGOFD121, 
          t1.DVGOPR121, 
          t1.DVGOPU121, 
          t1.DVGOSR121, 
          t1.DVGOTH121, 
          t1.DVGPRV121, 
          t1.DVGPTR121, 
          t1.DVGSLF121, 
          t1.DVGSTL121, 
          t1.DVGTCH121, 
          t1.DVGTRI121, 
          t1.DVGVA121, 
          t1.DVGWCP121, 
          t1.DVOEXP121, 
          t1.DVOMCD121, 
          t1.DVOMCR121, 
          t1.DVOOFD121, 
          t1.DVOOPR121, 
          t1.DVOOPU121, 
          t1.DVOOSR121, 
          t1.DVOOTH121, 
          t1.DVOPRV121, 
          t1.DVOPTR121, 
          t1.DVORTH121, 
          t1.DVOSLF121, 
          t1.DVOSTL121, 
          t1.DVOTCH121, 
          t1.DVOTRI121, 
          t1.DVOVA121, 
          t1.DVOWCP121, 
          t1.DVTEXP121, 
          t1.DVTMCD121, 
          t1.DVTMCR121, 
          t1.DVTOFD121, 
          t1.DVTOPR121, 
          t1.DVTOPU121, 
          t1.DVTOSR121, 
          t1.DVTOT121, 
          t1.DVTOTH121, 
          t1.DVTPRV121, 
          t1.DVTPTR121, 
          t1.DVTSLF121, 
          t1.DVTSTL121, 
          t1.DVTTCH121, 
          t1.DVTTRI121, 
          t1.DVTVA121, 
          t1.DVTWCP121, 
          t1.EMPHDX1, 
          t1.EMPST311, 
          t1.EMPST421, 
          t1.EMPST531, 
          t1.OFREMP311, 
          t1.OFREMP421, 
          t1.OFREMP531, 
          t1.ERDOPR121, 
          t1.ERDOPU121, 
          t1.ERDTCH121, 
          t1.ERTTCH121, 
          t1.ERFTCH121, 
          t1.ERTOTH121, 
          t1.ERDMCD121, 
          t1.ERFMCD121, 
          t1.ERTMCD121, 
          t1.ERDMCR121, 
          t1.ERFMCR121, 
          t1.ERTMCR121, 
          t1.ERDOTH121, 
          t1.ERFOTH121, 
          t1.ERTOPR121, 
          t1.ERTOPU121, 
          t1.ERDSTL121, 
          t1.ERFSTL121, 
          t1.ERTSTL121, 
          t1.ERDOSR121, 
          t1.ERFOSR121, 
          t1.ERTOSR121, 
          t1.ERDOFD121, 
          t1.ERTOFD121, 
          t1.ERFOFD121, 
          t1.ERFOPR121, 
          t1.ERFOPU121, 
          t1.ERTPRV121, 
          t1.ERDPRV121, 
          t1.ERFPRV121, 
          t1.ERDPTR121, 
          t1.ERFPTR121, 
          t1.ERTPTR121, 
          t1.ERDSLF121, 
          t1.ERFSLF121, 
          t1.ERTSLF121, 
          t1.ERDTRI121, 
          t1.ERFTRI121, 
          t1.ERTTRI121, 
          t1.ERDVA121, 
          t1.ERFVA121, 
          t1.ERTVA121, 
          t1.ERDWCP121, 
          t1.ERFWCP121, 
          t1.ERTWCP121, 
          t1.MIDX1, 
          t1.ERTOT121, 
          t1.ARTHDX1, 
          t1.ENGCMF421, 
          t1.ENGSPK421, 
          t1.LANGHM421, 
          t1.STRKDX1, 
          t1.PHQ2421, 
          t1.K6SUM421, 
          t1.SFFLAG421, 
          t1.MCS421, 
          t1.PCS421, 
          t1.JTPAIN311, 
          t1.JTPAIN531, 
          t1.INS12X1, 
          t1.INS53X1, 
          t1.INS31X1, 
          t1.INS42X1, 
          t1.INSAT31X1, 
          t1.INSAT42X1, 
          t1.INSAT53X1, 
          t1.INSAT12X1, 
          t1.INSCOV121, 
          t1.USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.EDUCYR_RECODE, 
          t1.EDUYRDEG_RECODE, 
          t1.ADGENH42_RECODE, 
          t1.ADDAYA42_RECODE, 
          t1.ADCLIM42_RECODE, 
          t1.ADPALS42_RECODE, 
          t1.ADPWLM42_RECODE, 
          t1.ADMALS42_RECODE, 
          t1.ADPAIN42_RECODE, 
          t1.AGE12X_RECODED, 
          t1.ADCAPE42_RECODE, 
          t1.REGION12_RECODE, 
          t1.ADNRGY42_RECODE, 
          t1.MARRY12X_RECODE, 
          t1.ADMWLM42_RECODE, 
          t1.ADDOWN42_RECODE, 
          t1.ADSOCA42_RECODE, 
          t1.ADHECR42_RECODED, 
          t1.ADHOPE42_RECODE, 
          t1.ADREST42_RECODE, 
          t1.ADSAD42_RECODE, 
          t1.ADWRTH42_RECODE, 
          t1.K6SUM42_RECODE, 
          t1.ADINSA42_RECODE, 
          t1.ADINSB42_RECODE, 
          t1.'PHQ242_ RECODE'n, 
          t1.ADNERV42_RECODE, 
          t1.CANCERDX_RECODE, 
          t1.INS12X_RECODE, 
          t1.MIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.STRKDX_RECODE, 
          t1.EMPHDX_RECODE, 
          t1.USBORN42_RECODE, 
          t1.ENGCMF42_RECODE, 
          t1.ENGSPK42_RECODE, 
          t1.JTPAIN31_RECODE, 
          t1.JTPAIN53_RECODE, 
          t1.ERECODE_RECODE, 
          /* MARRY12X_CAT */
            (CASE  
               WHEN t1.MARRY12X_RECODE = 1
               THEN 1
               WHEN t1.MARRY12X_RECODE = 5
               THEN 2
               ELSE 3
            END) LABEL="Categories for married, 1 = married, 2 = never married, 3 = no longer married" AS MARRY12X_CAT
      FROM WORK.QUERY_FOR_MEPS_FILTER_FULLY_0002 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis MARRY12X Recoded vs Cat   */
%LET _CLIENTTASKLABEL='Table Analysis MARRY12X Recoded vs Cat';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:14 PM
   By task: Table Analysis MARRY12X Recoded vs Cat

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY_0003
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY_0003
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY12X_RECODE, T.MARRY12X_CAT
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLY_0003(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MARRY12X_RECODE * MARRY12X_CAT /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: EDRECODE_CAT   */
%LET _CLIENTTASKLABEL='EDRECODE_CAT';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FILTER_FULLY_0004);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FILTER_FULLY_0004 AS 
   SELECT t1.ERECODE_RECODE, 
          /* EDRECODE_CAT */
            (CASE  
               WHEN t1.ERECODE_RECODE <=12
               THEN 1
               WHEN t1.ERECODE_RECODE =  13
               THEN 2
               WHEN t1.ERECODE_RECODE = 14
               THEN 3
               WHEN t1.ERECODE_RECODE = 15
               THEN 4
               ELSE 5
            END) LABEL="Education Recode Categorical" AS EDRECODE_CAT
      FROM WORK.QUERY_FOR_MEPS_FILTER_FULLY_0002 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis for EDRECODE CAT vs Quant   */
%LET _CLIENTTASKLABEL='Table Analysis for EDRECODE CAT vs Quant';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:15 PM
   By task: Table Analysis for EDRECODE CAT vs Quant

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY_0004
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLY_0004
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ERECODE_RECODE, T.EDRECODE_CAT
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLY_0004(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ERECODE_RECODE * EDRECODE_CAT /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Reverse code SF-12 variables   */
%LET _CLIENTTASKLABEL='Reverse code SF-12 variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_SF12_REVCODE);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_SF12_REVCODE(label="QUERY_FOR_MEPS_SF12_REVCODE") AS 
   SELECT t1.DUPERSID1, 
          t1.AGE12X1, 
          t1.SEX1, 
          t1.EDUCYR1, 
          t1.EDUYRDEG1, 
          t1.REGION121, 
          t1.RACETHX1, 
          t1.MARRY12X1, 
          t1.SAQELIG1, 
          t1.SAQWT12F1, 
          t1.ACTDTY311, 
          t1.ACTDTY421, 
          t1.ACTDTY531, 
          t1.ADAPPT421, 
          t1.ADCAPE421, 
          t1.ADCLIM421, 
          t1.ADCMPD421, 
          t1.ADCMPM421, 
          t1.ADCMPY421, 
          t1.ADDAYA421, 
          t1.ADDOWN421, 
          t1.ADDPRS421, 
          t1.ADDRBP421, 
          t1.ADEFRT421, 
          t1.ADEGMC421, 
          t1.ADEXPL421, 
          t1.ADEZUN421, 
          t1.ADFFRM421, 
          t1.ADFHLP421, 
          t1.ADGENH421, 
          t1.ADHECR421, 
          t1.ADHOPE421, 
          t1.ADILCR421, 
          t1.ADMWLM42_RECODE, 
          t1.ADDOWN42_RECODE, 
          t1.ADSOCA42_RECODE, 
          t1.ADILWW421, 
          t1.ADINSA421, 
          t1.ADINSB421, 
          t1.ADINST421, 
          t1.ADINTR421, 
          t1.ADLANG421, 
          t1.ADLIST421, 
          t1.ADMALS421, 
          t1.ADMWLM421, 
          t1.ADNDCR421, 
          t1.ADNERV421, 
          t1.ADNRGY421, 
          t1.ADNSMK421, 
          t1.ADOVER421, 
          t1.ADPAIN421, 
          t1.ADPALS421, 
          t1.ADPRTM421, 
          t1.ADPRX421, 
          t1.ADPWLM421, 
          t1.ADRESP421, 
          t1.ADREST421, 
          t1.ADRISK421, 
          t1.ADRTCR421, 
          t1.ADRTWW421, 
          t1.ADSAD421, 
          t1.ADSMOK421, 
          t1.ADSOCA421, 
          t1.ADSPEC421, 
          t1.ADSPRF421, 
          t1.ADTLHW421, 
          t1.ADWRTH421, 
          t1.CANCERDX1, 
          t1.DVGEN121, 
          t1.DVGEXP121, 
          t1.DVGMCD121, 
          t1.DVGMCR121, 
          t1.DVGOFD121, 
          t1.DVGOPR121, 
          t1.DVGOPU121, 
          t1.DVGOSR121, 
          t1.DVGOTH121, 
          t1.DVGPRV121, 
          t1.DVGPTR121, 
          t1.DVGSLF121, 
          t1.DVGSTL121, 
          t1.DVGTCH121, 
          t1.DVGTRI121, 
          t1.DVGVA121, 
          t1.DVGWCP121, 
          t1.DVOEXP121, 
          t1.DVOMCD121, 
          t1.DVOMCR121, 
          t1.DVOOFD121, 
          t1.DVOOPR121, 
          t1.DVOOPU121, 
          t1.DVOOSR121, 
          t1.DVOOTH121, 
          t1.DVOPRV121, 
          t1.DVOPTR121, 
          t1.DVORTH121, 
          t1.DVOSLF121, 
          t1.DVOSTL121, 
          t1.DVOTCH121, 
          t1.DVOTRI121, 
          t1.DVOVA121, 
          t1.DVOWCP121, 
          t1.DVTEXP121, 
          t1.DVTMCD121, 
          t1.DVTMCR121, 
          t1.DVTOFD121, 
          t1.DVTOPR121, 
          t1.DVTOPU121, 
          t1.DVTOSR121, 
          t1.DVTOT121, 
          t1.DVTOTH121, 
          t1.DVTPRV121, 
          t1.DVTPTR121, 
          t1.DVTSLF121, 
          t1.DVTSTL121, 
          t1.DVTTCH121, 
          t1.DVTTRI121, 
          t1.DVTVA121, 
          t1.DVTWCP121, 
          t1.EMPHDX1, 
          t1.EMPST311, 
          t1.EMPST421, 
          t1.EMPST531, 
          t1.OFREMP311, 
          t1.OFREMP421, 
          t1.OFREMP531, 
          t1.ERDOPR121, 
          t1.ERDOPU121, 
          t1.ERDTCH121, 
          t1.ERTTCH121, 
          t1.ERFTCH121, 
          t1.ERTOTH121, 
          t1.ERDMCD121, 
          t1.ERFMCD121, 
          t1.ERTMCD121, 
          t1.ERDMCR121, 
          t1.ERFMCR121, 
          t1.ERTMCR121, 
          t1.ERDOTH121, 
          t1.ERFOTH121, 
          t1.ERTOPR121, 
          t1.ERTOPU121, 
          t1.ERDSTL121, 
          t1.ERFSTL121, 
          t1.ERTSTL121, 
          t1.ERDOSR121, 
          t1.ERFOSR121, 
          t1.ERTOSR121, 
          t1.ERDOFD121, 
          t1.ERTOFD121, 
          t1.ERFOFD121, 
          t1.ERFOPR121, 
          t1.ERFOPU121, 
          t1.ERTPRV121, 
          t1.ERDPRV121, 
          t1.ERFPRV121, 
          t1.ERDPTR121, 
          t1.ERFPTR121, 
          t1.ERTPTR121, 
          t1.ERDSLF121, 
          t1.ERFSLF121, 
          t1.ERTSLF121, 
          t1.ERDTRI121, 
          t1.ERFTRI121, 
          t1.ERTTRI121, 
          t1.ERDVA121, 
          t1.ERFVA121, 
          t1.ERTVA121, 
          t1.ERDWCP121, 
          t1.ERFWCP121, 
          t1.ERTWCP121, 
          t1.MIDX1, 
          t1.ERTOT121, 
          t1.ARTHDX1, 
          t1.ENGCMF421, 
          t1.ENGSPK421, 
          t1.LANGHM421, 
          t1.STRKDX1, 
          t1.PHQ2421, 
          t1.K6SUM421, 
          t1.SFFLAG421, 
          t1.MCS421, 
          t1.PCS421, 
          t1.JTPAIN311, 
          t1.JTPAIN531, 
          t1.INS12X1, 
          t1.INS53X1, 
          t1.INS31X1, 
          t1.INS42X1, 
          t1.INSAT31X1, 
          t1.INSAT42X1, 
          t1.INSAT53X1, 
          t1.INSAT12X1, 
          t1.INSCOV121, 
          t1.USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.EDUCYR_RECODE, 
          t1.EDUYRDEG_RECODE, 
          t1.ADGENH42_RECODE, 
          t1.ADDAYA42_RECODE, 
          t1.ADCLIM42_RECODE, 
          t1.ADPALS42_RECODE, 
          t1.ADPWLM42_RECODE, 
          t1.ADMALS42_RECODE, 
          t1.ADPAIN42_RECODE, 
          t1.AGE12X_RECODED, 
          t1.ADCAPE42_RECODE, 
          t1.REGION12_RECODE, 
          t1.ADNRGY42_RECODE, 
          t1.MARRY12X_RECODE, 
          t1.ADHECR42_RECODED, 
          t1.ADHOPE42_RECODE, 
          t1.ADREST42_RECODE, 
          t1.ADSAD42_RECODE, 
          t1.ADWRTH42_RECODE, 
          t1.K6SUM42_RECODE, 
          t1.ADINSA42_RECODE, 
          t1.ADINSB42_RECODE, 
          t1.'PHQ242_ RECODE'n, 
          t1.ADNERV42_RECODE, 
          t1.CANCERDX_RECODE, 
          t1.INS12X_RECODE, 
          t1.MIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.STRKDX_RECODE, 
          t1.EMPHDX_RECODE, 
          t1.USBORN42_RECODE, 
          t1.ENGCMF42_RECODE, 
          t1.ENGSPK42_RECODE, 
          t1.JTPAIN31_RECODE, 
          t1.JTPAIN53_RECODE, 
          /* ADGENH42_REVCODE */
            (6-t1.ADGENH42_RECODE) LABEL="SF-12 Health in general rescored" AS ADGENH42_REVCODE, 
          /* ADPAIN42_REVCODE */
            (6-t1.ADPAIN42_RECODE) LABEL="SF12 Amount of pain rescored" AS ADPAIN42_REVCODE, 
          /* ADNRGY42_REVCODE */
            (6-t1.ADNRGY42_RECODE) LABEL="SF-12 Amount of energy rescored" AS ADNRGY42_REVCODE, 
          /* ADCAPE42_REVCODE */
            (6-t1.ADCAPE42_RECODE) LABEL="SF-12 Felt calm and peaceful rescored" AS ADCAPE42_REVCODE, 
          t1.ADMWLM42_RECODE AS ADMWLM42_RECODE1, 
          t1.ADDOWN42_RECODE AS ADDOWN42_RECODE1, 
          t1.ADSOCA42_RECODE AS ADSOCA42_RECODE1
      FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADGENH42_REVCODE   */
%LET _CLIENTTASKLABEL='ADGENH42_REVCODE';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:15 PM
   By task: ADGENH42_REVCODE

   Input Data: Local:WORK.QUERY_FOR_MEPS_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADGENH42_RECODE, T.ADGENH42_REVCODE
	FROM WORK.QUERY_FOR_MEPS_SF12_REVCODE(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADGENH42_RECODE * ADGENH42_REVCODE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADPAIN42_REVCODE   */
%LET _CLIENTTASKLABEL='ADPAIN42_REVCODE';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:16 PM
   By task: ADPAIN42_REVCODE

   Input Data: Local:WORK.QUERY_FOR_MEPS_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADPAIN42_RECODE, T.ADPAIN42_REVCODE
	FROM WORK.QUERY_FOR_MEPS_SF12_REVCODE(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPAIN42_RECODE * ADPAIN42_REVCODE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADNRGY42_REVCODE   */
%LET _CLIENTTASKLABEL='ADNRGY42_REVCODE';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:16 PM
   By task: ADNRGY42_REVCODE

   Input Data: Local:WORK.QUERY_FOR_MEPS_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADNRGY42_RECODE, T.ADNRGY42_REVCODE
	FROM WORK.QUERY_FOR_MEPS_SF12_REVCODE(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADNRGY42_RECODE * ADNRGY42_REVCODE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADCAPE42_REVCODE   */
%LET _CLIENTTASKLABEL='ADCAPE42_REVCODE';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:17 PM
   By task: ADCAPE42_REVCODE

   Input Data: Local:WORK.QUERY_FOR_MEPS_SF12_REVCODE
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_SF12_REVCODE
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADCAPE42_RECODE, T.ADCAPE42_REVCODE
	FROM WORK.QUERY_FOR_MEPS_SF12_REVCODE(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADCAPE42_RECODE * ADCAPE42_REVCODE /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG(label="QUERY_FOR_MEPS_SF12_REVCODE_AGG") AS 
   SELECT t1.DUPERSID1, 
          t1.AGE12X1, 
          t1.SEX1, 
          t1.EDUCYR1, 
          t1.EDUYRDEG1, 
          t1.REGION121, 
          t1.RACETHX1, 
          t1.MARRY12X1, 
          t1.SAQELIG1, 
          t1.SAQWT12F1, 
          t1.ACTDTY311, 
          t1.ACTDTY421, 
          t1.ACTDTY531, 
          t1.ADAPPT421, 
          t1.ADCAPE421, 
          t1.ADCLIM421, 
          t1.ADCMPD421, 
          t1.ADCMPM421, 
          t1.ADCMPY421, 
          t1.ADDAYA421, 
          t1.ADDOWN421, 
          t1.ADDPRS421, 
          t1.ADDRBP421, 
          t1.ADEFRT421, 
          t1.ADEGMC421, 
          t1.ADEXPL421, 
          t1.ADEZUN421, 
          t1.ADFFRM421, 
          t1.ADFHLP421, 
          t1.ADGENH421, 
          t1.ADHECR421, 
          t1.ADHOPE421, 
          t1.ADILCR421, 
          t1.ADMWLM42_RECODE, 
          t1.ADDOWN42_RECODE, 
          t1.ADSOCA42_RECODE, 
          t1.ADILWW421, 
          t1.ADINSA421, 
          t1.ADINSB421, 
          t1.ADINST421, 
          t1.ADINTR421, 
          t1.ADLANG421, 
          t1.ADLIST421, 
          t1.ADMALS421, 
          t1.ADMWLM421, 
          t1.ADNDCR421, 
          t1.ADNERV421, 
          t1.ADNRGY421, 
          t1.ADNSMK421, 
          t1.ADOVER421, 
          t1.ADPAIN421, 
          t1.ADPALS421, 
          t1.ADPRTM421, 
          t1.ADPRX421, 
          t1.ADPWLM421, 
          t1.ADRESP421, 
          t1.ADREST421, 
          t1.ADRISK421, 
          t1.ADRTCR421, 
          t1.ADRTWW421, 
          t1.ADSAD421, 
          t1.ADSMOK421, 
          t1.ADSOCA421, 
          t1.ADSPEC421, 
          t1.ADSPRF421, 
          t1.ADTLHW421, 
          t1.ADWRTH421, 
          t1.CANCERDX1, 
          t1.DVGEN121, 
          t1.DVGEXP121, 
          t1.DVGMCD121, 
          t1.DVGMCR121, 
          t1.DVGOFD121, 
          t1.DVGOPR121, 
          t1.DVGOPU121, 
          t1.DVGOSR121, 
          t1.DVGOTH121, 
          t1.DVGPRV121, 
          t1.DVGPTR121, 
          t1.DVGSLF121, 
          t1.DVGSTL121, 
          t1.DVGTCH121, 
          t1.DVGTRI121, 
          t1.DVGVA121, 
          t1.DVGWCP121, 
          t1.DVOEXP121, 
          t1.DVOMCD121, 
          t1.DVOMCR121, 
          t1.DVOOFD121, 
          t1.DVOOPR121, 
          t1.DVOOPU121, 
          t1.DVOOSR121, 
          t1.DVOOTH121, 
          t1.DVOPRV121, 
          t1.DVOPTR121, 
          t1.DVORTH121, 
          t1.DVOSLF121, 
          t1.DVOSTL121, 
          t1.DVOTCH121, 
          t1.DVOTRI121, 
          t1.DVOVA121, 
          t1.DVOWCP121, 
          t1.DVTEXP121, 
          t1.DVTMCD121, 
          t1.DVTMCR121, 
          t1.DVTOFD121, 
          t1.DVTOPR121, 
          t1.DVTOPU121, 
          t1.DVTOSR121, 
          t1.DVTOT121, 
          t1.DVTOTH121, 
          t1.DVTPRV121, 
          t1.DVTPTR121, 
          t1.DVTSLF121, 
          t1.DVTSTL121, 
          t1.DVTTCH121, 
          t1.DVTTRI121, 
          t1.DVTVA121, 
          t1.DVTWCP121, 
          t1.EMPHDX1, 
          t1.EMPST311, 
          t1.EMPST421, 
          t1.EMPST531, 
          t1.OFREMP311, 
          t1.OFREMP421, 
          t1.OFREMP531, 
          t1.ERDOPR121, 
          t1.ERDOPU121, 
          t1.ERDTCH121, 
          t1.ERTTCH121, 
          t1.ERFTCH121, 
          t1.ERTOTH121, 
          t1.ERDMCD121, 
          t1.ERFMCD121, 
          t1.ERTMCD121, 
          t1.ERDMCR121, 
          t1.ERFMCR121, 
          t1.ERTMCR121, 
          t1.ERDOTH121, 
          t1.ERFOTH121, 
          t1.ERTOPR121, 
          t1.ERTOPU121, 
          t1.ERDSTL121, 
          t1.ERFSTL121, 
          t1.ERTSTL121, 
          t1.ERDOSR121, 
          t1.ERFOSR121, 
          t1.ERTOSR121, 
          t1.ERDOFD121, 
          t1.ERTOFD121, 
          t1.ERFOFD121, 
          t1.ERFOPR121, 
          t1.ERFOPU121, 
          t1.ERTPRV121, 
          t1.ERDPRV121, 
          t1.ERFPRV121, 
          t1.ERDPTR121, 
          t1.ERFPTR121, 
          t1.ERTPTR121, 
          t1.ERDSLF121, 
          t1.ERFSLF121, 
          t1.ERTSLF121, 
          t1.ERDTRI121, 
          t1.ERFTRI121, 
          t1.ERTTRI121, 
          t1.ERDVA121, 
          t1.ERFVA121, 
          t1.ERTVA121, 
          t1.ERDWCP121, 
          t1.ERFWCP121, 
          t1.ERTWCP121, 
          t1.MIDX1, 
          t1.ERTOT121, 
          t1.ARTHDX1, 
          t1.ENGCMF421, 
          t1.ENGSPK421, 
          t1.LANGHM421, 
          t1.STRKDX1, 
          t1.PHQ2421, 
          t1.K6SUM421, 
          t1.SFFLAG421, 
          t1.MCS421, 
          t1.PCS421, 
          t1.JTPAIN311, 
          t1.JTPAIN531, 
          t1.INS12X1, 
          t1.INS53X1, 
          t1.INS31X1, 
          t1.INS42X1, 
          t1.INSAT31X1, 
          t1.INSAT42X1, 
          t1.INSAT53X1, 
          t1.INSAT12X1, 
          t1.INSCOV121, 
          t1.USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.EDUCYR_RECODE, 
          t1.EDUYRDEG_RECODE, 
          t1.ADGENH42_RECODE, 
          t1.ADDAYA42_RECODE, 
          t1.ADCLIM42_RECODE, 
          t1.ADPALS42_RECODE, 
          t1.ADPWLM42_RECODE, 
          t1.ADMALS42_RECODE, 
          t1.ADPAIN42_RECODE, 
          t1.AGE12X_RECODED, 
          t1.ADCAPE42_RECODE, 
          t1.REGION12_RECODE, 
          t1.ADNRGY42_RECODE, 
          t1.MARRY12X_RECODE, 
          t1.ADHECR42_RECODED, 
          t1.ADHOPE42_RECODE, 
          t1.ADREST42_RECODE, 
          t1.ADSAD42_RECODE, 
          t1.ADWRTH42_RECODE, 
          t1.K6SUM42_RECODE, 
          t1.ADINSA42_RECODE, 
          t1.ADINSB42_RECODE, 
          t1.'PHQ242_ RECODE'n, 
          t1.ADNERV42_RECODE, 
          t1.CANCERDX_RECODE, 
          t1.INS12X_RECODE, 
          t1.MIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.STRKDX_RECODE, 
          t1.EMPHDX_RECODE, 
          t1.USBORN42_RECODE, 
          t1.ENGCMF42_RECODE, 
          t1.ENGSPK42_RECODE, 
          t1.JTPAIN31_RECODE, 
          t1.JTPAIN53_RECODE, 
          t1.ADGENH42_REVCODE, 
          t1.ADPAIN42_REVCODE, 
          t1.ADNRGY42_REVCODE, 
          t1.ADCAPE42_REVCODE, 
          t1.ADMWLM42_RECODE1, 
          t1.ADDOWN42_RECODE1, 
          t1.ADSOCA42_RECODE1, 
          /* SUM_AGGREGATE_QUERY */
            
            (SUM(t1.ADSOCA42_RECODE1,t1.ADDOWN42_RECODE1,t1.ADMWLM42_RECODE1,t1.ADCAPE42_REVCODE,t1.ADNRGY42_REVCODE,t1.ADPAIN42_REVCODE,t1.ADGENH42_REVCODE,t1.ADDAYA42_RECODE,t1.ADCLIM42_RECODE,t1.ADPALS42_RECODE,t1.ADPWLM42_RECODE,t1.ADMALS42_RECODE)) 
            LABEL="Aggregate of all variables" AS SUM_AGGREGATE_QUERY
      FROM WORK.QUERY_FOR_MEPS_SF12_REVCODE t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: AGGREGATE_LIST_DATA_SF12   */
%LET _CLIENTTASKLABEL='AGGREGATE_LIST_DATA_SF12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:17 PM
   By task: AGGREGATE_LIST_DATA_SF12

   Input Data: Local:MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADDAYA42_RECODE, T.ADCLIM42_RECODE, T.ADPALS42_RECODE, T.ADPWLM42_RECODE, T.ADMALS42_RECODE, T.ADGENH42_REVCODE, T.ADPAIN42_REVCODE, T.ADNRGY42_REVCODE, T.ADCAPE42_REVCODE, T.ADMWLM42_RECODE1, T.ADDOWN42_RECODE1
		     , T.ADSOCA42_RECODE1, T.SUM_AGGREGATE_QUERY
	FROM MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR ADDAYA42_RECODE ADCLIM42_RECODE ADPALS42_RECODE ADPWLM42_RECODE ADMALS42_RECODE ADGENH42_REVCODE ADPAIN42_REVCODE ADNRGY42_REVCODE ADCAPE42_REVCODE ADMWLM42_RECODE1 ADDOWN42_RECODE1 ADSOCA42_RECODE1 SUM_AGGREGATE_QUERY;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics for SF-12V2 AGG SUM   */
%LET _CLIENTTASKLABEL='Summary Statistics for SF-12V2 AGG SUM';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:18 PM
   By task: Summary Statistics for SF-12V2 AGG SUM

   Input Data: Local:MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_AGGREGATE_QUERY
	FROM MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for 2012 AGG SUM SF-12V2";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR SUM_AGGREGATE_QUERY;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for AGGSUM SF-12V2   */
%LET _CLIENTTASKLABEL='Distribution Analysis for AGGSUM SF-12V2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:19 PM
   By task: Distribution Analysis for AGGSUM SF-12V2

   Input Data: Local:MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_AGGREGATE_QUERY
	FROM MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: SUM_AGGREGATE_QUERY for SF-12V2 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR SUM_AGGREGATE_QUERY;
	HISTOGRAM   SUM_AGGREGATE_QUERY / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:20 PM
   By task: One-Way Frequencies

   Input Data: Local:MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ADDAYA42_RECODE, T.ADCLIM42_RECODE, T.ADPALS42_RECODE, T.ADPWLM42_RECODE, T.ADMALS42_RECODE, T.ADGENH42_REVCODE, T.ADPAIN42_REVCODE, T.ADNRGY42_REVCODE, T.ADCAPE42_REVCODE, T.ADMWLM42_RECODE1, T.ADDOWN42_RECODE1
		     , T.ADSOCA42_RECODE1
	FROM MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ADDAYA42_RECODE /  SCORES=TABLE;
	TABLES ADCLIM42_RECODE /  SCORES=TABLE;
	TABLES ADPALS42_RECODE /  SCORES=TABLE;
	TABLES ADPWLM42_RECODE /  SCORES=TABLE;
	TABLES ADMALS42_RECODE /  SCORES=TABLE;
	TABLES ADGENH42_REVCODE /  SCORES=TABLE;
	TABLES ADPAIN42_REVCODE /  SCORES=TABLE;
	TABLES ADNRGY42_REVCODE /  SCORES=TABLE;
	TABLES ADCAPE42_REVCODE /  SCORES=TABLE;
	TABLES ADMWLM42_RECODE1 /  SCORES=TABLE;
	TABLES ADDOWN42_RECODE1 /  SCORES=TABLE;
	TABLES ADSOCA42_RECODE1 /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Aggregate SF12V2 Categorical   */
%LET _CLIENTTASKLABEL='Aggregate SF12V2 Categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_FOR_MEPS_SF12_CATEGORICAL);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_FOR_MEPS_SF12_CATEGORICAL(label="QUERY_FOR_MEPS_SF12_CATEGORICAL") AS 
   SELECT /* Categorical_Aggregate_SF12V2 */
            (CASE  
               WHEN t1.SUM_AGGREGATE_QUERY >= 2 and t1.SUM_AGGREGATE_QUERY < 41 
               THEN 1
               WHEN t1.SUM_AGGREGATE_QUERY >= 41 and t1.SUM_AGGREGATE_QUERY < 48 
               THEN 2
               WHEN t1.SUM_AGGREGATE_QUERY >= 48 and t1.SUM_AGGREGATE_QUERY < 52
               THEN 3
               ELSE 4
            END) LABEL="SF12V2 categorized total health aggregation" AS Categorical_Aggregate_SF12V2, 
          t1.DUPERSID1, 
          t1.AGE12X1, 
          t1.SEX1, 
          t1.EDUCYR1, 
          t1.EDUYRDEG1, 
          t1.REGION121, 
          t1.RACETHX1, 
          t1.MARRY12X1, 
          t1.SAQELIG1, 
          t1.SAQWT12F1, 
          t1.ACTDTY311, 
          t1.ACTDTY421, 
          t1.ACTDTY531, 
          t1.ADAPPT421, 
          t1.ADCAPE421, 
          t1.ADCLIM421, 
          t1.ADCMPD421, 
          t1.ADCMPM421, 
          t1.ADCMPY421, 
          t1.ADDAYA421, 
          t1.ADDOWN421, 
          t1.ADDPRS421, 
          t1.ADDRBP421, 
          t1.ADEFRT421, 
          t1.ADEGMC421, 
          t1.ADEXPL421, 
          t1.ADEZUN421, 
          t1.ADFFRM421, 
          t1.ADFHLP421, 
          t1.ADGENH421, 
          t1.ADHECR421, 
          t1.ADHOPE421, 
          t1.ADILCR421, 
          t1.ADMWLM42_RECODE, 
          t1.ADDOWN42_RECODE, 
          t1.ADSOCA42_RECODE, 
          t1.ADILWW421, 
          t1.ADINSA421, 
          t1.ADINSB421, 
          t1.ADINST421, 
          t1.ADINTR421, 
          t1.ADLANG421, 
          t1.ADLIST421, 
          t1.ADMALS421, 
          t1.ADMWLM421, 
          t1.ADNDCR421, 
          t1.ADNERV421, 
          t1.ADNRGY421, 
          t1.ADNSMK421, 
          t1.ADOVER421, 
          t1.ADPAIN421, 
          t1.ADPALS421, 
          t1.ADPRTM421, 
          t1.ADPRX421, 
          t1.ADPWLM421, 
          t1.ADRESP421, 
          t1.ADREST421, 
          t1.ADRISK421, 
          t1.ADRTCR421, 
          t1.ADRTWW421, 
          t1.ADSAD421, 
          t1.ADSMOK421, 
          t1.ADSOCA421, 
          t1.ADSPEC421, 
          t1.ADSPRF421, 
          t1.ADTLHW421, 
          t1.ADWRTH421, 
          t1.CANCERDX1, 
          t1.DVGEN121, 
          t1.DVGEXP121, 
          t1.DVGMCD121, 
          t1.DVGMCR121, 
          t1.DVGOFD121, 
          t1.DVGOPR121, 
          t1.DVGOPU121, 
          t1.DVGOSR121, 
          t1.DVGOTH121, 
          t1.DVGPRV121, 
          t1.DVGPTR121, 
          t1.DVGSLF121, 
          t1.DVGSTL121, 
          t1.DVGTCH121, 
          t1.DVGTRI121, 
          t1.DVGVA121, 
          t1.DVGWCP121, 
          t1.DVOEXP121, 
          t1.DVOMCD121, 
          t1.DVOMCR121, 
          t1.DVOOFD121, 
          t1.DVOOPR121, 
          t1.DVOOPU121, 
          t1.DVOOSR121, 
          t1.DVOOTH121, 
          t1.DVOPRV121, 
          t1.DVOPTR121, 
          t1.DVORTH121, 
          t1.DVOSLF121, 
          t1.DVOSTL121, 
          t1.DVOTCH121, 
          t1.DVOTRI121, 
          t1.DVOVA121, 
          t1.DVOWCP121, 
          t1.DVTEXP121, 
          t1.DVTMCD121, 
          t1.DVTMCR121, 
          t1.DVTOFD121, 
          t1.DVTOPR121, 
          t1.DVTOPU121, 
          t1.DVTOSR121, 
          t1.DVTOT121, 
          t1.DVTOTH121, 
          t1.DVTPRV121, 
          t1.DVTPTR121, 
          t1.DVTSLF121, 
          t1.DVTSTL121, 
          t1.DVTTCH121, 
          t1.DVTTRI121, 
          t1.DVTVA121, 
          t1.DVTWCP121, 
          t1.EMPHDX1, 
          t1.EMPST311, 
          t1.EMPST421, 
          t1.EMPST531, 
          t1.OFREMP311, 
          t1.OFREMP421, 
          t1.OFREMP531, 
          t1.ERDOPR121, 
          t1.ERDOPU121, 
          t1.ERDTCH121, 
          t1.ERTTCH121, 
          t1.ERFTCH121, 
          t1.ERTOTH121, 
          t1.ERDMCD121, 
          t1.ERFMCD121, 
          t1.ERTMCD121, 
          t1.ERDMCR121, 
          t1.ERFMCR121, 
          t1.ERTMCR121, 
          t1.ERDOTH121, 
          t1.ERFOTH121, 
          t1.ERTOPR121, 
          t1.ERTOPU121, 
          t1.ERDSTL121, 
          t1.ERFSTL121, 
          t1.ERTSTL121, 
          t1.ERDOSR121, 
          t1.ERFOSR121, 
          t1.ERTOSR121, 
          t1.ERDOFD121, 
          t1.ERTOFD121, 
          t1.ERFOFD121, 
          t1.ERFOPR121, 
          t1.ERFOPU121, 
          t1.ERTPRV121, 
          t1.ERDPRV121, 
          t1.ERFPRV121, 
          t1.ERDPTR121, 
          t1.ERFPTR121, 
          t1.ERTPTR121, 
          t1.ERDSLF121, 
          t1.ERFSLF121, 
          t1.ERTSLF121, 
          t1.ERDTRI121, 
          t1.ERFTRI121, 
          t1.ERTTRI121, 
          t1.ERDVA121, 
          t1.ERFVA121, 
          t1.ERTVA121, 
          t1.ERDWCP121, 
          t1.ERFWCP121, 
          t1.ERTWCP121, 
          t1.MIDX1, 
          t1.ERTOT121, 
          t1.ARTHDX1, 
          t1.ENGCMF421, 
          t1.ENGSPK421, 
          t1.LANGHM421, 
          t1.STRKDX1, 
          t1.PHQ2421, 
          t1.K6SUM421, 
          t1.SFFLAG421, 
          t1.MCS421, 
          t1.PCS421, 
          t1.JTPAIN311, 
          t1.JTPAIN531, 
          t1.INS12X1, 
          t1.INS53X1, 
          t1.INS31X1, 
          t1.INS42X1, 
          t1.INSAT31X1, 
          t1.INSAT42X1, 
          t1.INSAT53X1, 
          t1.INSAT12X1, 
          t1.INSCOV121, 
          t1.USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.EDUCYR_RECODE, 
          t1.EDUYRDEG_RECODE, 
          t1.ADGENH42_RECODE, 
          t1.ADDAYA42_RECODE, 
          t1.ADCLIM42_RECODE, 
          t1.ADPALS42_RECODE, 
          t1.ADPWLM42_RECODE, 
          t1.ADMALS42_RECODE, 
          t1.ADPAIN42_RECODE, 
          t1.AGE12X_RECODED, 
          t1.ADCAPE42_RECODE, 
          t1.REGION12_RECODE, 
          t1.ADNRGY42_RECODE, 
          t1.MARRY12X_RECODE, 
          t1.ADHECR42_RECODED, 
          t1.ADHOPE42_RECODE, 
          t1.ADREST42_RECODE, 
          t1.ADSAD42_RECODE, 
          t1.ADWRTH42_RECODE, 
          t1.K6SUM42_RECODE, 
          t1.ADINSA42_RECODE, 
          t1.ADINSB42_RECODE, 
          t1.'PHQ242_ RECODE'n, 
          t1.ADNERV42_RECODE, 
          t1.CANCERDX_RECODE, 
          t1.INS12X_RECODE, 
          t1.MIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.STRKDX_RECODE, 
          t1.EMPHDX_RECODE, 
          t1.USBORN42_RECODE, 
          t1.ENGCMF42_RECODE, 
          t1.ENGSPK42_RECODE, 
          t1.JTPAIN31_RECODE, 
          t1.JTPAIN53_RECODE, 
          t1.ADGENH42_REVCODE, 
          t1.ADPAIN42_REVCODE, 
          t1.ADNRGY42_REVCODE, 
          t1.ADCAPE42_REVCODE, 
          t1.ADMWLM42_RECODE1, 
          t1.ADDOWN42_RECODE1, 
          t1.ADSOCA42_RECODE1, 
          t1.SUM_AGGREGATE_QUERY
      FROM MYDATA.QUERY_FOR_MEPS_SF12_REVCODE_AGG t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for Categorical   */
%LET _CLIENTTASKLABEL='Distribution Analysis for Categorical';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:21 PM
   By task: Distribution Analysis for Categorical

   Input Data: Local:MYDATA.QUERY_FOR_MEPS_SF12_CATEGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_MEPS_SF12_CATEGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Categorical_Aggregate_SF12V2
	FROM MYDATA.QUERY_FOR_MEPS_SF12_CATEGORICAL(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Categorical_Aggregate_SF12V2";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR Categorical_Aggregate_SF12V2;
	HISTOGRAM   Categorical_Aggregate_SF12V2 / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis Quant vs Cat SF12V2   */
%LET _CLIENTTASKLABEL='Table Analysis Quant vs Cat SF12V2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:22 PM
   By task: Table Analysis Quant vs Cat SF12V2

   Input Data: Local:MYDATA.QUERY_FOR_MEPS_SF12_CATEGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_MEPS_SF12_CATEGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.SUM_AGGREGATE_QUERY, T.Categorical_Aggregate_SF12V2
	FROM MYDATA.QUERY_FOR_MEPS_SF12_CATEGORICAL(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for comparison of quantitative and catagorical aggregate for SF-12V2";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES SUM_AGGREGATE_QUERY * Categorical_Aggregate_SF12V2 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: INFULLYRCOMB   */
%LET _CLIENTTASKLABEL='INFULLYRCOMB';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.INFULLYR_COMB);

PROC SQL;
   CREATE TABLE MYDATA.INFULLYR_COMB(label="INFULLYR_COMB") AS 
   SELECT t1.Categorical_Aggregate_SF12V2, 
          t1.DUPERSID1, 
          t1.AGE12X1, 
          t1.SEX1, 
          t1.EDUCYR1, 
          t1.EDUYRDEG1, 
          t1.REGION121, 
          t1.RACETHX1, 
          t1.MARRY12X1, 
          t1.SAQELIG1, 
          t1.SAQWT12F1, 
          t1.ACTDTY311, 
          t1.ACTDTY421, 
          t1.ACTDTY531, 
          t1.ADAPPT421, 
          t1.ADCAPE421, 
          t1.ADCLIM421, 
          t1.ADCMPD421, 
          t1.ADCMPM421, 
          t1.ADCMPY421, 
          t1.ADDAYA421, 
          t1.ADDOWN421, 
          t1.ADDPRS421, 
          t1.ADDRBP421, 
          t1.ADEFRT421, 
          t1.ADEGMC421, 
          t1.ADEXPL421, 
          t1.ADEZUN421, 
          t1.ADFFRM421, 
          t1.ADFHLP421, 
          t1.ADGENH421, 
          t1.ADHECR421, 
          t1.ADHOPE421, 
          t1.ADILCR421, 
          t1.ADMWLM42_RECODE, 
          t1.ADDOWN42_RECODE, 
          t1.ADSOCA42_RECODE, 
          t1.ADILWW421, 
          t1.ADINSA421, 
          t1.ADINSB421, 
          t1.ADINST421, 
          t1.ADINTR421, 
          t1.ADLANG421, 
          t1.ADLIST421, 
          t1.ADMALS421, 
          t1.ADMWLM421, 
          t1.ADNDCR421, 
          t1.ADNERV421, 
          t1.ADNRGY421, 
          t1.ADNSMK421, 
          t1.ADOVER421, 
          t1.ADPAIN421, 
          t1.ADPALS421, 
          t1.ADPRTM421, 
          t1.ADPRX421, 
          t1.ADPWLM421, 
          t1.ADRESP421, 
          t1.ADREST421, 
          t1.ADRISK421, 
          t1.ADRTCR421, 
          t1.ADRTWW421, 
          t1.ADSAD421, 
          t1.ADSMOK421, 
          t1.ADSOCA421, 
          t1.ADSPEC421, 
          t1.ADSPRF421, 
          t1.ADTLHW421, 
          t1.ADWRTH421, 
          t1.CANCERDX1, 
          t1.DVGEN121, 
          t1.DVGEXP121, 
          t1.DVGMCD121, 
          t1.DVGMCR121, 
          t1.DVGOFD121, 
          t1.DVGOPR121, 
          t1.DVGOPU121, 
          t1.DVGOSR121, 
          t1.DVGOTH121, 
          t1.DVGPRV121, 
          t1.DVGPTR121, 
          t1.DVGSLF121, 
          t1.DVGSTL121, 
          t1.DVGTCH121, 
          t1.DVGTRI121, 
          t1.DVGVA121, 
          t1.DVGWCP121, 
          t1.DVOEXP121, 
          t1.DVOMCD121, 
          t1.DVOMCR121, 
          t1.DVOOFD121, 
          t1.DVOOPR121, 
          t1.DVOOPU121, 
          t1.DVOOSR121, 
          t1.DVOOTH121, 
          t1.DVOPRV121, 
          t1.DVOPTR121, 
          t1.DVORTH121, 
          t1.DVOSLF121, 
          t1.DVOSTL121, 
          t1.DVOTCH121, 
          t1.DVOTRI121, 
          t1.DVOVA121, 
          t1.DVOWCP121, 
          t1.DVTEXP121, 
          t1.DVTMCD121, 
          t1.DVTMCR121, 
          t1.DVTOFD121, 
          t1.DVTOPR121, 
          t1.DVTOPU121, 
          t1.DVTOSR121, 
          t1.DVTOT121, 
          t1.DVTOTH121, 
          t1.DVTPRV121, 
          t1.DVTPTR121, 
          t1.DVTSLF121, 
          t1.DVTSTL121, 
          t1.DVTTCH121, 
          t1.DVTTRI121, 
          t1.DVTVA121, 
          t1.DVTWCP121, 
          t1.EMPHDX1, 
          t1.EMPST311, 
          t1.EMPST421, 
          t1.EMPST531, 
          t1.OFREMP311, 
          t1.OFREMP421, 
          t1.OFREMP531, 
          t1.ERDOPR121, 
          t1.ERDOPU121, 
          t1.ERDTCH121, 
          t1.ERTTCH121, 
          t1.ERFTCH121, 
          t1.ERTOTH121, 
          t1.ERDMCD121, 
          t1.ERFMCD121, 
          t1.ERTMCD121, 
          t1.ERDMCR121, 
          t1.ERFMCR121, 
          t1.ERTMCR121, 
          t1.ERDOTH121, 
          t1.ERFOTH121, 
          t1.ERTOPR121, 
          t1.ERTOPU121, 
          t1.ERDSTL121, 
          t1.ERFSTL121, 
          t1.ERTSTL121, 
          t1.ERDOSR121, 
          t1.ERFOSR121, 
          t1.ERTOSR121, 
          t1.ERDOFD121, 
          t1.ERTOFD121, 
          t1.ERFOFD121, 
          t1.ERFOPR121, 
          t1.ERFOPU121, 
          t1.ERTPRV121, 
          t1.ERDPRV121, 
          t1.ERFPRV121, 
          t1.ERDPTR121, 
          t1.ERFPTR121, 
          t1.ERTPTR121, 
          t1.ERDSLF121, 
          t1.ERFSLF121, 
          t1.ERTSLF121, 
          t1.ERDTRI121, 
          t1.ERFTRI121, 
          t1.ERTTRI121, 
          t1.ERDVA121, 
          t1.ERFVA121, 
          t1.ERTVA121, 
          t1.ERDWCP121, 
          t1.ERFWCP121, 
          t1.ERTWCP121, 
          t1.MIDX1, 
          t1.ERTOT121, 
          t1.ARTHDX1, 
          t1.ENGCMF421, 
          t1.ENGSPK421, 
          t1.LANGHM421, 
          t1.STRKDX1, 
          t1.PHQ2421, 
          t1.K6SUM421, 
          t1.SFFLAG421, 
          t1.MCS421, 
          t1.PCS421, 
          t1.JTPAIN311, 
          t1.JTPAIN531, 
          t1.INS12X1, 
          t1.INS53X1, 
          t1.INS31X1, 
          t1.INS42X1, 
          t1.INSAT31X1, 
          t1.INSAT42X1, 
          t1.INSAT53X1, 
          t1.INSAT12X1, 
          t1.INSCOV121, 
          t1.USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.EDUCYR_RECODE, 
          t1.EDUYRDEG_RECODE, 
          t1.ADGENH42_RECODE, 
          t1.ADDAYA42_RECODE, 
          t1.ADCLIM42_RECODE, 
          t1.ADPALS42_RECODE, 
          t1.ADPWLM42_RECODE, 
          t1.ADMALS42_RECODE, 
          t1.ADPAIN42_RECODE, 
          t1.AGE12X_RECODED, 
          t1.ADCAPE42_RECODE, 
          t1.REGION12_RECODE, 
          t1.ADNRGY42_RECODE, 
          t1.MARRY12X_RECODE, 
          t1.ADHECR42_RECODED, 
          t1.ADHOPE42_RECODE, 
          t1.ADREST42_RECODE, 
          t1.ADSAD42_RECODE, 
          t1.ADWRTH42_RECODE, 
          t1.K6SUM42_RECODE, 
          t1.ADINSA42_RECODE, 
          t1.ADINSB42_RECODE, 
          t1.'PHQ242_ RECODE'n, 
          t1.ADNERV42_RECODE, 
          t1.CANCERDX_RECODE, 
          t1.INS12X_RECODE, 
          t1.MIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.STRKDX_RECODE, 
          t1.EMPHDX_RECODE, 
          t1.USBORN42_RECODE, 
          t1.ENGCMF42_RECODE, 
          t1.ENGSPK42_RECODE, 
          t1.JTPAIN31_RECODE, 
          t1.JTPAIN53_RECODE, 
          t1.ADGENH42_REVCODE, 
          t1.ADPAIN42_REVCODE, 
          t1.ADNRGY42_REVCODE, 
          t1.ADCAPE42_REVCODE, 
          t1.ADMWLM42_RECODE1, 
          t1.ADDOWN42_RECODE1, 
          t1.ADSOCA42_RECODE1, 
          t1.SUM_AGGREGATE_QUERY, 
          /* INFULLYRCOMB */
            (1) LABEL="INFULLYR2012" AS INFULLYRCOMB
      FROM MYDATA.QUERY_FOR_MEPS_SF12_CATEGORICAL t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: INER   */
LIBNAME EC100065 "P:\QAC\qac200\Data\MEPS";


%LET _CLIENTTASKLABEL='INER';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.INER2012);

PROC SQL;
   CREATE TABLE MYDATA.INER2012(label="INER2012") AS 
   SELECT t1.DUID AS DUID1, 
          t1.PID AS PID1, 
          t1.DUPERSID AS DUPERSID1, 
          t1.EVNTIDX AS EVNTIDX1, 
          t1.EVENTRN AS EVENTRN1, 
          t1.ERHEVIDX AS ERHEVIDX1, 
          t1.FFEEIDX AS FFEEIDX1, 
          t1.PANEL AS PANEL1, 
          t1.MPCDATA AS MPCDATA1, 
          t1.ERDATEYR AS ERDATEYR1, 
          t1.ERDATEMM AS ERDATEMM1, 
          t1.ERDATEDD AS ERDATEDD1, 
          t1.SEEDOC AS SEEDOC1, 
          t1.VSTCTGRY AS VSTCTGRY1, 
          t1.VSTRELCN AS VSTRELCN1, 
          t1.LABTEST AS LABTEST1, 
          t1.SONOGRAM AS SONOGRAM1, 
          t1.XRAYS AS XRAYS1, 
          t1.MAMMOG AS MAMMOG1, 
          t1.MRI AS MRI1, 
          t1.EKG AS EKG1, 
          t1.EEG AS EEG1, 
          t1.RCVVAC AS RCVVAC1, 
          t1.ANESTH AS ANESTH1, 
          t1.THRTSWAB AS THRTSWAB1, 
          t1.OTHSVCE AS OTHSVCE1, 
          t1.SURGPROC AS SURGPROC1, 
          t1.MEDPRESC AS MEDPRESC1, 
          t1.ERICD1X AS ERICD1X1, 
          t1.ERICD2X AS ERICD2X1, 
          t1.ERICD3X AS ERICD3X1, 
          t1.ERPRO1X AS ERPRO1X1, 
          t1.ERCCC1X AS ERCCC1X1, 
          t1.ERCCC2X AS ERCCC2X1, 
          t1.ERCCC3X AS ERCCC3X1, 
          t1.FFERTYPE AS FFERTYPE1, 
          t1.FFBEF12 AS FFBEF121, 
          t1.ERXP12X AS ERXP12X1, 
          t1.ERTC12X AS ERTC12X1, 
          t1.ERFSF12X AS ERFSF12X1, 
          t1.ERFMR12X AS ERFMR12X1, 
          t1.ERFMD12X AS ERFMD12X1, 
          t1.ERFPV12X AS ERFPV12X1, 
          t1.ERFVA12X AS ERFVA12X1, 
          t1.ERFTR12X AS ERFTR12X1, 
          t1.ERFOF12X AS ERFOF12X1, 
          t1.ERFSL12X AS ERFSL12X1, 
          t1.ERFWC12X AS ERFWC12X1, 
          t1.ERFOR12X AS ERFOR12X1, 
          t1.ERFOU12X AS ERFOU12X1, 
          t1.ERFOT12X AS ERFOT12X1, 
          t1.ERFXP12X AS ERFXP12X1, 
          t1.ERFTC12X AS ERFTC12X1, 
          t1.ERDSF12X AS ERDSF12X1, 
          t1.ERDMR12X AS ERDMR12X1, 
          t1.ERDMD12X AS ERDMD12X1, 
          t1.ERDPV12X AS ERDPV12X1, 
          t1.ERDVA12X AS ERDVA12X1, 
          t1.ERDTR12X AS ERDTR12X1, 
          t1.ERDOF12X AS ERDOF12X1, 
          t1.ERDSL12X AS ERDSL12X1, 
          t1.ERDWC12X AS ERDWC12X1, 
          t1.ERDOR12X AS ERDOR12X1, 
          t1.ERDOU12X AS ERDOU12X1, 
          t1.ERDOT12X AS ERDOT12X1, 
          t1.ERDXP12X AS ERDXP12X1, 
          t1.ERDTC12X AS ERDTC12X1, 
          t1.IMPFLAG AS IMPFLAG1, 
          t1.PERWT12F AS PERWT12F1, 
          t1.VARSTR AS VARSTR1, 
          t1.VARPSU AS VARPSU1, 
          /* ERFULLYR */
            (1) LABEL="ERFULLYR2012" AS ERFULLYR
      FROM EC100065.meps_er_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: DATA_SET_MERGER   */
%LET _CLIENTTASKLABEL='DATA_SET_MERGER';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_FOR_INFULLYR_COMB);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_FOR_INFULLYR_COMB(label="QUERY_FOR_INFULLYR_COMB") AS 
   SELECT t1.Categorical_Aggregate_SF12V2, 
          t1.DUPERSID1, 
          t1.AGE12X1, 
          t1.SEX1, 
          t1.EDUCYR1, 
          t1.EDUYRDEG1, 
          t1.REGION121, 
          t1.RACETHX1, 
          t1.MARRY12X1, 
          t1.SAQELIG1, 
          t1.SAQWT12F1, 
          t1.ACTDTY311, 
          t1.ACTDTY421, 
          t1.ACTDTY531, 
          t1.ADAPPT421, 
          t1.ADCAPE421, 
          t1.ADCLIM421, 
          t1.ADCMPD421, 
          t1.ADCMPM421, 
          t1.ADCMPY421, 
          t1.ADDAYA421, 
          t1.ADDOWN421, 
          t1.ADDPRS421, 
          t1.ADDRBP421, 
          t1.ADEFRT421, 
          t1.ADEGMC421, 
          t1.ADEXPL421, 
          t1.ADEZUN421, 
          t1.ADFFRM421, 
          t1.ADFHLP421, 
          t1.ADGENH421, 
          t1.ADHECR421, 
          t1.ADHOPE421, 
          t1.ADILCR421, 
          t1.ADMWLM42_RECODE, 
          t1.ADDOWN42_RECODE, 
          t1.ADSOCA42_RECODE, 
          t1.ADILWW421, 
          t1.ADINSA421, 
          t1.ADINSB421, 
          t1.ADINST421, 
          t1.ADINTR421, 
          t1.ADLANG421, 
          t1.ADLIST421, 
          t1.ADMALS421, 
          t1.ADMWLM421, 
          t1.ADNDCR421, 
          t1.ADNERV421, 
          t1.ADNRGY421, 
          t1.ADNSMK421, 
          t1.ADOVER421, 
          t1.ADPAIN421, 
          t1.ADPALS421, 
          t1.ADPRTM421, 
          t1.ADPRX421, 
          t1.ADPWLM421, 
          t1.ADRESP421, 
          t1.ADREST421, 
          t1.ADRISK421, 
          t1.ADRTCR421, 
          t1.ADRTWW421, 
          t1.ADSAD421, 
          t1.ADSMOK421, 
          t1.ADSOCA421, 
          t1.ADSPEC421, 
          t1.ADSPRF421, 
          t1.ADTLHW421, 
          t1.ADWRTH421, 
          t1.CANCERDX1, 
          t1.DVGEN121, 
          t1.DVGEXP121, 
          t1.DVGMCD121, 
          t1.DVGMCR121, 
          t1.DVGOFD121, 
          t1.DVGOPR121, 
          t1.DVGOPU121, 
          t1.DVGOSR121, 
          t1.DVGOTH121, 
          t1.DVGPRV121, 
          t1.DVGPTR121, 
          t1.DVGSLF121, 
          t1.DVGSTL121, 
          t1.DVGTCH121, 
          t1.DVGTRI121, 
          t1.DVGVA121, 
          t1.DVGWCP121, 
          t1.DVOEXP121, 
          t1.DVOMCD121, 
          t1.DVOMCR121, 
          t1.DVOOFD121, 
          t1.DVOOPR121, 
          t1.DVOOPU121, 
          t1.DVOOSR121, 
          t1.DVOOTH121, 
          t1.DVOPRV121, 
          t1.DVOPTR121, 
          t1.DVORTH121, 
          t1.DVOSLF121, 
          t1.DVOSTL121, 
          t1.DVOTCH121, 
          t1.DVOTRI121, 
          t1.DVOVA121, 
          t1.DVOWCP121, 
          t1.DVTEXP121, 
          t1.DVTMCD121, 
          t1.DVTMCR121, 
          t1.DVTOFD121, 
          t1.DVTOPR121, 
          t1.DVTOPU121, 
          t1.DVTOSR121, 
          t1.DVTOT121, 
          t1.DVTOTH121, 
          t1.DVTPRV121, 
          t1.DVTPTR121, 
          t1.DVTSLF121, 
          t1.DVTSTL121, 
          t1.DVTTCH121, 
          t1.DVTTRI121, 
          t1.DVTVA121, 
          t1.DVTWCP121, 
          t1.EMPHDX1, 
          t1.EMPST311, 
          t1.EMPST421, 
          t1.EMPST531, 
          t1.OFREMP311, 
          t1.OFREMP421, 
          t1.OFREMP531, 
          t1.ERDOPR121, 
          t1.ERDOPU121, 
          t1.ERDTCH121, 
          t1.ERTTCH121, 
          t1.ERFTCH121, 
          t1.ERTOTH121, 
          t1.ERDMCD121, 
          t1.ERFMCD121, 
          t1.ERTMCD121, 
          t1.ERDMCR121, 
          t1.ERFMCR121, 
          t1.ERTMCR121, 
          t1.ERDOTH121, 
          t1.ERFOTH121, 
          t1.ERTOPR121, 
          t1.ERTOPU121, 
          t1.ERDSTL121, 
          t1.ERFSTL121, 
          t1.ERTSTL121, 
          t1.ERDOSR121, 
          t1.ERFOSR121, 
          t1.ERTOSR121, 
          t1.ERDOFD121, 
          t1.ERTOFD121, 
          t1.ERFOFD121, 
          t1.ERFOPR121, 
          t1.ERFOPU121, 
          t1.ERTPRV121, 
          t1.ERDPRV121, 
          t1.ERFPRV121, 
          t1.ERDPTR121, 
          t1.ERFPTR121, 
          t1.ERTPTR121, 
          t1.ERDSLF121, 
          t1.ERFSLF121, 
          t1.ERTSLF121, 
          t1.ERDTRI121, 
          t1.ERFTRI121, 
          t1.ERTTRI121, 
          t1.ERDVA121, 
          t1.ERFVA121, 
          t1.ERTVA121, 
          t1.ERDWCP121, 
          t1.ERFWCP121, 
          t1.ERTWCP121, 
          t1.MIDX1, 
          t1.ERTOT121, 
          t1.ARTHDX1, 
          t1.ENGCMF421, 
          t1.ENGSPK421, 
          t1.LANGHM421, 
          t1.STRKDX1, 
          t1.PHQ2421, 
          t1.K6SUM421, 
          t1.SFFLAG421, 
          t1.MCS421, 
          t1.PCS421, 
          t1.JTPAIN311, 
          t1.JTPAIN531, 
          t1.INS12X1, 
          t1.INS53X1, 
          t1.INS31X1, 
          t1.INS42X1, 
          t1.INSAT31X1, 
          t1.INSAT42X1, 
          t1.INSAT53X1, 
          t1.INSAT12X1, 
          t1.INSCOV121, 
          t1.USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.EDUCYR_RECODE, 
          t1.EDUYRDEG_RECODE, 
          t1.ADGENH42_RECODE, 
          t1.ADDAYA42_RECODE, 
          t1.ADCLIM42_RECODE, 
          t1.ADPALS42_RECODE, 
          t1.ADPWLM42_RECODE, 
          t1.ADMALS42_RECODE, 
          t1.ADPAIN42_RECODE, 
          t1.AGE12X_RECODED, 
          t1.ADCAPE42_RECODE, 
          t1.REGION12_RECODE, 
          t1.ADNRGY42_RECODE, 
          t1.MARRY12X_RECODE, 
          t1.ADHECR42_RECODED, 
          t1.ADHOPE42_RECODE, 
          t1.ADREST42_RECODE, 
          t1.ADSAD42_RECODE, 
          t1.ADWRTH42_RECODE, 
          t1.K6SUM42_RECODE, 
          t1.ADINSA42_RECODE, 
          t1.ADINSB42_RECODE, 
          t1.'PHQ242_ RECODE'n, 
          t1.ADNERV42_RECODE, 
          t1.CANCERDX_RECODE, 
          t1.INS12X_RECODE, 
          t1.MIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.STRKDX_RECODE, 
          t1.EMPHDX_RECODE, 
          t1.USBORN42_RECODE, 
          t1.ENGCMF42_RECODE, 
          t1.ENGSPK42_RECODE, 
          t1.JTPAIN31_RECODE, 
          t1.JTPAIN53_RECODE, 
          t1.ADGENH42_REVCODE, 
          t1.ADPAIN42_REVCODE, 
          t1.ADNRGY42_REVCODE, 
          t1.ADCAPE42_REVCODE, 
          t1.ADMWLM42_RECODE1, 
          t1.ADDOWN42_RECODE1, 
          t2.DUID1, 
          t2.PID1, 
          t2.DUPERSID1 AS DUPERSID11, 
          t2.EVNTIDX1, 
          t2.EVENTRN1, 
          t2.ERHEVIDX1, 
          t2.FFEEIDX1, 
          t2.PANEL1, 
          t2.MPCDATA1, 
          t2.ERDATEYR1, 
          t2.ERDATEMM1, 
          t2.ERDATEDD1, 
          t2.SEEDOC1, 
          t2.VSTCTGRY1, 
          t2.VSTRELCN1, 
          t2.LABTEST1, 
          t2.SONOGRAM1, 
          t2.XRAYS1, 
          t2.MAMMOG1, 
          t2.MRI1, 
          t2.EKG1, 
          t2.EEG1, 
          t2.RCVVAC1, 
          t2.ANESTH1, 
          t2.THRTSWAB1, 
          t2.OTHSVCE1, 
          t2.SURGPROC1, 
          t2.MEDPRESC1, 
          t2.ERICD1X1, 
          t2.ERICD2X1, 
          t2.ERICD3X1, 
          t2.ERPRO1X1, 
          t2.ERCCC1X1, 
          t2.ERCCC2X1, 
          t2.ERCCC3X1, 
          t2.FFERTYPE1, 
          t2.FFBEF121, 
          t2.ERXP12X1, 
          t2.ERTC12X1, 
          t2.ERFSF12X1, 
          t2.ERFMR12X1, 
          t2.ERFMD12X1, 
          t2.ERFPV12X1, 
          t2.ERFVA12X1, 
          t2.ERFTR12X1, 
          t2.ERFOF12X1, 
          t2.ERFSL12X1, 
          t2.ERFWC12X1, 
          t2.ERFOR12X1, 
          t2.ERFOU12X1, 
          t2.ERFOT12X1, 
          t2.ERFXP12X1, 
          t2.ERFTC12X1, 
          t2.ERDSF12X1, 
          t2.ERDMR12X1, 
          t2.ERDMD12X1, 
          t2.ERDPV12X1, 
          t2.ERDVA12X1, 
          t2.ERDTR12X1, 
          t2.ERDOF12X1, 
          t2.ERDSL12X1, 
          t2.ERDWC12X1, 
          t2.ERDOR12X1, 
          t2.ERDOU12X1, 
          t2.ERDOT12X1, 
          t2.ERDXP12X1, 
          t2.ERDTC12X1, 
          t2.IMPFLAG1, 
          t2.PERWT12F1, 
          t2.VARSTR1, 
          t2.VARPSU1, 
          t2.ERFULLYR, 
          t1.ADSOCA42_RECODE1, 
          t1.SUM_AGGREGATE_QUERY, 
          t1.INFULLYRCOMB
      FROM MYDATA.INFULLYR_COMB t1
           FULL JOIN MYDATA.INER2012 t2 ON (t1.DUPERSID1 = t2.DUPERSID1)
      WHERE t2.ERFULLYR = 1 AND t1.INFULLYRCOMB = 1;
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data for ER   */
%LET _CLIENTTASKLABEL='List Data for ER';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:24 PM
   By task: List Data for ER

   Input Data: Local:MYDATA.QUERY_FOR_INFULLYR_COMB
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_INFULLYR_COMB
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ERFULLYR, T.DUPERSID1, T.DUPERSID11
	FROM MYDATA.QUERY_FOR_INFULLYR_COMB(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR ERFULLYR DUPERSID1 DUPERSID11;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes FOR COMB   */
%LET _CLIENTTASKLABEL='Data Set Attributes FOR COMB';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:26 PM
   By task: Data Set Attributes FOR COMB

   Input Data: Local:MYDATA.QUERY_FOR_INFULLYR_COMB
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForQUERY_FOR_INFULLY);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.QUERY_FOR_INFULLYR_COMB OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsForQUERY_FOR_INFULLY(LABEL="Contents Details for QUERY_FOR_INFULLYR_COMB");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsForQUERY_FOR_INFULLY
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='QUERY_FOR_INFULLYR_COMB';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsForQUERY_FOR_INFULLY OUT=WORK.CONTContentsForQUERY_FOR_INFULLY;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsForQUERY_FOR_INFULLY
		WHERE memname='QUERY_FOR_INFULLYR_COMB';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: XRAY_MRI_RECODE   */
%LET _CLIENTTASKLABEL='XRAY_MRI_RECODE';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_FOR_XRAY_MRI);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_FOR_XRAY_MRI(label="QUERY_FOR_XRAY_MRI") AS 
   SELECT t1.Categorical_Aggregate_SF12V2, 
          t1.DUPERSID1, 
          t1.AGE12X1, 
          t1.SEX1, 
          t1.EDUCYR1, 
          t1.EDUYRDEG1, 
          t1.REGION121, 
          t1.RACETHX1, 
          t1.MARRY12X1, 
          t1.SAQELIG1, 
          t1.SAQWT12F1, 
          t1.ACTDTY311, 
          t1.ACTDTY421, 
          t1.ACTDTY531, 
          t1.ADAPPT421, 
          t1.ADCAPE421, 
          t1.ADCLIM421, 
          t1.ADCMPD421, 
          t1.ADCMPM421, 
          t1.ADCMPY421, 
          t1.ADDAYA421, 
          t1.ADDOWN421, 
          t1.ADDPRS421, 
          t1.ADDRBP421, 
          t1.ADEFRT421, 
          t1.ADEGMC421, 
          t1.ADEXPL421, 
          t1.ADEZUN421, 
          t1.ADFFRM421, 
          t1.ADFHLP421, 
          t1.ADGENH421, 
          t1.ADHECR421, 
          t1.ADHOPE421, 
          t1.ADILCR421, 
          t1.ADMWLM42_RECODE, 
          t1.ADDOWN42_RECODE, 
          t1.ADSOCA42_RECODE, 
          t1.ADILWW421, 
          t1.ADINSA421, 
          t1.ADINSB421, 
          t1.ADINST421, 
          t1.ADINTR421, 
          t1.ADLANG421, 
          t1.ADLIST421, 
          t1.ADMALS421, 
          t1.ADMWLM421, 
          t1.ADNDCR421, 
          t1.ADNERV421, 
          t1.ADNRGY421, 
          t1.ADNSMK421, 
          t1.ADOVER421, 
          t1.ADPAIN421, 
          t1.ADPALS421, 
          t1.ADPRTM421, 
          t1.ADPRX421, 
          t1.ADPWLM421, 
          t1.ADRESP421, 
          t1.ADREST421, 
          t1.ADRISK421, 
          t1.ADRTCR421, 
          t1.ADRTWW421, 
          t1.ADSAD421, 
          t1.ADSMOK421, 
          t1.ADSOCA421, 
          t1.ADSPEC421, 
          t1.ADSPRF421, 
          t1.ADTLHW421, 
          t1.ADWRTH421, 
          t1.CANCERDX1, 
          t1.DVGEN121, 
          t1.DVGEXP121, 
          t1.DVGMCD121, 
          t1.DVGMCR121, 
          t1.DVGOFD121, 
          t1.DVGOPR121, 
          t1.DVGOPU121, 
          t1.DVGOSR121, 
          t1.DVGOTH121, 
          t1.DVGPRV121, 
          t1.DVGPTR121, 
          t1.DVGSLF121, 
          t1.DVGSTL121, 
          t1.DVGTCH121, 
          t1.DVGTRI121, 
          t1.DVGVA121, 
          t1.DVGWCP121, 
          t1.DVOEXP121, 
          t1.DVOMCD121, 
          t1.DVOMCR121, 
          t1.DVOOFD121, 
          t1.DVOOPR121, 
          t1.DVOOPU121, 
          t1.DVOOSR121, 
          t1.DVOOTH121, 
          t1.DVOPRV121, 
          t1.DVOPTR121, 
          t1.DVORTH121, 
          t1.DVOSLF121, 
          t1.DVOSTL121, 
          t1.DVOTCH121, 
          t1.DVOTRI121, 
          t1.DVOVA121, 
          t1.DVOWCP121, 
          t1.DVTEXP121, 
          t1.DVTMCD121, 
          t1.DVTMCR121, 
          t1.DVTOFD121, 
          t1.DVTOPR121, 
          t1.DVTOPU121, 
          t1.DVTOSR121, 
          t1.DVTOT121, 
          t1.DVTOTH121, 
          t1.DVTPRV121, 
          t1.DVTPTR121, 
          t1.DVTSLF121, 
          t1.DVTSTL121, 
          t1.DVTTCH121, 
          t1.DVTTRI121, 
          t1.DVTVA121, 
          t1.DVTWCP121, 
          t1.EMPHDX1, 
          t1.EMPST311, 
          t1.EMPST421, 
          t1.EMPST531, 
          t1.OFREMP311, 
          t1.OFREMP421, 
          t1.OFREMP531, 
          t1.ERDOPR121, 
          t1.ERDOPU121, 
          t1.ERDTCH121, 
          t1.ERTTCH121, 
          t1.ERFTCH121, 
          t1.ERTOTH121, 
          t1.ERDMCD121, 
          t1.ERFMCD121, 
          t1.ERTMCD121, 
          t1.ERDMCR121, 
          t1.ERFMCR121, 
          t1.ERTMCR121, 
          t1.ERDOTH121, 
          t1.ERFOTH121, 
          t1.ERTOPR121, 
          t1.ERTOPU121, 
          t1.ERDSTL121, 
          t1.ERFSTL121, 
          t1.ERTSTL121, 
          t1.ERDOSR121, 
          t1.ERFOSR121, 
          t1.ERTOSR121, 
          t1.ERDOFD121, 
          t1.ERTOFD121, 
          t1.ERFOFD121, 
          t1.ERFOPR121, 
          t1.ERFOPU121, 
          t1.ERTPRV121, 
          t1.ERDPRV121, 
          t1.ERFPRV121, 
          t1.ERDPTR121, 
          t1.ERFPTR121, 
          t1.ERTPTR121, 
          t1.ERDSLF121, 
          t1.ERFSLF121, 
          t1.ERTSLF121, 
          t1.ERDTRI121, 
          t1.ERFTRI121, 
          t1.ERTTRI121, 
          t1.ERDVA121, 
          t1.ERFVA121, 
          t1.ERTVA121, 
          t1.ERDWCP121, 
          t1.ERFWCP121, 
          t1.ERTWCP121, 
          t1.MIDX1, 
          t1.ERTOT121, 
          t1.ARTHDX1, 
          t1.ENGCMF421, 
          t1.ENGSPK421, 
          t1.LANGHM421, 
          t1.STRKDX1, 
          t1.PHQ2421, 
          t1.K6SUM421, 
          t1.SFFLAG421, 
          t1.MCS421, 
          t1.PCS421, 
          t1.JTPAIN311, 
          t1.JTPAIN531, 
          t1.INS12X1, 
          t1.INS53X1, 
          t1.INS31X1, 
          t1.INS42X1, 
          t1.INSAT31X1, 
          t1.INSAT42X1, 
          t1.INSAT53X1, 
          t1.INSAT12X1, 
          t1.INSCOV121, 
          t1.USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.EDUCYR_RECODE, 
          t1.EDUYRDEG_RECODE, 
          t1.ADGENH42_RECODE, 
          t1.ADDAYA42_RECODE, 
          t1.ADCLIM42_RECODE, 
          t1.ADPALS42_RECODE, 
          t1.ADPWLM42_RECODE, 
          t1.ADMALS42_RECODE, 
          t1.ADPAIN42_RECODE, 
          t1.AGE12X_RECODED, 
          t1.ADCAPE42_RECODE, 
          t1.REGION12_RECODE, 
          t1.ADNRGY42_RECODE, 
          t1.MARRY12X_RECODE, 
          t1.ADHECR42_RECODED, 
          t1.ADHOPE42_RECODE, 
          t1.ADREST42_RECODE, 
          t1.ADSAD42_RECODE, 
          t1.ADWRTH42_RECODE, 
          t1.K6SUM42_RECODE, 
          t1.ADINSA42_RECODE, 
          t1.ADINSB42_RECODE, 
          t1.'PHQ242_ RECODE'n, 
          t1.ADNERV42_RECODE, 
          t1.CANCERDX_RECODE, 
          t1.INS12X_RECODE, 
          t1.MIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.STRKDX_RECODE, 
          t1.EMPHDX_RECODE, 
          t1.USBORN42_RECODE, 
          t1.ENGCMF42_RECODE, 
          t1.ENGSPK42_RECODE, 
          t1.JTPAIN31_RECODE, 
          t1.JTPAIN53_RECODE, 
          t1.ADGENH42_REVCODE, 
          t1.ADPAIN42_REVCODE, 
          t1.ADNRGY42_REVCODE, 
          t1.ADCAPE42_REVCODE, 
          t1.ADMWLM42_RECODE1, 
          t1.ADDOWN42_RECODE1, 
          t1.DUID1, 
          t1.PID1, 
          t1.DUPERSID11, 
          t1.EVNTIDX1, 
          t1.EVENTRN1, 
          t1.ERHEVIDX1, 
          t1.FFEEIDX1, 
          t1.PANEL1, 
          t1.MPCDATA1, 
          t1.ERDATEYR1, 
          t1.ERDATEMM1, 
          t1.ERDATEDD1, 
          t1.SEEDOC1, 
          t1.VSTCTGRY1, 
          t1.VSTRELCN1, 
          t1.LABTEST1, 
          t1.SONOGRAM1, 
          t1.XRAYS1, 
          t1.MAMMOG1, 
          t1.MRI1, 
          t1.EKG1, 
          t1.EEG1, 
          t1.RCVVAC1, 
          t1.ANESTH1, 
          t1.THRTSWAB1, 
          t1.OTHSVCE1, 
          t1.SURGPROC1, 
          t1.MEDPRESC1, 
          t1.ERICD1X1, 
          t1.ERICD2X1, 
          t1.ERICD3X1, 
          t1.ERPRO1X1, 
          t1.ERCCC1X1, 
          t1.ERCCC2X1, 
          t1.ERCCC3X1, 
          t1.FFERTYPE1, 
          t1.FFBEF121, 
          t1.ERXP12X1, 
          t1.ERTC12X1, 
          t1.ERFSF12X1, 
          t1.ERFMR12X1, 
          t1.ERFMD12X1, 
          t1.ERFPV12X1, 
          t1.ERFVA12X1, 
          t1.ERFTR12X1, 
          t1.ERFOF12X1, 
          t1.ERFSL12X1, 
          t1.ERFWC12X1, 
          t1.ERFOR12X1, 
          t1.ERFOU12X1, 
          t1.ERFOT12X1, 
          t1.ERFXP12X1, 
          t1.ERFTC12X1, 
          t1.ERDSF12X1, 
          t1.ERDMR12X1, 
          t1.ERDMD12X1, 
          t1.ERDPV12X1, 
          t1.ERDVA12X1, 
          t1.ERDTR12X1, 
          t1.ERDOF12X1, 
          t1.ERDSL12X1, 
          t1.ERDWC12X1, 
          t1.ERDOR12X1, 
          t1.ERDOU12X1, 
          t1.ERDOT12X1, 
          t1.ERDXP12X1, 
          t1.ERDTC12X1, 
          t1.IMPFLAG1, 
          t1.PERWT12F1, 
          t1.VARSTR1, 
          t1.VARPSU1, 
          t1.ERFULLYR, 
          t1.ADSOCA42_RECODE1, 
          t1.SUM_AGGREGATE_QUERY, 
          t1.INFULLYRCOMB, 
          /* XRAYS_RECODE */
            (CASE 
               WHEN -7 = t1.XRAYS1 THEN .
               WHEN -8 = t1.XRAYS1 THEN .
               WHEN -9 = t1.XRAYS1 THEN .
               WHEN 95 = t1.XRAYS1 THEN 2
               ELSE t1.XRAYS1
            END) LABEL="XRAYS recoded" AS XRAYS_RECODE, 
          /* MRI_RECODE */
            (CASE 
               WHEN -7 = t1.MRI1 THEN .
               WHEN -8 = t1.MRI1 THEN .
               WHEN -9 = t1.MRI1 THEN .
               WHEN 95 = t1.MRI1 THEN 2
               ELSE t1.MRI1
            END) LABEL="MRI Recoded" AS MRI_RECODE
      FROM MYDATA.QUERY_FOR_INFULLYR_COMB t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies2   */
%LET _CLIENTTASKLABEL='One-Way Frequencies2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:28 PM
   By task: One-Way Frequencies2

   Input Data: Local:MYDATA.QUERY_FOR_XRAY_MRI
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_XRAY_MRI
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.MRI_RECODE, T.XRAYS_RECODE
	FROM MYDATA.QUERY_FOR_XRAY_MRI as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MRI_RECODE /  SCORES=TABLE;
	TABLES XRAYS_RECODE /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: DUPERSID_count   */
%LET _CLIENTTASKLABEL='DUPERSID_count';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.XRAYMRI_COUNT);

PROC SQL;
   CREATE TABLE MYDATA.XRAYMRI_COUNT(label="XRAYMRI_COUNT") AS 
   SELECT t1.DUPERSID1, 
          /* COUNT_of_DUPERSID1 */
            (COUNT(t1.DUPERSID1)) AS COUNT_of_DUPERSID1
      FROM MYDATA.QUERY_FOR_XRAY_MRI t1
      GROUP BY t1.DUPERSID1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: MERGED_XRAYMRI_COUNT   */
%LET _CLIENTTASKLABEL='MERGED_XRAYMRI_COUNT';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.NEWXRAYMRI);

PROC SQL;
   CREATE TABLE MYDATA.NEWXRAYMRI(label="NEWXRAYMRI") AS 
   SELECT t1.Categorical_Aggregate_SF12V2, 
          t1.DUPERSID1, 
          t1.AGE12X1, 
          t1.SEX1, 
          t1.EDUCYR1, 
          t1.EDUYRDEG1, 
          t1.REGION121, 
          t1.RACETHX1, 
          t1.MARRY12X1, 
          t1.SAQELIG1, 
          t2.DUPERSID1 AS DUPERSID12, 
          t2.COUNT_of_DUPERSID1, 
          t1.SAQWT12F1, 
          t1.ACTDTY311, 
          t1.ACTDTY421, 
          t1.ACTDTY531, 
          t1.ADAPPT421, 
          t1.ADCAPE421, 
          t1.ADCLIM421, 
          t1.ADCMPD421, 
          t1.ADCMPM421, 
          t1.ADCMPY421, 
          t1.ADDAYA421, 
          t1.ADDOWN421, 
          t1.ADDPRS421, 
          t1.ADDRBP421, 
          t1.ADEFRT421, 
          t1.ADEGMC421, 
          t1.ADEXPL421, 
          t1.ADEZUN421, 
          t1.ADFFRM421, 
          t1.ADFHLP421, 
          t1.ADGENH421, 
          t1.ADHECR421, 
          t1.ADHOPE421, 
          t1.ADILCR421, 
          t1.ADMWLM42_RECODE, 
          t1.ADDOWN42_RECODE, 
          t1.ADSOCA42_RECODE, 
          t1.ADILWW421, 
          t1.ADINSA421, 
          t1.ADINSB421, 
          t1.ADINST421, 
          t1.ADINTR421, 
          t1.ADLANG421, 
          t1.ADLIST421, 
          t1.ADMALS421, 
          t1.ADMWLM421, 
          t1.ADNDCR421, 
          t1.ADNERV421, 
          t1.ADNRGY421, 
          t1.ADNSMK421, 
          t1.ADOVER421, 
          t1.ADPAIN421, 
          t1.ADPALS421, 
          t1.ADPRTM421, 
          t1.ADPRX421, 
          t1.ADPWLM421, 
          t1.ADRESP421, 
          t1.ADREST421, 
          t1.ADRISK421, 
          t1.ADRTCR421, 
          t1.ADRTWW421, 
          t1.ADSAD421, 
          t1.ADSMOK421, 
          t1.ADSOCA421, 
          t1.ADSPEC421, 
          t1.ADSPRF421, 
          t1.ADTLHW421, 
          t1.ADWRTH421, 
          t1.CANCERDX1, 
          t1.DVGEN121, 
          t1.DVGEXP121, 
          t1.DVGMCD121, 
          t1.DVGMCR121, 
          t1.DVGOFD121, 
          t1.DVGOPR121, 
          t1.DVGOPU121, 
          t1.DVGOSR121, 
          t1.DVGOTH121, 
          t1.DVGPRV121, 
          t1.DVGPTR121, 
          t1.DVGSLF121, 
          t1.DVGSTL121, 
          t1.DVGTCH121, 
          t1.DVGTRI121, 
          t1.DVGVA121, 
          t1.DVGWCP121, 
          t1.DVOEXP121, 
          t1.DVOMCD121, 
          t1.DVOMCR121, 
          t1.DVOOFD121, 
          t1.DVOOPR121, 
          t1.DVOOPU121, 
          t1.DVOOSR121, 
          t1.DVOOTH121, 
          t1.DVOPRV121, 
          t1.DVOPTR121, 
          t1.DVORTH121, 
          t1.DVOSLF121, 
          t1.DVOSTL121, 
          t1.DVOTCH121, 
          t1.DVOTRI121, 
          t1.DVOVA121, 
          t1.DVOWCP121, 
          t1.DVTEXP121, 
          t1.DVTMCD121, 
          t1.DVTMCR121, 
          t1.DVTOFD121, 
          t1.DVTOPR121, 
          t1.DVTOPU121, 
          t1.DVTOSR121, 
          t1.DVTOT121, 
          t1.DVTOTH121, 
          t1.DVTPRV121, 
          t1.DVTPTR121, 
          t1.DVTSLF121, 
          t1.DVTSTL121, 
          t1.DVTTCH121, 
          t1.DVTTRI121, 
          t1.DVTVA121, 
          t1.DVTWCP121, 
          t1.EMPHDX1, 
          t1.EMPST311, 
          t1.EMPST421, 
          t1.EMPST531, 
          t1.OFREMP311, 
          t1.OFREMP421, 
          t1.OFREMP531, 
          t1.ERDOPR121, 
          t1.ERDOPU121, 
          t1.ERDTCH121, 
          t1.ERTTCH121, 
          t1.ERFTCH121, 
          t1.ERTOTH121, 
          t1.ERDMCD121, 
          t1.ERFMCD121, 
          t1.ERTMCD121, 
          t1.ERDMCR121, 
          t1.ERFMCR121, 
          t1.ERTMCR121, 
          t1.ERDOTH121, 
          t1.ERFOTH121, 
          t1.ERTOPR121, 
          t1.ERTOPU121, 
          t1.ERDSTL121, 
          t1.ERFSTL121, 
          t1.ERTSTL121, 
          t1.ERDOSR121, 
          t1.ERFOSR121, 
          t1.ERTOSR121, 
          t1.ERDOFD121, 
          t1.ERTOFD121, 
          t1.ERFOFD121, 
          t1.ERFOPR121, 
          t1.ERFOPU121, 
          t1.ERTPRV121, 
          t1.ERDPRV121, 
          t1.ERFPRV121, 
          t1.ERDPTR121, 
          t1.ERFPTR121, 
          t1.ERTPTR121, 
          t1.ERDSLF121, 
          t1.ERFSLF121, 
          t1.ERTSLF121, 
          t1.ERDTRI121, 
          t1.ERFTRI121, 
          t1.ERTTRI121, 
          t1.ERDVA121, 
          t1.ERFVA121, 
          t1.ERTVA121, 
          t1.ERDWCP121, 
          t1.ERFWCP121, 
          t1.ERTWCP121, 
          t1.MIDX1, 
          t1.ERTOT121, 
          t1.ARTHDX1, 
          t1.ENGCMF421, 
          t1.ENGSPK421, 
          t1.LANGHM421, 
          t1.STRKDX1, 
          t1.PHQ2421, 
          t1.K6SUM421, 
          t1.SFFLAG421, 
          t1.MCS421, 
          t1.PCS421, 
          t1.JTPAIN311, 
          t1.JTPAIN531, 
          t1.INS12X1, 
          t1.INS53X1, 
          t1.INS31X1, 
          t1.INS42X1, 
          t1.INSAT31X1, 
          t1.INSAT42X1, 
          t1.INSAT53X1, 
          t1.INSAT12X1, 
          t1.INSCOV121, 
          t1.USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.EDUCYR_RECODE, 
          t1.EDUYRDEG_RECODE, 
          t1.ADGENH42_RECODE, 
          t1.ADDAYA42_RECODE, 
          t1.ADCLIM42_RECODE, 
          t1.ADPALS42_RECODE, 
          t1.ADPWLM42_RECODE, 
          t1.ADMALS42_RECODE, 
          t1.ADPAIN42_RECODE, 
          t1.AGE12X_RECODED, 
          t1.ADCAPE42_RECODE, 
          t1.REGION12_RECODE, 
          t1.ADNRGY42_RECODE, 
          t1.MARRY12X_RECODE, 
          t1.ADHECR42_RECODED, 
          t1.ADHOPE42_RECODE, 
          t1.ADREST42_RECODE, 
          t1.ADSAD42_RECODE, 
          t1.ADWRTH42_RECODE, 
          t1.K6SUM42_RECODE, 
          t1.ADINSA42_RECODE, 
          t1.ADINSB42_RECODE, 
          t1.'PHQ242_ RECODE'n, 
          t1.ADNERV42_RECODE, 
          t1.CANCERDX_RECODE, 
          t1.INS12X_RECODE, 
          t1.MIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.STRKDX_RECODE, 
          t1.EMPHDX_RECODE, 
          t1.USBORN42_RECODE, 
          t1.ENGCMF42_RECODE, 
          t1.ENGSPK42_RECODE, 
          t1.JTPAIN31_RECODE, 
          t1.JTPAIN53_RECODE, 
          t1.ADGENH42_REVCODE, 
          t1.ADPAIN42_REVCODE, 
          t1.ADNRGY42_REVCODE, 
          t1.ADCAPE42_REVCODE, 
          t1.ADMWLM42_RECODE1, 
          t1.ADDOWN42_RECODE1, 
          t1.DUID1, 
          t1.PID1, 
          t1.DUPERSID11, 
          t1.EVNTIDX1, 
          t1.EVENTRN1, 
          t1.ERHEVIDX1, 
          t1.FFEEIDX1, 
          t1.PANEL1, 
          t1.MPCDATA1, 
          t1.ERDATEYR1, 
          t1.ERDATEMM1, 
          t1.ERDATEDD1, 
          t1.SEEDOC1, 
          t1.VSTCTGRY1, 
          t1.VSTRELCN1, 
          t1.LABTEST1, 
          t1.SONOGRAM1, 
          t1.XRAYS1, 
          t1.MAMMOG1, 
          t1.MRI1, 
          t1.EKG1, 
          t1.EEG1, 
          t1.RCVVAC1, 
          t1.ANESTH1, 
          t1.THRTSWAB1, 
          t1.OTHSVCE1, 
          t1.SURGPROC1, 
          t1.MEDPRESC1, 
          t1.ERICD1X1, 
          t1.ERICD2X1, 
          t1.ERICD3X1, 
          t1.ERPRO1X1, 
          t1.ERCCC1X1, 
          t1.ERCCC2X1, 
          t1.ERCCC3X1, 
          t1.FFERTYPE1, 
          t1.FFBEF121, 
          t1.ERXP12X1, 
          t1.ERTC12X1, 
          t1.ERFSF12X1, 
          t1.ERFMR12X1, 
          t1.ERFMD12X1, 
          t1.ERFPV12X1, 
          t1.ERFVA12X1, 
          t1.ERFTR12X1, 
          t1.ERFOF12X1, 
          t1.ERFSL12X1, 
          t1.ERFWC12X1, 
          t1.ERFOR12X1, 
          t1.ERFOU12X1, 
          t1.ERFOT12X1, 
          t1.ERFXP12X1, 
          t1.ERFTC12X1, 
          t1.ERDSF12X1, 
          t1.ERDMR12X1, 
          t1.ERDMD12X1, 
          t1.ERDPV12X1, 
          t1.ERDVA12X1, 
          t1.ERDTR12X1, 
          t1.ERDOF12X1, 
          t1.ERDSL12X1, 
          t1.ERDWC12X1, 
          t1.ERDOR12X1, 
          t1.ERDOU12X1, 
          t1.ERDOT12X1, 
          t1.ERDXP12X1, 
          t1.ERDTC12X1, 
          t1.IMPFLAG1, 
          t1.PERWT12F1, 
          t1.VARSTR1, 
          t1.VARPSU1, 
          t1.ERFULLYR, 
          t1.ADSOCA42_RECODE1, 
          t1.SUM_AGGREGATE_QUERY, 
          t1.INFULLYRCOMB, 
          t1.XRAYS_RECODE, 
          t1.MRI_RECODE
      FROM MYDATA.QUERY_FOR_XRAY_MRI t1
           INNER JOIN MYDATA.XRAYMRI_COUNT t2 ON (t1.DUPERSID1 = t2.DUPERSID1);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies ER   */
%LET _CLIENTTASKLABEL='One-Way Frequencies ER';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:30 PM
   By task: One-Way Frequencies ER

   Input Data: Local:MYDATA.NEWXRAYMRI
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.NEWXRAYMRI
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID1
	FROM MYDATA.NEWXRAYMRI(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID1 /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis of DUPERCOUNT   */
%LET _CLIENTTASKLABEL='Distribution Analysis of DUPERCOUNT';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:31 PM
   By task: Distribution Analysis of DUPERCOUNT

   Input Data: Local:MYDATA.NEWXRAYMRI
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.NEWXRAYMRI
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID1
	FROM MYDATA.NEWXRAYMRI(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: COUNT_of_DUPERSID1";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR COUNT_of_DUPERSID1;
	HISTOGRAM   COUNT_of_DUPERSID1 / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: MRIXRAY_CAT   */
%LET _CLIENTTASKLABEL='MRIXRAY_CAT';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_NEWXRAYMRI_CAT);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_NEWXRAYMRI_CAT(label="QUERY_NEWXRAYMRI_CAT") AS 
   SELECT t1.Categorical_Aggregate_SF12V2, 
          t1.DUPERSID1, 
          t1.AGE12X1, 
          t1.SEX1, 
          t1.EDUCYR1, 
          t1.EDUYRDEG1, 
          t1.REGION121, 
          t1.RACETHX1, 
          t1.MARRY12X1, 
          t1.SAQELIG1, 
          t1.DUPERSID12, 
          t1.COUNT_of_DUPERSID1, 
          t1.SAQWT12F1, 
          t1.ACTDTY311, 
          t1.ACTDTY421, 
          t1.ACTDTY531, 
          t1.ADAPPT421, 
          t1.ADCAPE421, 
          t1.ADCLIM421, 
          t1.ADCMPD421, 
          t1.ADCMPM421, 
          t1.ADCMPY421, 
          t1.ADDAYA421, 
          t1.ADDOWN421, 
          t1.ADDPRS421, 
          t1.ADDRBP421, 
          t1.ADEFRT421, 
          t1.ADEGMC421, 
          t1.ADEXPL421, 
          t1.ADEZUN421, 
          t1.ADFFRM421, 
          t1.ADFHLP421, 
          t1.ADGENH421, 
          t1.ADHECR421, 
          t1.ADHOPE421, 
          t1.ADILCR421, 
          t1.ADMWLM42_RECODE, 
          t1.ADDOWN42_RECODE, 
          t1.ADSOCA42_RECODE, 
          t1.ADILWW421, 
          t1.ADINSA421, 
          t1.ADINSB421, 
          t1.ADINST421, 
          t1.ADINTR421, 
          t1.ADLANG421, 
          t1.ADLIST421, 
          t1.ADMALS421, 
          t1.ADMWLM421, 
          t1.ADNDCR421, 
          t1.ADNERV421, 
          t1.ADNRGY421, 
          t1.ADNSMK421, 
          t1.ADOVER421, 
          t1.ADPAIN421, 
          t1.ADPALS421, 
          t1.ADPRTM421, 
          t1.ADPRX421, 
          t1.ADPWLM421, 
          t1.ADRESP421, 
          t1.ADREST421, 
          t1.ADRISK421, 
          t1.ADRTCR421, 
          t1.ADRTWW421, 
          t1.ADSAD421, 
          t1.ADSMOK421, 
          t1.ADSOCA421, 
          t1.ADSPEC421, 
          t1.ADSPRF421, 
          t1.ADTLHW421, 
          t1.ADWRTH421, 
          t1.CANCERDX1, 
          t1.DVGEN121, 
          t1.DVGEXP121, 
          t1.DVGMCD121, 
          t1.DVGMCR121, 
          t1.DVGOFD121, 
          t1.DVGOPR121, 
          t1.DVGOPU121, 
          t1.DVGOSR121, 
          t1.DVGOTH121, 
          t1.DVGPRV121, 
          t1.DVGPTR121, 
          t1.DVGSLF121, 
          t1.DVGSTL121, 
          t1.DVGTCH121, 
          t1.DVGTRI121, 
          t1.DVGVA121, 
          t1.DVGWCP121, 
          t1.DVOEXP121, 
          t1.DVOMCD121, 
          t1.DVOMCR121, 
          t1.DVOOFD121, 
          t1.DVOOPR121, 
          t1.DVOOPU121, 
          t1.DVOOSR121, 
          t1.DVOOTH121, 
          t1.DVOPRV121, 
          t1.DVOPTR121, 
          t1.DVORTH121, 
          t1.DVOSLF121, 
          t1.DVOSTL121, 
          t1.DVOTCH121, 
          t1.DVOTRI121, 
          t1.DVOVA121, 
          t1.DVOWCP121, 
          t1.DVTEXP121, 
          t1.DVTMCD121, 
          t1.DVTMCR121, 
          t1.DVTOFD121, 
          t1.DVTOPR121, 
          t1.DVTOPU121, 
          t1.DVTOSR121, 
          t1.DVTOT121, 
          t1.DVTOTH121, 
          t1.DVTPRV121, 
          t1.DVTPTR121, 
          t1.DVTSLF121, 
          t1.DVTSTL121, 
          t1.DVTTCH121, 
          t1.DVTTRI121, 
          t1.DVTVA121, 
          t1.DVTWCP121, 
          t1.EMPHDX1, 
          t1.EMPST311, 
          t1.EMPST421, 
          t1.EMPST531, 
          t1.OFREMP311, 
          t1.OFREMP421, 
          t1.OFREMP531, 
          t1.ERDOPR121, 
          t1.ERDOPU121, 
          t1.ERDTCH121, 
          t1.ERTTCH121, 
          t1.ERFTCH121, 
          t1.ERTOTH121, 
          t1.ERDMCD121, 
          t1.ERFMCD121, 
          t1.ERTMCD121, 
          t1.ERDMCR121, 
          t1.ERFMCR121, 
          t1.ERTMCR121, 
          t1.ERDOTH121, 
          t1.ERFOTH121, 
          t1.ERTOPR121, 
          t1.ERTOPU121, 
          t1.ERDSTL121, 
          t1.ERFSTL121, 
          t1.ERTSTL121, 
          t1.ERDOSR121, 
          t1.ERFOSR121, 
          t1.ERTOSR121, 
          t1.ERDOFD121, 
          t1.ERTOFD121, 
          t1.ERFOFD121, 
          t1.ERFOPR121, 
          t1.ERFOPU121, 
          t1.ERTPRV121, 
          t1.ERDPRV121, 
          t1.ERFPRV121, 
          t1.ERDPTR121, 
          t1.ERFPTR121, 
          t1.ERTPTR121, 
          t1.ERDSLF121, 
          t1.ERFSLF121, 
          t1.ERTSLF121, 
          t1.ERDTRI121, 
          t1.ERFTRI121, 
          t1.ERTTRI121, 
          t1.ERDVA121, 
          t1.ERFVA121, 
          t1.ERTVA121, 
          t1.ERDWCP121, 
          t1.ERFWCP121, 
          t1.ERTWCP121, 
          t1.MIDX1, 
          t1.ERTOT121, 
          t1.ARTHDX1, 
          t1.ENGCMF421, 
          t1.ENGSPK421, 
          t1.LANGHM421, 
          t1.STRKDX1, 
          t1.PHQ2421, 
          t1.K6SUM421, 
          t1.SFFLAG421, 
          t1.MCS421, 
          t1.PCS421, 
          t1.JTPAIN311, 
          t1.JTPAIN531, 
          t1.INS12X1, 
          t1.INS53X1, 
          t1.INS31X1, 
          t1.INS42X1, 
          t1.INSAT31X1, 
          t1.INSAT42X1, 
          t1.INSAT53X1, 
          t1.INSAT12X1, 
          t1.INSCOV121, 
          t1.USBORN422, 
          t1.INSAT31X, 
          t1.INSAT42X, 
          t1.INSAT53X, 
          t1.INSAT12X, 
          t1.INSCOV12, 
          t1.EDUCYR_RECODE, 
          t1.EDUYRDEG_RECODE, 
          t1.ADGENH42_RECODE, 
          t1.ADDAYA42_RECODE, 
          t1.ADCLIM42_RECODE, 
          t1.ADPALS42_RECODE, 
          t1.ADPWLM42_RECODE, 
          t1.ADMALS42_RECODE, 
          t1.ADPAIN42_RECODE, 
          t1.AGE12X_RECODED, 
          t1.ADCAPE42_RECODE, 
          t1.REGION12_RECODE, 
          t1.ADNRGY42_RECODE, 
          t1.MARRY12X_RECODE, 
          t1.ADHECR42_RECODED, 
          t1.ADHOPE42_RECODE, 
          t1.ADREST42_RECODE, 
          t1.ADSAD42_RECODE, 
          t1.ADWRTH42_RECODE, 
          t1.K6SUM42_RECODE, 
          t1.ADINSA42_RECODE, 
          t1.ADINSB42_RECODE, 
          t1.'PHQ242_ RECODE'n, 
          t1.ADNERV42_RECODE, 
          t1.CANCERDX_RECODE, 
          t1.INS12X_RECODE, 
          t1.MIDX_RECODE, 
          t1.ARTHDX_RECODE, 
          t1.STRKDX_RECODE, 
          t1.EMPHDX_RECODE, 
          t1.USBORN42_RECODE, 
          t1.ENGCMF42_RECODE, 
          t1.ENGSPK42_RECODE, 
          t1.JTPAIN31_RECODE, 
          t1.JTPAIN53_RECODE, 
          t1.ADGENH42_REVCODE, 
          t1.ADPAIN42_REVCODE, 
          t1.ADNRGY42_REVCODE, 
          t1.ADCAPE42_REVCODE, 
          t1.ADMWLM42_RECODE1, 
          t1.ADDOWN42_RECODE1, 
          t1.DUID1, 
          t1.PID1, 
          t1.DUPERSID11, 
          t1.EVNTIDX1, 
          t1.EVENTRN1, 
          t1.ERHEVIDX1, 
          t1.FFEEIDX1, 
          t1.PANEL1, 
          t1.MPCDATA1, 
          t1.ERDATEYR1, 
          t1.ERDATEMM1, 
          t1.ERDATEDD1, 
          t1.SEEDOC1, 
          t1.VSTCTGRY1, 
          t1.VSTRELCN1, 
          t1.LABTEST1, 
          t1.SONOGRAM1, 
          t1.XRAYS1, 
          t1.MAMMOG1, 
          t1.MRI1, 
          t1.EKG1, 
          t1.EEG1, 
          t1.RCVVAC1, 
          t1.ANESTH1, 
          t1.THRTSWAB1, 
          t1.OTHSVCE1, 
          t1.SURGPROC1, 
          t1.MEDPRESC1, 
          t1.ERICD1X1, 
          t1.ERICD2X1, 
          t1.ERICD3X1, 
          t1.ERPRO1X1, 
          t1.ERCCC1X1, 
          t1.ERCCC2X1, 
          t1.ERCCC3X1, 
          t1.FFERTYPE1, 
          t1.FFBEF121, 
          t1.ERXP12X1, 
          t1.ERTC12X1, 
          t1.ERFSF12X1, 
          t1.ERFMR12X1, 
          t1.ERFMD12X1, 
          t1.ERFPV12X1, 
          t1.ERFVA12X1, 
          t1.ERFTR12X1, 
          t1.ERFOF12X1, 
          t1.ERFSL12X1, 
          t1.ERFWC12X1, 
          t1.ERFOR12X1, 
          t1.ERFOU12X1, 
          t1.ERFOT12X1, 
          t1.ERFXP12X1, 
          t1.ERFTC12X1, 
          t1.ERDSF12X1, 
          t1.ERDMR12X1, 
          t1.ERDMD12X1, 
          t1.ERDPV12X1, 
          t1.ERDVA12X1, 
          t1.ERDTR12X1, 
          t1.ERDOF12X1, 
          t1.ERDSL12X1, 
          t1.ERDWC12X1, 
          t1.ERDOR12X1, 
          t1.ERDOU12X1, 
          t1.ERDOT12X1, 
          t1.ERDXP12X1, 
          t1.ERDTC12X1, 
          t1.IMPFLAG1, 
          t1.PERWT12F1, 
          t1.VARSTR1, 
          t1.VARPSU1, 
          t1.ERFULLYR, 
          t1.ADSOCA42_RECODE1, 
          t1.SUM_AGGREGATE_QUERY, 
          t1.INFULLYRCOMB, 
          t1.XRAYS_RECODE, 
          t1.MRI_RECODE, 
          /* ERCOUNT_CATEGORICAL */
            (CASE  
               WHEN t1.COUNT_of_DUPERSID1 = 1
               THEN 1
               WHEN t1.COUNT_of_DUPERSID1 >= 2 and t1.COUNT_of_DUPERSID1 < 4
               THEN 2   
               ELSE 3
            END) LABEL="Categorical count of ER visits for XRAYs and MRI's" AS ERCOUNT_CATEGORICAL
      FROM MYDATA.NEWXRAYMRI t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis of COUNT   */
%LET _CLIENTTASKLABEL='Table Analysis of COUNT';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:33 PM
   By task: Table Analysis of COUNT

   Input Data: Local:MYDATA.QUERY_NEWXRAYMRI_CAT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_NEWXRAYMRI_CAT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ERCOUNT_CATEGORICAL, T.COUNT_of_DUPERSID1
	FROM MYDATA.QUERY_NEWXRAYMRI_CAT(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID1 * ERCOUNT_CATEGORICAL /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies For DUPCOUNT   */
%LET _CLIENTTASKLABEL='One-Way Frequencies For DUPCOUNT';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:34 PM
   By task: One-Way Frequencies For DUPCOUNT

   Input Data: Local:MYDATA.QUERY_NEWXRAYMRI_CAT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_NEWXRAYMRI_CAT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ERCOUNT_CATEGORICAL
	FROM MYDATA.QUERY_NEWXRAYMRI_CAT(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ERCOUNT_CATEGORICAL /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for DUPCOUNT   */
%LET _CLIENTTASKLABEL='Distribution Analysis for DUPCOUNT';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_011315_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_011315_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:34:35 PM
   By task: Distribution Analysis for DUPCOUNT

   Input Data: Local:MYDATA.QUERY_NEWXRAYMRI_CAT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_NEWXRAYMRI_CAT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ERCOUNT_CATEGORICAL
	FROM MYDATA.QUERY_NEWXRAYMRI_CAT(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: ERCOUNT_CATEGORICAL";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poutler";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR ERCOUNT_CATEGORICAL;
	HISTOGRAM   ERCOUNT_CATEGORICAL / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
