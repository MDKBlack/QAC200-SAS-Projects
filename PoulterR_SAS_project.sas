/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Friday, January 09, 2015     TIME: 2:31:29 PM
PROJECT: PoulterR_SAS_project_010915_sas
PROJECT PATH: P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA  "P:\QAC\qac200\students\rpoulter\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:48 PM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

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
          t1.USBORN42
      FROM EC100005.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Code For Data Set Filtered   */
%LET SYSLAST=MYDATA.MEPS_FILTER_FULLYR_2012;
%LET _CLIENTTASKLABEL='Code For Data Set Filtered';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:49 PM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_FILTER_FULLYR);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_FILTER_FULLYR AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.EDUCYR, 
          t1.USBORN42 AS USBORN421, 
          t1.EDUYRDEG, 
          t1.REGION12, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.SAQELIG, 
          t1.SAQWT12F, 
          t1.ACTDTY31, 
          t1.USBORN42, 
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
          /* AGE12X_RECODED */
            (CASE 
               WHEN -1 = t1.AGE12X THEN .
               ELSE t1.AGE12X
            END) LABEL="AGE AS OF 123112 Recode" AS AGE12X_RECODED, 
          /* REGION12_RECODE */
            (CASE 
               WHEN -1 = t1.REGION12 THEN .
               ELSE t1.REGION12
            END) LABEL="CENSUS REGION AS OF 123112 Recode" AS REGION12_RECODE, 
          /* MARRY12X_RECODE */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="MARITAL STATUS 123112 Recoded" AS MARRY12X_RECODE, 
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
            END) LABEL="JOINT PAIN LAST 12 MONTHS 5/3 Recoded" AS JTPAIN53_RECODE
      FROM MYDATA.MEPS_FILTER_FULLYR_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: AGE12X   */
%LET _CLIENTTASKLABEL='AGE12X';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:51 PM
   By task: AGE12X

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGE12X_RECODED, T.AGE12X
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results For age 18 or greater 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES AGE12X * AGE12X_RECODED /
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


/*   START OF NODE: EDUYRDEG   */
%LET _CLIENTTASKLABEL='EDUYRDEG';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:52 PM
   By task: EDUYRDEG

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDUYRDEG, T.EDUYRDEG_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for highest degree received";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDUYRDEG * EDUYRDEG_RECODE /
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


/*   START OF NODE: ADGEN42   */
%LET _CLIENTTASKLABEL='ADGEN42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:52 PM
   By task: ADGEN42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADGENH42, T.ADGENH42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for ADGEN";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADGENH42 * ADGENH42_RECODE /
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


/*   START OF NODE: EDUCYR   */
%LET _CLIENTTASKLABEL='EDUCYR';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:53 PM
   By task: EDUCYR

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDUCYR, T.EDUCYR_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for high grade completed 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDUCYR * EDUCYR_RECODE /
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


/*   START OF NODE: ADDAYA42   */
%LET _CLIENTTASKLABEL='ADDAYA42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:53 PM
   By task: ADDAYA42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADDAYA42, T.ADDAYA42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for ADDAYA 42";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADDAYA42 * ADDAYA42_RECODE /
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


/*   START OF NODE: ADPALS42   */
%LET _CLIENTTASKLABEL='ADPALS42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:53 PM
   By task: ADPALS42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADPALS42, T.ADPALS42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results ADPALS42 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPALS42 * ADPALS42_RECODE /
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


/*   START OF NODE: ADCLIM42   */
%LET _CLIENTTASKLABEL='ADCLIM42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:54 PM
   By task: ADCLIM42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADCLIM42, T.ADCLIM42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for ADCLIMB 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADCLIM42 * ADCLIM42_RECODE /
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


/*   START OF NODE: ADPWLM42   */
%LET _CLIENTTASKLABEL='ADPWLM42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:54 PM
   By task: ADPWLM42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADPWLM42, T.ADPWLM42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results ADPWLM42";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPWLM42 * ADPWLM42_RECODE /
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


/*   START OF NODE: K6SUM42   */
%LET _CLIENTTASKLABEL='K6SUM42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:55 PM
   By task: K6SUM42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.K6SUM42, T.K6SUM42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for K6SUM42";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES K6SUM42 * K6SUM42_RECODE /
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


/*   START OF NODE: ADINSA42   */
%LET _CLIENTTASKLABEL='ADINSA42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:55 PM
   By task: ADINSA42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADINSA42, T.ADINSA42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for insurance 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADINSA42 * ADINSA42_RECODE /
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


/*   START OF NODE: ADINSB42   */
%LET _CLIENTTASKLABEL='ADINSB42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:55 PM
   By task: ADINSB42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADINSB42, T.ADINSB42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for insurance 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADINSB42 * ADINSB42_RECODE /
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


/*   START OF NODE: PHQ242   */
%LET _CLIENTTASKLABEL='PHQ242';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:56 PM
   By task: PHQ242

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.PHQ242, T."PHQ242_ RECODE"n
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for PHQ242";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES PHQ242 * "PHQ242_ RECODE"n /
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


/*   START OF NODE: ADMALS42   */
%LET _CLIENTTASKLABEL='ADMALS42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:56 PM
   By task: ADMALS42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADMALS42, T.ADMALS42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for ADMALS42";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADMALS42 * ADMALS42_RECODE /
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


/*   START OF NODE: ADNERV42   */
%LET _CLIENTTASKLABEL='ADNERV42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:57 PM
   By task: ADNERV42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADNERV42, T.ADNERV42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for how nervous someone felt 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADNERV42 * ADNERV42_RECODE /
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


/*   START OF NODE: REGION12   */
%LET _CLIENTTASKLABEL='REGION12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:57 PM
   By task: REGION12

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.REGION12, T.REGION12_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for REGION12";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES REGION12 * REGION12_RECODE /
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


/*   START OF NODE: CANCERDX   */
%LET _CLIENTTASKLABEL='CANCERDX';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:57 PM
   By task: CANCERDX

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CANCERDX, T.CANCERDX_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for cancer diagnosed 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES CANCERDX * CANCERDX_RECODE /
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


/*   START OF NODE: MARRY12X   */
%LET _CLIENTTASKLABEL='MARRY12X';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:58 PM
   By task: MARRY12X

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY12X, T.MARRY12X_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for MARRY12X";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MARRY12X * MARRY12X_RECODE /
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


/*   START OF NODE: INS12X   */
%LET _CLIENTTASKLABEL='INS12X';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:58 PM
   By task: INS12X

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.INS12X, T.INS12X_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for INS12X";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES INS12X * INS12X_RECODE /
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


/*   START OF NODE: ADHEC42   */
%LET _CLIENTTASKLABEL='ADHEC42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:59 PM
   By task: ADHEC42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADHECR42, T.ADHECR42_RECODED
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for ADHEC42";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADHECR42 * ADHECR42_RECODED /
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


/*   START OF NODE: MIDX   */
%LET _CLIENTTASKLABEL='MIDX';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:29:59 PM
   By task: MIDX

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MIDX, T.MIDX_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for heart attack disgnosed 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MIDX * MIDX_RECODE /
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


/*   START OF NODE: USBORN42   */
%LET _CLIENTTASKLABEL='USBORN42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:30:00 PM
   By task: USBORN42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.USBORN421, T.USBORN42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for USBORN or not 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES USBORN421 * USBORN42_RECODE /
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


/*   START OF NODE: ADHOPE42   */
%LET _CLIENTTASKLABEL='ADHOPE42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:30:00 PM
   By task: ADHOPE42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADHOPE42, T.ADHOPE42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for how someone felt in terms of hope 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADHOPE42 * ADHOPE42_RECODE /
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


/*   START OF NODE: ARTHDX   */
%LET _CLIENTTASKLABEL='ARTHDX';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:30:00 PM
   By task: ARTHDX

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ARTHDX, T.ARTHDX_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for arthritis diagnosed";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ARTHDX * ARTHDX_RECODE /
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


/*   START OF NODE: ENGCMF42   */
%LET _CLIENTTASKLABEL='ENGCMF42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:30:01 PM
   By task: ENGCMF42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ENGCMF42, T.ENGCMF42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results comfort with English 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ENGCMF42 * ENGCMF42_RECODE /
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


/*   START OF NODE: ADREST42   */
%LET _CLIENTTASKLABEL='ADREST42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:30:01 PM
   By task: ADREST42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADREST42, T.ADREST42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for amount of rest 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADREST42 * ADREST42_RECODE /
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


/*   START OF NODE: ADSAD42   */
%LET _CLIENTTASKLABEL='ADSAD42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:30:01 PM
   By task: ADSAD42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADSAD42, T.ADSAD42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for how sad someone felt 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADSAD42 * ADSAD42_RECODE /
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


/*   START OF NODE: ENGSPK42   */
%LET _CLIENTTASKLABEL='ENGSPK42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:30:02 PM
   By task: ENGSPK42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ENGSPK42, T.ENGSPK42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for speak english 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ENGSPK42 * ENGSPK42_RECODE /
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


/*   START OF NODE: ADWRTH42   */
%LET _CLIENTTASKLABEL='ADWRTH42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:30:02 PM
   By task: ADWRTH42

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADWRTH42, T.ADWRTH42_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for how someone felt about their worth 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADWRTH42 * ADWRTH42_RECODE /
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


/*   START OF NODE: STRKDX   */
%LET _CLIENTTASKLABEL='STRKDX';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:30:03 PM
   By task: STRKDX

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.STRKDX, T.STRKDX_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for stroke diagnosed";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES STRKDX * STRKDX_RECODE /
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


/*   START OF NODE: EMPHDX   */
%LET _CLIENTTASKLABEL='EMPHDX';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:30:03 PM
   By task: EMPHDX

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EMPHDX, T.EMPHDX_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for emphuzima diagnosis";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EMPHDX * EMPHDX_RECODE /
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


/*   START OF NODE: JTPAIN31   */
%LET _CLIENTTASKLABEL='JTPAIN31';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:30:03 PM
   By task: JTPAIN31

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.JTPAIN31, T.JTPAIN31_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for joint pain 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES JTPAIN31 * JTPAIN31_RECODE /
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


/*   START OF NODE: JTPAIN53   */
%LET _CLIENTTASKLABEL='JTPAIN53';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010915_sas.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010915_sas.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:30:04 PM
   By task: JTPAIN53

   Input Data: Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MEPS_FILTER_FULLYR
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.JTPAIN53, T.JTPAIN53_RECODE
	FROM WORK.QUERY_FOR_MEPS_FILTER_FULLYR(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for joint pain 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Ryan Poulter";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES JTPAIN53 * JTPAIN53_RECODE /
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

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
