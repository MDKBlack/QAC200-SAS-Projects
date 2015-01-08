/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Thursday, January 08, 2015     TIME: 11:22:58 AM
PROJECT: PoulterR_SAS_project_010715
PROJECT PATH: P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010715.egp
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
FILENAME EGPDFX TEMP;
ODS PDF(ID=EGPDFX) FILE=EGPDFX STYLE=printer SAS;
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010715.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010715.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA  "P:\QAC\qac200\students\rpoulter\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010715.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010715.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 11:22:47 AM
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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010715.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010715.egp';

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
          t1.AMCEXP12, 
          t1.AMCHIR12, 
          t1.AMCMCD12, 
          t1.AMCMCR12, 
          t1.AMCOFD12, 
          t1.AMCOPR12, 
          t1.AMCOPU12, 
          t1.AMCOSR12, 
          t1.AMCOTH12, 
          t1.AMCPRV12, 
          t1.AMCPTR12, 
          t1.AMCSLF12, 
          t1.AMCSTL12, 
          t1.AMCTCH12, 
          t1.AMCTRI12, 
          t1.AMCVA12, 
          t1.AMCWCP12, 
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
          t1.INSCOV12
      FROM EC100032.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Filtered   */
%LET _CLIENTTASKLABEL='Data Set Filtered';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\rpoulter\Assignments\PoulterR_SAS_project_010715.egp';
%LET _CLIENTPROJECTNAME='PoulterR_SAS_project_010715.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 11:22:47 AM
   By task: Data Set Filtered

   Input Data: Local:MYDATA.MEPS_FILTER_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTENTSFORFILTER_FOR_MEPS);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.MEPS_FILTER_FULLYR_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTENTSFORFILTER_FOR_MEPS(LABEL="Contents Details for MEPS_FILTER_FULLYR_2012");
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
			typemem LABEL="Data Set Type" FROM WORK.CONTENTSFORFILTER_FOR_MEPS
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FILTER_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTENTSFORFILTER_FOR_MEPS OUT=WORK.CONTENTSFORFILTER_FOR_MEPS;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTENTSFORFILTER_FOR_MEPS
		WHERE memname='MEPS_FILTER_FULLYR_2012';
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

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
