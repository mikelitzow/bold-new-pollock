REM------------------------------------------------------------------------------------------------------------------------
REM   FIRST CREATE A COMPREHENSIVE (AS DESIRED) HAUL TABLE TO WORK WITH

REM  START WITH THE NBS SURVEY OF 2010, 2017, 2019
drop table HAUL_nbs;
drop view HAUL_nbs;
create table HAUL_nbs as
SELECT  
  to_number(to_char(a.start_time,'yyyy')) year,'NBS' survey,a.region,a.CRUISEJOIN,a.hauljoin,a.vessel,a.cruise,
a.HAUL,a.HAUL_TYPE,a.PERFORMANCE,START_TIME,DURATION,DISTANCE_FISHED,NET_WIDTH,NET_MEASURED,NET_HEIGHT,
a.STRATUM,START_LATITUDE,END_LATITUDE,START_LONGITUDE,END_LONGITUDE,STATIONID,GEAR_DEPTH,BOTTOM_DEPTH,
BOTTOM_TYPE,SURFACE_TEMPERATURE,GEAR_TEMPERATURE,WIRE_LENGTH,GEAR,ACCESSORIES,SUBSAMPLE,AUDITJOIN
FROM  
	RACEBASE.HAUL A
JOIN 
	RACE_DATA.V_CRUISES B
ON 
	(B.CRUISEJOIN = A.CRUISEJOIN)
WHERE 
	A.PERFORMANCE >= 0
	AND A.HAUL_TYPE = 3
	AND A.STATIONID IS NOT NULL
	AND B.SURVEY_DEFINITION_ID = 143
	AND B.YEAR in (2010,2017,2019);

drop  table haul_ebs; 
drop  view haul_ebs; 
create table haul_ebs as 
SELECT  to_number(to_char(a.start_time,'yyyy')) year,A.*
FROM RACEBASE.HAUL A
JOIN RACE_DATA.V_CRUISES B
ON (B.CRUISEJOIN = A.CRUISEJOIN)
WHERE A.PERFORMANCE >= 0
AND A.HAUL_TYPE = 3
AND A.STATIONID IS NOT NULL
AND A.STRATUM IN (10,20,31,32,41,42,43,50,61,62,82,90)
AND B.SURVEY_DEFINITION_ID = 98;


REM  NOW CREATE NBS DATA FOR THE 2018 SURVEY
drop table haul_nbs_2018;
create table haul_nbs_2018 as
select 
  to_number(to_char(a.start_time,'yyyy')) year,
  'NBS' survey,a.region,a.CRUISEJOIN,a.hauljoin,a.vessel,a.cruise,a.HAUL,a.HAUL_TYPE,a.PERFORMANCE,START_TIME,DURATION,
  DISTANCE_FISHED,NET_WIDTH,NET_MEASURED,NET_HEIGHT,a.STRATUM,START_LATITUDE,END_LATITUDE,START_LONGITUDE,
  END_LONGITUDE,STATIONID,GEAR_DEPTH,BOTTOM_DEPTH,BOTTOM_TYPE,SURFACE_TEMPERATURE,GEAR_TEMPERATURE,WIRE_LENGTH,
  GEAR,ACCESSORIES,SUBSAMPLE,AUDITJOIN
 from racebase.haul a, RACE_DATA.V_CRUISES b
WHERE 
  A.STATIONID IS NOT NULL
	AND B.SURVEY_DEFINITION_ID = 98
  and a.performance>=0
  and a.haul_type=13
  and to_number(to_char(a.start_time,'yyyy'))=2018
and a.cruisejoin=b.cruisejoin;


REM  NOW CREATE DATA FOR THE EBS SHELF SURVEY
drop  table haul_ebs; 
drop  view haul_ebs; 
create table haul_ebs as 
SELECT  to_number(to_char(a.start_time,'yyyy')) year,A.*
FROM RACEBASE.HAUL A
JOIN RACE_DATA.V_CRUISES B
ON (B.CRUISEJOIN = A.CRUISEJOIN)
WHERE A.PERFORMANCE >= 0
AND A.HAUL_TYPE = 3
AND A.STATIONID IS NOT NULL
AND A.STRATUM IN (10,20,31,32,41,42,43,50,61,62,82,90)
AND B.SURVEY_DEFINITION_ID = 98;

REM  NOW CREATE DATA FOR THE GOA 2019 SURVEY
drop table haul_goa;
drop view haul_goa;
create table haul_goa as 
select * from racebase.haul where region='GOA' and cruise=201901 and abundance_haul='Y';


REM  NOW PUT THE HAUL TABLES TOGETHER
drop table haul_ebs_nbs_goa;
create table haul_ebs_nbs_goa as select * from haul_nbs;
insert into haul_ebs_nbs_goa select * from haul_nbs_2018;
insert into haul_ebs_nbs_goa select 
  YEAR,'EBS' survey,region,CRUISEJOIN,hauljoin,vessel,cruise,HAUL,HAUL_TYPE,PERFORMANCE,START_TIME,DURATION,DISTANCE_FISHED,
  NET_WIDTH,NET_MEASURED,NET_HEIGHT,STRATUM,START_LATITUDE,END_LATITUDE,START_LONGITUDE,END_LONGITUDE,STATIONID,
  GEAR_DEPTH,BOTTOM_DEPTH,BOTTOM_TYPE,SURFACE_TEMPERATURE,GEAR_TEMPERATURE,WIRE_LENGTH,GEAR,ACCESSORIES,SUBSAMPLE,AUDITJOIN
  from haul_ebs where year =2019;
insert into haul_ebs_nbs_goa select 
  trunc(cruise/100) YEAR,'GOA' survey,region,CRUISEJOIN,hauljoin,vessel,cruise,HAUL,HAUL_TYPE,PERFORMANCE,START_TIME,DURATION,DISTANCE_FISHED,
  NET_WIDTH,NET_MEASURED,NET_HEIGHT,STRATUM,START_LATITUDE,END_LATITUDE,START_LONGITUDE,END_LONGITUDE,STATIONID,
  GEAR_DEPTH,BOTTOM_DEPTH,BOTTOM_TYPE,SURFACE_TEMPERATURE,GEAR_TEMPERATURE,WIRE_LENGTH,GEAR,ACCESSORIES,SUBSAMPLE,AUDITJOIN
from haul_goa;



REM------------------------------------------------------------------------------------------------------------------------


REM  NOW SELECT THE SPECIMEN DATA FROM THE REQUESTED SURVEY HAULS
select h.survey, h.hauljoin, h.cruise, h.region, h.stratum,h.haul, h.stationid, h.start_latitude latitude, h.start_longitude longitude, 
h.gear_temperature bottom_temperature, h.surface_temperature, s.species_code, s.specimenid, s.sex, s.length, s.weight, s.age 
from haul_ebs_nbs_goa h, racebase.specimen s where h.hauljoin=s.hauljoin and species_code=21740 
order by h.survey, h.cruise, h.vessel, h.haul, specimenid;







