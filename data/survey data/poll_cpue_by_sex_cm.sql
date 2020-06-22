REM  SCRIPT MODIFIED 03/04/2020 NICHOL
REM THIS SCRIPT CALCULATES THE BOTH CPUE_NOHA & CPUE_KGHA BY HAUL BY SEX CM-LENGTH FOR ONE SPECIES
REM  ***NEED TO REPLACE SPECIES_CODE AND SPECIES TEXT NAME (E.G, POLL) IF YOU WANT TO DO A NEW SPECIES***
REM  ***NEED TO REPLACE LENGTH-WEIGHT REGRESSION PARAMETER VALUES SPECIFIC TO THE SPECIES***

REM NOTE THAT CASES ZERO CATCHS ARE INCLUDED WITH SEX=-9 AND LENGTH=-9
REM ALSO, CASES OF HAULS WITH CATCH BUT NO LENGTHS ARE INCLUDED, AGAIN WITH SEX=-9 AND LENGTH=-9 (SO CPUE TOTALS ARE NOT LOST)


REM FIRST SELECT ALL GOOD STANDARDLY CONDUCTED SURVEY BOTTOM-TRAWLS FROM STANDARD PLUSW STATION AND NORTHERN BERING SEA STATIONS
REM (STRATA 10,20,31,32,41,42,43,50,61,62,  82,90,  70,71,81)

REM --------------------------------------------------------------------------------------------------------------------------------------
REM SELECT STANDARD PLUSNW STATIONS, 1982-ON
drop  table haulname; 
drop  view haulname; 
create table haulname as 
SELECT  to_number(to_char(a.start_time,'yyyy')) year,A.*
FROM RACEBASE.HAUL A
JOIN RACE_DATA.V_CRUISES B
ON (B.CRUISEJOIN = A.CRUISEJOIN)
WHERE A.PERFORMANCE >= 0
AND A.HAUL_TYPE = 3
AND A.STATIONID IS NOT NULL
AND A.STRATUM IN (10,20,31,32,41,42,43,50,61,62,82,90)
AND B.SURVEY_DEFINITION_ID = 98;
REM --------------------------------------------------------------------------------------------------------------------------------------



REM --------------------------------------------------------------------------------------------------------------------------------------
REM THIS SELECT GRABS ALL NORTH SHELF STATION SAMPLED WITH HAUL_TYPE=3 (NEED TO EXCLUDE FOREIGN VESSELS)
drop table temp;
create table temp as
SELECT  
	to_number(to_char(a.start_time,'yyyy')) year,
	A.*
FROM 
	RACEBASE.HAUL A
JOIN 
	RACE_DATA.V_CRUISES B
ON 
	(B.CRUISEJOIN = A.CRUISEJOIN)
WHERE 
 B.SURVEY_DEFINITION_ID = 143
union
SELECT  
	to_number(to_char(a.start_time,'yyyy')) year,
	A.*
FROM 
	RACEBASE.HAUL A
JOIN 
	RACE_DATA.V_CRUISES B
ON 
	(B.CRUISEJOIN = A.CRUISEJOIN)
WHERE 
 B.SURVEY_DEFINITION_ID = 98
  and a.start_latitude>60.49
  and a.start_latitude<65.4
  and a.start_longitude>-176
  and a.stationid NOT IN('V-28','V-27','V-26','V-25','V25','V26','V27',
  'U-28','U-27','U-26','U-25','U-29','U28','U27','U26','U25','U29',
  'T-28','T-27','T-26','T-25','T-29','T-30','T28','T27','T26','T25','T29','T30',
  'S-28','S-27','S-26','S-25','S-29','S-30','S-31','S-28','S-22','S-23','S-24',
  'S27','S26','S25','S29','S30','S31','S22','S23','S24',
  'R-28','R-27','R-26','R-25','R-29','R-30','R-31','R-32','R-22','R-23','R-24',
  'R28','R27','R26','R25','R29','R30','R31','R32','R22','R23','R24') 
UNION
SELECT  
	to_number(to_char(a.start_time,'yyyy')) year,
	A.*
FROM 
	RACEBASE.HAUL a
where
  start_latitude>60.49
  and start_latitude<65.4
  and start_longitude>-176
  AND cruisejoin in(43,44,45,49,50,51,52,73,74,76,86,87,
  102,111,112,114,118,120,128,140,180,
  1414142,1414141,1236188,1236155,162174)
  and start_latitude>60.49
  and start_latitude<65.4
  and start_longitude>-176
  and a.stationid NOT IN('V-28','V-27','V-26','V-25','V25','V26','V27',
  'U-28','U-27','U-26','U-25','U-29','U28','U27','U26','U25','U29',
  'T-28','T-27','T-26','T-25','T-29','T-30','T28','T27','T26','T25','T29','T30',
  'S-28','S-27','S-26','S-25','S-29','S-30','S-31','S-28','S-22','S-23','S-24',
  'S27','S26','S25','S29','S30','S31','S22','S23','S24','S28',
  'R-28','R-27','R-26','R-25','R-29','R-30','R-31','R-32','R-22','R-23','R-24',
  'R28','R27','R26','R25','R29','R30','R31','R32','R22','R23','R24')
UNION
SELECT  
	to_number(to_char(a.start_time,'yyyy')) year,
	A.*
FROM 
	RACEBASE.HAUL a
where
  start_latitude>60.49
  and start_latitude<65.4
  and start_longitude>-176
  AND cruisejoin in(43,44,45,49,50,51,52,73,74,76,86,87,
  102,106,111,112,114,118,120,128,140,152,162,180,
  1414142,1414141,1236188,1236155,162174,771364)
  and start_latitude>60.49
  and start_latitude<65.4
  and start_longitude>-176  
  and a.stationid is null
  and hauljoin not in(3411,3412,3413,3414,3415,3437,3438,
  1704,1705,1706,1740,1741,1742,
  5789,5790,5814,5815,5816,5889,5890,
  7700,7701,7702,7703,7640,7645,7648,7649,7650,7699,
  16609,16610,16611,16612,16615,16742,
  12317,13919,13920,13931,13932,13933,13934,13948,13949,13950,
  14356,14357,14358,14359,14360,
  16614,
  17029,17032,17033,17034,17035,17036,17037,17038,17039,17040,17041,17043,17044,
  17045,17279,17287,17288,17289,17290,17301,17302,17303,17304,17305,17314,17315,
  17316,17321,17322,17323,17327,17328,17329,17478,17495,17496,17497,
  17498,17499,17500,17502,17503,17712,17720,17721,17722,17724,17732,17733,17734,
  17735,17742,17782,17783,17784,17785,17789,17790,17791,17792,17793,17801,17802,
  17803,17804,17805,17829,17861,17862,17863,
  18292,18502,18503,18504,18505,18011,18013,18282,18288,18289,18291,18292,18502,
  18010,18014,18015,18283)
UNION
SELECT  
	to_number(to_char(a.start_time,'yyyy')) year,
	A.*
FROM 
	RACEBASE.HAUL a
where
  hauljoin in(1312716,1312715,13233);  
  
REM REMOVE NON-STANDARD SURVEY HAULS
delete from temp where performance<0;
delete from temp where haul_type !=3;
REM DELETE THE FOREIGN VESSELS 
delete from temp where vessel >=700;
REM DELETE YEARS PRIOR TO 1982;
delete from temp where year<1982;

insert into haulname select * from temp;



REM --------------------------------------------------------------------------------------------------------------------------------------
REM NOW ADD IN THE 2018 NBS STATIONS (WHICH ARE NOT STANDARD LOCATIONS, **EG. 30 NM APART**, HAUL_TYPE=13)
drop table temp;
create table temp as select trunC(cruise/100) YEAR,
CRUISEJOIN,HAULJOIN,REGION,VESSEL,CRUISE,HAUL,HAUL_TYPE,PERFORMANCE,START_TIME,DURATION,DISTANCE_FISHED,NET_WIDTH,NET_MEASURED,
NET_HEIGHT,STRATUM,START_LATITUDE,END_LATITUDE,START_LONGITUDE,END_LONGITUDE,STATIONID,GEAR_DEPTH,BOTTOM_DEPTH,BOTTOM_TYPE,SURFACE_TEMPERATURE,
GEAR_TEMPERATURE,WIRE_LENGTH,GEAR,ACCESSORIES,SUBSAMPLE,ABUNDANCE_HAUL,AUDITJOIN
from racebase.haul where cruise=201801 and vessel in (94,162) 
  and haul_type=13 and performance>=0 order by vessel, haul;

insert into haulname select * from temp;
REM --------------------------------------------------------------------------------------------------------------------------------------

REM ADD IN SURVEY NAME
alter table haulname add survey_name varchar2(80);
update haulname h set h.survey_name=(select c.survey_name from racebase.cruise c where h.cruisejoin=c.cruisejoin);




drop table poll_lengths;
create table poll_lengths as
select h.year,l.cruisejoin,l.hauljoin,l.catchjoin,l.region,l.vessel,l.cruise,l.haul,l.species_code,l.length,l.frequency,l.sex
from racebase.length l, haulname h
where l.hauljoin=h.hauljoin and l.species_code=21740;

drop table poll_by_sex;
create table poll_by_sex as
select year, cruisejoin, hauljoin, catchjoin, vessel,cruise,haul,
species_code, sex, length, sum(frequency) totfreq
from poll_lengths
group by year, cruisejoin, hauljoin, catchjoin, vessel,cruise,haul,
species_code, sex, length;

drop table poll_lengths;

drop table poll_length_biomass;
create table poll_length_biomass as
select year, cruisejoin, hauljoin, catchjoin, vessel,cruise,haul,
species_code, sex, length, ((0.00000932* (power(length,3.033))*totfreq)) totwgt, totfreq
from poll_by_sex;

drop table poll_by_sex;

drop table poll_haul_sumall;
create table poll_haul_sumall as
select year, cruisejoin, hauljoin, catchjoin,vessel,cruise,haul,
species_code,  sum(totwgt) haulwgt, sum(totfreq) haultotnum
from poll_length_biomass
group by year, cruisejoin, hauljoin, catchjoin,vessel,cruise,haul,
species_code;

REM THIS GETS THE PROPORTION OF CPUE AT EACH FISH LENGTH FOR EACH YEAR/VESSEL/HAUL/SEX
drop table poll_prop_length_int_all;
create table poll_prop_length_int_all as
select a.year, a.cruisejoin, a.hauljoin, a.catchjoin,a.vessel,a.cruise,a.haul,
a.species_code, a.sex,a.length, (a.totwgt/b.haulwgt) lenint_propwgt, (a.totfreq/b.haultotnum) lenint_propnum
from poll_length_biomass a, poll_haul_sumall b
where a.hauljoin=b.hauljoin ;





/*   THIS VIEW GETS THE APPROPRIATE CATCH RECORDS */
drop table catchview;
drop  view catchview;
create table catchview as
select h.year, c.hauljoin,c.vessel,c.cruise,c.haul,c.species_code,
c.weight,c.number_fish from racebase.catch c, haulname h 
where  c.hauljoin = h.hauljoin and species_code = 21740;


/* THIS SECTION CALCULATES THE CPUE.  THE PART ABOVE THE "UNION" */
/* GETS THE HAULS WHERE THE SPECIES WAS NOT CAUGHT (0'S) AND THE */
/* PART BELOW HANDLES THOSE HAULS WHERE THE SPECIES WAS CAUGHT */
drop table temp1;
create table temp1 as select
year,vessel,stratum,hauljoin,haul, stationid, start_latitude latitude, start_longitude longitude,
DISTANCE_FISHED*NET_WIDTH/10 area_fished from haulname;
drop table temp2;
create table temp2 as select species_code from catchview group by species_code order by species_code;
drop table cpue_zeros;
create table cpue_zeros as select species_code, year, vessel,stratum, hauljoin, haul, stationid, latitude, longitude,
 0 wgtcpue_zero, 0 numcpue_zero, area_fished 
from temp1, temp2 
order by species_code, year, stratum ;

REM NOW CALC THE WGTCPUE & NUMCPUE WHERE THE SPECIES IS PRESENT IN A HAUL
drop table cpue_present;
create table cpue_present as
select species_code,h.year,h.vessel,h.stratum,h.hauljoin, h.haul, h.stationid, h.start_latitude latitude, h.start_longitude longitude,
(weight/((distance_fished*net_width)/10)) wgtcpue_present,
((number_fish)/((distance_fished*net_width)/10)) numcpue_present,
DISTANCE_FISHED*NET_WIDTH/10 area_fished
from catchview c, haulname h
where c.hauljoin=h.hauljoin ;

REM NOW COMBINE THE CPUES ZEROS AND THOSE WHERE THE SPECIES WAS PRESENT
drop table temp_cpue;
drop view temp_cpue;
create table temp_cpue as select
z.species_code, z.year,z.vessel,z.stratum, z.hauljoin, z.haul, z.stationid, z.latitude, z.longitude,
(wgtcpue_zero+wgtcpue_present) cpue_kgha, (numcpue_zero+numcpue_present) cpue_noha, z.area_fished area_fished_ha 
from cpue_zeros z, cpue_present p 
where z.species_code=p.species_code(+) and z.year=p.year(+) and z.stratum=p.stratum(+) and z.hauljoin=p.hauljoin(+);

REM NOW CHANGE NULLS TO ZEROS FOR THE HAULS IN WHICH THE SPECIES WAS NOT PRESENT
REM  ***NEED TO BE CAREFUL HERE BECAUSE THERE MIGHT BE ACTUAL NULLS -E.G. WHERE THERE ARE WGTS BUT NO NUMBERS ***

update temp_cpue set cpue_noha=999999 where cpue_kgha is not null and cpue_noha is null;
commit;
update temp_cpue set cpue_kgha=0 where cpue_kgha is null;
commit;
update temp_cpue set cpue_noha=0 where cpue_noha is null;
commit;
update temp_cpue set cpue_noha=null where cpue_noha=999999;
commit;


drop table poll_cpueall;
create table poll_cpueall as select a.species_code, b.species_name, b.common_name, 
a.year, a.vessel, a.stratum, a.hauljoin, a.haul, stationid, latitude, longitude, 
round(cpue_kgha,4) cpue_kgha, round(cpue_noha,4) cpue_noha, area_fished_ha 
from temp_cpue a, racebase.species b where a.species_code=b.species_code;

drop table poll_haul_sumall;
drop table poll_length_biomass;

REM FINALIZE THE PROPORTIONAL CPUE AT SEX/LENGTH - INCLUDE HAULS/STATIONS WHERE CATCHES WERE ZERO AND 
REM   AND SET THOSE TO SEX=-9 AND LENGTH=-9 (SECOND PART OF UNION)
drop table poll_cpueall_by_sex_cm;
create table poll_cpueall_by_sex_cm as
select a.year,a.vessel, a.stratum, a.haul, a.hauljoin, a.latitude, a.longitude, a.stationid, a.species_code, b.sex,
b.length, (a.cpue_kgha*b.lenint_propwgt) wgtcpue_length, (a.cpue_noha*b.lenint_propnum) numcpue_length
from poll_cpueall a, poll_prop_length_int_all b
where a.hauljoin=b.hauljoin 
UNION
select year,vessel, stratum, haul, hauljoin, latitude, longitude, stationid, species_code, -9 sex,
-9 length, 0 wgtcpue_length, 0 numcpue_length
from poll_cpueall where cpue_kgha=0 and cpue_noha=0;

REM------------------------------------------------------------------------------------------------------------------------------------
REM NOW ADD IN CASES WHERE THE CATCH OF THE SPECIES WAS PRESENT, BUT NO LENGTHS ARE IN THE DATABASE (E.G., LENGTH MEASURING SCREW-UP)
REM  AND ADD IN THE ROWS WITH A LENGTH INDICATOR OF -9
drop table temp;
create table temp as 
select year, vessel, haul, stationid, hauljoin from haulname 
  where (year, vessel, haul, stationid, hauljoin) in
    (select year, vessel, haul, stationid, hauljoin from haulname)
      minus
     (select year, vessel, haul, stationid, hauljoin from poll_cpueall_by_sex_cm);

insert into poll_cpueall_by_sex_cm 
 select p.year, p.vessel, p.stratum, p.haul, p.hauljoin, p.latitude, p.longitude, p.stationid, p.species_code,
-9 sex,-9 length, p.cpue_kgha wgtcpue_length, p.cpue_noha numcpue_length 
  from poll_cpueall p, temp t
where p.hauljoin=t.hauljoin;

REM------------------------------------------------------------------------------------------------------------------------------------
REM  RUN SOME CHECKS TO SEE IF DATA ADDS UP CORRECTLY

REM CHECK THE SUM OF CPUE WT VALUES AGAINST NORMAL CPUE RUNS 
select year, vessel, haul, stationid, 
sum(wgtcpue_length),
sum(numcpue_length)
 from 
poll_cpueall_by_sex_cm where year=1982 group by year, vessel, haul, stationid ORDER BY year, vessel, haul;

REM CHECK THE NUMBER FOR DISTINCT STATIONS PER YEAR TO SEE IF IT LOOKS OK
select year, count(distinct hauljoin) from haulname group by year order by year;
select year, count(distinct hauljoin) from poll_cpueall_by_sex_cm group by year order by year;

REM CHECK WHERE SPECIES WAS CAPTURED IN HAULS, BUT NO NUMBER_FISH RECORDED
select year, count(*) 
from poll_cpueall where cpue_kgha >0 and cpue_noha is null group by year order by year;
REM-----------------------------------------------------------------------------------------------------------------------------------


NOW SELECT OUTPUT FOR EXPORT TO A FLAT FILE
select 
  year, 
  vessel, 
  stratum, 
  haul, 
  hauljoin, 
  latitude, 
  longitude, 
  stationid, 
  species_code, 
  sex, 
  length, 
  round(wgtcpue_length,6) wgtcpue_length, 
  round(numcpue_length, 6) numcpue_length 
from poll_cpueall_by_sex_cm 
 order by year, vessel, haul, sex, length;



