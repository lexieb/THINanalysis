#install and load packages


library(xgboost)
library(DataLakeR)
library(vtreat)
library(tidyverse)


#Get data back from the lake
#health <- get_datalake("SELECT * FROM THIN_Analysis.dbo.AleksandraBlawat_patients_sample5")
#patients_sample5 has 7.8m observations 
health <- get_datalake("SELECT * FROM THIN_Analysis.dbo.AleksandraBlawat_patients_full1")
#32m observations of 1.8m people

#format evdatereal (check this one), regdate, deathdate, startdate and xferdate
health$evdatereal1 <- as.Date(strptime(health$evdatereal,format='%Y-%m-%d', tz = "GMT"))
health$deathdte1 <- as.Date(strptime(health$deathdte,format='%d/%m/%Y', tz = "GMT"))
health$xferdate1 <- as.Date(strptime(health$xferdate,format='%d/%m/%Y', tz = "GMT"))
health$regdate1 <- as.Date(strptime(health$regdate,format='%d/%m/%Y', tz = "GMT"))
health$startdate1 <- as.Date(strptime(health$startdate,format='%d/%m/%Y', tz = "GMT"))
health$dob1 <- as.Date(strptime(health$dob,format='%Y-%m-%d', tz = "GMT"))


#calculate leaving date 
health$leaving_date <- with(health, pmin(xferdate1, deathdte1))
#default leaving date for those still in dataset
health$leaving_date[is.na(health$leaving_date)] <- "2018-09-30"


#calculate difference between start and end time in dataset 
health$time_in_dataset <- difftime(health$leaving_date, health$startdate1, "GMT", units = "days")


#drop patients with less than 10 years spent in dataset - note: check units worked properly
health <- subset(health, time_in_dataset >= 3650)



#display medical codes as diseases

diabetes_codes1 <- c('001434' , '14F4.00' , '14O8.00' , '14O8000' , '1I0..00' , '1IA..00' , '1JL..00' , '1M8..00' , '2126300' , '212H.00' , '2BBF.00' , '2BBG.00' , '2BBJ.00' , '2BBK.00' , '2BBL.00' , '2BBM.00', '2BBP.00' , '2BBQ.00' , '2BBR.00' , '2BBS.00' , '2BBT.00' , '2BBV.00' , '2BBW.00' , '2BBX.00' , '2BBk.00', '2BBl.00' , '2BBo.00' , '2BBr.00' , '2G51000' , '2G51100' , '2G5A.00' , '2G5B.00' , '2G5C.00' , '2G5D.00' , '2G5E.00' , '2G5F.00' , '2G5G.00' , '2G5H.00' , '2G5I.00' , '2G5J.00' , '2G5K.00' , '2G5L.00' , '2G5V.00' , '2G5W.00' , '2G5d.00' , '2G5e.00' , '3881' , '3882' , '3883' , '38DK.00' , '38DM.00' , '38Gj.00' , '38Gv.00' , '42W..00' , '42WZ.00' , '42c..00' , '44V3.00' , '46Z0.00' , '661M400' , '661N400' , '66A..00' , '66A1.00' , '66A2.00' , '66A3.00' , '66A4.00' , '66A5.00' , '66A8.00' , '66A9.00' , '66AA.11' , '66AD.00' , '66AG.00' , '66AH.00' , '66AH200' , '66AI.00' , '66AJ.00' , '66AJ.11' , '66AJ100' , '66AJz00' , '66AK.00' , '66AL.00' , '66AM.00' , '66AN.00' , '66AO.00' , '66AP.00' , '66AQ.00' , '66AQ000' , '66AQ100' , '66AR.00' , '66AS.00' , '66AS000' , '66AT.00' , '66AU.00' , '66AV.00' , '66AW.00' , '66AX.00' , '66AY.00' , '66AZ.00' , '66Aa.00' , '66Ab.00' , '66Ac.00' , '66Af.00' , '66Ai.00' , '66Ak.00' , '66Al.00' , '66An.00' , '66Ao.00' , '66Aq.00' , '66As.00' , '66At.00' , '66At000' , '66At011' , '66At100' , '66At111' , '66Au.00' , '66Av.00' , '66Ay.00' , '66Az.00' , '66b1.00' , '66o..00' , '66o1.00' , '66o2.00' , '66o5.00' , '66o6.00' , '6761' , '679L.00' , '679L000' , '679L200' , '679L211' , '679L300' , '679R.00' , '679l.00' , '67D8.00' , '67H9.00' , '67HA.00' , '67IJ100' , '67W1.00' , '6872' , '68A7.00' , '68A9.00' , '68AB.00' , '7276' , '8A12.00' , '8A13.00')
diabetes_codes4 <- c('8B3l.00' , '8BL2.00' , '8CA4100' , '8CE0.00' , '8CE0000' , '8CMW700' , '8CP2.00' , '8CR2.00' , '8CS0.00' , '8H2J.00' , '8H3O.00' , '8H4F.00' , '8H4e.00' , '8H7C.00' , '8H7f.00' , '8H7r.00' , '8HBG.00' , '8HBH.00' , '8HHy.00' , '8HKE.00' , '8HLE.00' , '8HME.00' , '8HTE100' , '8HTe.00' , '8HTi.00' , '8HTk.00' , '8HVU.00' , '8Hg4.00')
diabetes_codes6 <- c('8HgC.00' , '8Hgd.00' , '8Hj0.00' , '8Hj1.00' , '8Hj3.00' , '8Hj4.00' , '8Hj5.00' , '8Hl1.00' , '8Hl4.00' , '8Hlc.00' , '8I3W.00' , '8I3X.00' , '8I57.00' , '8I6F.00' , '8I6G.00' , '8I81.00' , '8I82.00' , '8I83.00' , '8I84.00' , '8I94.00' , '8IAs.00' , '8IE2.00' , '8IEQ.00' , '8IEa.00' , '8OA3.00' , '8OAH.00' , '8OAK.00' , '918T.00' , '9360' , '93C4.00' , '9M00.00' , '9M10.00' , '9N0m.00' , '9N0n.00' , '9N0o.00' , '9N1Q.00' , '9N1i.00' , '9N1o.00' , '9N1v.00' , '9N2d.00' , '9N2i.00' , '9N4I.00' , '9N4p.00' , '9NJy.00')
diabetes_codes5 <- c('9NM0.00' , '9NN8.00' , '9NN9.00' , '9NND.00' , '9Na5100' , '9NiA.00' , '9NiC.00' , '9NiD.00' , '9NiE.00' , '9NiZ.00' , '9Nl4.00' , '9OL..00' , '9OL..11' , '9OL1.00' , '9OL2.00' , '9OL3.00' , '9OL4.00' , '9OL5.00' , '9OL6.00' , '9OL7.00' , '9OL8.00' , '9OL9.00' , '9OLA.00' , '9OLA.11' , '9OLB.00' , '9OLC.00' , '9OLD.00' , '9OLF.00' , '9OLG.00' , '9OLH.00' , '9OLJ.00' , '9OLK.00' , '9OLL.00' , '9OLM.00' , '9OLN.00' , '9OLZ.00' , '9Oy..00' , '9Oy0.00' , '9Oy0000' , '9Oy0200' , '9Oy0300' , '9Oy0400' , '9b92000' , '9h4..00' , '9h41.00' , '9h42.00' , '9h43.00' , '9m0..00' , '9m00.00' , '9m01.00' , '9m02.00' , '9m03.00' , '9m04.00' , '9m05.00' , '9m06.00' , '9m07.00' , '9m08.00' , '9m09.00' , '9m0A.00' , '9m0C.00' , '9m0D.00' , '9m0E.00' , 'C10..00' , 'C100.00' , 'C100000' , 'C100011' , 'C100100' , 'C100111' , 'C100112' , 'C100z00' , 'C101.00' , 'C101000' , 'C101100' , 'C101y00' , 'C101z00' , 'C102.00' , 'C102000' , 'C102100' , 'C102z00' , 'C103.00' , 'C103000' , 'C103100' , 'C103y00' , 'C103z00' , 'C104.00' , 'C104.11' , 'C104000' , 'C104100' , 'C104y00' , 'C104z00' , 'C105.00' , 'C105000' , 'C105100' , 'C105y00' , 'C105z00' , 'C106.00' , 'C106.11' , 'C106.12' , 'C106.13' , 'C106000' , 'C106100')
diabetes_codes3 <- c('C106y00' , 'C106z00' , 'C107.00' , 'C107.11' , 'C107.12' , 'C107000' , 'C107100' , 'C107200' , 'C107z00' , 'C108.00' , 'C108.11' , 'C108.12' , 'C108.13' , 'C108000' , 'C108011' , 'C108012' , 'C108100' , 'C108112' , 'C108200' , 'C108211' , 'C108212' , 'C108300' , 'C108311' , 'C108400' , 'C108411' , 'C108412' , 'C108500' , 'C108511' , 'C108512' , 'C108600' , 'C108700' , 'C108711' , 'C108712' , 'C108800' , 'C108811' , 'C108812' , 'C108900' , 'C108911' , 'C108912' , 'C108A00' , 'C108A11' , 'C108A12' , 'C108B00' , 'C108B11' , 'C108C00' , 'C108D00' , 'C108D11' , 'C108E00' , 'C108E11' , 'C108E12' , 'C108F00' , 'C108F11' , 'C108F12' , 'C108H00' , 'C108H11' , 'C108J11' , 'C108J12' , 'C108y00' , 'C108z00' , 'C109.00' , 'C109.11' , 'C109.12' , 'C109.13' , 'C109000' , 'C109011' , 'C109012' , 'C109100' , 'C109111' , 'C109112' , 'C109200' , 'C109211' , 'C109212' , 'C109300' , 'C109312' , 'C109400' , 'C109411' , 'C109412' , 'C109500' , 'C109511' , 'C109512' , 'C109600' , 'C109611' , 'C109612' , 'C109700' , 'C109711' , 'C109712' , 'C109900' , 'C109911' , 'C109912' , 'C109A00' , 'C109A11' , 'C109B00' , 'C109B11' , 'C109B12' , 'C109C00' , 'C109C11' , 'C109C12' , 'C109D00' , 'C109D11' , 'C109D12' , 'C109E00' , 'C109E11' , 'C109E12' , 'C109F11' , 'C109F12' , 'C109G00' , 'C109G11' , 'C109G12' , 'C109H11' , 'C109H12' , 'C109J00' , 'C109J11' , 'C109J12' , 'C109K00' , 'C10A.00' , 'C10A000' , 'C10A100' , 'C10A500' , 'C10B.00' , 'C10B000' , 'C10C.00' , 'C10C.11' , 'C10C.12' , 'C10D.00' , 'C10D.11' , 'C10E.00' , 'C10E.11' , 'C10E.12' , 'C10E000' , 'C10E011' , 'C10E012' , 'C10E100' , 'C10E111' , 'C10E112' , 'C10E200' , 'C10E212' , 'C10E300' , 'C10E311' , 'C10E312' , 'C10E400' , 'C10E411' , 'C10E412' , 'C10E500' , 'C10E511' , 'C10E512' , 'C10E600' , 'C10E611' , 'C10E612' , 'C10E700' , 'C10E711' , 'C10E712' , 'C10E800' , 'C10E811' , 'C10E812' , 'C10E900' , 'C10E911' , 'C10E912' , 'C10EA00' , 'C10EA11' , 'C10EA12' , 'C10EB00' , 'C10EC00' , 'C10EC11' , 'C10EC12' , 'C10ED00')
diabetes_codes2 <- c('C10ED12' , 'C10EE00' , 'C10EE12' , 'C10EF00' , 'C10EF12' , 'C10EG00' , 'C10EH00' , 'C10EJ00' , 'C10EK00' , 'C10EL00' , 'C10EL11' , 'C10EM00' , 'C10EM11' , 'C10EN00' , 'C10EN11' , 'C10EP00' , 'C10EP11' , 'C10EQ00' , 'C10EQ11' , 'C10ER00' , 'C10F.00' , 'C10F.11' , 'C10F000' , 'C10F011' , 'C10F100' , 'C10F111' , 'C10F200' , 'C10F211' , 'C10F300' , 'C10F311' , 'C10F400' , 'C10F411' , 'C10F500' , 'C10F511' , 'C10F600' , 'C10F611' , 'C10F700' , 'C10F711' , 'C10F900' , 'C10F911' , 'C10FA00' , 'C10FA11' , 'C10FB00' , 'C10FB11' , 'C10FC00' , 'C10FC11' , 'C10FD00' , 'C10FD11' , 'C10FE00' , 'C10FE11' , 'C10FF00' , 'C10FF11' , 'C10FG00' , 'C10FG11' , 'C10FH00' , 'C10FH11' , 'C10FJ00' , 'C10FJ11' , 'C10FK00' , 'C10FK11' , 'C10FL00' , 'C10FL11' , 'C10FM00' , 'C10FM11' , 'C10FN00' , 'C10FN11' , 'C10FP00' , 'C10FP11' , 'C10FQ00' , 'C10FR00' , 'C10FS00' , 'C10G.00' , 'C10G000' , 'C10H.00' , 'C10M.00' , 'C10N.00' , 'C10N000' , 'C10N100' , 'C10P.00' , 'C10P000' , 'C10P011' , 'C10P100' , 'C10P111' , 'C10y.00' , 'C10y000' , 'C10y100' , 'C10yy00' , 'C10yz00' , 'C10z.00' , 'C10z000' , 'C10z100' , 'C10zy00' , 'C10zz00' , 'C11y000' , 'C11y500' , 'C135.00' , 'C135.12' , 'C135000' , 'C314.11' , 'C350011' , 'Cyu2.00' , 'Cyu2000' , 'Cyu2300' , 'F171100' , 'F345000' , 'F35z000' , 'F372.00' , 'F372.11' , 'F372.12' , 'F372000' , 'F372100' , 'F372200' , 'F381300' , 'F381311' , 'F3y0.00' , 'F420.00' , 'F420000' , 'F420100' , 'F420200' , 'F420300' , 'F420400' , 'F420500' , 'F420600' , 'F420700' , 'F420800' , 'F420z00' , 'F440700' , 'F464000' , 'G73y000' , 'K01x100' , 'K081.00' , 'K081000' , 'K08yA00' , 'K08yA11' , 'K27y700' , 'Kyu0300' , 'L180.00' , 'L180000' , 'L180100' , 'L180300' , 'L180400' , 'L180500' , 'L180600' , 'L180700' , 'L180800' , 'L180811' , 'L180900' , 'L180X00' , 'L180z00' , 'M037200' , 'M271000' , 'M271100' , 'M271200' , 'N030000' , 'N030011' , 'N030100' , 'Q440.00' , 'Q441.00' , 'Q44B.00' , 'Q44y100' , 'R054200' , 'R054300' , 'R102.11' , 'SL23.00' , 'TJ23.00' , 'TJ23z00' , 'U602311' , 'U60231E' , 'ZC2C800' , 'ZC2C900' , 'ZC2C911' , 'ZC2CA00' , 'ZC2CB00' , 'ZL22500' , 'ZL62500' , 'ZL62600' , 'ZLA2500' , 'ZLD7500' , 'ZRB4.00' , 'ZRB4.11' , 'ZRB5.00' , 'ZRB5.11' , 'ZRB6.00' , 'ZRB6.11' , 'ZRBa.00' , 'ZRbH.00' , 'ZV13F00' , 'ZV18000' , 'ZV18011' , 'ZV19800' , 'ZV65312' , 'ZV77100' , 'ZV77111')
liver_disease_codes <- c('R024.00' , 'J633.00' , 'A701.11' , 'J63..00' , 'J614.00' , 'J614200' , 'R024111' , 'J66y600' , 'J611.00' , 'J63yz00' , 'J633z00' , 'R024000' , '2274.11' , '1675.11' , 'J600000' , '25G4.00' , 'J617.00' , '7804200' , 'J661.00' , '780B000' , 'J63z.00' , 'J661700' , 'J614z00' , 'J633000' , 'J661200' , 'J600100' , '25G3.00' , 'J62y.13' , '780A112' , 'J635300' , 'J613000' , 'R024100' , 'A705400' , '12E3.11' , 'J601100' , 'J622.11' , 'J635000' , 'J622.00' , 'J614000' , 'J646.00' , 'J625.00' , 'J620100' , '1675' , 'J63y.00' , 'J601000' , 'J63y100' , '2274' , 'J661y00' , 'J61yz00' , 'J661400' , '780A000' , 'J635100' , 'SP14200' , 'J635.00' , 'R024z00' , 'J635500' , 'J635200' , 'J635700' , 'J600.00' , 'J635X00' , 'J61y.00' , 'J635600' , 'J661900' , 'J62y.00' , 'J60..00' , 'J661100' , 'J614300' , 'J600200' , 'J614y00' , 'J600z00' , 'J62y.11' , 'J601.00' , 'J661z00' , 'J636.00' , 'J635400' , '780A111' , 'J60z.00' , 'AD05.00' , 'J661500' , 'J614400' , 'J601200' , 'J601z00' , '780F000' , 'J661600' , '780A113') 
dementia_codes <- c('E000.00' , 'E001000' , 'E004000' , 'E00..12' , 'E002000' , 'E002z00' , 'E002.00' , 'E002100' , 'E003.00' , 'E00..11' , 'E001200' , 'E001300' , 'E001100' , 'E001z00' , 'E001.00' , 'E004.11' , 'ZS7C500' , '1461' , '9hD..00' , '9hD0.00' , '9hD1.00' , 'E02y100' , '9Ou4.00' , '9Ou3.00' , '9Ou5.00' , '9Ou2.00' , '9Ou1.00' , '9Ou..00' , '66h..00' , 'E041.00' , '6AB..00' , 'E004200' , 'E004300' , 'E004100' , 'E004z00' , 'E004.00' , 'ZR1T.00' , 'Eu01z00' , 'Eu01000' , 'Eu01.00' , 'Eu01200' , 'Eu00112' , 'Eu00012' , 'Eu00113' , 'Eu00011' , 'Eu01111' , 'Eu01y00' , 'Eu01100' , 'Eu02500' , 'Eu02y00' , 'Eu02.00' , 'Eu00z00' , 'Eu00100' , 'Eu00000' , 'Eu00.00' , 'Eu00200' , 'Eu04100' , 'Eu01.11' , 'Eu00z11' , 'Eu02z00' , 'Eu02z16' , 'Eu02z14' , 'Eu02z13' , 'Eu02z11')
hypertension_codes <- c('14A2.00' , '2126100' , '212K.00' , '9OI9.00' , '1JD..00' , '246M.00' , '662..12' , '6629' , '662H.00' , '662P.00' , '8CR4.00' , '9N03.00' , '9N1y200' , '9N4L.00' , '9OI..00' , '9OI..11' , '9OI1.00' , '9OI2.00' , '9OI3.00' , '9OI4.00' , '9OI5.00' , '9OI6.00' , '9OI7.00' , '9OI8.00' , '9OIA.00' , '9OIA.11' , '9OIZ.00' , '9h3..00' , '9h31.00' , '9h32.00' , '6624' , '6627' , '6628' , '662F.00' , '662G.00' , '662O.00' , '662b.00' , '662c.00' , '662d.00' , '662r.00' , '7Q01.00' , '8B26.00' , '8BL0.00' , '8I3N.00' , 'F404200' , 'F421300' , 'G2...00' , 'G2...11' , 'G20..00' , 'G200.00' , 'G201.00' , 'G202.00' , 'G203.00' , 'G20z.00' , 'G20z.11' , 'G21..00' , 'G210.00' , 'G210000' , 'G210100' , 'G211.00' , 'G211000' , 'G211100' , 'G21z.00' , 'G21z000' , 'G21z011' , 'G21z100' , 'G21zz00' , 'G22..00' , 'G220.00' , 'G221.00' , 'G222.00' , 'G22z.00' , 'G22z.11' , 'G23..00' , 'G230.00' , 'G231.00' , 'G232.00' , 'G233.00' , 'G234.00' , 'G23z.00' , 'G2y..00' , 'G2z..00' , 'G672.00' , 'G672.11' , 'Gyu2.00' , 'L122.00' , 'L122000' , 'L122100' , 'L122300' , 'L122z00' , 'L127.00' , 'L127z00' , 'L128.00' , 'L128000' , 'L128200' , 'TJC7.00' , 'TJC7z00' , 'U60C500' , 'U60C511' , 'U60C51A' , '6146200' , 'G24..00' , 'G240.00' , 'G240000' , 'G240z00' , 'G241.00' , 'G241000' , 'G241z00' , 'G244.00' , 'G24z.00' , 'G24z000' , 'G24z100' , 'G24zz00' , 'Gyu2100') 
strokes_codes <- c('Fyu5600' , 'G60..00' , 'G600.00' , 'G601.00' , 'G602.00' , 'G603.00' , 'G604.00' , 'G605.00' , 'G606.00' , 'G60X.00' , 'G60z.00' , 'G61..00' , 'G61..11' , 'G61..12' , 'G610.00' , 'G611.00' , 'G612.00' , 'G613.00' , 'G614.00' , 'G615.00' , 'G616.00' , 'G617.00' , 'G618.00' , 'G619.00' , 'G61X.00' , 'G61X000' , 'G61X100' , 'G61z.00' , 'G62..00' , 'G620.00' , 'G621.00' , 'G622.00' , 'G623.00' , 'G62z.00' , 'G63..00' , 'G63..11' , 'G630.00' , 'G631.00' , 'G631.12' , 'G632.00' , 'G633.00' , 'G63y.00' , 'G63y000' , 'G63y100' , 'G63z.00' , 'G64..00' , 'G64..11' , 'G64..12' , 'G64..13' , 'G640.00' , 'G640000' , 'G641.00' , 'G641.11' , 'G641000' , 'G64z.00' , 'G64z.11' , 'G64z.12' , 'G64z000' , 'G64z200' , 'G64z300' , 'G64z400' , 'G66..00' , 'G66..11' , 'G66..12' , 'G66..13' , 'G660.00' , 'G661.00' , 'G662.00' , 'G663.00' , 'G664.00' , 'G665.00' , 'G666.00' , 'G667.00' , 'G668.00' , 'G671.00' , 'G671000' , 'G671z00' , 'G676000' , 'G677000' , 'G677100' , 'G677200' , 'G677300' , 'G677400' , 'G679.00' , 'G67y.00' , 'G67z.00' , 'G6W..00' , 'G6X..00' , 'G6y..00' , 'G6z..00' , 'Gyu6.00' , 'Gyu6000' , 'Gyu6100' , 'Gyu6200' , 'Gyu6300' , 'Gyu6400' , 'Gyu6500' , 'Gyu6600' , 'Gyu6E00' , 'Gyu6F00' , 'Gyu6G00' , 'L440.11' , 'L440.12') 
COPD_codes <- c('h32..00' , 'h3...11' , 'h3...00' , 'h3z..00' , '66yb.00' , 'h38..00' , 'h37..00' , 'h36..00' , 'h322.00' , '66ym.00' , 'h3y..00' , 'h312100' , 'h32yz00' , '66yl.11' , '66yl.00' , '9oi..00' , 'h320z00' , '66ys.00' , 'h320.00' , '9oi0.00' , 'h32z.00' , '9oi1.00' , '9oi2.00' , 'h3z..11' , '66yd.00' , '9oi4.00' , '9oi3.00' , '679v.00' , '66yg.00' , '66yh.00' , '8cr1.00' , '66yt.00' , 'h321.00' , 'h320200' , 'hyu3100' , 'h3y..11' , 'h39..00' , 'h3a..00' , '9ngp.11' , '9ngp.00' , '8cmw500' , '9nk7000') 


#label each visit with the condition to which it relates - note none related to 'any other condition' rather than none
health$condition <- case_when(health$medcode %in% diabetes_codes1 ~ "diabetes",
                              health$medcode %in% diabetes_codes2 ~ "diabetes",
                              health$medcode %in% diabetes_codes3 ~ "diabetes",
                              health$medcode %in% diabetes_codes4 ~ "diabetes",
                              health$medcode %in% diabetes_codes5 ~ "diabetes",
                              health$medcode %in% diabetes_codes6 ~ "diabetes",
                              health$medcode %in% liver_disease_codes ~ "liverdisease",
                              health$medcode %in% dementia_codes ~ "dementia",
                              health$medcode %in% hypertension_codes ~ "hypertension",
                              health$medcode %in% strokes_codes ~ "stroke",
                              health$medcode %in% COPD_codes ~ "COPD",
                              TRUE ~ "none")

health$condition[health$condition == "none" ] <- NA

health$patid <- as.character(health$patid)

#create unique patient identifier by combining patient id with practice id
health$patid.full <- paste(health$patid, health$prac, sep="_")

#create a variable that flags if a patient develops each disease over the course of their time in the dataset
#create summary statistics by counting the number of patients with each condition (note there may be MMs)
health <- health %>%
  group_by(patid.full) %>%
  mutate(has_diabetes = max(condition  == "diabetes")) %>%
  mutate(has_liverdisease = max((condition == "liverdisease"))) %>%
  mutate(has_dementia = max((condition == "dementia"))) %>%
  mutate(has_stroke = max((condition == "stroke"))) %>%
  mutate(has_hypertension = max((condition == "hypertension"))) %>%
  mutate(has_COPD = max((condition == "COPD")))


#count the number of patients with each condition
health %>%
  group_by(has_dementia) %>%
  summarize(count = n_distinct(patid.full))

health %>%
  ungroup() %>%
  filter(has_dementia == 1 & has_diabetes == 1) %>%
  summarize(count = n_distinct(patid.full))

# tidyr to pivot data - transform condition columns into column of categories and then yes/no


# earliest consultation per patient per disease 
health <- health %>% 
  group_by(condition, patid.full) %>%
  mutate(earliest_consultation = min(evdatereal1)) 

#time_since_last_cons
health <- health %>% 
  group_by(patid.full, condition) %>%
  mutate(time_since_first_cons_days = as.numeric(evdatereal1 - earliest_consultation))

#create variable consultation_year - works
health$cons_year <- health$evdatereal1 %>%
  format('%Y')%>%
  as.numeric 

#create variable death_year
health$death_year <- health$deathdte1 %>% 
  format('%Y') %>% 
  as.numeric 

#whether patient died that year
health$died_year <- case_when(health$death_year == health$cons_year ~ 1, TRUE ~ 0)

#check whether this works
health <- health %>%
  group_by(patid.full) %>%
  arrange(evdatereal1) %>%
  mutate(cum_unique_conditions = dense_rank(desc(condition)))


#Duration recorded as hhmmss (hours minutes seconds) - change to code as minutes - doesn't work
health$duration_new <-formatC(health$duration, width = 6, format = "d", flag = "0")
health$duration_new_recoded <- as.POSIXct(health$duration_new, format="%H%M%S")
health$timebaseline <- as.POSIXct("000000", format = "%H%M%S")
health$duration_inseconds <- as.numeric(health$duration_new_recoded - health$timebaseline)


#health$duration_recoded <- case_when(health$duration_recoded < 0.5 ~ 0, health$duration_recoded <= 1 ~ 1, TRUE ~ health$duration_recoded) 


#doesn't seem to be working properly for calculating annual cost! 
#annual cost per patient year disease. £219 is hourly cost of GP patient time, including qualification cost, excluding direct care staff costs, based on https://www.pssru.ac.uk/pub/uc/uc2018/community-based-health-care-staff.pdf, 2016/17 price year
#health <- health %>% 
# group_by(patid.full, cons_year)  %>%
#mutate(annual_cost = (sum(duration_inseconds) * 219.0/3600))

health <- health %>% 
  group_by(patid.full, cons_year) %>%
  arrange(evdatereal1) %>%
  mutate(cumulative_annual_cost = cumsum(duration_inseconds) * 219.0/3600)

health$duration_inseconds[health$duration_inseconds > 3600] <- 3600
max(health$duration_inseconds, na.rm = TRUE)

#create time since diagnosis variable
health <- health %>% 
  group_by(patid.full, condition) %>% 
  mutate(time_since_diagnosis = as.numeric(evdatereal1 - earliest_consultation))

health <- health %>%
  group_by(cons_year) %>%
  mutate(day_year_end = paste("31/12", cons_year, sep = "/", collapse = NULL))

health$day_year_end <- as.Date(strptime(health$day_year_end,format='%d/%m/%Y', tz = "GMT"))

health <- health %>%
  group_by(cons_year) %>%
  mutate(time_to_year_end = as.numeric(day_year_end - evdatereal1))

health$bmi <- as.numeric(health$bmi)
health$bmi[health$bmi >= 70] <- NA

#plot distributions + QA
ggplot(health, aes(duration_inseconds)) + geom_histogram() + xlim(c(0,3000))
ggplot(health, aes(bmi)) + geom_density()

ggplot(health, aes(age)) + geom_histogram() 

health %>%
  ungroup() %>%
  filter(cumulative_annual_cost > 1000) %>%
  summarize(count = n_distinct(patid.full))



#check for negative
health <- health %>%
  group_by(patid.full) %>% 
  mutate(age = as.numeric(evdatereal1 - dob1))
health$age[health$age < 0] <- NA

#unique number for each unique practid
health$pracid1 <- transform(health$pracid,id=as.numeric(factor(health$pracid)))
health$pracid2 <- health$pracid1$id


#end of creating relevant variables
#summary stats


#xgboost doesn't accept categorical variables. Need to use vtreat. Need cleaned data that is all numerical with no missing values.
vars <- c("condition", 
          "pracid2", 
          "cumulative_annual_cost", 
          "white", 
          "town", 
          "mix", 
          "asian", 
          "black", 
          "other", 
          "age", 
          "smoking", 
          "bmi", 
          "alcohol", 
          "sex", 
          "evdatereal1", 
          "time_since_first_cons_days", 
          "died_year", 
          "cum_unique_conditions", 
          "time_to_year_end")
treatplan <- designTreatmentsZ(health, vars)

scoreFrame <- treatplan %>% 
  magrittr::use_series(scoreFrame) %>% 
  select(varName, origName, code)

newvars <- scoreFrame %>%
  filter(code %in% c("clean", "lev")) %>%
  magrittr::use_series(varName)

health_treated <- prepare(treatplan, health, varRestriction = newvars)

#health_treated is now the final dataset - it has more variables
#could do cross-validation with sample of data, then test the best performing xgboost model 
health$cumulative_annual_cost[is.na(health$cumulative_annual_cost)] <- 0.00     


subsample <- seq(from=0.6, to=1, by=0.2)
eta <- c(0.1, 0.05, 0.01)
depth <- seq(from=6, to=10, by=2)
nrounds <- 1000
# use expand.grid to create a dataframe of possible parameter combinations
param_grid <- expand.grid(subsample = subsample, 
                          colsample = subsample,                        
                          depth = depth,                          
                          eta = eta)
# shuffle the parameter dataframe for random order
param_grid <- param_grid[sample(nrow(param_grid)),]
# initiate a results list
results = list()
# make a loop for training/testing your model
#use function stratified to use a selection of patients to train the model 
# splitstackshape or sample_n from dplyr

vartypes <- sapply(health_treated, typeof)
upload_to_datalake(df, "THIN_Analysis", "LB_DataLakeR_heath_treated", vartypes, append = FALSE)

health_treated_sample <- health_treated %>% 
  group_by(patid.full, cons_year) %>% 
  sample_frac(0.001) %>%
  ungroup()

for(i in 1:nrow(param_grid)){
  # create a parameter list to feed into xgb.cv (expects list class)  
  params <- list(booster = "gbtree",
                 objective = "reg:linear",
                 eta = param_grid[i,"eta"], 
                 max_depth = param_grid[i,"depth"],   
                 subsample = param_grid[i,"subsample"],
                 colsample_bytree = param_grid[i,"colsample"]) 
  xgbcv <- xgb.cv(params = params, 
                  label = health_treated_sample$cumulative_annual_cost,
                  data= as.matrix(health_treated_sample %>% select(-cumulative_annual_cost)), 
                  nrounds = nrounds, 
                  depth = depth,
                  nfold = 5,
                  early_stopping_rounds = 10) 
  
  logs <- as.data.frame(xgbcv$evaluation_log)
  
  
  results[[i]] = data.frame(params,                           
                            rounds = nrow(logs),                            
                            test_mse = logs[nrow(logs), "test_rmse_mean"],
                            train_mse = logs[nrow(logs), "train_rmse_mean"]) 
  gc(verbose = FALSE, full = TRUE)
  }
results_df <- dplyr::bind_rows(results)

#grid search eta, nrounds, depth - function called expand grid
#early stopping - if rounds haven't improved performance after x rounds
#verbose = 1
#sample and column sample - secondary thing to start training on
#parameters for tree boosting eg. learing rate
#maxdepth - 4 to 12 by 2
#subsample and col sample by tree
#grid search, shuffle, build into a loop, pass row in gridsearch into the loop, 

upload_to_datalake(results_df, "THIN_Analysis", "LB_DataLakeR_xgboostresults_1", vartypes, append = FALSE)

view(results_df)

results_df <- results_df %>%
  mutate(diff = test_mse - train_mse)

model <- xgboost(data = as.matrix(health_treated %>% select(-cumulative_annual_cost)), label = health_treated$cumulative_annual_cost, objective = "reg:linear", nrounds = 1000, eta = 0.01, depth = 8, subsample = 0.8, colsample_bytree = 1.0) 
#plot importances
#test with cross-validation - test on 20% using early stopping rounds and optimal parameters

health$pred <- predict(model, as.matrix(health_treated)) 

importances <- xgb.importance(feature_names = NULL, model = model, trees = NULL)
install.packages("Ckmeans.1d.dp")
library("Ckmeans.1d.dp")

xgb.ggplot.importance(importance_matrix = importances, top_n = 20, measure = 'Gain')
# do summary statistics
# have we only picked up consultations for these diseases or all consultations for patients with these diseases? 
view(health$cumulative_annual_cost)

