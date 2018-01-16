#include "unboxery.h"

module RU(rU,rUs,rU') where
import Types

rU
  = Nuc
      (Tfo FL_LIT(-0.0359)  FL_LIT(-0.8071)   FL_LIT(0.5894)  -- dgf_base_tfo
           FL_LIT(-0.2669)   FL_LIT(0.5761)   FL_LIT(0.7726)
           FL_LIT(-0.9631)  FL_LIT(-0.1296)  FL_LIT(-0.2361)
            FL_LIT(0.1584)   FL_LIT(8.3434)   FL_LIT(0.5434))
      (Tfo FL_LIT(-0.8313)  FL_LIT(-0.4738)  FL_LIT(-0.2906)  -- p_o3'_275_tfo
            FL_LIT(0.0649)   FL_LIT(0.4366)  FL_LIT(-0.8973)
            FL_LIT(0.5521)  FL_LIT(-0.7648)  FL_LIT(-0.3322)
            FL_LIT(1.6833)   FL_LIT(6.8060)  FL_LIT(-7.0011))
      (Tfo  FL_LIT(0.3445)  FL_LIT(-0.7630)   FL_LIT(0.5470)  -- p_o3'_180_tfo
           FL_LIT(-0.4628)  FL_LIT(-0.6450)  FL_LIT(-0.6082)
            FL_LIT(0.8168)  FL_LIT(-0.0436)  FL_LIT(-0.5753)
           FL_LIT(-6.8179)  FL_LIT(-3.9778)  FL_LIT(-5.9887))
      (Tfo  FL_LIT(0.5855)   FL_LIT(0.7931)  FL_LIT(-0.1682)  -- p_o3'_60_tfo
            FL_LIT(0.8103)  FL_LIT(-0.5790)   FL_LIT(0.0906)
           FL_LIT(-0.0255)  FL_LIT(-0.1894)  FL_LIT(-0.9816)
            FL_LIT(6.1203)  FL_LIT(-7.1051)   FL_LIT(3.1984))
      (Pt  FL_LIT(2.6760)  FL_LIT(-8.4960)   FL_LIT(3.2880)) -- P   
      (Pt  FL_LIT(1.4950)  FL_LIT(-7.6230)   FL_LIT(3.4770)) -- O1P 
      (Pt  FL_LIT(2.9490)  FL_LIT(-9.4640)   FL_LIT(4.3740)) -- O2P 
      (Pt  FL_LIT(3.9730)  FL_LIT(-7.5950)   FL_LIT(3.0340)) -- O5' 
      (Pt  FL_LIT(5.2430)  FL_LIT(-8.2420)   FL_LIT(2.8260)) -- C5' 
      (Pt  FL_LIT(5.1974)  FL_LIT(-8.8497)   FL_LIT(1.9223)) -- H5' 
      (Pt  FL_LIT(5.5548)  FL_LIT(-8.7348)   FL_LIT(3.7469)) -- H5''
      (Pt  FL_LIT(6.3140)  FL_LIT(-7.2060)   FL_LIT(2.5510)) -- C4' 
      (Pt  FL_LIT(7.2954)  FL_LIT(-7.6762)   FL_LIT(2.4898)) -- H4' 
      (Pt  FL_LIT(6.0140)  FL_LIT(-6.5420)   FL_LIT(1.2890)) -- O4' 
      (Pt  FL_LIT(6.4190)  FL_LIT(-5.1840)   FL_LIT(1.3620)) -- C1' 
      (Pt  FL_LIT(7.1608)  FL_LIT(-5.0495)   FL_LIT(0.5747)) -- H1' 
      (Pt  FL_LIT(7.0760)  FL_LIT(-4.9560)   FL_LIT(2.7270)) -- C2' 
      (Pt  FL_LIT(6.7770)  FL_LIT(-3.9803)   FL_LIT(3.1099)) -- H2''
      (Pt  FL_LIT(8.4500)  FL_LIT(-5.1930)   FL_LIT(2.5810)) -- O2' 
      (Pt  FL_LIT(8.8309)  FL_LIT(-4.8755)   FL_LIT(1.7590)) -- H2' 
      (Pt  FL_LIT(6.4060)  FL_LIT(-6.0590)   FL_LIT(3.5580)) -- C3' 
      (Pt  FL_LIT(5.4021)  FL_LIT(-5.7313)   FL_LIT(3.8281)) -- H3' 
      (Pt  FL_LIT(7.1570)  FL_LIT(-6.4240)   FL_LIT(4.7070)) -- O3' 
      (Pt  FL_LIT(5.2170)  FL_LIT(-4.3260)   FL_LIT(1.1690)) -- N1  
      (Pt  FL_LIT(4.2960)  FL_LIT(-2.2560)   FL_LIT(0.6290)) -- N3  
      (Pt  FL_LIT(5.4330)  FL_LIT(-3.0200)   FL_LIT(0.7990)) -- C2  
      (Pt  FL_LIT(2.9930)  FL_LIT(-2.6780)   FL_LIT(0.7940)) -- C4  
      (Pt  FL_LIT(2.8670)  FL_LIT(-4.0630)   FL_LIT(1.1830)) -- C5  
      (Pt  FL_LIT(3.9570)  FL_LIT(-4.8300)   FL_LIT(1.3550)) -- C6
      (U
      (Pt  FL_LIT(6.5470)  FL_LIT(-2.5560)   FL_LIT(0.6290)) -- O2  
      (Pt  FL_LIT(2.0540)  FL_LIT(-1.9000)   FL_LIT(0.6130)) -- O4  
      (Pt  FL_LIT(4.4300)  FL_LIT(-1.3020)   FL_LIT(0.3600)) -- H3  
      (Pt  FL_LIT(1.9590)  FL_LIT(-4.4570)   FL_LIT(1.3250)) -- H5  
      (Pt  FL_LIT(3.8460)  FL_LIT(-5.7860)   FL_LIT(1.6240)) -- H6  
      )

rU01
  = Nuc
      (Tfo FL_LIT(-0.0137)  FL_LIT(-0.8012)   FL_LIT(0.5983)  -- dgf_base_tfo
           FL_LIT(-0.2523)   FL_LIT(0.5817)   FL_LIT(0.7733)
           FL_LIT(-0.9675)  FL_LIT(-0.1404)  FL_LIT(-0.2101)
            FL_LIT(0.2031)   FL_LIT(8.3874)   FL_LIT(0.4228))
      (Tfo FL_LIT(-0.8313)  FL_LIT(-0.4738)  FL_LIT(-0.2906)  -- p_o3'_275_tfo
            FL_LIT(0.0649)   FL_LIT(0.4366)  FL_LIT(-0.8973)
            FL_LIT(0.5521)  FL_LIT(-0.7648)  FL_LIT(-0.3322)
            FL_LIT(1.6833)   FL_LIT(6.8060)  FL_LIT(-7.0011))
      (Tfo  FL_LIT(0.3445)  FL_LIT(-0.7630)   FL_LIT(0.5470)  -- p_o3'_180_tfo
           FL_LIT(-0.4628)  FL_LIT(-0.6450)  FL_LIT(-0.6082)
            FL_LIT(0.8168)  FL_LIT(-0.0436)  FL_LIT(-0.5753)
           FL_LIT(-6.8179)  FL_LIT(-3.9778)  FL_LIT(-5.9887))
      (Tfo  FL_LIT(0.5855)   FL_LIT(0.7931)  FL_LIT(-0.1682)  -- p_o3'_60_tfo
            FL_LIT(0.8103)  FL_LIT(-0.5790)   FL_LIT(0.0906)
           FL_LIT(-0.0255)  FL_LIT(-0.1894)  FL_LIT(-0.9816)
            FL_LIT(6.1203)  FL_LIT(-7.1051)   FL_LIT(3.1984))
      (Pt  FL_LIT(2.6760)  FL_LIT(-8.4960)   FL_LIT(3.2880)) -- P   
      (Pt  FL_LIT(1.4950)  FL_LIT(-7.6230)   FL_LIT(3.4770)) -- O1P 
      (Pt  FL_LIT(2.9490)  FL_LIT(-9.4640)   FL_LIT(4.3740)) -- O2P 
      (Pt  FL_LIT(3.9730)  FL_LIT(-7.5950)   FL_LIT(3.0340)) -- O5' 
      (Pt  FL_LIT(5.2416)  FL_LIT(-8.2422)   FL_LIT(2.8181)) -- C5' 
      (Pt  FL_LIT(5.2050)  FL_LIT(-8.8128)   FL_LIT(1.8901)) -- H5' 
      (Pt  FL_LIT(5.5368)  FL_LIT(-8.7738)   FL_LIT(3.7227)) -- H5''
      (Pt  FL_LIT(6.3232)  FL_LIT(-7.2037)   FL_LIT(2.6002)) -- C4' 
      (Pt  FL_LIT(7.3048)  FL_LIT(-7.6757)   FL_LIT(2.5577)) -- H4' 
      (Pt  FL_LIT(6.0635)  FL_LIT(-6.5092)   FL_LIT(1.3456)) -- O4' 
      (Pt  FL_LIT(6.4697)  FL_LIT(-5.1547)   FL_LIT(1.4629)) -- C1' 
      (Pt  FL_LIT(7.2354)  FL_LIT(-5.0043)   FL_LIT(0.7018)) -- H1' 
      (Pt  FL_LIT(7.0856)  FL_LIT(-4.9610)   FL_LIT(2.8521)) -- C2' 
      (Pt  FL_LIT(6.7777)  FL_LIT(-3.9935)   FL_LIT(3.2487)) -- H2''
      (Pt  FL_LIT(8.4627)  FL_LIT(-5.1992)   FL_LIT(2.7423)) -- O2' 
      (Pt  FL_LIT(8.8693)  FL_LIT(-4.8638)   FL_LIT(1.9399)) -- H2' 
      (Pt  FL_LIT(6.3877)  FL_LIT(-6.0809)   FL_LIT(3.6362)) -- C3' 
      (Pt  FL_LIT(5.3770)  FL_LIT(-5.7562)   FL_LIT(3.8834)) -- H3' 
      (Pt  FL_LIT(7.1024)  FL_LIT(-6.4754)   FL_LIT(4.7985)) -- O3' 
      (Pt  FL_LIT(5.2764)  FL_LIT(-4.2883)   FL_LIT(1.2538)) -- N1  
      (Pt  FL_LIT(4.3777)  FL_LIT(-2.2062)   FL_LIT(0.7229)) -- N3  
      (Pt  FL_LIT(5.5069)  FL_LIT(-2.9779)   FL_LIT(0.9088)) -- C2  
      (Pt  FL_LIT(3.0693)  FL_LIT(-2.6246)   FL_LIT(0.8500)) -- C4  
      (Pt  FL_LIT(2.9279)  FL_LIT(-4.0146)   FL_LIT(1.2149)) -- C5  
      (Pt  FL_LIT(4.0101)  FL_LIT(-4.7892)   FL_LIT(1.4017)) -- C6
      (U
      (Pt  FL_LIT(6.6267)  FL_LIT(-2.5166)   FL_LIT(0.7728)) -- O2  
      (Pt  FL_LIT(2.1383)  FL_LIT(-1.8396)   FL_LIT(0.6581)) -- O4  
      (Pt  FL_LIT(4.5223)  FL_LIT(-1.2489)   FL_LIT(0.4716)) -- H3  
      (Pt  FL_LIT(2.0151)  FL_LIT(-4.4065)   FL_LIT(1.3290)) -- H5  
      (Pt  FL_LIT(3.8886)  FL_LIT(-5.7486)   FL_LIT(1.6535)) -- H6  
      )

rU02
  = Nuc
      (Tfo  FL_LIT(0.5141)   FL_LIT(0.0246)   FL_LIT(0.8574)  -- dgf_base_tfo
           FL_LIT(-0.5547)  FL_LIT(-0.7529)   FL_LIT(0.3542)
            FL_LIT(0.6542)  FL_LIT(-0.6577)  FL_LIT(-0.3734)
           FL_LIT(-9.1111)  FL_LIT(-3.4598)  FL_LIT(-3.2939))
      (Tfo FL_LIT(-0.8313)  FL_LIT(-0.4738)  FL_LIT(-0.2906)  -- p_o3'_275_tfo
            FL_LIT(0.0649)   FL_LIT(0.4366)  FL_LIT(-0.8973)
            FL_LIT(0.5521)  FL_LIT(-0.7648)  FL_LIT(-0.3322)
            FL_LIT(1.6833)   FL_LIT(6.8060)  FL_LIT(-7.0011))
      (Tfo  FL_LIT(0.3445)  FL_LIT(-0.7630)   FL_LIT(0.5470)  -- p_o3'_180_tfo
           FL_LIT(-0.4628)  FL_LIT(-0.6450)  FL_LIT(-0.6082)
            FL_LIT(0.8168)  FL_LIT(-0.0436)  FL_LIT(-0.5753)
           FL_LIT(-6.8179)  FL_LIT(-3.9778)  FL_LIT(-5.9887))
      (Tfo  FL_LIT(0.5855)   FL_LIT(0.7931)  FL_LIT(-0.1682)  -- p_o3'_60_tfo
            FL_LIT(0.8103)  FL_LIT(-0.5790)   FL_LIT(0.0906)
           FL_LIT(-0.0255)  FL_LIT(-0.1894)  FL_LIT(-0.9816)
            FL_LIT(6.1203)  FL_LIT(-7.1051)   FL_LIT(3.1984))
      (Pt  FL_LIT(2.6760)  FL_LIT(-8.4960)   FL_LIT(3.2880)) -- P   
      (Pt  FL_LIT(1.4950)  FL_LIT(-7.6230)   FL_LIT(3.4770)) -- O1P 
      (Pt  FL_LIT(2.9490)  FL_LIT(-9.4640)   FL_LIT(4.3740)) -- O2P 
      (Pt  FL_LIT(3.9730)  FL_LIT(-7.5950)   FL_LIT(3.0340)) -- O5' 
      (Pt  FL_LIT(4.3825)  FL_LIT(-6.6585)   FL_LIT(4.0489)) -- C5' 
      (Pt  FL_LIT(4.6841)  FL_LIT(-7.2019)   FL_LIT(4.9443)) -- H5' 
      (Pt  FL_LIT(3.6189)  FL_LIT(-5.8889)   FL_LIT(4.1625)) -- H5''
      (Pt  FL_LIT(5.6255)  FL_LIT(-5.9175)   FL_LIT(3.5998)) -- C4' 
      (Pt  FL_LIT(5.8732)  FL_LIT(-5.1228)   FL_LIT(4.3034)) -- H4' 
      (Pt  FL_LIT(6.7337)  FL_LIT(-6.8605)   FL_LIT(3.5222)) -- O4' 
      (Pt  FL_LIT(7.5932)  FL_LIT(-6.4923)   FL_LIT(2.4548)) -- C1' 
      (Pt  FL_LIT(8.5661)  FL_LIT(-6.2983)   FL_LIT(2.9064)) -- H1' 
      (Pt  FL_LIT(7.0527)  FL_LIT(-5.2012)   FL_LIT(1.8322)) -- C2' 
      (Pt  FL_LIT(7.1627)  FL_LIT(-5.2525)   FL_LIT(0.7490)) -- H2''
      (Pt  FL_LIT(7.6666)  FL_LIT(-4.1249)   FL_LIT(2.4880)) -- O2' 
      (Pt  FL_LIT(8.5944)  FL_LIT(-4.2543)   FL_LIT(2.6981)) -- H2' 
      (Pt  FL_LIT(5.5661)  FL_LIT(-5.3029)   FL_LIT(2.2009)) -- C3' 
      (Pt  FL_LIT(5.0841)  FL_LIT(-6.0018)   FL_LIT(1.5172)) -- H3' 
      (Pt  FL_LIT(4.9062)  FL_LIT(-4.0452)   FL_LIT(2.2042)) -- O3' 
      (Pt  FL_LIT(7.6298)  FL_LIT(-7.6136)   FL_LIT(1.4752)) -- N1  
      (Pt  FL_LIT(8.6945)  FL_LIT(-8.7046)  FL_LIT(-0.2857)) -- N3  
      (Pt  FL_LIT(8.6943)  FL_LIT(-7.6514)   FL_LIT(0.6066)) -- C2  
      (Pt  FL_LIT(7.7426)  FL_LIT(-9.6987)  FL_LIT(-0.3801)) -- C4  
      (Pt  FL_LIT(6.6642)  FL_LIT(-9.5742)   FL_LIT(0.5722)) -- C5  
      (Pt  FL_LIT(6.6391)  FL_LIT(-8.5592)   FL_LIT(1.4526)) -- C6
      (U
      (Pt  FL_LIT(9.5840)  FL_LIT(-6.8186)   FL_LIT(0.6136)) -- O2  
      (Pt  FL_LIT(7.8505) FL_LIT(-10.5925)  FL_LIT(-1.2223)) -- O4  
      (Pt  FL_LIT(9.4601)  FL_LIT(-8.7514)  FL_LIT(-0.9277)) -- H3  
      (Pt  FL_LIT(5.9281) FL_LIT(-10.2509)   FL_LIT(0.5782)) -- H5  
      (Pt  FL_LIT(5.8831)  FL_LIT(-8.4931)   FL_LIT(2.1028)) -- H6  
      )

rU03
  = Nuc
      (Tfo FL_LIT(-0.4993)   FL_LIT(0.0476)   FL_LIT(0.8651)  -- dgf_base_tfo
            FL_LIT(0.8078)  FL_LIT(-0.3353)   FL_LIT(0.4847)
            FL_LIT(0.3132)   FL_LIT(0.9409)   FL_LIT(0.1290)
            FL_LIT(6.2989)  FL_LIT(-5.2303)  FL_LIT(-3.8577))
      (Tfo FL_LIT(-0.8313)  FL_LIT(-0.4738)  FL_LIT(-0.2906)  -- p_o3'_275_tfo
            FL_LIT(0.0649)   FL_LIT(0.4366)  FL_LIT(-0.8973)
            FL_LIT(0.5521)  FL_LIT(-0.7648)  FL_LIT(-0.3322)
            FL_LIT(1.6833)   FL_LIT(6.8060)  FL_LIT(-7.0011))
      (Tfo  FL_LIT(0.3445)  FL_LIT(-0.7630)   FL_LIT(0.5470)  -- p_o3'_180_tfo
           FL_LIT(-0.4628)  FL_LIT(-0.6450)  FL_LIT(-0.6082)
            FL_LIT(0.8168)  FL_LIT(-0.0436)  FL_LIT(-0.5753)
           FL_LIT(-6.8179)  FL_LIT(-3.9778)  FL_LIT(-5.9887))
      (Tfo  FL_LIT(0.5855)   FL_LIT(0.7931)  FL_LIT(-0.1682)  -- p_o3'_60_tfo
            FL_LIT(0.8103)  FL_LIT(-0.5790)   FL_LIT(0.0906)
           FL_LIT(-0.0255)  FL_LIT(-0.1894)  FL_LIT(-0.9816)
            FL_LIT(6.1203)  FL_LIT(-7.1051)   FL_LIT(3.1984))
      (Pt  FL_LIT(2.6760)  FL_LIT(-8.4960)   FL_LIT(3.2880)) -- P   
      (Pt  FL_LIT(1.4950)  FL_LIT(-7.6230)   FL_LIT(3.4770)) -- O1P 
      (Pt  FL_LIT(2.9490)  FL_LIT(-9.4640)   FL_LIT(4.3740)) -- O2P 
      (Pt  FL_LIT(3.9730)  FL_LIT(-7.5950)   FL_LIT(3.0340)) -- O5' 
      (Pt  FL_LIT(3.9938)  FL_LIT(-6.7042)   FL_LIT(1.9023)) -- C5' 
      (Pt  FL_LIT(3.2332)  FL_LIT(-5.9343)   FL_LIT(2.0319)) -- H5' 
      (Pt  FL_LIT(3.9666)  FL_LIT(-7.2863)   FL_LIT(0.9812)) -- H5''
      (Pt  FL_LIT(5.3098)  FL_LIT(-5.9546)   FL_LIT(1.8564)) -- C4' 
      (Pt  FL_LIT(5.3863)  FL_LIT(-5.3702)   FL_LIT(0.9395)) -- H4' 
      (Pt  FL_LIT(5.3851)  FL_LIT(-5.0642)   FL_LIT(3.0076)) -- O4' 
      (Pt  FL_LIT(6.7315)  FL_LIT(-4.9724)   FL_LIT(3.4462)) -- C1' 
      (Pt  FL_LIT(7.0033)  FL_LIT(-3.9202)   FL_LIT(3.3619)) -- H1' 
      (Pt  FL_LIT(7.5997)  FL_LIT(-5.8018)   FL_LIT(2.4948)) -- C2' 
      (Pt  FL_LIT(8.3627)  FL_LIT(-6.3254)   FL_LIT(3.0707)) -- H2''
      (Pt  FL_LIT(8.0410)  FL_LIT(-4.9501)   FL_LIT(1.4724)) -- O2' 
      (Pt  FL_LIT(8.2781)  FL_LIT(-4.0644)   FL_LIT(1.7570)) -- H2' 
      (Pt  FL_LIT(6.5701)  FL_LIT(-6.8129)   FL_LIT(1.9714)) -- C3' 
      (Pt  FL_LIT(6.4186)  FL_LIT(-7.5809)   FL_LIT(2.7299)) -- H3' 
      (Pt  FL_LIT(6.9357)  FL_LIT(-7.3841)   FL_LIT(0.7235)) -- O3' 
      (Pt  FL_LIT(6.8024)  FL_LIT(-5.4718)   FL_LIT(4.8475)) -- N1  
      (Pt  FL_LIT(7.9218)  FL_LIT(-5.5700)   FL_LIT(6.8877)) -- N3  
      (Pt  FL_LIT(7.8908)  FL_LIT(-5.0886)   FL_LIT(5.5944)) -- C2  
      (Pt  FL_LIT(6.9789)  FL_LIT(-6.3827)   FL_LIT(7.4823)) -- C4  
      (Pt  FL_LIT(5.8742)  FL_LIT(-6.7319)   FL_LIT(6.6202)) -- C5  
      (Pt  FL_LIT(5.8182)  FL_LIT(-6.2769)   FL_LIT(5.3570)) -- C6
      (U
      (Pt  FL_LIT(8.7747)  FL_LIT(-4.3728)   FL_LIT(5.1568)) -- O2  
      (Pt  FL_LIT(7.1154)  FL_LIT(-6.7509)   FL_LIT(8.6509)) -- O4  
      (Pt  FL_LIT(8.7055)  FL_LIT(-5.3037)   FL_LIT(7.4491)) -- H3  
      (Pt  FL_LIT(5.1416)  FL_LIT(-7.3178)   FL_LIT(6.9665)) -- H5  
      (Pt  FL_LIT(5.0441)  FL_LIT(-6.5310)   FL_LIT(4.7784)) -- H6  
      )

rU04
  = Nuc
      (Tfo FL_LIT(-0.5669)  FL_LIT(-0.8012)   FL_LIT(0.1918)  -- dgf_base_tfo
           FL_LIT(-0.8129)   FL_LIT(0.5817)   FL_LIT(0.0273)
           FL_LIT(-0.1334)  FL_LIT(-0.1404)  FL_LIT(-0.9811)
           FL_LIT(-0.3279)   FL_LIT(8.3874)   FL_LIT(0.3355))
      (Tfo FL_LIT(-0.8313)  FL_LIT(-0.4738)  FL_LIT(-0.2906)  -- p_o3'_275_tfo
            FL_LIT(0.0649)   FL_LIT(0.4366)  FL_LIT(-0.8973)
            FL_LIT(0.5521)  FL_LIT(-0.7648)  FL_LIT(-0.3322)
            FL_LIT(1.6833)   FL_LIT(6.8060)  FL_LIT(-7.0011))
      (Tfo  FL_LIT(0.3445)  FL_LIT(-0.7630)   FL_LIT(0.5470)  -- p_o3'_180_tfo
           FL_LIT(-0.4628)  FL_LIT(-0.6450)  FL_LIT(-0.6082)
            FL_LIT(0.8168)  FL_LIT(-0.0436)  FL_LIT(-0.5753)
           FL_LIT(-6.8179)  FL_LIT(-3.9778)  FL_LIT(-5.9887))
      (Tfo  FL_LIT(0.5855)   FL_LIT(0.7931)  FL_LIT(-0.1682)  -- p_o3'_60_tfo
            FL_LIT(0.8103)  FL_LIT(-0.5790)   FL_LIT(0.0906)
           FL_LIT(-0.0255)  FL_LIT(-0.1894)  FL_LIT(-0.9816)
            FL_LIT(6.1203)  FL_LIT(-7.1051)   FL_LIT(3.1984))
      (Pt  FL_LIT(2.6760)  FL_LIT(-8.4960)   FL_LIT(3.2880)) -- P   
      (Pt  FL_LIT(1.4950)  FL_LIT(-7.6230)   FL_LIT(3.4770)) -- O1P 
      (Pt  FL_LIT(2.9490)  FL_LIT(-9.4640)   FL_LIT(4.3740)) -- O2P 
      (Pt  FL_LIT(3.9730)  FL_LIT(-7.5950)   FL_LIT(3.0340)) -- O5' 
      (Pt  FL_LIT(5.2416)  FL_LIT(-8.2422)   FL_LIT(2.8181)) -- C5' 
      (Pt  FL_LIT(5.2050)  FL_LIT(-8.8128)   FL_LIT(1.8901)) -- H5' 
      (Pt  FL_LIT(5.5368)  FL_LIT(-8.7738)   FL_LIT(3.7227)) -- H5''
      (Pt  FL_LIT(6.3232)  FL_LIT(-7.2037)   FL_LIT(2.6002)) -- C4' 
      (Pt  FL_LIT(7.3048)  FL_LIT(-7.6757)   FL_LIT(2.5577)) -- H4' 
      (Pt  FL_LIT(6.0635)  FL_LIT(-6.5092)   FL_LIT(1.3456)) -- O4' 
      (Pt  FL_LIT(6.4697)  FL_LIT(-5.1547)   FL_LIT(1.4629)) -- C1' 
      (Pt  FL_LIT(7.2354)  FL_LIT(-5.0043)   FL_LIT(0.7018)) -- H1' 
      (Pt  FL_LIT(7.0856)  FL_LIT(-4.9610)   FL_LIT(2.8521)) -- C2' 
      (Pt  FL_LIT(6.7777)  FL_LIT(-3.9935)   FL_LIT(3.2487)) -- H2''
      (Pt  FL_LIT(8.4627)  FL_LIT(-5.1992)   FL_LIT(2.7423)) -- O2' 
      (Pt  FL_LIT(8.8693)  FL_LIT(-4.8638)   FL_LIT(1.9399)) -- H2' 
      (Pt  FL_LIT(6.3877)  FL_LIT(-6.0809)   FL_LIT(3.6362)) -- C3' 
      (Pt  FL_LIT(5.3770)  FL_LIT(-5.7562)   FL_LIT(3.8834)) -- H3' 
      (Pt  FL_LIT(7.1024)  FL_LIT(-6.4754)   FL_LIT(4.7985)) -- O3' 
      (Pt  FL_LIT(5.2764)  FL_LIT(-4.2883)   FL_LIT(1.2538)) -- N1  
      (Pt  FL_LIT(3.8961)  FL_LIT(-3.0896)  FL_LIT(-0.1893)) -- N3  
      (Pt  FL_LIT(5.0095)  FL_LIT(-3.8907)  FL_LIT(-0.0346)) -- C2  
      (Pt  FL_LIT(3.0480)  FL_LIT(-2.6632)   FL_LIT(0.8116)) -- C4  
      (Pt  FL_LIT(3.4093)  FL_LIT(-3.1310)   FL_LIT(2.1292)) -- C5  
      (Pt  FL_LIT(4.4878)  FL_LIT(-3.9124)   FL_LIT(2.3088)) -- C6
      (U
      (Pt  FL_LIT(5.7005)  FL_LIT(-4.2164)  FL_LIT(-0.9842)) -- O2  
      (Pt  FL_LIT(2.0800)  FL_LIT(-1.9458)   FL_LIT(0.5503)) -- O4  
      (Pt  FL_LIT(3.6834)  FL_LIT(-2.7882)  FL_LIT(-1.1190)) -- H3  
      (Pt  FL_LIT(2.8508)  FL_LIT(-2.8721)   FL_LIT(2.9172)) -- H5  
      (Pt  FL_LIT(4.7188)  FL_LIT(-4.2247)   FL_LIT(3.2295)) -- H6  
      )

rU05
  = Nuc
      (Tfo FL_LIT(-0.6298)   FL_LIT(0.0246)   FL_LIT(0.7763)  -- dgf_base_tfo
           FL_LIT(-0.5226)  FL_LIT(-0.7529)  FL_LIT(-0.4001)
            FL_LIT(0.5746)  FL_LIT(-0.6577)   FL_LIT(0.4870)
           FL_LIT(-0.0208)  FL_LIT(-3.4598)  FL_LIT(-9.6882))
      (Tfo FL_LIT(-0.8313)  FL_LIT(-0.4738)  FL_LIT(-0.2906)  -- p_o3'_275_tfo
            FL_LIT(0.0649)   FL_LIT(0.4366)  FL_LIT(-0.8973)
            FL_LIT(0.5521)  FL_LIT(-0.7648)  FL_LIT(-0.3322)
            FL_LIT(1.6833)   FL_LIT(6.8060)  FL_LIT(-7.0011))
      (Tfo  FL_LIT(0.3445)  FL_LIT(-0.7630)   FL_LIT(0.5470)  -- p_o3'_180_tfo
           FL_LIT(-0.4628)  FL_LIT(-0.6450)  FL_LIT(-0.6082)
            FL_LIT(0.8168)  FL_LIT(-0.0436)  FL_LIT(-0.5753)
           FL_LIT(-6.8179)  FL_LIT(-3.9778)  FL_LIT(-5.9887))
      (Tfo  FL_LIT(0.5855)   FL_LIT(0.7931)  FL_LIT(-0.1682)  -- p_o3'_60_tfo
            FL_LIT(0.8103)  FL_LIT(-0.5790)   FL_LIT(0.0906)
           FL_LIT(-0.0255)  FL_LIT(-0.1894)  FL_LIT(-0.9816)
            FL_LIT(6.1203)  FL_LIT(-7.1051)   FL_LIT(3.1984))
      (Pt  FL_LIT(2.6760)  FL_LIT(-8.4960)   FL_LIT(3.2880)) -- P   
      (Pt  FL_LIT(1.4950)  FL_LIT(-7.6230)   FL_LIT(3.4770)) -- O1P 
      (Pt  FL_LIT(2.9490)  FL_LIT(-9.4640)   FL_LIT(4.3740)) -- O2P 
      (Pt  FL_LIT(3.9730)  FL_LIT(-7.5950)   FL_LIT(3.0340)) -- O5' 
      (Pt  FL_LIT(4.3825)  FL_LIT(-6.6585)   FL_LIT(4.0489)) -- C5' 
      (Pt  FL_LIT(4.6841)  FL_LIT(-7.2019)   FL_LIT(4.9443)) -- H5' 
      (Pt  FL_LIT(3.6189)  FL_LIT(-5.8889)   FL_LIT(4.1625)) -- H5''
      (Pt  FL_LIT(5.6255)  FL_LIT(-5.9175)   FL_LIT(3.5998)) -- C4' 
      (Pt  FL_LIT(5.8732)  FL_LIT(-5.1228)   FL_LIT(4.3034)) -- H4' 
      (Pt  FL_LIT(6.7337)  FL_LIT(-6.8605)   FL_LIT(3.5222)) -- O4' 
      (Pt  FL_LIT(7.5932)  FL_LIT(-6.4923)   FL_LIT(2.4548)) -- C1' 
      (Pt  FL_LIT(8.5661)  FL_LIT(-6.2983)   FL_LIT(2.9064)) -- H1' 
      (Pt  FL_LIT(7.0527)  FL_LIT(-5.2012)   FL_LIT(1.8322)) -- C2' 
      (Pt  FL_LIT(7.1627)  FL_LIT(-5.2525)   FL_LIT(0.7490)) -- H2''
      (Pt  FL_LIT(7.6666)  FL_LIT(-4.1249)   FL_LIT(2.4880)) -- O2' 
      (Pt  FL_LIT(8.5944)  FL_LIT(-4.2543)   FL_LIT(2.6981)) -- H2' 
      (Pt  FL_LIT(5.5661)  FL_LIT(-5.3029)   FL_LIT(2.2009)) -- C3' 
      (Pt  FL_LIT(5.0841)  FL_LIT(-6.0018)   FL_LIT(1.5172)) -- H3' 
      (Pt  FL_LIT(4.9062)  FL_LIT(-4.0452)   FL_LIT(2.2042)) -- O3' 
      (Pt  FL_LIT(7.6298)  FL_LIT(-7.6136)   FL_LIT(1.4752)) -- N1  
      (Pt  FL_LIT(8.5977)  FL_LIT(-9.5977)   FL_LIT(0.7329)) -- N3  
      (Pt  FL_LIT(8.5951)  FL_LIT(-8.5745)   FL_LIT(1.6594)) -- C2  
      (Pt  FL_LIT(7.7372)  FL_LIT(-9.7371)  FL_LIT(-0.3364)) -- C4  
      (Pt  FL_LIT(6.7596)  FL_LIT(-8.6801)  FL_LIT(-0.4476)) -- C5  
      (Pt  FL_LIT(6.7338)  FL_LIT(-7.6721)   FL_LIT(0.4408)) -- C6
      (U
      (Pt  FL_LIT(9.3993)  FL_LIT(-8.5377)   FL_LIT(2.5743)) -- O2  
      (Pt  FL_LIT(7.8374) FL_LIT(-10.6990)  FL_LIT(-1.1008)) -- O4  
      (Pt  FL_LIT(9.2924) FL_LIT(-10.3081)   FL_LIT(0.8477)) -- H3  
      (Pt  FL_LIT(6.0932)  FL_LIT(-8.6982)  FL_LIT(-1.1929)) -- H5  
      (Pt  FL_LIT(6.0481)  FL_LIT(-6.9515)   FL_LIT(0.3446)) -- H6  
      )

rU06
  = Nuc
      (Tfo FL_LIT(-0.9837)   FL_LIT(0.0476)  FL_LIT(-0.1733)  -- dgf_base_tfo
           FL_LIT(-0.1792)  FL_LIT(-0.3353)   FL_LIT(0.9249)
           FL_LIT(-0.0141)   FL_LIT(0.9409)   FL_LIT(0.3384)
            FL_LIT(5.7793)  FL_LIT(-5.2303)   FL_LIT(4.5997))
      (Tfo FL_LIT(-0.8313)  FL_LIT(-0.4738)  FL_LIT(-0.2906)  -- p_o3'_275_tfo
            FL_LIT(0.0649)   FL_LIT(0.4366)  FL_LIT(-0.8973)
            FL_LIT(0.5521)  FL_LIT(-0.7648)  FL_LIT(-0.3322)
            FL_LIT(1.6833)   FL_LIT(6.8060)  FL_LIT(-7.0011))
      (Tfo  FL_LIT(0.3445)  FL_LIT(-0.7630)   FL_LIT(0.5470)  -- p_o3'_180_tfo
           FL_LIT(-0.4628)  FL_LIT(-0.6450)  FL_LIT(-0.6082)
            FL_LIT(0.8168)  FL_LIT(-0.0436)  FL_LIT(-0.5753)
           FL_LIT(-6.8179)  FL_LIT(-3.9778)  FL_LIT(-5.9887))
      (Tfo  FL_LIT(0.5855)   FL_LIT(0.7931)  FL_LIT(-0.1682)  -- p_o3'_60_tfo
            FL_LIT(0.8103)  FL_LIT(-0.5790)   FL_LIT(0.0906)
           FL_LIT(-0.0255)  FL_LIT(-0.1894)  FL_LIT(-0.9816)
            FL_LIT(6.1203)  FL_LIT(-7.1051)   FL_LIT(3.1984))
      (Pt  FL_LIT(2.6760)  FL_LIT(-8.4960)   FL_LIT(3.2880)) -- P   
      (Pt  FL_LIT(1.4950)  FL_LIT(-7.6230)   FL_LIT(3.4770)) -- O1P 
      (Pt  FL_LIT(2.9490)  FL_LIT(-9.4640)   FL_LIT(4.3740)) -- O2P 
      (Pt  FL_LIT(3.9730)  FL_LIT(-7.5950)   FL_LIT(3.0340)) -- O5' 
      (Pt  FL_LIT(3.9938)  FL_LIT(-6.7042)   FL_LIT(1.9023)) -- C5' 
      (Pt  FL_LIT(3.2332)  FL_LIT(-5.9343)   FL_LIT(2.0319)) -- H5' 
      (Pt  FL_LIT(3.9666)  FL_LIT(-7.2863)   FL_LIT(0.9812)) -- H5''
      (Pt  FL_LIT(5.3098)  FL_LIT(-5.9546)   FL_LIT(1.8564)) -- C4' 
      (Pt  FL_LIT(5.3863)  FL_LIT(-5.3702)   FL_LIT(0.9395)) -- H4' 
      (Pt  FL_LIT(5.3851)  FL_LIT(-5.0642)   FL_LIT(3.0076)) -- O4' 
      (Pt  FL_LIT(6.7315)  FL_LIT(-4.9724)   FL_LIT(3.4462)) -- C1' 
      (Pt  FL_LIT(7.0033)  FL_LIT(-3.9202)   FL_LIT(3.3619)) -- H1' 
      (Pt  FL_LIT(7.5997)  FL_LIT(-5.8018)   FL_LIT(2.4948)) -- C2' 
      (Pt  FL_LIT(8.3627)  FL_LIT(-6.3254)   FL_LIT(3.0707)) -- H2''
      (Pt  FL_LIT(8.0410)  FL_LIT(-4.9501)   FL_LIT(1.4724)) -- O2' 
      (Pt  FL_LIT(8.2781)  FL_LIT(-4.0644)   FL_LIT(1.7570)) -- H2' 
      (Pt  FL_LIT(6.5701)  FL_LIT(-6.8129)   FL_LIT(1.9714)) -- C3' 
      (Pt  FL_LIT(6.4186)  FL_LIT(-7.5809)   FL_LIT(2.7299)) -- H3' 
      (Pt  FL_LIT(6.9357)  FL_LIT(-7.3841)   FL_LIT(0.7235)) -- O3' 
      (Pt  FL_LIT(6.8024)  FL_LIT(-5.4718)   FL_LIT(4.8475)) -- N1  
      (Pt  FL_LIT(6.6920)  FL_LIT(-5.0495)   FL_LIT(7.1354)) -- N3  
      (Pt  FL_LIT(6.6201)  FL_LIT(-4.5500)   FL_LIT(5.8506)) -- C2  
      (Pt  FL_LIT(6.9254)  FL_LIT(-6.3614)   FL_LIT(7.4926)) -- C4  
      (Pt  FL_LIT(7.1046)  FL_LIT(-7.2543)   FL_LIT(6.3718)) -- C5  
      (Pt  FL_LIT(7.0391)  FL_LIT(-6.7951)   FL_LIT(5.1106)) -- C6
      (U
      (Pt  FL_LIT(6.4083)  FL_LIT(-3.3696)   FL_LIT(5.6340)) -- O2  
      (Pt  FL_LIT(6.9679)  FL_LIT(-6.6901)   FL_LIT(8.6800)) -- O4  
      (Pt  FL_LIT(6.5626)  FL_LIT(-4.3957)   FL_LIT(7.8812)) -- H3  
      (Pt  FL_LIT(7.2781)  FL_LIT(-8.2254)   FL_LIT(6.5350)) -- H5  
      (Pt  FL_LIT(7.1657)  FL_LIT(-7.4312)   FL_LIT(4.3503)) -- H6  
      )

rU07
  = Nuc
      (Tfo FL_LIT(-0.9434)   FL_LIT(0.3172)   FL_LIT(0.0971)  -- dgf_base_tfo
            FL_LIT(0.2294)   FL_LIT(0.4125)   FL_LIT(0.8816)
            FL_LIT(0.2396)   FL_LIT(0.8539)  FL_LIT(-0.4619)
            FL_LIT(8.3625) FL_LIT(-52.7147)   FL_LIT(1.3745))
      (Tfo  FL_LIT(0.2765)  FL_LIT(-0.1121)  FL_LIT(-0.9545)  -- p_o3'_275_tfo
           FL_LIT(-0.8297)   FL_LIT(0.4733)  FL_LIT(-0.2959)
            FL_LIT(0.4850)   FL_LIT(0.8737)   FL_LIT(0.0379)
          FL_LIT(-14.7774) FL_LIT(-45.2464)  FL_LIT(21.9088))
      (Tfo  FL_LIT(0.1063)  FL_LIT(-0.6334)  FL_LIT(-0.7665)  -- p_o3'_180_tfo
           FL_LIT(-0.5932)  FL_LIT(-0.6591)   FL_LIT(0.4624)
           FL_LIT(-0.7980)   FL_LIT(0.4055)  FL_LIT(-0.4458)
           FL_LIT(43.7634)   FL_LIT(4.3296)  FL_LIT(28.4890))
      (Tfo  FL_LIT(0.7136)  FL_LIT(-0.5032)  FL_LIT(-0.4873)  -- p_o3'_60_tfo
            FL_LIT(0.6803)   FL_LIT(0.3317)   FL_LIT(0.6536)
           FL_LIT(-0.1673)  FL_LIT(-0.7979)   FL_LIT(0.5791)
          FL_LIT(-17.1858)  FL_LIT(41.4390) FL_LIT(-27.0751))
      (Pt FL_LIT(21.3880)  FL_LIT(15.0780)  FL_LIT(45.5770)) -- P   
      (Pt FL_LIT(21.9980)  FL_LIT(14.5500)  FL_LIT(46.8210)) -- O1P 
      (Pt FL_LIT(21.1450)  FL_LIT(14.0270)  FL_LIT(44.5420)) -- O2P 
      (Pt FL_LIT(22.1250)  FL_LIT(16.3600)  FL_LIT(44.9460)) -- O5' 
      (Pt FL_LIT(21.5037)  FL_LIT(16.8594)  FL_LIT(43.7323)) -- C5' 
      (Pt FL_LIT(20.8147)  FL_LIT(17.6663)  FL_LIT(43.9823)) -- H5' 
      (Pt FL_LIT(21.1086)  FL_LIT(16.0230)  FL_LIT(43.1557)) -- H5''
      (Pt FL_LIT(22.5654)  FL_LIT(17.4874)  FL_LIT(42.8616)) -- C4' 
      (Pt FL_LIT(22.1584)  FL_LIT(17.7243)  FL_LIT(41.8785)) -- H4' 
      (Pt FL_LIT(23.0557)  FL_LIT(18.6826)  FL_LIT(43.4751)) -- O4' 
      (Pt FL_LIT(24.4788)  FL_LIT(18.6151)  FL_LIT(43.6455)) -- C1' 
      (Pt FL_LIT(24.9355)  FL_LIT(19.0840)  FL_LIT(42.7739)) -- H1' 
      (Pt FL_LIT(24.7958)  FL_LIT(17.1427)  FL_LIT(43.6474)) -- C2' 
      (Pt FL_LIT(24.5652)  FL_LIT(16.7400)  FL_LIT(44.6336)) -- H2''
      (Pt FL_LIT(26.1041)  FL_LIT(16.8773)  FL_LIT(43.2455)) -- O2' 
      (Pt FL_LIT(26.7516)  FL_LIT(17.5328)  FL_LIT(43.5149)) -- H2' 
      (Pt FL_LIT(23.8109)  FL_LIT(16.5979)  FL_LIT(42.6377)) -- C3' 
      (Pt FL_LIT(23.5756)  FL_LIT(15.5686)  FL_LIT(42.9084)) -- H3' 
      (Pt FL_LIT(24.2890)  FL_LIT(16.7447)  FL_LIT(41.2729)) -- O3' 
      (Pt FL_LIT(24.9420)  FL_LIT(19.2174)  FL_LIT(44.8923)) -- N1  
      (Pt FL_LIT(25.2655)  FL_LIT(20.5636)  FL_LIT(44.8883)) -- N3  
      (Pt FL_LIT(25.1663)  FL_LIT(21.2219)  FL_LIT(43.8561)) -- C2  
      (Pt FL_LIT(25.6911)  FL_LIT(21.1219)  FL_LIT(46.0494)) -- C4  
      (Pt FL_LIT(25.8051)  FL_LIT(20.4068)  FL_LIT(47.2048)) -- C5  
      (Pt FL_LIT(26.2093)  FL_LIT(20.9962)  FL_LIT(48.2534)) -- C6
      (U
      (Pt FL_LIT(25.4692)  FL_LIT(19.0221)  FL_LIT(47.2053)) -- O2  
      (Pt FL_LIT(25.0502)  FL_LIT(18.4827)  FL_LIT(46.0370)) -- O4  
      (Pt FL_LIT(25.9599)  FL_LIT(22.1772)  FL_LIT(46.0966)) -- H3  
      (Pt FL_LIT(25.5545)  FL_LIT(18.4409)  FL_LIT(48.1234)) -- H5  
      (Pt FL_LIT(24.7854)  FL_LIT(17.4265)  FL_LIT(45.9883)) -- H6  
      )

rU08
  = Nuc
      (Tfo FL_LIT(-0.0080)  FL_LIT(-0.7928)   FL_LIT(0.6094)  -- dgf_base_tfo
           FL_LIT(-0.7512)   FL_LIT(0.4071)   FL_LIT(0.5197)
           FL_LIT(-0.6601)  FL_LIT(-0.4536)  FL_LIT(-0.5988)
           FL_LIT(44.1482)  FL_LIT(30.7036)   FL_LIT(2.1088))
      (Tfo  FL_LIT(0.2765)  FL_LIT(-0.1121)  FL_LIT(-0.9545)  -- p_o3'_275_tfo
           FL_LIT(-0.8297)   FL_LIT(0.4733)  FL_LIT(-0.2959)
            FL_LIT(0.4850)   FL_LIT(0.8737)   FL_LIT(0.0379)
          FL_LIT(-14.7774) FL_LIT(-45.2464)  FL_LIT(21.9088))
      (Tfo  FL_LIT(0.1063)  FL_LIT(-0.6334)  FL_LIT(-0.7665)  -- p_o3'_180_tfo
           FL_LIT(-0.5932)  FL_LIT(-0.6591)   FL_LIT(0.4624)
           FL_LIT(-0.7980)   FL_LIT(0.4055)  FL_LIT(-0.4458)
           FL_LIT(43.7634)   FL_LIT(4.3296)  FL_LIT(28.4890))
      (Tfo  FL_LIT(0.7136)  FL_LIT(-0.5032)  FL_LIT(-0.4873)  -- p_o3'_60_tfo
            FL_LIT(0.6803)   FL_LIT(0.3317)   FL_LIT(0.6536)
           FL_LIT(-0.1673)  FL_LIT(-0.7979)   FL_LIT(0.5791)
          FL_LIT(-17.1858)  FL_LIT(41.4390) FL_LIT(-27.0751))
      (Pt FL_LIT(21.3880)  FL_LIT(15.0780)  FL_LIT(45.5770)) -- P   
      (Pt FL_LIT(21.9980)  FL_LIT(14.5500)  FL_LIT(46.8210)) -- O1P 
      (Pt FL_LIT(21.1450)  FL_LIT(14.0270)  FL_LIT(44.5420)) -- O2P 
      (Pt FL_LIT(22.1250)  FL_LIT(16.3600)  FL_LIT(44.9460)) -- O5' 
      (Pt FL_LIT(23.5096)  FL_LIT(16.1227)  FL_LIT(44.5783)) -- C5' 
      (Pt FL_LIT(23.5649)  FL_LIT(15.8588)  FL_LIT(43.5222)) -- H5' 
      (Pt FL_LIT(23.9621)  FL_LIT(15.4341)  FL_LIT(45.2919)) -- H5''
      (Pt FL_LIT(24.2805)  FL_LIT(17.4138)  FL_LIT(44.7151)) -- C4' 
      (Pt FL_LIT(25.3492)  FL_LIT(17.2309)  FL_LIT(44.6030)) -- H4' 
      (Pt FL_LIT(23.8497)  FL_LIT(18.3471)  FL_LIT(43.7208)) -- O4' 
      (Pt FL_LIT(23.4090)  FL_LIT(19.5681)  FL_LIT(44.3321)) -- C1' 
      (Pt FL_LIT(24.2595)  FL_LIT(20.2496)  FL_LIT(44.3524)) -- H1' 
      (Pt FL_LIT(23.0418)  FL_LIT(19.1813)  FL_LIT(45.7407)) -- C2' 
      (Pt FL_LIT(22.0532)  FL_LIT(18.7224)  FL_LIT(45.7273)) -- H2''
      (Pt FL_LIT(23.1307)  FL_LIT(20.2521)  FL_LIT(46.6291)) -- O2' 
      (Pt FL_LIT(22.8888)  FL_LIT(21.1051)  FL_LIT(46.2611)) -- H2' 
      (Pt FL_LIT(24.0799)  FL_LIT(18.1326)  FL_LIT(46.0700)) -- C3' 
      (Pt FL_LIT(23.6490)  FL_LIT(17.4370)  FL_LIT(46.7900)) -- H3' 
      (Pt FL_LIT(25.3329)  FL_LIT(18.7227)  FL_LIT(46.5109)) -- O3' 
      (Pt FL_LIT(22.2515)  FL_LIT(20.1624)  FL_LIT(43.6698)) -- N1  
      (Pt FL_LIT(22.4760)  FL_LIT(21.0609)  FL_LIT(42.6406)) -- N3  
      (Pt FL_LIT(23.6229)  FL_LIT(21.3462)  FL_LIT(42.3061)) -- C2  
      (Pt FL_LIT(21.3986)  FL_LIT(21.6081)  FL_LIT(42.0236)) -- C4  
      (Pt FL_LIT(20.1189)  FL_LIT(21.3012)  FL_LIT(42.3804)) -- C5  
      (Pt FL_LIT(19.1599)  FL_LIT(21.8516)  FL_LIT(41.7578)) -- C6
      (U
      (Pt FL_LIT(19.8919)  FL_LIT(20.3745)  FL_LIT(43.4387)) -- O2  
      (Pt FL_LIT(20.9790)  FL_LIT(19.8423)  FL_LIT(44.0440)) -- O4  
      (Pt FL_LIT(21.5235)  FL_LIT(22.3222)  FL_LIT(41.2097)) -- H3  
      (Pt FL_LIT(18.8732)  FL_LIT(20.1200)  FL_LIT(43.7312)) -- H5  
      (Pt FL_LIT(20.8545)  FL_LIT(19.1313)  FL_LIT(44.8608)) -- H6  
      )

rU09
  = Nuc
      (Tfo FL_LIT(-0.0317)   FL_LIT(0.1374)   FL_LIT(0.9900)  -- dgf_base_tfo
           FL_LIT(-0.3422)  FL_LIT(-0.9321)   FL_LIT(0.1184)
            FL_LIT(0.9391)  FL_LIT(-0.3351)   FL_LIT(0.0765)
          FL_LIT(-32.1929)  FL_LIT(25.8198) FL_LIT(-28.5088))
      (Tfo  FL_LIT(0.2765)  FL_LIT(-0.1121)  FL_LIT(-0.9545)  -- p_o3'_275_tfo
           FL_LIT(-0.8297)   FL_LIT(0.4733)  FL_LIT(-0.2959)
            FL_LIT(0.4850)   FL_LIT(0.8737)   FL_LIT(0.0379)
          FL_LIT(-14.7774) FL_LIT(-45.2464)  FL_LIT(21.9088))
      (Tfo  FL_LIT(0.1063)  FL_LIT(-0.6334)  FL_LIT(-0.7665)  -- p_o3'_180_tfo
           FL_LIT(-0.5932)  FL_LIT(-0.6591)   FL_LIT(0.4624)
           FL_LIT(-0.7980)   FL_LIT(0.4055)  FL_LIT(-0.4458)
           FL_LIT(43.7634)   FL_LIT(4.3296)  FL_LIT(28.4890))
      (Tfo  FL_LIT(0.7136)  FL_LIT(-0.5032)  FL_LIT(-0.4873)  -- p_o3'_60_tfo
            FL_LIT(0.6803)   FL_LIT(0.3317)   FL_LIT(0.6536)
           FL_LIT(-0.1673)  FL_LIT(-0.7979)   FL_LIT(0.5791)
          FL_LIT(-17.1858)  FL_LIT(41.4390) FL_LIT(-27.0751))
      (Pt FL_LIT(21.3880)  FL_LIT(15.0780)  FL_LIT(45.5770)) -- P   
      (Pt FL_LIT(21.9980)  FL_LIT(14.5500)  FL_LIT(46.8210)) -- O1P 
      (Pt FL_LIT(21.1450)  FL_LIT(14.0270)  FL_LIT(44.5420)) -- O2P 
      (Pt FL_LIT(22.1250)  FL_LIT(16.3600)  FL_LIT(44.9460)) -- O5' 
      (Pt FL_LIT(21.5037)  FL_LIT(16.8594)  FL_LIT(43.7323)) -- C5' 
      (Pt FL_LIT(20.8147)  FL_LIT(17.6663)  FL_LIT(43.9823)) -- H5' 
      (Pt FL_LIT(21.1086)  FL_LIT(16.0230)  FL_LIT(43.1557)) -- H5''
      (Pt FL_LIT(22.5654)  FL_LIT(17.4874)  FL_LIT(42.8616)) -- C4' 
      (Pt FL_LIT(23.0565)  FL_LIT(18.3036)  FL_LIT(43.3915)) -- H4' 
      (Pt FL_LIT(23.5375)  FL_LIT(16.5054)  FL_LIT(42.4925)) -- O4' 
      (Pt FL_LIT(23.6574)  FL_LIT(16.4257)  FL_LIT(41.0649)) -- C1' 
      (Pt FL_LIT(24.4701)  FL_LIT(17.0882)  FL_LIT(40.7671)) -- H1' 
      (Pt FL_LIT(22.3525)  FL_LIT(16.9643)  FL_LIT(40.5396)) -- C2' 
      (Pt FL_LIT(21.5993)  FL_LIT(16.1799)  FL_LIT(40.6133)) -- H2''
      (Pt FL_LIT(22.4693)  FL_LIT(17.4849)  FL_LIT(39.2515)) -- O2' 
      (Pt FL_LIT(23.0899)  FL_LIT(17.0235)  FL_LIT(38.6827)) -- H2' 
      (Pt FL_LIT(22.0341)  FL_LIT(18.0633)  FL_LIT(41.5279)) -- C3' 
      (Pt FL_LIT(20.9509)  FL_LIT(18.1709)  FL_LIT(41.5846)) -- H3' 
      (Pt FL_LIT(22.7249)  FL_LIT(19.3020)  FL_LIT(41.2100)) -- O3' 
      (Pt FL_LIT(23.8580)  FL_LIT(15.0648)  FL_LIT(40.5757)) -- N1  
      (Pt FL_LIT(25.1556)  FL_LIT(14.5982)  FL_LIT(40.4523)) -- N3  
      (Pt FL_LIT(26.1047)  FL_LIT(15.3210)  FL_LIT(40.7448)) -- C2  
      (Pt FL_LIT(25.3391)  FL_LIT(13.3315)  FL_LIT(40.0020)) -- C4  
      (Pt FL_LIT(24.2974)  FL_LIT(12.5148)  FL_LIT(39.6749)) -- C5  
      (Pt FL_LIT(24.5450)  FL_LIT(11.3410)  FL_LIT(39.2610)) -- C6
      (U
      (Pt FL_LIT(22.9633)  FL_LIT(12.9979)  FL_LIT(39.8053)) -- O2  
      (Pt FL_LIT(22.8009)  FL_LIT(14.2648)  FL_LIT(40.2524)) -- O4  
      (Pt FL_LIT(26.3414)  FL_LIT(12.9194)  FL_LIT(39.8855)) -- H3  
      (Pt FL_LIT(22.1227)  FL_LIT(12.3533)  FL_LIT(39.5486)) -- H5  
      (Pt FL_LIT(21.7989)  FL_LIT(14.6788)  FL_LIT(40.3650)) -- H6  
      )

rU10
  = Nuc
      (Tfo FL_LIT(-0.9674)   FL_LIT(0.1021)  FL_LIT(-0.2318)  -- dgf_base_tfo
           FL_LIT(-0.2514)  FL_LIT(-0.2766)   FL_LIT(0.9275)
            FL_LIT(0.0306)   FL_LIT(0.9555)   FL_LIT(0.2933)
           FL_LIT(27.8571) FL_LIT(-42.1305) FL_LIT(-24.4563))
      (Tfo  FL_LIT(0.2765)  FL_LIT(-0.1121)  FL_LIT(-0.9545)  -- p_o3'_275_tfo
           FL_LIT(-0.8297)   FL_LIT(0.4733)  FL_LIT(-0.2959)
            FL_LIT(0.4850)   FL_LIT(0.8737)   FL_LIT(0.0379)
          FL_LIT(-14.7774) FL_LIT(-45.2464)  FL_LIT(21.9088))
      (Tfo  FL_LIT(0.1063)  FL_LIT(-0.6334)  FL_LIT(-0.7665)  -- p_o3'_180_tfo
           FL_LIT(-0.5932)  FL_LIT(-0.6591)   FL_LIT(0.4624)
           FL_LIT(-0.7980)   FL_LIT(0.4055)  FL_LIT(-0.4458)
           FL_LIT(43.7634)   FL_LIT(4.3296)  FL_LIT(28.4890))
      (Tfo  FL_LIT(0.7136)  FL_LIT(-0.5032)  FL_LIT(-0.4873)  -- p_o3'_60_tfo
            FL_LIT(0.6803)   FL_LIT(0.3317)   FL_LIT(0.6536)
           FL_LIT(-0.1673)  FL_LIT(-0.7979)   FL_LIT(0.5791)
          FL_LIT(-17.1858)  FL_LIT(41.4390) FL_LIT(-27.0751))
      (Pt FL_LIT(21.3880)  FL_LIT(15.0780)  FL_LIT(45.5770)) -- P   
      (Pt FL_LIT(21.9980)  FL_LIT(14.5500)  FL_LIT(46.8210)) -- O1P 
      (Pt FL_LIT(21.1450)  FL_LIT(14.0270)  FL_LIT(44.5420)) -- O2P 
      (Pt FL_LIT(22.1250)  FL_LIT(16.3600)  FL_LIT(44.9460)) -- O5' 
      (Pt FL_LIT(23.5096)  FL_LIT(16.1227)  FL_LIT(44.5783)) -- C5' 
      (Pt FL_LIT(23.5649)  FL_LIT(15.8588)  FL_LIT(43.5222)) -- H5' 
      (Pt FL_LIT(23.9621)  FL_LIT(15.4341)  FL_LIT(45.2919)) -- H5''
      (Pt FL_LIT(24.2805)  FL_LIT(17.4138)  FL_LIT(44.7151)) -- C4' 
      (Pt FL_LIT(23.8509)  FL_LIT(18.1819)  FL_LIT(44.0720)) -- H4' 
      (Pt FL_LIT(24.2506)  FL_LIT(17.8583)  FL_LIT(46.0741)) -- O4' 
      (Pt FL_LIT(25.5830)  FL_LIT(18.0320)  FL_LIT(46.5775)) -- C1' 
      (Pt FL_LIT(25.8569)  FL_LIT(19.0761)  FL_LIT(46.4256)) -- H1' 
      (Pt FL_LIT(26.4410)  FL_LIT(17.1555)  FL_LIT(45.7033)) -- C2' 
      (Pt FL_LIT(26.3459)  FL_LIT(16.1253)  FL_LIT(46.0462)) -- H2''
      (Pt FL_LIT(27.7649)  FL_LIT(17.5888)  FL_LIT(45.6478)) -- O2' 
      (Pt FL_LIT(28.1004)  FL_LIT(17.9719)  FL_LIT(46.4616)) -- H2' 
      (Pt FL_LIT(25.7796)  FL_LIT(17.2997)  FL_LIT(44.3513)) -- C3' 
      (Pt FL_LIT(25.9478)  FL_LIT(16.3824)  FL_LIT(43.7871)) -- H3' 
      (Pt FL_LIT(26.2154)  FL_LIT(18.4984)  FL_LIT(43.6541)) -- O3' 
      (Pt FL_LIT(25.7321)  FL_LIT(17.6281)  FL_LIT(47.9726)) -- N1  
      (Pt FL_LIT(25.5136)  FL_LIT(18.5779)  FL_LIT(48.9560)) -- N3  
      (Pt FL_LIT(25.2079)  FL_LIT(19.7276)  FL_LIT(48.6503)) -- C2  
      (Pt FL_LIT(25.6482)  FL_LIT(18.1987)  FL_LIT(50.2518)) -- C4  
      (Pt FL_LIT(25.9847)  FL_LIT(16.9266)  FL_LIT(50.6092)) -- C5  
      (Pt FL_LIT(26.0918)  FL_LIT(16.6439)  FL_LIT(51.8416)) -- C6
      (U
      (Pt FL_LIT(26.2067)  FL_LIT(15.9515)  FL_LIT(49.5943)) -- O2  
      (Pt FL_LIT(26.0713)  FL_LIT(16.3497)  FL_LIT(48.3080)) -- O4  
      (Pt FL_LIT(25.4890)  FL_LIT(18.9105)  FL_LIT(51.0618)) -- H3  
      (Pt FL_LIT(26.4742)  FL_LIT(14.9310)  FL_LIT(49.8682)) -- H5  
      (Pt FL_LIT(26.2346)  FL_LIT(15.6394)  FL_LIT(47.4975)) -- H6  
      )

rUs = [rU01,rU02,rU03,rU04,rU05,rU06,rU07,rU08,rU09,rU10]

rU'
  = Nuc
      (Tfo FL_LIT(-0.0109)   FL_LIT(0.5907)   FL_LIT(0.8068)  -- dgf_base_tfo
            FL_LIT(0.2217)  FL_LIT(-0.7853)   FL_LIT(0.5780)
            FL_LIT(0.9751)   FL_LIT(0.1852)  FL_LIT(-0.1224)
           FL_LIT(-1.4225) FL_LIT(-11.0956)  FL_LIT(-2.5217))
      (Tfo FL_LIT(-0.8313)  FL_LIT(-0.4738)  FL_LIT(-0.2906)  -- p_o3'_275_tfo
            FL_LIT(0.0649)   FL_LIT(0.4366)  FL_LIT(-0.8973)
            FL_LIT(0.5521)  FL_LIT(-0.7648)  FL_LIT(-0.3322)
            FL_LIT(1.6833)   FL_LIT(6.8060)  FL_LIT(-7.0011))
      (Tfo  FL_LIT(0.3445)  FL_LIT(-0.7630)   FL_LIT(0.5470)  -- p_o3'_180_tfo
           FL_LIT(-0.4628)  FL_LIT(-0.6450)  FL_LIT(-0.6082)
            FL_LIT(0.8168)  FL_LIT(-0.0436)  FL_LIT(-0.5753)
           FL_LIT(-6.8179)  FL_LIT(-3.9778)  FL_LIT(-5.9887))
      (Tfo  FL_LIT(0.5855)   FL_LIT(0.7931)  FL_LIT(-0.1682)  -- p_o3'_60_tfo
            FL_LIT(0.8103)  FL_LIT(-0.5790)   FL_LIT(0.0906)
           FL_LIT(-0.0255)  FL_LIT(-0.1894)  FL_LIT(-0.9816)
            FL_LIT(6.1203)  FL_LIT(-7.1051)   FL_LIT(3.1984))
      (Pt  FL_LIT(2.6760)  FL_LIT(-8.4960)   FL_LIT(3.2880)) -- P   
      (Pt  FL_LIT(1.4950)  FL_LIT(-7.6230)   FL_LIT(3.4770)) -- O1P 
      (Pt  FL_LIT(2.9490)  FL_LIT(-9.4640)   FL_LIT(4.3740)) -- O2P 
      (Pt  FL_LIT(3.9730)  FL_LIT(-7.5950)   FL_LIT(3.0340)) -- O5' 
      (Pt  FL_LIT(5.2430)  FL_LIT(-8.2420)   FL_LIT(2.8260)) -- C5' 
      (Pt  FL_LIT(5.1974)  FL_LIT(-8.8497)   FL_LIT(1.9223)) -- H5' 
      (Pt  FL_LIT(5.5548)  FL_LIT(-8.7348)   FL_LIT(3.7469)) -- H5''
      (Pt  FL_LIT(6.3140)  FL_LIT(-7.2060)   FL_LIT(2.5510)) -- C4' 
      (Pt  FL_LIT(5.8744)  FL_LIT(-6.2116)   FL_LIT(2.4731)) -- H4' 
      (Pt  FL_LIT(7.2798)  FL_LIT(-7.2260)   FL_LIT(3.6420)) -- O4' 
      (Pt  FL_LIT(8.5733)  FL_LIT(-6.9410)   FL_LIT(3.1329)) -- C1' 
      (Pt  FL_LIT(8.9047)  FL_LIT(-6.0374)   FL_LIT(3.6446)) -- H1' 
      (Pt  FL_LIT(8.4429)  FL_LIT(-6.6596)   FL_LIT(1.6327)) -- C2' 
      (Pt  FL_LIT(9.2880)  FL_LIT(-7.1071)   FL_LIT(1.1096)) -- H2''
      (Pt  FL_LIT(8.2502)  FL_LIT(-5.2799)   FL_LIT(1.4754)) -- O2' 
      (Pt  FL_LIT(8.7676)  FL_LIT(-4.7284)   FL_LIT(2.0667)) -- H2' 
      (Pt  FL_LIT(7.1642)  FL_LIT(-7.4416)   FL_LIT(1.3021)) -- C3' 
      (Pt  FL_LIT(7.4125)  FL_LIT(-8.5002)   FL_LIT(1.2260)) -- H3' 
      (Pt  FL_LIT(6.5160)  FL_LIT(-6.9772)   FL_LIT(0.1267)) -- O3' 
      (Pt  FL_LIT(9.4531)  FL_LIT(-8.1107)   FL_LIT(3.4087)) -- N1  
      (Pt FL_LIT(11.5931)  FL_LIT(-9.0015)   FL_LIT(3.6357)) -- N3  
      (Pt FL_LIT(10.8101)  FL_LIT(-7.8950)   FL_LIT(3.3748)) -- C2  
      (Pt FL_LIT(11.1439) FL_LIT(-10.2744)   FL_LIT(3.9206)) -- C4  
      (Pt  FL_LIT(9.7056) FL_LIT(-10.4026)   FL_LIT(3.9332)) -- C5  
      (Pt  FL_LIT(8.9192)  FL_LIT(-9.3419)   FL_LIT(3.6833)) -- C6
      (U
      (Pt FL_LIT(11.3013)  FL_LIT(-6.8063)   FL_LIT(3.1326)) -- O2  
      (Pt FL_LIT(11.9431) FL_LIT(-11.1876)   FL_LIT(4.1375)) -- O4  
      (Pt FL_LIT(12.5840)  FL_LIT(-8.8673)   FL_LIT(3.6158)) -- H3  
      (Pt  FL_LIT(9.2891) FL_LIT(-11.2898)   FL_LIT(4.1313)) -- H5  
      (Pt  FL_LIT(7.9263)  FL_LIT(-9.4537)   FL_LIT(3.6977)) -- H6  
      )

