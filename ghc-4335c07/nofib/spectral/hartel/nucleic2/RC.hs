#include "unboxery.h"

module RC(rC,rCs) where
import Types

rC
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
      (C
      (Pt  FL_LIT(2.0187)  FL_LIT(-1.8047)   FL_LIT(0.5874)) -- N4  
      (Pt  FL_LIT(6.5470)  FL_LIT(-2.5560)   FL_LIT(0.6290)) -- O2  
      (Pt  FL_LIT(1.0684)  FL_LIT(-2.1236)   FL_LIT(0.7109)) -- H41 
      (Pt  FL_LIT(2.2344)  FL_LIT(-0.8560)   FL_LIT(0.3162)) -- H42 
      (Pt  FL_LIT(1.8797)  FL_LIT(-4.4972)   FL_LIT(1.3404)) -- H5  
      (Pt  FL_LIT(3.8479)  FL_LIT(-5.8742)   FL_LIT(1.6480)) -- H6  
      )

rC01
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
      (C
      (Pt  FL_LIT(2.1040)  FL_LIT(-1.7437)   FL_LIT(0.6331)) -- N4  
      (Pt  FL_LIT(6.6267)  FL_LIT(-2.5166)   FL_LIT(0.7728)) -- O2  
      (Pt  FL_LIT(1.1496)  FL_LIT(-2.0600)   FL_LIT(0.7287)) -- H41 
      (Pt  FL_LIT(2.3303)  FL_LIT(-0.7921)   FL_LIT(0.3815)) -- H42 
      (Pt  FL_LIT(1.9353)  FL_LIT(-4.4465)   FL_LIT(1.3419)) -- H5  
      (Pt  FL_LIT(3.8895)  FL_LIT(-5.8371)   FL_LIT(1.6762)) -- H6  
      )

rC02
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
      (C
      (Pt  FL_LIT(7.9033) FL_LIT(-10.6371)  FL_LIT(-1.3010)) -- N4  
      (Pt  FL_LIT(9.5840)  FL_LIT(-6.8186)   FL_LIT(0.6136)) -- O2  
      (Pt  FL_LIT(7.2009) FL_LIT(-11.3604)  FL_LIT(-1.3619)) -- H41 
      (Pt  FL_LIT(8.7058) FL_LIT(-10.6168)  FL_LIT(-1.9140)) -- H42 
      (Pt  FL_LIT(5.8585) FL_LIT(-10.3083)   FL_LIT(0.5822)) -- H5  
      (Pt  FL_LIT(5.8197)  FL_LIT(-8.4773)   FL_LIT(2.1667)) -- H6  
      )

rC03
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
      (C
      (Pt  FL_LIT(7.1702)  FL_LIT(-6.7511)   FL_LIT(8.7402)) -- N4  
      (Pt  FL_LIT(8.7747)  FL_LIT(-4.3728)   FL_LIT(5.1568)) -- O2  
      (Pt  FL_LIT(6.4741)  FL_LIT(-7.3461)   FL_LIT(9.1662)) -- H41 
      (Pt  FL_LIT(7.9889)  FL_LIT(-6.4396)   FL_LIT(9.2429)) -- H42 
      (Pt  FL_LIT(5.0736)  FL_LIT(-7.3713)   FL_LIT(6.9922)) -- H5  
      (Pt  FL_LIT(4.9784)  FL_LIT(-6.5473)   FL_LIT(4.7170)) -- H6  
      )

rC04
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
      (C
      (Pt  FL_LIT(2.0216)  FL_LIT(-1.8941)   FL_LIT(0.4804)) -- N4  
      (Pt  FL_LIT(5.7005)  FL_LIT(-4.2164)  FL_LIT(-0.9842)) -- O2  
      (Pt  FL_LIT(1.4067)  FL_LIT(-1.5873)   FL_LIT(1.2205)) -- H41 
      (Pt  FL_LIT(1.8721)  FL_LIT(-1.6319)  FL_LIT(-0.4835)) -- H42 
      (Pt  FL_LIT(2.8048)  FL_LIT(-2.8507)   FL_LIT(2.9918)) -- H5  
      (Pt  FL_LIT(4.7491)  FL_LIT(-4.2593)   FL_LIT(3.3085)) -- H6  
      )

rC05
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
      (C
      (Pt  FL_LIT(7.8849) FL_LIT(-10.7881)  FL_LIT(-1.1289)) -- N4  
      (Pt  FL_LIT(9.3993)  FL_LIT(-8.5377)   FL_LIT(2.5743)) -- O2  
      (Pt  FL_LIT(7.2499) FL_LIT(-10.8809)  FL_LIT(-1.9088)) -- H41 
      (Pt  FL_LIT(8.6122) FL_LIT(-11.4649)  FL_LIT(-0.9468)) -- H42 
      (Pt  FL_LIT(6.0317)  FL_LIT(-8.6941)  FL_LIT(-1.2588)) -- H5  
      (Pt  FL_LIT(5.9901)  FL_LIT(-6.8809)   FL_LIT(0.3459)) -- H6  
      )

rC06
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
      (C
      (Pt  FL_LIT(6.9614)  FL_LIT(-6.6648)   FL_LIT(8.7815)) -- N4  
      (Pt  FL_LIT(6.4083)  FL_LIT(-3.3696)   FL_LIT(5.6340)) -- O2  
      (Pt  FL_LIT(7.1329)  FL_LIT(-7.6280)   FL_LIT(9.0324)) -- H41 
      (Pt  FL_LIT(6.8204)  FL_LIT(-5.9469)   FL_LIT(9.4777)) -- H42 
      (Pt  FL_LIT(7.2954)  FL_LIT(-8.3135)   FL_LIT(6.5440)) -- H5  
      (Pt  FL_LIT(7.1753)  FL_LIT(-7.4798)   FL_LIT(4.2735)) -- H6  
      )

rC07
  = Nuc
      (Tfo  FL_LIT(0.0033)   FL_LIT(0.2720)  FL_LIT(-0.9623)  -- dgf_base_tfo
            FL_LIT(0.3013)  FL_LIT(-0.9179)  FL_LIT(-0.2584)
           FL_LIT(-0.9535)  FL_LIT(-0.2891)  FL_LIT(-0.0850)
           FL_LIT(43.0403)  FL_LIT(13.7233)  FL_LIT(34.5710))
      (Tfo  FL_LIT(0.9187)   FL_LIT(0.2887)   FL_LIT(0.2694)  -- p_o3'_275_tfo
            FL_LIT(0.0302)  FL_LIT(-0.7316)   FL_LIT(0.6811)
            FL_LIT(0.3938)  FL_LIT(-0.6176)  FL_LIT(-0.6808)
          FL_LIT(-48.4330)  FL_LIT(26.3254)  FL_LIT(13.6383))
      (Tfo FL_LIT(-0.1504)   FL_LIT(0.7744)  FL_LIT(-0.6145)  -- p_o3'_180_tfo
            FL_LIT(0.7581)   FL_LIT(0.4893)   FL_LIT(0.4311)
            FL_LIT(0.6345)  FL_LIT(-0.4010)  FL_LIT(-0.6607)
          FL_LIT(-31.9784) FL_LIT(-13.4285)  FL_LIT(44.9650))
      (Tfo FL_LIT(-0.6236)  FL_LIT(-0.7810)  FL_LIT(-0.0337)  -- p_o3'_60_tfo
           FL_LIT(-0.6890)   FL_LIT(0.5694)  FL_LIT(-0.4484)
            FL_LIT(0.3694)  FL_LIT(-0.2564)  FL_LIT(-0.8932)
           FL_LIT(12.1105)  FL_LIT(30.8774)  FL_LIT(46.0946))
      (Pt FL_LIT(33.3400)  FL_LIT(11.0980)  FL_LIT(46.1750)) -- P   
      (Pt FL_LIT(34.5130)  FL_LIT(10.2320)  FL_LIT(46.4660)) -- O1P 
      (Pt FL_LIT(33.4130)  FL_LIT(12.3960)  FL_LIT(46.9340)) -- O2P 
      (Pt FL_LIT(31.9810)  FL_LIT(10.3390)  FL_LIT(46.4820)) -- O5' 
      (Pt FL_LIT(30.8152)  FL_LIT(11.1619)  FL_LIT(46.2003)) -- C5' 
      (Pt FL_LIT(30.4519)  FL_LIT(10.9454)  FL_LIT(45.1957)) -- H5' 
      (Pt FL_LIT(31.0379)  FL_LIT(12.2016)  FL_LIT(46.4400)) -- H5''
      (Pt FL_LIT(29.7081)  FL_LIT(10.7448)  FL_LIT(47.1428)) -- C4' 
      (Pt FL_LIT(28.8710)  FL_LIT(11.4416)  FL_LIT(47.0982)) -- H4' 
      (Pt FL_LIT(29.2550)   FL_LIT(9.4394)  FL_LIT(46.8162)) -- O4' 
      (Pt FL_LIT(29.3907)   FL_LIT(8.5625)  FL_LIT(47.9460)) -- C1' 
      (Pt FL_LIT(28.4416)   FL_LIT(8.5669)  FL_LIT(48.4819)) -- H1' 
      (Pt FL_LIT(30.4468)   FL_LIT(9.2031)  FL_LIT(48.7952)) -- C2' 
      (Pt FL_LIT(31.4222)   FL_LIT(8.9651)  FL_LIT(48.3709)) -- H2''
      (Pt FL_LIT(30.3701)   FL_LIT(8.9157)  FL_LIT(50.1624)) -- O2' 
      (Pt FL_LIT(30.0652)   FL_LIT(8.0304)  FL_LIT(50.3740)) -- H2' 
      (Pt FL_LIT(30.1622)  FL_LIT(10.6879)  FL_LIT(48.6120)) -- C3' 
      (Pt FL_LIT(31.0952)  FL_LIT(11.2399)  FL_LIT(48.7254)) -- H3' 
      (Pt FL_LIT(29.1076)  FL_LIT(11.1535)  FL_LIT(49.4702)) -- O3' 
      (Pt FL_LIT(29.7883)   FL_LIT(7.2209)  FL_LIT(47.5235)) -- N1  
      (Pt FL_LIT(29.1825)   FL_LIT(5.0438)  FL_LIT(46.8275)) -- N3  
      (Pt FL_LIT(28.8008)   FL_LIT(6.2912)  FL_LIT(47.2263)) -- C2  
      (Pt FL_LIT(30.4888)   FL_LIT(4.6890)  FL_LIT(46.7186)) -- C4  
      (Pt FL_LIT(31.5034)   FL_LIT(5.6405)  FL_LIT(47.0249)) -- C5  
      (Pt FL_LIT(31.1091)   FL_LIT(6.8691)  FL_LIT(47.4156)) -- C6
      (C
      (Pt FL_LIT(30.8109)   FL_LIT(3.4584)  FL_LIT(46.3336)) -- N4  
      (Pt FL_LIT(27.6171)   FL_LIT(6.5989)  FL_LIT(47.3189)) -- O2  
      (Pt FL_LIT(31.7923)   FL_LIT(3.2301)  FL_LIT(46.2638)) -- H41 
      (Pt FL_LIT(30.0880)   FL_LIT(2.7857)  FL_LIT(46.1215)) -- H42 
      (Pt FL_LIT(32.5542)   FL_LIT(5.3634)  FL_LIT(46.9395)) -- H5  
      (Pt FL_LIT(31.8523)   FL_LIT(7.6279)  FL_LIT(47.6603)) -- H6  
      )

rC08
  = Nuc
      (Tfo  FL_LIT(0.0797)  FL_LIT(-0.6026)  FL_LIT(-0.7941)  -- dgf_base_tfo
            FL_LIT(0.7939)   FL_LIT(0.5201)  FL_LIT(-0.3150)
            FL_LIT(0.6028)  FL_LIT(-0.6054)   FL_LIT(0.5198)
          FL_LIT(-36.8341)  FL_LIT(41.5293)   FL_LIT(1.6628))
      (Tfo  FL_LIT(0.9187)   FL_LIT(0.2887)   FL_LIT(0.2694)  -- p_o3'_275_tfo
            FL_LIT(0.0302)  FL_LIT(-0.7316)   FL_LIT(0.6811)
            FL_LIT(0.3938)  FL_LIT(-0.6176)  FL_LIT(-0.6808)
          FL_LIT(-48.4330)  FL_LIT(26.3254)  FL_LIT(13.6383))
      (Tfo FL_LIT(-0.1504)   FL_LIT(0.7744)  FL_LIT(-0.6145)  -- p_o3'_180_tfo
            FL_LIT(0.7581)   FL_LIT(0.4893)   FL_LIT(0.4311)
            FL_LIT(0.6345)  FL_LIT(-0.4010)  FL_LIT(-0.6607)
          FL_LIT(-31.9784) FL_LIT(-13.4285)  FL_LIT(44.9650))
      (Tfo FL_LIT(-0.6236)  FL_LIT(-0.7810)  FL_LIT(-0.0337)  -- p_o3'_60_tfo
           FL_LIT(-0.6890)   FL_LIT(0.5694)  FL_LIT(-0.4484)
            FL_LIT(0.3694)  FL_LIT(-0.2564)  FL_LIT(-0.8932)
           FL_LIT(12.1105)  FL_LIT(30.8774)  FL_LIT(46.0946))
      (Pt FL_LIT(33.3400)  FL_LIT(11.0980)  FL_LIT(46.1750)) -- P   
      (Pt FL_LIT(34.5130)  FL_LIT(10.2320)  FL_LIT(46.4660)) -- O1P 
      (Pt FL_LIT(33.4130)  FL_LIT(12.3960)  FL_LIT(46.9340)) -- O2P 
      (Pt FL_LIT(31.9810)  FL_LIT(10.3390)  FL_LIT(46.4820)) -- O5' 
      (Pt FL_LIT(31.8779)   FL_LIT(9.9369)  FL_LIT(47.8760)) -- C5' 
      (Pt FL_LIT(31.3239)  FL_LIT(10.6931)  FL_LIT(48.4322)) -- H5' 
      (Pt FL_LIT(32.8647)   FL_LIT(9.6624)  FL_LIT(48.2489)) -- H5''
      (Pt FL_LIT(31.0429)   FL_LIT(8.6773)  FL_LIT(47.9401)) -- C4' 
      (Pt FL_LIT(31.0779)   FL_LIT(8.2331)  FL_LIT(48.9349)) -- H4' 
      (Pt FL_LIT(29.6956)   FL_LIT(8.9669)  FL_LIT(47.5983)) -- O4' 
      (Pt FL_LIT(29.2784)   FL_LIT(8.1700)  FL_LIT(46.4782)) -- C1' 
      (Pt FL_LIT(28.8006)   FL_LIT(7.2731)  FL_LIT(46.8722)) -- H1' 
      (Pt FL_LIT(30.5544)   FL_LIT(7.7940)  FL_LIT(45.7875)) -- C2' 
      (Pt FL_LIT(30.8837)   FL_LIT(8.6410)  FL_LIT(45.1856)) -- H2''
      (Pt FL_LIT(30.5100)   FL_LIT(6.6007)  FL_LIT(45.0582)) -- O2' 
      (Pt FL_LIT(29.6694)   FL_LIT(6.4168)  FL_LIT(44.6326)) -- H2' 
      (Pt FL_LIT(31.5146)   FL_LIT(7.5954)  FL_LIT(46.9527)) -- C3' 
      (Pt FL_LIT(32.5255)   FL_LIT(7.8261)  FL_LIT(46.6166)) -- H3' 
      (Pt FL_LIT(31.3876)   FL_LIT(6.2951)  FL_LIT(47.5516)) -- O3' 
      (Pt FL_LIT(28.3976)   FL_LIT(8.9302)  FL_LIT(45.5933)) -- N1  
      (Pt FL_LIT(26.2155)   FL_LIT(9.6135)  FL_LIT(44.9910)) -- N3  
      (Pt FL_LIT(27.0281)   FL_LIT(8.8961)  FL_LIT(45.8192)) -- C2  
      (Pt FL_LIT(26.7044)  FL_LIT(10.3489)  FL_LIT(43.9595)) -- C4  
      (Pt FL_LIT(28.1088)  FL_LIT(10.3837)  FL_LIT(43.7247)) -- C5  
      (Pt FL_LIT(28.8978)   FL_LIT(9.6708)  FL_LIT(44.5535)) -- C6
      (C
      (Pt FL_LIT(25.8715)  FL_LIT(11.0249)  FL_LIT(43.1749)) -- N4  
      (Pt FL_LIT(26.5733)   FL_LIT(8.2371)  FL_LIT(46.7484)) -- O2  
      (Pt FL_LIT(26.2707)  FL_LIT(11.5609)  FL_LIT(42.4177)) -- H41 
      (Pt FL_LIT(24.8760)  FL_LIT(10.9939)  FL_LIT(43.3427)) -- H42 
      (Pt FL_LIT(28.5089)  FL_LIT(10.9722)  FL_LIT(42.8990)) -- H5  
      (Pt FL_LIT(29.9782)   FL_LIT(9.6687)  FL_LIT(44.4097)) -- H6  
      )

rC09
  = Nuc
      (Tfo  FL_LIT(0.8727)   FL_LIT(0.4760)  FL_LIT(-0.1091)  -- dgf_base_tfo
           FL_LIT(-0.4188)   FL_LIT(0.6148)  FL_LIT(-0.6682)
           FL_LIT(-0.2510)   FL_LIT(0.6289)   FL_LIT(0.7359)
           FL_LIT(-8.1687) FL_LIT(-52.0761) FL_LIT(-25.0726))
      (Tfo  FL_LIT(0.9187)   FL_LIT(0.2887)   FL_LIT(0.2694)  -- p_o3'_275_tfo
            FL_LIT(0.0302)  FL_LIT(-0.7316)   FL_LIT(0.6811)
            FL_LIT(0.3938)  FL_LIT(-0.6176)  FL_LIT(-0.6808)
          FL_LIT(-48.4330)  FL_LIT(26.3254)  FL_LIT(13.6383))
      (Tfo FL_LIT(-0.1504)   FL_LIT(0.7744)  FL_LIT(-0.6145)  -- p_o3'_180_tfo
            FL_LIT(0.7581)   FL_LIT(0.4893)   FL_LIT(0.4311)
            FL_LIT(0.6345)  FL_LIT(-0.4010)  FL_LIT(-0.6607)
          FL_LIT(-31.9784) FL_LIT(-13.4285)  FL_LIT(44.9650))
      (Tfo FL_LIT(-0.6236)  FL_LIT(-0.7810)  FL_LIT(-0.0337)  -- p_o3'_60_tfo
           FL_LIT(-0.6890)   FL_LIT(0.5694)  FL_LIT(-0.4484)
            FL_LIT(0.3694)  FL_LIT(-0.2564)  FL_LIT(-0.8932)
           FL_LIT(12.1105)  FL_LIT(30.8774)  FL_LIT(46.0946))
      (Pt FL_LIT(33.3400)  FL_LIT(11.0980)  FL_LIT(46.1750)) -- P   
      (Pt FL_LIT(34.5130)  FL_LIT(10.2320)  FL_LIT(46.4660)) -- O1P 
      (Pt FL_LIT(33.4130)  FL_LIT(12.3960)  FL_LIT(46.9340)) -- O2P 
      (Pt FL_LIT(31.9810)  FL_LIT(10.3390)  FL_LIT(46.4820)) -- O5' 
      (Pt FL_LIT(30.8152)  FL_LIT(11.1619)  FL_LIT(46.2003)) -- C5' 
      (Pt FL_LIT(30.4519)  FL_LIT(10.9454)  FL_LIT(45.1957)) -- H5' 
      (Pt FL_LIT(31.0379)  FL_LIT(12.2016)  FL_LIT(46.4400)) -- H5''
      (Pt FL_LIT(29.7081)  FL_LIT(10.7448)  FL_LIT(47.1428)) -- C4' 
      (Pt FL_LIT(29.4506)   FL_LIT(9.6945)  FL_LIT(47.0059)) -- H4' 
      (Pt FL_LIT(30.1045)  FL_LIT(10.9634)  FL_LIT(48.4885)) -- O4' 
      (Pt FL_LIT(29.1794)  FL_LIT(11.8418)  FL_LIT(49.1490)) -- C1' 
      (Pt FL_LIT(28.4388)  FL_LIT(11.2210)  FL_LIT(49.6533)) -- H1' 
      (Pt FL_LIT(28.5211)  FL_LIT(12.6008)  FL_LIT(48.0367)) -- C2' 
      (Pt FL_LIT(29.1947)  FL_LIT(13.3949)  FL_LIT(47.7147)) -- H2''
      (Pt FL_LIT(27.2316)  FL_LIT(13.0683)  FL_LIT(48.3134)) -- O2' 
      (Pt FL_LIT(27.0851)  FL_LIT(13.3391)  FL_LIT(49.2227)) -- H2' 
      (Pt FL_LIT(28.4131)  FL_LIT(11.5507)  FL_LIT(46.9391)) -- C3' 
      (Pt FL_LIT(28.4451)  FL_LIT(12.0512)  FL_LIT(45.9713)) -- H3' 
      (Pt FL_LIT(27.2707)  FL_LIT(10.6955)  FL_LIT(47.1097)) -- O3' 
      (Pt FL_LIT(29.8751)  FL_LIT(12.7405)  FL_LIT(50.0682)) -- N1  
      (Pt FL_LIT(30.7172)  FL_LIT(13.1841)  FL_LIT(52.2328)) -- N3  
      (Pt FL_LIT(30.0617)  FL_LIT(12.3404)  FL_LIT(51.3847)) -- C2  
      (Pt FL_LIT(31.1834)  FL_LIT(14.3941)  FL_LIT(51.8297)) -- C4  
      (Pt FL_LIT(30.9913)  FL_LIT(14.8074)  FL_LIT(50.4803)) -- C5  
      (Pt FL_LIT(30.3434)  FL_LIT(13.9610)  FL_LIT(49.6548)) -- C6
      (C
      (Pt FL_LIT(31.8090)  FL_LIT(15.1847)  FL_LIT(52.6957)) -- N4  
      (Pt FL_LIT(29.6470)  FL_LIT(11.2494)  FL_LIT(51.7616)) -- O2  
      (Pt FL_LIT(32.1422)  FL_LIT(16.0774)  FL_LIT(52.3606)) -- H41 
      (Pt FL_LIT(31.9392)  FL_LIT(14.8893)  FL_LIT(53.6527)) -- H42 
      (Pt FL_LIT(31.3632)  FL_LIT(15.7771)  FL_LIT(50.1491)) -- H5  
      (Pt FL_LIT(30.1742)  FL_LIT(14.2374)  FL_LIT(48.6141)) -- H6  
      )

rC10
  = Nuc
      (Tfo  FL_LIT(0.1549)   FL_LIT(0.8710)  FL_LIT(-0.4663)  -- dgf_base_tfo
            FL_LIT(0.6768)  FL_LIT(-0.4374)  FL_LIT(-0.5921)
           FL_LIT(-0.7197)  FL_LIT(-0.2239)  FL_LIT(-0.6572)
           FL_LIT(25.2447) FL_LIT(-14.1920)  FL_LIT(50.3201))
      (Tfo  FL_LIT(0.9187)   FL_LIT(0.2887)   FL_LIT(0.2694)  -- p_o3'_275_tfo
            FL_LIT(0.0302)  FL_LIT(-0.7316)   FL_LIT(0.6811)
            FL_LIT(0.3938)  FL_LIT(-0.6176)  FL_LIT(-0.6808)
          FL_LIT(-48.4330)  FL_LIT(26.3254)  FL_LIT(13.6383))
      (Tfo FL_LIT(-0.1504)   FL_LIT(0.7744)  FL_LIT(-0.6145)  -- p_o3'_180_tfo
            FL_LIT(0.7581)   FL_LIT(0.4893)   FL_LIT(0.4311)
            FL_LIT(0.6345)  FL_LIT(-0.4010)  FL_LIT(-0.6607)
          FL_LIT(-31.9784) FL_LIT(-13.4285)  FL_LIT(44.9650))
      (Tfo FL_LIT(-0.6236)  FL_LIT(-0.7810)  FL_LIT(-0.0337)  -- p_o3'_60_tfo
           FL_LIT(-0.6890)   FL_LIT(0.5694)  FL_LIT(-0.4484)
            FL_LIT(0.3694)  FL_LIT(-0.2564)  FL_LIT(-0.8932)
           FL_LIT(12.1105)  FL_LIT(30.8774)  FL_LIT(46.0946))
      (Pt FL_LIT(33.3400)  FL_LIT(11.0980)  FL_LIT(46.1750)) -- P   
      (Pt FL_LIT(34.5130)  FL_LIT(10.2320)  FL_LIT(46.4660)) -- O1P 
      (Pt FL_LIT(33.4130)  FL_LIT(12.3960)  FL_LIT(46.9340)) -- O2P 
      (Pt FL_LIT(31.9810)  FL_LIT(10.3390)  FL_LIT(46.4820)) -- O5' 
      (Pt FL_LIT(31.8779)   FL_LIT(9.9369)  FL_LIT(47.8760)) -- C5' 
      (Pt FL_LIT(31.3239)  FL_LIT(10.6931)  FL_LIT(48.4322)) -- H5' 
      (Pt FL_LIT(32.8647)   FL_LIT(9.6624)  FL_LIT(48.2489)) -- H5''
      (Pt FL_LIT(31.0429)   FL_LIT(8.6773)  FL_LIT(47.9401)) -- C4' 
      (Pt FL_LIT(30.0440)   FL_LIT(8.8473)  FL_LIT(47.5383)) -- H4' 
      (Pt FL_LIT(31.6749)   FL_LIT(7.6351)  FL_LIT(47.2119)) -- O4' 
      (Pt FL_LIT(31.9159)   FL_LIT(6.5022)  FL_LIT(48.0616)) -- C1' 
      (Pt FL_LIT(31.0691)   FL_LIT(5.8243)  FL_LIT(47.9544)) -- H1' 
      (Pt FL_LIT(31.9300)   FL_LIT(7.0685)  FL_LIT(49.4493)) -- C2' 
      (Pt FL_LIT(32.9024)   FL_LIT(7.5288)  FL_LIT(49.6245)) -- H2''
      (Pt FL_LIT(31.5672)   FL_LIT(6.1750)  FL_LIT(50.4632)) -- O2' 
      (Pt FL_LIT(31.8416)   FL_LIT(5.2663)  FL_LIT(50.3200)) -- H2' 
      (Pt FL_LIT(30.8618)   FL_LIT(8.1514)  FL_LIT(49.3749)) -- C3' 
      (Pt FL_LIT(31.1122)   FL_LIT(8.9396)  FL_LIT(50.0850)) -- H3' 
      (Pt FL_LIT(29.5351)   FL_LIT(7.6245)  FL_LIT(49.5409)) -- O3' 
      (Pt FL_LIT(33.1890)   FL_LIT(5.8629)  FL_LIT(47.7343)) -- N1  
      (Pt FL_LIT(34.4004)   FL_LIT(4.2636)  FL_LIT(46.4828)) -- N3  
      (Pt FL_LIT(33.2062)   FL_LIT(4.8497)  FL_LIT(46.7851)) -- C2  
      (Pt FL_LIT(35.5600)   FL_LIT(4.6374)  FL_LIT(47.0822)) -- C4  
      (Pt FL_LIT(35.5444)   FL_LIT(5.6751)  FL_LIT(48.0577)) -- C5  
      (Pt FL_LIT(34.3565)   FL_LIT(6.2450)  FL_LIT(48.3432)) -- C6
      (C
      (Pt FL_LIT(36.6977)   FL_LIT(4.0305)  FL_LIT(46.7598)) -- N4  
      (Pt FL_LIT(32.1661)   FL_LIT(4.5034)  FL_LIT(46.2348)) -- O2  
      (Pt FL_LIT(37.5405)   FL_LIT(4.3347)  FL_LIT(47.2259)) -- H41 
      (Pt FL_LIT(36.7033)   FL_LIT(3.2923)  FL_LIT(46.0706)) -- H42 
      (Pt FL_LIT(36.4713)   FL_LIT(5.9811)  FL_LIT(48.5428)) -- H5  
      (Pt FL_LIT(34.2986)   FL_LIT(7.0426)  FL_LIT(49.0839)) -- H6  
      )

rCs = [rC01,rC02,rC03,rC04,rC05,rC06,rC07,rC08,rC09,rC10]

