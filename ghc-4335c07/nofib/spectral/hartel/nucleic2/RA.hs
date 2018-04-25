#include "unboxery.h"

module RA(rA,rAs) where
import Types

rA
  = Nuc
      (Tfo FL_LIT(-0.0018)  FL_LIT(-0.8207)   FL_LIT(0.5714)  -- dgf_base_tfo
            FL_LIT(0.2679)  FL_LIT(-0.5509)  FL_LIT(-0.7904)
            FL_LIT(0.9634)   FL_LIT(0.1517)   FL_LIT(0.2209)
            FL_LIT(0.0073)   FL_LIT(8.4030)   FL_LIT(0.6232))
      (Tfo FL_LIT(-0.8143)  FL_LIT(-0.5091)  FL_LIT(-0.2788)  -- p_o3'_275_tfo
           FL_LIT(-0.0433)  FL_LIT(-0.4257)   FL_LIT(0.9038)
           FL_LIT(-0.5788)   FL_LIT(0.7480)   FL_LIT(0.3246)
            FL_LIT(1.5227)   FL_LIT(6.9114)  FL_LIT(-7.0765))
      (Tfo  FL_LIT(0.3822)  FL_LIT(-0.7477)   FL_LIT(0.5430)  -- p_o3'_180_tfo
            FL_LIT(0.4552)   FL_LIT(0.6637)   FL_LIT(0.5935)
           FL_LIT(-0.8042)   FL_LIT(0.0203)   FL_LIT(0.5941)
           FL_LIT(-6.9472)  FL_LIT(-4.1186)  FL_LIT(-5.9108))
      (Tfo  FL_LIT(0.5640)   FL_LIT(0.8007)  FL_LIT(-0.2022)  -- p_o3'_60_tfo
           FL_LIT(-0.8247)   FL_LIT(0.5587)  FL_LIT(-0.0878)
            FL_LIT(0.0426)   FL_LIT(0.2162)   FL_LIT(0.9754)
            FL_LIT(6.2694)  FL_LIT(-7.0540)   FL_LIT(3.3316))
      (Pt  FL_LIT(2.8930)   FL_LIT(8.5380)  FL_LIT(-3.3280)) -- P   
      (Pt  FL_LIT(1.6980)   FL_LIT(7.6960)  FL_LIT(-3.5570)) -- O1P 
      (Pt  FL_LIT(3.2260)   FL_LIT(9.5010)  FL_LIT(-4.4020)) -- O2P 
      (Pt  FL_LIT(4.1590)   FL_LIT(7.6040)  FL_LIT(-3.0340)) -- O5' 
      (Pt  FL_LIT(5.4550)   FL_LIT(8.2120)  FL_LIT(-2.8810)) -- C5' 
      (Pt  FL_LIT(5.4546)   FL_LIT(8.8508)  FL_LIT(-1.9978)) -- H5' 
      (Pt  FL_LIT(5.7588)   FL_LIT(8.6625)  FL_LIT(-3.8259)) -- H5''
      (Pt  FL_LIT(6.4970)   FL_LIT(7.1480)  FL_LIT(-2.5980)) -- C4' 
      (Pt  FL_LIT(7.4896)   FL_LIT(7.5919)  FL_LIT(-2.5214)) -- H4' 
      (Pt  FL_LIT(6.1630)   FL_LIT(6.4860)  FL_LIT(-1.3440)) -- O4' 
      (Pt  FL_LIT(6.5400)   FL_LIT(5.1200)  FL_LIT(-1.4190)) -- C1' 
      (Pt  FL_LIT(7.2763)   FL_LIT(4.9681)  FL_LIT(-0.6297)) -- H1' 
      (Pt  FL_LIT(7.1940)   FL_LIT(4.8830)  FL_LIT(-2.7770)) -- C2' 
      (Pt  FL_LIT(6.8667)   FL_LIT(3.9183)  FL_LIT(-3.1647)) -- H2''
      (Pt  FL_LIT(8.5860)   FL_LIT(5.0910)  FL_LIT(-2.6140)) -- O2' 
      (Pt  FL_LIT(8.9510)   FL_LIT(4.7626)  FL_LIT(-1.7890)) -- H2' 
      (Pt  FL_LIT(6.5720)   FL_LIT(6.0040)  FL_LIT(-3.6090)) -- C3' 
      (Pt  FL_LIT(5.5636)   FL_LIT(5.7066)  FL_LIT(-3.8966)) -- H3' 
      (Pt  FL_LIT(7.3801)   FL_LIT(6.3562)  FL_LIT(-4.7350)) -- O3' 
      (Pt  FL_LIT(4.7150)   FL_LIT(0.4910)  FL_LIT(-0.1360)) -- N1  
      (Pt  FL_LIT(6.3490)   FL_LIT(2.1730)  FL_LIT(-0.6020)) -- N3  
      (Pt  FL_LIT(5.9530)   FL_LIT(0.9650)  FL_LIT(-0.2670)) -- C2  
      (Pt  FL_LIT(5.2900)   FL_LIT(2.9790)  FL_LIT(-0.8260)) -- C4  
      (Pt  FL_LIT(3.9720)   FL_LIT(2.6390)  FL_LIT(-0.7330)) -- C5  
      (Pt  FL_LIT(3.6770)   FL_LIT(1.3160)  FL_LIT(-0.3660)) -- C6
      (A
      (Pt  FL_LIT(2.4280)   FL_LIT(0.8450)  FL_LIT(-0.2360)) -- N6  
      (Pt  FL_LIT(3.1660)   FL_LIT(3.7290)  FL_LIT(-1.0360)) -- N7  
      (Pt  FL_LIT(5.3170)   FL_LIT(4.2990)  FL_LIT(-1.1930)) -- N9  
      (Pt  FL_LIT(4.0100)   FL_LIT(4.6780)  FL_LIT(-1.2990)) -- C8  
      (Pt  FL_LIT(6.6890)   FL_LIT(0.1903)  FL_LIT(-0.0518)) -- H2  
      (Pt  FL_LIT(1.6470)   FL_LIT(1.4460)  FL_LIT(-0.4040)) -- H61 
      (Pt  FL_LIT(2.2780)  FL_LIT(-0.1080)  FL_LIT(-0.0280)) -- H62 
      (Pt  FL_LIT(3.4421)   FL_LIT(5.5744)  FL_LIT(-1.5482)) -- H8  
      )

rA01
  = Nuc
      (Tfo FL_LIT(-0.0043)  FL_LIT(-0.8175)   FL_LIT(0.5759)  -- dgf_base_tfo
            FL_LIT(0.2617)  FL_LIT(-0.5567)  FL_LIT(-0.7884)
            FL_LIT(0.9651)   FL_LIT(0.1473)   FL_LIT(0.2164)
            FL_LIT(0.0359)   FL_LIT(8.3929)   FL_LIT(0.5532))
      (Tfo FL_LIT(-0.8143)  FL_LIT(-0.5091)  FL_LIT(-0.2788)  -- p_o3'_275_tfo
           FL_LIT(-0.0433)  FL_LIT(-0.4257)   FL_LIT(0.9038)
           FL_LIT(-0.5788)   FL_LIT(0.7480)   FL_LIT(0.3246)
            FL_LIT(1.5227)   FL_LIT(6.9114)  FL_LIT(-7.0765))
      (Tfo  FL_LIT(0.3822)  FL_LIT(-0.7477)   FL_LIT(0.5430)  -- p_o3'_180_tfo
            FL_LIT(0.4552)   FL_LIT(0.6637)   FL_LIT(0.5935)
           FL_LIT(-0.8042)   FL_LIT(0.0203)   FL_LIT(0.5941)
           FL_LIT(-6.9472)  FL_LIT(-4.1186)  FL_LIT(-5.9108))
      (Tfo  FL_LIT(0.5640)   FL_LIT(0.8007)  FL_LIT(-0.2022)  -- p_o3'_60_tfo
           FL_LIT(-0.8247)   FL_LIT(0.5587)  FL_LIT(-0.0878)
            FL_LIT(0.0426)   FL_LIT(0.2162)   FL_LIT(0.9754)
            FL_LIT(6.2694)  FL_LIT(-7.0540)   FL_LIT(3.3316))
      (Pt  FL_LIT(2.8930)   FL_LIT(8.5380)  FL_LIT(-3.3280)) -- P   
      (Pt  FL_LIT(1.6980)   FL_LIT(7.6960)  FL_LIT(-3.5570)) -- O1P 
      (Pt  FL_LIT(3.2260)   FL_LIT(9.5010)  FL_LIT(-4.4020)) -- O2P 
      (Pt  FL_LIT(4.1590)   FL_LIT(7.6040)  FL_LIT(-3.0340)) -- O5' 
      (Pt  FL_LIT(5.4352)   FL_LIT(8.2183)  FL_LIT(-2.7757)) -- C5' 
      (Pt  FL_LIT(5.3830)   FL_LIT(8.7883)  FL_LIT(-1.8481)) -- H5' 
      (Pt  FL_LIT(5.7729)   FL_LIT(8.7436)  FL_LIT(-3.6691)) -- H5''
      (Pt  FL_LIT(6.4830)   FL_LIT(7.1518)  FL_LIT(-2.5252)) -- C4' 
      (Pt  FL_LIT(7.4749)   FL_LIT(7.5972)  FL_LIT(-2.4482)) -- H4' 
      (Pt  FL_LIT(6.1626)   FL_LIT(6.4620)  FL_LIT(-1.2827)) -- O4' 
      (Pt  FL_LIT(6.5431)   FL_LIT(5.0992)  FL_LIT(-1.3905)) -- C1' 
      (Pt  FL_LIT(7.2871)   FL_LIT(4.9328)  FL_LIT(-0.6114)) -- H1' 
      (Pt  FL_LIT(7.1852)   FL_LIT(4.8935)  FL_LIT(-2.7592)) -- C2' 
      (Pt  FL_LIT(6.8573)   FL_LIT(3.9363)  FL_LIT(-3.1645)) -- H2''
      (Pt  FL_LIT(8.5780)   FL_LIT(5.1025)  FL_LIT(-2.6046)) -- O2' 
      (Pt  FL_LIT(8.9516)   FL_LIT(4.7577)  FL_LIT(-1.7902)) -- H2' 
      (Pt  FL_LIT(6.5522)   FL_LIT(6.0300)  FL_LIT(-3.5612)) -- C3' 
      (Pt  FL_LIT(5.5420)   FL_LIT(5.7356)  FL_LIT(-3.8459)) -- H3' 
      (Pt  FL_LIT(7.3487)   FL_LIT(6.4089)  FL_LIT(-4.6867)) -- O3' 
      (Pt  FL_LIT(4.7442)   FL_LIT(0.4514)  FL_LIT(-0.1390)) -- N1  
      (Pt  FL_LIT(6.3687)   FL_LIT(2.1459)  FL_LIT(-0.5926)) -- N3  
      (Pt  FL_LIT(5.9795)   FL_LIT(0.9335)  FL_LIT(-0.2657)) -- C2  
      (Pt  FL_LIT(5.3052)   FL_LIT(2.9471)  FL_LIT(-0.8125)) -- C4  
      (Pt  FL_LIT(3.9891)   FL_LIT(2.5987)  FL_LIT(-0.7230)) -- C5  
      (Pt  FL_LIT(3.7016)   FL_LIT(1.2717)  FL_LIT(-0.3647)) -- C6
      (A
      (Pt  FL_LIT(2.4553)   FL_LIT(0.7925)  FL_LIT(-0.2390)) -- N6  
      (Pt  FL_LIT(3.1770)   FL_LIT(3.6859)  FL_LIT(-1.0198)) -- N7  
      (Pt  FL_LIT(5.3247)   FL_LIT(4.2695)  FL_LIT(-1.1710)) -- N9  
      (Pt  FL_LIT(4.0156)   FL_LIT(4.6415)  FL_LIT(-1.2759)) -- C8  
      (Pt  FL_LIT(6.7198)   FL_LIT(0.1618)  FL_LIT(-0.0547)) -- H2  
      (Pt  FL_LIT(1.6709)   FL_LIT(1.3900)  FL_LIT(-0.4039)) -- H61 
      (Pt  FL_LIT(2.3107)  FL_LIT(-0.1627)  FL_LIT(-0.0373)) -- H62 
      (Pt  FL_LIT(3.4426)   FL_LIT(5.5361)  FL_LIT(-1.5199)) -- H8  
      )

rA02
  = Nuc
      (Tfo  FL_LIT(0.5566)   FL_LIT(0.0449)   FL_LIT(0.8296)  -- dgf_base_tfo
            FL_LIT(0.5125)   FL_LIT(0.7673)  FL_LIT(-0.3854)
           FL_LIT(-0.6538)   FL_LIT(0.6397)   FL_LIT(0.4041)
           FL_LIT(-9.1161)  FL_LIT(-3.7679)  FL_LIT(-2.9968))
      (Tfo FL_LIT(-0.8143)  FL_LIT(-0.5091)  FL_LIT(-0.2788)  -- p_o3'_275_tfo
           FL_LIT(-0.0433)  FL_LIT(-0.4257)   FL_LIT(0.9038)
           FL_LIT(-0.5788)   FL_LIT(0.7480)   FL_LIT(0.3246)
            FL_LIT(1.5227)   FL_LIT(6.9114)  FL_LIT(-7.0765))
      (Tfo  FL_LIT(0.3822)  FL_LIT(-0.7477)   FL_LIT(0.5430)  -- p_o3'_180_tfo
            FL_LIT(0.4552)   FL_LIT(0.6637)   FL_LIT(0.5935)
           FL_LIT(-0.8042)   FL_LIT(0.0203)   FL_LIT(0.5941)
           FL_LIT(-6.9472)  FL_LIT(-4.1186)  FL_LIT(-5.9108))
      (Tfo  FL_LIT(0.5640)   FL_LIT(0.8007)  FL_LIT(-0.2022)  -- p_o3'_60_tfo
           FL_LIT(-0.8247)   FL_LIT(0.5587)  FL_LIT(-0.0878)
            FL_LIT(0.0426)   FL_LIT(0.2162)   FL_LIT(0.9754)
            FL_LIT(6.2694)  FL_LIT(-7.0540)   FL_LIT(3.3316))
      (Pt  FL_LIT(2.8930)   FL_LIT(8.5380)  FL_LIT(-3.3280)) -- P   
      (Pt  FL_LIT(1.6980)   FL_LIT(7.6960)  FL_LIT(-3.5570)) -- O1P 
      (Pt  FL_LIT(3.2260)   FL_LIT(9.5010)  FL_LIT(-4.4020)) -- O2P 
      (Pt  FL_LIT(4.1590)   FL_LIT(7.6040)  FL_LIT(-3.0340)) -- O5' 
      (Pt  FL_LIT(4.5778)   FL_LIT(6.6594)  FL_LIT(-4.0364)) -- C5' 
      (Pt  FL_LIT(4.9220)   FL_LIT(7.1963)  FL_LIT(-4.9204)) -- H5' 
      (Pt  FL_LIT(3.7996)   FL_LIT(5.9091)  FL_LIT(-4.1764)) -- H5''
      (Pt  FL_LIT(5.7873)   FL_LIT(5.8869)  FL_LIT(-3.5482)) -- C4' 
      (Pt  FL_LIT(6.0405)   FL_LIT(5.0875)  FL_LIT(-4.2446)) -- H4' 
      (Pt  FL_LIT(6.9135)   FL_LIT(6.8036)  FL_LIT(-3.4310)) -- O4' 
      (Pt  FL_LIT(7.7293)   FL_LIT(6.4084)  FL_LIT(-2.3392)) -- C1' 
      (Pt  FL_LIT(8.7078)   FL_LIT(6.1815)  FL_LIT(-2.7624)) -- H1' 
      (Pt  FL_LIT(7.1305)   FL_LIT(5.1418)  FL_LIT(-1.7347)) -- C2' 
      (Pt  FL_LIT(7.2040)   FL_LIT(5.1982)  FL_LIT(-0.6486)) -- H2''
      (Pt  FL_LIT(7.7417)   FL_LIT(4.0392)  FL_LIT(-2.3813)) -- O2' 
      (Pt  FL_LIT(8.6785)   FL_LIT(4.1443)  FL_LIT(-2.5630)) -- H2' 
      (Pt  FL_LIT(5.6666)   FL_LIT(5.2728)  FL_LIT(-2.1536)) -- C3' 
      (Pt  FL_LIT(5.1747)   FL_LIT(5.9805)  FL_LIT(-1.4863)) -- H3' 
      (Pt  FL_LIT(4.9997)   FL_LIT(4.0086)  FL_LIT(-2.1973)) -- O3' 
      (Pt FL_LIT(10.3245)   FL_LIT(8.5459)   FL_LIT(1.5467)) -- N1  
      (Pt  FL_LIT(9.8051)   FL_LIT(6.9432)  FL_LIT(-0.1497)) -- N3  
      (Pt FL_LIT(10.5175)   FL_LIT(7.4328)   FL_LIT(0.8408)) -- C2  
      (Pt  FL_LIT(8.7523)   FL_LIT(7.7422)  FL_LIT(-0.4228)) -- C4  
      (Pt  FL_LIT(8.4257)   FL_LIT(8.9060)   FL_LIT(0.2099)) -- C5  
      (Pt  FL_LIT(9.2665)   FL_LIT(9.3242)   FL_LIT(1.2540)) -- C6
      (A
      (Pt  FL_LIT(9.0664)  FL_LIT(10.4462)   FL_LIT(1.9610)) -- N6  
      (Pt  FL_LIT(7.2750)   FL_LIT(9.4537)  FL_LIT(-0.3428)) -- N7  
      (Pt  FL_LIT(7.7962)   FL_LIT(7.5519)  FL_LIT(-1.3859)) -- N9  
      (Pt  FL_LIT(6.9479)   FL_LIT(8.6157)  FL_LIT(-1.2771)) -- C8  
      (Pt FL_LIT(11.4063)   FL_LIT(6.9047)   FL_LIT(1.1859)) -- H2  
      (Pt  FL_LIT(8.2845)  FL_LIT(11.0341)   FL_LIT(1.7552)) -- H61 
      (Pt  FL_LIT(9.6584)  FL_LIT(10.6647)   FL_LIT(2.7198)) -- H62 
      (Pt  FL_LIT(6.0430)   FL_LIT(8.9853)  FL_LIT(-1.7594)) -- H8  
      )

rA03
  = Nuc
      (Tfo FL_LIT(-0.5021)   FL_LIT(0.0731)   FL_LIT(0.8617)  -- dgf_base_tfo
           FL_LIT(-0.8112)   FL_LIT(0.3054)  FL_LIT(-0.4986)
           FL_LIT(-0.2996)  FL_LIT(-0.9494)  FL_LIT(-0.0940)
            FL_LIT(6.4273)  FL_LIT(-5.1944)  FL_LIT(-3.7807))
      (Tfo FL_LIT(-0.8143)  FL_LIT(-0.5091)  FL_LIT(-0.2788)  -- p_o3'_275_tfo
           FL_LIT(-0.0433)  FL_LIT(-0.4257)   FL_LIT(0.9038)
           FL_LIT(-0.5788)   FL_LIT(0.7480)   FL_LIT(0.3246)
            FL_LIT(1.5227)   FL_LIT(6.9114)  FL_LIT(-7.0765))
      (Tfo  FL_LIT(0.3822)  FL_LIT(-0.7477)   FL_LIT(0.5430)  -- p_o3'_180_tfo
            FL_LIT(0.4552)   FL_LIT(0.6637)   FL_LIT(0.5935)
           FL_LIT(-0.8042)   FL_LIT(0.0203)   FL_LIT(0.5941)
           FL_LIT(-6.9472)  FL_LIT(-4.1186)  FL_LIT(-5.9108))
      (Tfo  FL_LIT(0.5640)   FL_LIT(0.8007)  FL_LIT(-0.2022)  -- p_o3'_60_tfo
           FL_LIT(-0.8247)   FL_LIT(0.5587)  FL_LIT(-0.0878)
            FL_LIT(0.0426)   FL_LIT(0.2162)   FL_LIT(0.9754)
            FL_LIT(6.2694)  FL_LIT(-7.0540)   FL_LIT(3.3316))
      (Pt  FL_LIT(2.8930)   FL_LIT(8.5380)  FL_LIT(-3.3280)) -- P   
      (Pt  FL_LIT(1.6980)   FL_LIT(7.6960)  FL_LIT(-3.5570)) -- O1P 
      (Pt  FL_LIT(3.2260)   FL_LIT(9.5010)  FL_LIT(-4.4020)) -- O2P 
      (Pt  FL_LIT(4.1590)   FL_LIT(7.6040)  FL_LIT(-3.0340)) -- O5' 
      (Pt  FL_LIT(4.1214)   FL_LIT(6.7116)  FL_LIT(-1.9049)) -- C5' 
      (Pt  FL_LIT(3.3465)   FL_LIT(5.9610)  FL_LIT(-2.0607)) -- H5' 
      (Pt  FL_LIT(4.0789)   FL_LIT(7.2928)  FL_LIT(-0.9837)) -- H5''
      (Pt  FL_LIT(5.4170)   FL_LIT(5.9293)  FL_LIT(-1.8186)) -- C4' 
      (Pt  FL_LIT(5.4506)   FL_LIT(5.3400)  FL_LIT(-0.9023)) -- H4' 
      (Pt  FL_LIT(5.5067)   FL_LIT(5.0417)  FL_LIT(-2.9703)) -- O4' 
      (Pt  FL_LIT(6.8650)   FL_LIT(4.9152)  FL_LIT(-3.3612)) -- C1' 
      (Pt  FL_LIT(7.1090)   FL_LIT(3.8577)  FL_LIT(-3.2603)) -- H1' 
      (Pt  FL_LIT(7.7152)   FL_LIT(5.7282)  FL_LIT(-2.3894)) -- C2' 
      (Pt  FL_LIT(8.5029)   FL_LIT(6.2356)  FL_LIT(-2.9463)) -- H2''
      (Pt  FL_LIT(8.1036)   FL_LIT(4.8568)  FL_LIT(-1.3419)) -- O2' 
      (Pt  FL_LIT(8.3270)   FL_LIT(3.9651)  FL_LIT(-1.6184)) -- H2' 
      (Pt  FL_LIT(6.7003)   FL_LIT(6.7565)  FL_LIT(-1.8911)) -- C3' 
      (Pt  FL_LIT(6.5898)   FL_LIT(7.5329)  FL_LIT(-2.6482)) -- H3' 
      (Pt  FL_LIT(7.0505)   FL_LIT(7.2878)  FL_LIT(-0.6105)) -- O3' 
      (Pt  FL_LIT(9.6740)   FL_LIT(4.7656)  FL_LIT(-7.6614)) -- N1  
      (Pt  FL_LIT(9.0739)   FL_LIT(4.3013)  FL_LIT(-5.3941)) -- N3  
      (Pt  FL_LIT(9.8416)   FL_LIT(4.2192)  FL_LIT(-6.4581)) -- C2  
      (Pt  FL_LIT(7.9885)   FL_LIT(5.0632)  FL_LIT(-5.6446)) -- C4  
      (Pt  FL_LIT(7.6822)   FL_LIT(5.6856)  FL_LIT(-6.8194)) -- C5  
      (Pt  FL_LIT(8.5831)   FL_LIT(5.5215)  FL_LIT(-7.8840)) -- C6
      (A
      (Pt  FL_LIT(8.4084)   FL_LIT(6.0747)  FL_LIT(-9.0933)) -- N6  
      (Pt  FL_LIT(6.4857)   FL_LIT(6.3816)  FL_LIT(-6.7035)) -- N7  
      (Pt  FL_LIT(6.9740)   FL_LIT(5.3703)  FL_LIT(-4.7760)) -- N9  
      (Pt  FL_LIT(6.1133)   FL_LIT(6.1613)  FL_LIT(-5.4808)) -- C8  
      (Pt FL_LIT(10.7627)   FL_LIT(3.6375)  FL_LIT(-6.4220)) -- H2  
      (Pt  FL_LIT(7.6031)   FL_LIT(6.6390)  FL_LIT(-9.2733)) -- H61 
      (Pt  FL_LIT(9.1004)   FL_LIT(5.9708)  FL_LIT(-9.7893)) -- H62 
      (Pt  FL_LIT(5.1705)   FL_LIT(6.6830)  FL_LIT(-5.3167)) -- H8  
      )

rA04
  = Nuc
      (Tfo FL_LIT(-0.5426)  FL_LIT(-0.8175)   FL_LIT(0.1929)  -- dgf_base_tfo
            FL_LIT(0.8304)  FL_LIT(-0.5567)  FL_LIT(-0.0237)
            FL_LIT(0.1267)   FL_LIT(0.1473)   FL_LIT(0.9809)
           FL_LIT(-0.5075)   FL_LIT(8.3929)   FL_LIT(0.2229))
      (Tfo FL_LIT(-0.8143)  FL_LIT(-0.5091)  FL_LIT(-0.2788)  -- p_o3'_275_tfo
           FL_LIT(-0.0433)  FL_LIT(-0.4257)   FL_LIT(0.9038)
           FL_LIT(-0.5788)   FL_LIT(0.7480)   FL_LIT(0.3246)
            FL_LIT(1.5227)   FL_LIT(6.9114)  FL_LIT(-7.0765))
      (Tfo  FL_LIT(0.3822)  FL_LIT(-0.7477)   FL_LIT(0.5430)  -- p_o3'_180_tfo
            FL_LIT(0.4552)   FL_LIT(0.6637)   FL_LIT(0.5935)
           FL_LIT(-0.8042)   FL_LIT(0.0203)   FL_LIT(0.5941)
           FL_LIT(-6.9472)  FL_LIT(-4.1186)  FL_LIT(-5.9108))
      (Tfo  FL_LIT(0.5640)   FL_LIT(0.8007)  FL_LIT(-0.2022)  -- p_o3'_60_tfo
           FL_LIT(-0.8247)   FL_LIT(0.5587)  FL_LIT(-0.0878)
            FL_LIT(0.0426)   FL_LIT(0.2162)   FL_LIT(0.9754)
            FL_LIT(6.2694)  FL_LIT(-7.0540)   FL_LIT(3.3316))
      (Pt  FL_LIT(2.8930)   FL_LIT(8.5380)  FL_LIT(-3.3280)) -- P   
      (Pt  FL_LIT(1.6980)   FL_LIT(7.6960)  FL_LIT(-3.5570)) -- O1P 
      (Pt  FL_LIT(3.2260)   FL_LIT(9.5010)  FL_LIT(-4.4020)) -- O2P 
      (Pt  FL_LIT(4.1590)   FL_LIT(7.6040)  FL_LIT(-3.0340)) -- O5' 
      (Pt  FL_LIT(5.4352)   FL_LIT(8.2183)  FL_LIT(-2.7757)) -- C5' 
      (Pt  FL_LIT(5.3830)   FL_LIT(8.7883)  FL_LIT(-1.8481)) -- H5' 
      (Pt  FL_LIT(5.7729)   FL_LIT(8.7436)  FL_LIT(-3.6691)) -- H5''
      (Pt  FL_LIT(6.4830)   FL_LIT(7.1518)  FL_LIT(-2.5252)) -- C4' 
      (Pt  FL_LIT(7.4749)   FL_LIT(7.5972)  FL_LIT(-2.4482)) -- H4' 
      (Pt  FL_LIT(6.1626)   FL_LIT(6.4620)  FL_LIT(-1.2827)) -- O4' 
      (Pt  FL_LIT(6.5431)   FL_LIT(5.0992)  FL_LIT(-1.3905)) -- C1' 
      (Pt  FL_LIT(7.2871)   FL_LIT(4.9328)  FL_LIT(-0.6114)) -- H1' 
      (Pt  FL_LIT(7.1852)   FL_LIT(4.8935)  FL_LIT(-2.7592)) -- C2' 
      (Pt  FL_LIT(6.8573)   FL_LIT(3.9363)  FL_LIT(-3.1645)) -- H2''
      (Pt  FL_LIT(8.5780)   FL_LIT(5.1025)  FL_LIT(-2.6046)) -- O2' 
      (Pt  FL_LIT(8.9516)   FL_LIT(4.7577)  FL_LIT(-1.7902)) -- H2' 
      (Pt  FL_LIT(6.5522)   FL_LIT(6.0300)  FL_LIT(-3.5612)) -- C3' 
      (Pt  FL_LIT(5.5420)   FL_LIT(5.7356)  FL_LIT(-3.8459)) -- H3' 
      (Pt  FL_LIT(7.3487)   FL_LIT(6.4089)  FL_LIT(-4.6867)) -- O3' 
      (Pt  FL_LIT(3.6343)   FL_LIT(2.6680)   FL_LIT(2.0783)) -- N1  
      (Pt  FL_LIT(5.4505)   FL_LIT(3.9805)   FL_LIT(1.2446)) -- N3  
      (Pt  FL_LIT(4.7540)   FL_LIT(3.3816)   FL_LIT(2.1851)) -- C2  
      (Pt  FL_LIT(4.8805)   FL_LIT(3.7951)   FL_LIT(0.0354)) -- C4  
      (Pt  FL_LIT(3.7416)   FL_LIT(3.0925)  FL_LIT(-0.2305)) -- C5  
      (Pt  FL_LIT(3.0873)   FL_LIT(2.4980)   FL_LIT(0.8606)) -- C6
      (A
      (Pt  FL_LIT(1.9600)   FL_LIT(1.7805)   FL_LIT(0.7462)) -- N6  
      (Pt  FL_LIT(3.4605)   FL_LIT(3.1184)  FL_LIT(-1.5906)) -- N7  
      (Pt  FL_LIT(5.3247)   FL_LIT(4.2695)  FL_LIT(-1.1710)) -- N9  
      (Pt  FL_LIT(4.4244)   FL_LIT(3.8244)  FL_LIT(-2.0953)) -- C8  
      (Pt  FL_LIT(5.0814)   FL_LIT(3.4352)   FL_LIT(3.2234)) -- H2  
      (Pt  FL_LIT(1.5423)   FL_LIT(1.6454)  FL_LIT(-0.1520)) -- H61 
      (Pt  FL_LIT(1.5716)   FL_LIT(1.3398)   FL_LIT(1.5392)) -- H62 
      (Pt  FL_LIT(4.2675)   FL_LIT(3.8876)  FL_LIT(-3.1721)) -- H8  
      )

rA05
  = Nuc
      (Tfo FL_LIT(-0.5891)   FL_LIT(0.0449)   FL_LIT(0.8068)  -- dgf_base_tfo
            FL_LIT(0.5375)   FL_LIT(0.7673)   FL_LIT(0.3498)
           FL_LIT(-0.6034)   FL_LIT(0.6397)  FL_LIT(-0.4762)
           FL_LIT(-0.3019)  FL_LIT(-3.7679)  FL_LIT(-9.5913))
      (Tfo FL_LIT(-0.8143)  FL_LIT(-0.5091)  FL_LIT(-0.2788)  -- p_o3'_275_tfo
           FL_LIT(-0.0433)  FL_LIT(-0.4257)   FL_LIT(0.9038)
           FL_LIT(-0.5788)   FL_LIT(0.7480)   FL_LIT(0.3246)
            FL_LIT(1.5227)   FL_LIT(6.9114)  FL_LIT(-7.0765))
      (Tfo  FL_LIT(0.3822)  FL_LIT(-0.7477)   FL_LIT(0.5430)  -- p_o3'_180_tfo
            FL_LIT(0.4552)   FL_LIT(0.6637)   FL_LIT(0.5935)
           FL_LIT(-0.8042)   FL_LIT(0.0203)   FL_LIT(0.5941)
           FL_LIT(-6.9472)  FL_LIT(-4.1186)  FL_LIT(-5.9108))
      (Tfo  FL_LIT(0.5640)   FL_LIT(0.8007)  FL_LIT(-0.2022)  -- p_o3'_60_tfo
           FL_LIT(-0.8247)   FL_LIT(0.5587)  FL_LIT(-0.0878)
            FL_LIT(0.0426)   FL_LIT(0.2162)   FL_LIT(0.9754)
            FL_LIT(6.2694)  FL_LIT(-7.0540)   FL_LIT(3.3316))
      (Pt  FL_LIT(2.8930)   FL_LIT(8.5380)  FL_LIT(-3.3280)) -- P   
      (Pt  FL_LIT(1.6980)   FL_LIT(7.6960)  FL_LIT(-3.5570)) -- O1P 
      (Pt  FL_LIT(3.2260)   FL_LIT(9.5010)  FL_LIT(-4.4020)) -- O2P 
      (Pt  FL_LIT(4.1590)   FL_LIT(7.6040)  FL_LIT(-3.0340)) -- O5' 
      (Pt  FL_LIT(4.5778)   FL_LIT(6.6594)  FL_LIT(-4.0364)) -- C5' 
      (Pt  FL_LIT(4.9220)   FL_LIT(7.1963)  FL_LIT(-4.9204)) -- H5' 
      (Pt  FL_LIT(3.7996)   FL_LIT(5.9091)  FL_LIT(-4.1764)) -- H5''
      (Pt  FL_LIT(5.7873)   FL_LIT(5.8869)  FL_LIT(-3.5482)) -- C4' 
      (Pt  FL_LIT(6.0405)   FL_LIT(5.0875)  FL_LIT(-4.2446)) -- H4' 
      (Pt  FL_LIT(6.9135)   FL_LIT(6.8036)  FL_LIT(-3.4310)) -- O4' 
      (Pt  FL_LIT(7.7293)   FL_LIT(6.4084)  FL_LIT(-2.3392)) -- C1' 
      (Pt  FL_LIT(8.7078)   FL_LIT(6.1815)  FL_LIT(-2.7624)) -- H1' 
      (Pt  FL_LIT(7.1305)   FL_LIT(5.1418)  FL_LIT(-1.7347)) -- C2' 
      (Pt  FL_LIT(7.2040)   FL_LIT(5.1982)  FL_LIT(-0.6486)) -- H2''
      (Pt  FL_LIT(7.7417)   FL_LIT(4.0392)  FL_LIT(-2.3813)) -- O2' 
      (Pt  FL_LIT(8.6785)   FL_LIT(4.1443)  FL_LIT(-2.5630)) -- H2' 
      (Pt  FL_LIT(5.6666)   FL_LIT(5.2728)  FL_LIT(-2.1536)) -- C3' 
      (Pt  FL_LIT(5.1747)   FL_LIT(5.9805)  FL_LIT(-1.4863)) -- H3' 
      (Pt  FL_LIT(4.9997)   FL_LIT(4.0086)  FL_LIT(-2.1973)) -- O3' 
      (Pt FL_LIT(10.2594)  FL_LIT(10.6774)  FL_LIT(-1.0056)) -- N1  
      (Pt  FL_LIT(9.7528)   FL_LIT(8.7080)  FL_LIT(-2.2631)) -- N3  
      (Pt FL_LIT(10.4471)   FL_LIT(9.7876)  FL_LIT(-1.9791)) -- C2  
      (Pt  FL_LIT(8.7271)   FL_LIT(8.5575)  FL_LIT(-1.3991)) -- C4  
      (Pt  FL_LIT(8.4100)   FL_LIT(9.3803)  FL_LIT(-0.3580)) -- C5  
      (Pt  FL_LIT(9.2294)  FL_LIT(10.5030)  FL_LIT(-0.1574)) -- C6
      (A
      (Pt  FL_LIT(9.0349)  FL_LIT(11.3951)   FL_LIT(0.8250)) -- N6  
      (Pt  FL_LIT(7.2891)   FL_LIT(8.9068)   FL_LIT(0.3121)) -- N7  
      (Pt  FL_LIT(7.7962)   FL_LIT(7.5519)  FL_LIT(-1.3859)) -- N9  
      (Pt  FL_LIT(6.9702)   FL_LIT(7.8292)  FL_LIT(-0.3353)) -- C8  
      (Pt FL_LIT(11.3132)  FL_LIT(10.0537)  FL_LIT(-2.5851)) -- H2  
      (Pt  FL_LIT(8.2741)  FL_LIT(11.2784)   FL_LIT(1.4629)) -- H61 
      (Pt  FL_LIT(9.6733)  FL_LIT(12.1368)   FL_LIT(0.9529)) -- H62 
      (Pt  FL_LIT(6.0888)   FL_LIT(7.3990)   FL_LIT(0.1403)) -- H8  
      )

rA06
  = Nuc
      (Tfo FL_LIT(-0.9815)   FL_LIT(0.0731)  FL_LIT(-0.1772)  -- dgf_base_tfo
            FL_LIT(0.1912)   FL_LIT(0.3054)  FL_LIT(-0.9328)
           FL_LIT(-0.0141)  FL_LIT(-0.9494)  FL_LIT(-0.3137)
            FL_LIT(5.7506)  FL_LIT(-5.1944)   FL_LIT(4.7470))
      (Tfo FL_LIT(-0.8143)  FL_LIT(-0.5091)  FL_LIT(-0.2788)  -- p_o3'_275_tfo
           FL_LIT(-0.0433)  FL_LIT(-0.4257)   FL_LIT(0.9038)
           FL_LIT(-0.5788)   FL_LIT(0.7480)   FL_LIT(0.3246)
            FL_LIT(1.5227)   FL_LIT(6.9114)  FL_LIT(-7.0765))
      (Tfo  FL_LIT(0.3822)  FL_LIT(-0.7477)   FL_LIT(0.5430)  -- p_o3'_180_tfo
            FL_LIT(0.4552)   FL_LIT(0.6637)   FL_LIT(0.5935)
           FL_LIT(-0.8042)   FL_LIT(0.0203)   FL_LIT(0.5941)
           FL_LIT(-6.9472)  FL_LIT(-4.1186)  FL_LIT(-5.9108))
      (Tfo  FL_LIT(0.5640)   FL_LIT(0.8007)  FL_LIT(-0.2022)  -- p_o3'_60_tfo
           FL_LIT(-0.8247)   FL_LIT(0.5587)  FL_LIT(-0.0878)
            FL_LIT(0.0426)   FL_LIT(0.2162)   FL_LIT(0.9754)
            FL_LIT(6.2694)  FL_LIT(-7.0540)   FL_LIT(3.3316))
      (Pt  FL_LIT(2.8930)   FL_LIT(8.5380)  FL_LIT(-3.3280)) -- P   
      (Pt  FL_LIT(1.6980)   FL_LIT(7.6960)  FL_LIT(-3.5570)) -- O1P 
      (Pt  FL_LIT(3.2260)   FL_LIT(9.5010)  FL_LIT(-4.4020)) -- O2P 
      (Pt  FL_LIT(4.1590)   FL_LIT(7.6040)  FL_LIT(-3.0340)) -- O5' 
      (Pt  FL_LIT(4.1214)   FL_LIT(6.7116)  FL_LIT(-1.9049)) -- C5' 
      (Pt  FL_LIT(3.3465)   FL_LIT(5.9610)  FL_LIT(-2.0607)) -- H5' 
      (Pt  FL_LIT(4.0789)   FL_LIT(7.2928)  FL_LIT(-0.9837)) -- H5''
      (Pt  FL_LIT(5.4170)   FL_LIT(5.9293)  FL_LIT(-1.8186)) -- C4' 
      (Pt  FL_LIT(5.4506)   FL_LIT(5.3400)  FL_LIT(-0.9023)) -- H4' 
      (Pt  FL_LIT(5.5067)   FL_LIT(5.0417)  FL_LIT(-2.9703)) -- O4' 
      (Pt  FL_LIT(6.8650)   FL_LIT(4.9152)  FL_LIT(-3.3612)) -- C1' 
      (Pt  FL_LIT(7.1090)   FL_LIT(3.8577)  FL_LIT(-3.2603)) -- H1' 
      (Pt  FL_LIT(7.7152)   FL_LIT(5.7282)  FL_LIT(-2.3894)) -- C2' 
      (Pt  FL_LIT(8.5029)   FL_LIT(6.2356)  FL_LIT(-2.9463)) -- H2''
      (Pt  FL_LIT(8.1036)   FL_LIT(4.8568)  FL_LIT(-1.3419)) -- O2' 
      (Pt  FL_LIT(8.3270)   FL_LIT(3.9651)  FL_LIT(-1.6184)) -- H2' 
      (Pt  FL_LIT(6.7003)   FL_LIT(6.7565)  FL_LIT(-1.8911)) -- C3' 
      (Pt  FL_LIT(6.5898)   FL_LIT(7.5329)  FL_LIT(-2.6482)) -- H3' 
      (Pt  FL_LIT(7.0505)   FL_LIT(7.2878)  FL_LIT(-0.6105)) -- O3' 
      (Pt  FL_LIT(6.6624)   FL_LIT(3.5061)  FL_LIT(-8.2986)) -- N1  
      (Pt  FL_LIT(6.5810)   FL_LIT(3.2570)  FL_LIT(-5.9221)) -- N3  
      (Pt  FL_LIT(6.5151)   FL_LIT(2.8263)  FL_LIT(-7.1625)) -- C2  
      (Pt  FL_LIT(6.8364)   FL_LIT(4.5817)  FL_LIT(-5.8882)) -- C4  
      (Pt  FL_LIT(7.0116)   FL_LIT(5.4064)  FL_LIT(-6.9609)) -- C5  
      (Pt  FL_LIT(6.9173)   FL_LIT(4.8260)  FL_LIT(-8.2361)) -- C6
      (A
      (Pt  FL_LIT(7.0668)   FL_LIT(5.5163)  FL_LIT(-9.3763)) -- N6  
      (Pt  FL_LIT(7.2573)   FL_LIT(6.7070)  FL_LIT(-6.5394)) -- N7  
      (Pt  FL_LIT(6.9740)   FL_LIT(5.3703)  FL_LIT(-4.7760)) -- N9  
      (Pt  FL_LIT(7.2238)   FL_LIT(6.6275)  FL_LIT(-5.2453)) -- C8  
      (Pt  FL_LIT(6.3146)   FL_LIT(1.7741)  FL_LIT(-7.3641)) -- H2  
      (Pt  FL_LIT(7.2568)   FL_LIT(6.4972)  FL_LIT(-9.3456)) -- H61 
      (Pt  FL_LIT(7.0437)   FL_LIT(5.0478) FL_LIT(-10.2446)) -- H62 
      (Pt  FL_LIT(7.4108)   FL_LIT(7.6227)  FL_LIT(-4.8418)) -- H8  
      )

rA07
  = Nuc
      (Tfo  FL_LIT(0.2379)   FL_LIT(0.1310)  FL_LIT(-0.9624)  -- dgf_base_tfo
           FL_LIT(-0.5876)  FL_LIT(-0.7696)  FL_LIT(-0.2499)
           FL_LIT(-0.7734)   FL_LIT(0.6249)  FL_LIT(-0.1061)
           FL_LIT(30.9870) FL_LIT(-26.9344)  FL_LIT(42.6416))
      (Tfo  FL_LIT(0.7529)   FL_LIT(0.1548)   FL_LIT(0.6397)  -- p_o3'_275_tfo
            FL_LIT(0.2952)  FL_LIT(-0.9481)  FL_LIT(-0.1180)
            FL_LIT(0.5882)   FL_LIT(0.2777)  FL_LIT(-0.7595)
          FL_LIT(-58.8919) FL_LIT(-11.3095)   FL_LIT(6.0866))
      (Tfo FL_LIT(-0.0239)   FL_LIT(0.9667)  FL_LIT(-0.2546)  -- p_o3'_180_tfo
            FL_LIT(0.9731)  FL_LIT(-0.0359)  FL_LIT(-0.2275)
           FL_LIT(-0.2290)  FL_LIT(-0.2532)  FL_LIT(-0.9399)
            FL_LIT(3.5401) FL_LIT(-29.7913)  FL_LIT(52.2796))
      (Tfo FL_LIT(-0.8912)  FL_LIT(-0.4531)   FL_LIT(0.0242)  -- p_o3'_60_tfo
           FL_LIT(-0.1183)   FL_LIT(0.1805)  FL_LIT(-0.9764)
            FL_LIT(0.4380)  FL_LIT(-0.8730)  FL_LIT(-0.2145)
           FL_LIT(19.9023)  FL_LIT(54.8054)  FL_LIT(15.2799))
      (Pt FL_LIT(41.8210)   FL_LIT(8.3880)  FL_LIT(43.5890)) -- P   
      (Pt FL_LIT(42.5400)   FL_LIT(8.0450)  FL_LIT(44.8330)) -- O1P 
      (Pt FL_LIT(42.2470)   FL_LIT(9.6920)  FL_LIT(42.9910)) -- O2P 
      (Pt FL_LIT(40.2550)   FL_LIT(8.2030)  FL_LIT(43.7340)) -- O5' 
      (Pt FL_LIT(39.3505)   FL_LIT(8.4697)  FL_LIT(42.6565)) -- C5' 
      (Pt FL_LIT(39.1377)   FL_LIT(7.5433)  FL_LIT(42.1230)) -- H5' 
      (Pt FL_LIT(39.7203)   FL_LIT(9.3119)  FL_LIT(42.0717)) -- H5''
      (Pt FL_LIT(38.0405)   FL_LIT(8.9195)  FL_LIT(43.2869)) -- C4' 
      (Pt FL_LIT(37.3687)   FL_LIT(9.3036)  FL_LIT(42.5193)) -- H4' 
      (Pt FL_LIT(37.4319)   FL_LIT(7.8146)  FL_LIT(43.9387)) -- O4' 
      (Pt FL_LIT(37.1959)   FL_LIT(8.1354)  FL_LIT(45.3237)) -- C1' 
      (Pt FL_LIT(36.1788)   FL_LIT(8.5202)  FL_LIT(45.3970)) -- H1' 
      (Pt FL_LIT(38.1721)   FL_LIT(9.2328)  FL_LIT(45.6504)) -- C2' 
      (Pt FL_LIT(39.1555)   FL_LIT(8.7939)  FL_LIT(45.8188)) -- H2''
      (Pt FL_LIT(37.7862)  FL_LIT(10.0617)  FL_LIT(46.7013)) -- O2' 
      (Pt FL_LIT(37.3087)   FL_LIT(9.6229)  FL_LIT(47.4092)) -- H2' 
      (Pt FL_LIT(38.1844)  FL_LIT(10.0268)  FL_LIT(44.3367)) -- C3' 
      (Pt FL_LIT(39.1578)  FL_LIT(10.5054)  FL_LIT(44.2289)) -- H3' 
      (Pt FL_LIT(37.0547)  FL_LIT(10.9127)  FL_LIT(44.3441)) -- O3' 
      (Pt FL_LIT(34.8811)   FL_LIT(4.2072)  FL_LIT(47.5784)) -- N1  
      (Pt FL_LIT(35.1084)   FL_LIT(6.1336)  FL_LIT(46.1818)) -- N3  
      (Pt FL_LIT(34.4108)   FL_LIT(5.1360)  FL_LIT(46.7207)) -- C2  
      (Pt FL_LIT(36.3908)   FL_LIT(6.1224)  FL_LIT(46.6053)) -- C4  
      (Pt FL_LIT(36.9819)   FL_LIT(5.2334)  FL_LIT(47.4697)) -- C5  
      (Pt FL_LIT(36.1786)   FL_LIT(4.1985)  FL_LIT(48.0035)) -- C6
      (A
      (Pt FL_LIT(36.6103)   FL_LIT(3.2749)  FL_LIT(48.8452)) -- N6  
      (Pt FL_LIT(38.3236)   FL_LIT(5.5522)  FL_LIT(47.6595)) -- N7  
      (Pt FL_LIT(37.3887)   FL_LIT(7.0024)  FL_LIT(46.2437)) -- N9  
      (Pt FL_LIT(38.5055)   FL_LIT(6.6096)  FL_LIT(46.9057)) -- C8  
      (Pt FL_LIT(33.3553)   FL_LIT(5.0152)  FL_LIT(46.4771)) -- H2  
      (Pt FL_LIT(37.5730)   FL_LIT(3.2804)  FL_LIT(49.1507)) -- H61 
      (Pt FL_LIT(35.9775)   FL_LIT(2.5638)  FL_LIT(49.1828)) -- H62 
      (Pt FL_LIT(39.5461)   FL_LIT(6.9184)  FL_LIT(47.0041)) -- H8  
      )

rA08
  = Nuc
      (Tfo  FL_LIT(0.1084)  FL_LIT(-0.0895)  FL_LIT(-0.9901)  -- dgf_base_tfo
            FL_LIT(0.9789)  FL_LIT(-0.1638)   FL_LIT(0.1220)
           FL_LIT(-0.1731)  FL_LIT(-0.9824)   FL_LIT(0.0698)
           FL_LIT(-2.9039)  FL_LIT(47.2655)  FL_LIT(33.0094))
      (Tfo  FL_LIT(0.7529)   FL_LIT(0.1548)   FL_LIT(0.6397)  -- p_o3'_275_tfo
            FL_LIT(0.2952)  FL_LIT(-0.9481)  FL_LIT(-0.1180)
            FL_LIT(0.5882)   FL_LIT(0.2777)  FL_LIT(-0.7595)
          FL_LIT(-58.8919) FL_LIT(-11.3095)   FL_LIT(6.0866))
      (Tfo FL_LIT(-0.0239)   FL_LIT(0.9667)  FL_LIT(-0.2546)  -- p_o3'_180_tfo
            FL_LIT(0.9731)  FL_LIT(-0.0359)  FL_LIT(-0.2275)
           FL_LIT(-0.2290)  FL_LIT(-0.2532)  FL_LIT(-0.9399)
            FL_LIT(3.5401) FL_LIT(-29.7913)  FL_LIT(52.2796))
      (Tfo FL_LIT(-0.8912)  FL_LIT(-0.4531)   FL_LIT(0.0242)  -- p_o3'_60_tfo
           FL_LIT(-0.1183)   FL_LIT(0.1805)  FL_LIT(-0.9764)
            FL_LIT(0.4380)  FL_LIT(-0.8730)  FL_LIT(-0.2145)
           FL_LIT(19.9023)  FL_LIT(54.8054)  FL_LIT(15.2799))
      (Pt FL_LIT(41.8210)   FL_LIT(8.3880)  FL_LIT(43.5890)) -- P   
      (Pt FL_LIT(42.5400)   FL_LIT(8.0450)  FL_LIT(44.8330)) -- O1P 
      (Pt FL_LIT(42.2470)   FL_LIT(9.6920)  FL_LIT(42.9910)) -- O2P 
      (Pt FL_LIT(40.2550)   FL_LIT(8.2030)  FL_LIT(43.7340)) -- O5' 
      (Pt FL_LIT(39.4850)   FL_LIT(8.9301)  FL_LIT(44.6977)) -- C5' 
      (Pt FL_LIT(39.0638)   FL_LIT(9.8199)  FL_LIT(44.2296)) -- H5' 
      (Pt FL_LIT(40.0757)   FL_LIT(9.0713)  FL_LIT(45.6029)) -- H5''
      (Pt FL_LIT(38.3102)   FL_LIT(8.0414)  FL_LIT(45.0789)) -- C4' 
      (Pt FL_LIT(37.7842)   FL_LIT(8.4637)  FL_LIT(45.9351)) -- H4' 
      (Pt FL_LIT(37.4200)   FL_LIT(7.9453)  FL_LIT(43.9769)) -- O4' 
      (Pt FL_LIT(37.2249)   FL_LIT(6.5609)  FL_LIT(43.6273)) -- C1' 
      (Pt FL_LIT(36.3360)   FL_LIT(6.2168)  FL_LIT(44.1561)) -- H1' 
      (Pt FL_LIT(38.4347)   FL_LIT(5.8414)  FL_LIT(44.1590)) -- C2' 
      (Pt FL_LIT(39.2688)   FL_LIT(5.9974)  FL_LIT(43.4749)) -- H2''
      (Pt FL_LIT(38.2344)   FL_LIT(4.4907)  FL_LIT(44.4348)) -- O2' 
      (Pt FL_LIT(37.6374)   FL_LIT(4.0386)  FL_LIT(43.8341)) -- H2' 
      (Pt FL_LIT(38.6926)   FL_LIT(6.6079)  FL_LIT(45.4637)) -- C3' 
      (Pt FL_LIT(39.7585)   FL_LIT(6.5640)  FL_LIT(45.6877)) -- H3' 
      (Pt FL_LIT(37.8238)   FL_LIT(6.0705)  FL_LIT(46.4723)) -- O3' 
      (Pt FL_LIT(33.9162)   FL_LIT(6.2598)  FL_LIT(39.7758)) -- N1  
      (Pt FL_LIT(34.6709)   FL_LIT(6.5759)  FL_LIT(42.0215)) -- N3  
      (Pt FL_LIT(33.7257)   FL_LIT(6.5186)  FL_LIT(41.0858)) -- C2  
      (Pt FL_LIT(35.8935)   FL_LIT(6.3324)  FL_LIT(41.5018)) -- C4  
      (Pt FL_LIT(36.2105)   FL_LIT(6.0601)  FL_LIT(40.1932)) -- C5  
      (Pt FL_LIT(35.1538)   FL_LIT(6.0151)  FL_LIT(39.2537)) -- C6
      (A
      (Pt FL_LIT(35.3088)   FL_LIT(5.7642)  FL_LIT(37.9649)) -- N6  
      (Pt FL_LIT(37.5818)   FL_LIT(5.8677)  FL_LIT(40.0507)) -- N7  
      (Pt FL_LIT(37.0932)   FL_LIT(6.3197)  FL_LIT(42.1810)) -- N9  
      (Pt FL_LIT(38.0509)   FL_LIT(6.0354)  FL_LIT(41.2635)) -- C8  
      (Pt FL_LIT(32.6830)   FL_LIT(6.6898)  FL_LIT(41.3532)) -- H2  
      (Pt FL_LIT(36.2305)   FL_LIT(5.5855)  FL_LIT(37.5925)) -- H61 
      (Pt FL_LIT(34.5056)   FL_LIT(5.7512)  FL_LIT(37.3528)) -- H62 
      (Pt FL_LIT(39.1318)   FL_LIT(5.8993)  FL_LIT(41.2285)) -- H8  
      )

rA09
  = Nuc
      (Tfo  FL_LIT(0.8467)   FL_LIT(0.4166)  FL_LIT(-0.3311)  -- dgf_base_tfo
           FL_LIT(-0.3962)   FL_LIT(0.9089)   FL_LIT(0.1303)
            FL_LIT(0.3552)   FL_LIT(0.0209)   FL_LIT(0.9346)
          FL_LIT(-42.7319) FL_LIT(-26.6223) FL_LIT(-29.8163))
      (Tfo  FL_LIT(0.7529)   FL_LIT(0.1548)   FL_LIT(0.6397)  -- p_o3'_275_tfo
            FL_LIT(0.2952)  FL_LIT(-0.9481)  FL_LIT(-0.1180)
            FL_LIT(0.5882)   FL_LIT(0.2777)  FL_LIT(-0.7595)
          FL_LIT(-58.8919) FL_LIT(-11.3095)   FL_LIT(6.0866))
      (Tfo FL_LIT(-0.0239)   FL_LIT(0.9667)  FL_LIT(-0.2546)  -- p_o3'_180_tfo
            FL_LIT(0.9731)  FL_LIT(-0.0359)  FL_LIT(-0.2275)
           FL_LIT(-0.2290)  FL_LIT(-0.2532)  FL_LIT(-0.9399)
            FL_LIT(3.5401) FL_LIT(-29.7913)  FL_LIT(52.2796))
      (Tfo FL_LIT(-0.8912)  FL_LIT(-0.4531)   FL_LIT(0.0242)  -- p_o3'_60_tfo
           FL_LIT(-0.1183)   FL_LIT(0.1805)  FL_LIT(-0.9764)
            FL_LIT(0.4380)  FL_LIT(-0.8730)  FL_LIT(-0.2145)
           FL_LIT(19.9023)  FL_LIT(54.8054)  FL_LIT(15.2799))
      (Pt FL_LIT(41.8210)   FL_LIT(8.3880)  FL_LIT(43.5890)) -- P   
      (Pt FL_LIT(42.5400)   FL_LIT(8.0450)  FL_LIT(44.8330)) -- O1P 
      (Pt FL_LIT(42.2470)   FL_LIT(9.6920)  FL_LIT(42.9910)) -- O2P 
      (Pt FL_LIT(40.2550)   FL_LIT(8.2030)  FL_LIT(43.7340)) -- O5' 
      (Pt FL_LIT(39.3505)   FL_LIT(8.4697)  FL_LIT(42.6565)) -- C5' 
      (Pt FL_LIT(39.1377)   FL_LIT(7.5433)  FL_LIT(42.1230)) -- H5' 
      (Pt FL_LIT(39.7203)   FL_LIT(9.3119)  FL_LIT(42.0717)) -- H5''
      (Pt FL_LIT(38.0405)   FL_LIT(8.9195)  FL_LIT(43.2869)) -- C4' 
      (Pt FL_LIT(37.6479)   FL_LIT(8.1347)  FL_LIT(43.9335)) -- H4' 
      (Pt FL_LIT(38.2691)  FL_LIT(10.0933)  FL_LIT(44.0524)) -- O4' 
      (Pt FL_LIT(37.3999)  FL_LIT(11.1488)  FL_LIT(43.5973)) -- C1' 
      (Pt FL_LIT(36.5061)  FL_LIT(11.1221)  FL_LIT(44.2206)) -- H1' 
      (Pt FL_LIT(37.0364)  FL_LIT(10.7838)  FL_LIT(42.1836)) -- C2' 
      (Pt FL_LIT(37.8636)  FL_LIT(11.0489)  FL_LIT(41.5252)) -- H2''
      (Pt FL_LIT(35.8275)  FL_LIT(11.3133)  FL_LIT(41.7379)) -- O2' 
      (Pt FL_LIT(35.6214)  FL_LIT(12.1896)  FL_LIT(42.0714)) -- H2' 
      (Pt FL_LIT(36.9316)   FL_LIT(9.2556)  FL_LIT(42.2837)) -- C3' 
      (Pt FL_LIT(37.1778)   FL_LIT(8.8260)  FL_LIT(41.3127)) -- H3' 
      (Pt FL_LIT(35.6285)   FL_LIT(8.9334)  FL_LIT(42.7926)) -- O3' 
      (Pt FL_LIT(38.1482)  FL_LIT(15.2833)  FL_LIT(46.4641)) -- N1  
      (Pt FL_LIT(37.3641)  FL_LIT(13.0968)  FL_LIT(45.9007)) -- N3  
      (Pt FL_LIT(37.5032)  FL_LIT(14.1288)  FL_LIT(46.7300)) -- C2  
      (Pt FL_LIT(37.9570)  FL_LIT(13.3377)  FL_LIT(44.7113)) -- C4  
      (Pt FL_LIT(38.6397)  FL_LIT(14.4660)  FL_LIT(44.3267)) -- C5  
      (Pt FL_LIT(38.7473)  FL_LIT(15.5229)  FL_LIT(45.2609)) -- C6
      (A
      (Pt FL_LIT(39.3720)  FL_LIT(16.6649)  FL_LIT(45.0297)) -- N6  
      (Pt FL_LIT(39.1079)  FL_LIT(14.3351)  FL_LIT(43.0223)) -- N7  
      (Pt FL_LIT(38.0132)  FL_LIT(12.4868)  FL_LIT(43.6280)) -- N9  
      (Pt FL_LIT(38.7058)  FL_LIT(13.1402)  FL_LIT(42.6620)) -- C8  
      (Pt FL_LIT(37.0731)  FL_LIT(14.0857)  FL_LIT(47.7306)) -- H2  
      (Pt FL_LIT(39.8113)  FL_LIT(16.8281)  FL_LIT(44.1350)) -- H61 
      (Pt FL_LIT(39.4100)  FL_LIT(17.3741)  FL_LIT(45.7478)) -- H62 
      (Pt FL_LIT(39.0412)  FL_LIT(12.9660)  FL_LIT(41.6397)) -- H8  
      )

rA10
  = Nuc
      (Tfo  FL_LIT(0.7063)   FL_LIT(0.6317)  FL_LIT(-0.3196)  -- dgf_base_tfo
           FL_LIT(-0.0403)  FL_LIT(-0.4149)  FL_LIT(-0.9090)
           FL_LIT(-0.7068)   FL_LIT(0.6549)  FL_LIT(-0.2676)
            FL_LIT(6.4402) FL_LIT(-52.1496)  FL_LIT(30.8246))
      (Tfo  FL_LIT(0.7529)   FL_LIT(0.1548)   FL_LIT(0.6397)  -- p_o3'_275_tfo
            FL_LIT(0.2952)  FL_LIT(-0.9481)  FL_LIT(-0.1180)
            FL_LIT(0.5882)   FL_LIT(0.2777)  FL_LIT(-0.7595)
          FL_LIT(-58.8919) FL_LIT(-11.3095)   FL_LIT(6.0866))
      (Tfo FL_LIT(-0.0239)   FL_LIT(0.9667)  FL_LIT(-0.2546)  -- p_o3'_180_tfo
            FL_LIT(0.9731)  FL_LIT(-0.0359)  FL_LIT(-0.2275)
           FL_LIT(-0.2290)  FL_LIT(-0.2532)  FL_LIT(-0.9399)
            FL_LIT(3.5401) FL_LIT(-29.7913)  FL_LIT(52.2796))
      (Tfo FL_LIT(-0.8912)  FL_LIT(-0.4531)   FL_LIT(0.0242)  -- p_o3'_60_tfo
           FL_LIT(-0.1183)   FL_LIT(0.1805)  FL_LIT(-0.9764)
            FL_LIT(0.4380)  FL_LIT(-0.8730)  FL_LIT(-0.2145)
           FL_LIT(19.9023)  FL_LIT(54.8054)  FL_LIT(15.2799))
      (Pt FL_LIT(41.8210)   FL_LIT(8.3880)  FL_LIT(43.5890)) -- P   
      (Pt FL_LIT(42.5400)   FL_LIT(8.0450)  FL_LIT(44.8330)) -- O1P 
      (Pt FL_LIT(42.2470)   FL_LIT(9.6920)  FL_LIT(42.9910)) -- O2P 
      (Pt FL_LIT(40.2550)   FL_LIT(8.2030)  FL_LIT(43.7340)) -- O5' 
      (Pt FL_LIT(39.4850)   FL_LIT(8.9301)  FL_LIT(44.6977)) -- C5' 
      (Pt FL_LIT(39.0638)   FL_LIT(9.8199)  FL_LIT(44.2296)) -- H5' 
      (Pt FL_LIT(40.0757)   FL_LIT(9.0713)  FL_LIT(45.6029)) -- H5''
      (Pt FL_LIT(38.3102)   FL_LIT(8.0414)  FL_LIT(45.0789)) -- C4' 
      (Pt FL_LIT(37.7099)   FL_LIT(7.8166)  FL_LIT(44.1973)) -- H4' 
      (Pt FL_LIT(38.8012)   FL_LIT(6.8321)  FL_LIT(45.6380)) -- O4' 
      (Pt FL_LIT(38.2431)   FL_LIT(6.6413)  FL_LIT(46.9529)) -- C1' 
      (Pt FL_LIT(37.3505)   FL_LIT(6.0262)  FL_LIT(46.8385)) -- H1' 
      (Pt FL_LIT(37.8484)   FL_LIT(8.0156)  FL_LIT(47.4214)) -- C2' 
      (Pt FL_LIT(38.7381)   FL_LIT(8.5406)  FL_LIT(47.7690)) -- H2''
      (Pt FL_LIT(36.8286)   FL_LIT(8.0368)  FL_LIT(48.3701)) -- O2' 
      (Pt FL_LIT(36.8392)   FL_LIT(7.3063)  FL_LIT(48.9929)) -- H2' 
      (Pt FL_LIT(37.3576)   FL_LIT(8.6512)  FL_LIT(46.1132)) -- C3' 
      (Pt FL_LIT(37.5207)   FL_LIT(9.7275)  FL_LIT(46.1671)) -- H3' 
      (Pt FL_LIT(35.9985)   FL_LIT(8.2392)  FL_LIT(45.9032)) -- O3' 
      (Pt FL_LIT(39.9117)   FL_LIT(2.2278)  FL_LIT(48.8527)) -- N1  
      (Pt FL_LIT(38.6207)   FL_LIT(3.6941)  FL_LIT(47.4757)) -- N3  
      (Pt FL_LIT(38.9872)   FL_LIT(2.4888)  FL_LIT(47.9057)) -- C2  
      (Pt FL_LIT(39.2961)   FL_LIT(4.6720)  FL_LIT(48.1174)) -- C4  
      (Pt FL_LIT(40.2546)   FL_LIT(4.5307)  FL_LIT(49.0912)) -- C5  
      (Pt FL_LIT(40.5932)   FL_LIT(3.2189)  FL_LIT(49.4985)) -- C6
      (A
      (Pt FL_LIT(41.4938)   FL_LIT(2.9317)  FL_LIT(50.4229)) -- N6  
      (Pt FL_LIT(40.7195)   FL_LIT(5.7755)  FL_LIT(49.5060)) -- N7  
      (Pt FL_LIT(39.1730)   FL_LIT(6.0305)  FL_LIT(47.9170)) -- N9  
      (Pt FL_LIT(40.0413)   FL_LIT(6.6250)  FL_LIT(48.7728)) -- C8  
      (Pt FL_LIT(38.5257)   FL_LIT(1.5960)  FL_LIT(47.4838)) -- H2  
      (Pt FL_LIT(41.9907)   FL_LIT(3.6753)  FL_LIT(50.8921)) -- H61 
      (Pt FL_LIT(41.6848)   FL_LIT(1.9687)  FL_LIT(50.6599)) -- H62 
      (Pt FL_LIT(40.3571)   FL_LIT(7.6321)  FL_LIT(49.0452)) -- H8  
      )

rAs = [rA01,rA02,rA03,rA04,rA05,rA06,rA07,rA08,rA09,rA10]

