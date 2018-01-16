#include "unboxery.h"

module RG(rG,rG') where
import Types

rG
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
      (G
      (Pt  FL_LIT(6.8426)   FL_LIT(0.0056)  FL_LIT(-0.0019)) -- N2  
      (Pt  FL_LIT(3.1660)   FL_LIT(3.7290)  FL_LIT(-1.0360)) -- N7  
      (Pt  FL_LIT(5.3170)   FL_LIT(4.2990)  FL_LIT(-1.1930)) -- N9  
      (Pt  FL_LIT(4.0100)   FL_LIT(4.6780)  FL_LIT(-1.2990)) -- C8  
      (Pt  FL_LIT(2.4280)   FL_LIT(0.8450)  FL_LIT(-0.2360)) -- O6  
      (Pt  FL_LIT(4.6151)  FL_LIT(-0.4677)   FL_LIT(0.1305)) -- H1  
      (Pt  FL_LIT(6.6463)  FL_LIT(-0.9463)   FL_LIT(0.2729)) -- H21 
      (Pt  FL_LIT(7.8170)   FL_LIT(0.2642)  FL_LIT(-0.0640)) -- H22 
      (Pt  FL_LIT(3.4421)   FL_LIT(5.5744)  FL_LIT(-1.5482)) -- H8  
      )

rG01
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
      (G
      (Pt  FL_LIT(6.8745)  FL_LIT(-0.0224)  FL_LIT(-0.0058)) -- N2  
      (Pt  FL_LIT(3.1770)   FL_LIT(3.6859)  FL_LIT(-1.0198)) -- N7  
      (Pt  FL_LIT(5.3247)   FL_LIT(4.2695)  FL_LIT(-1.1710)) -- N9  
      (Pt  FL_LIT(4.0156)   FL_LIT(4.6415)  FL_LIT(-1.2759)) -- C8  
      (Pt  FL_LIT(2.4553)   FL_LIT(0.7925)  FL_LIT(-0.2390)) -- O6  
      (Pt  FL_LIT(4.6497)  FL_LIT(-0.5095)   FL_LIT(0.1212)) -- H1  
      (Pt  FL_LIT(6.6836)  FL_LIT(-0.9771)   FL_LIT(0.2627)) -- H21 
      (Pt  FL_LIT(7.8474)   FL_LIT(0.2424)  FL_LIT(-0.0653)) -- H22 
      (Pt  FL_LIT(3.4426)   FL_LIT(5.5361)  FL_LIT(-1.5199)) -- H8  
      )

rG02
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
      (G
      (Pt FL_LIT(11.6077)   FL_LIT(6.7966)   FL_LIT(1.2752)) -- N2  
      (Pt  FL_LIT(7.2750)   FL_LIT(9.4537)  FL_LIT(-0.3428)) -- N7  
      (Pt  FL_LIT(7.7962)   FL_LIT(7.5519)  FL_LIT(-1.3859)) -- N9  
      (Pt  FL_LIT(6.9479)   FL_LIT(8.6157)  FL_LIT(-1.2771)) -- C8  
      (Pt  FL_LIT(9.0664)  FL_LIT(10.4462)   FL_LIT(1.9610)) -- O6  
      (Pt FL_LIT(10.9838)   FL_LIT(8.7524)   FL_LIT(2.2697)) -- H1  
      (Pt FL_LIT(12.2274)   FL_LIT(7.0896)   FL_LIT(2.0170)) -- H21 
      (Pt FL_LIT(11.8502)   FL_LIT(5.9398)   FL_LIT(0.7984)) -- H22 
      (Pt  FL_LIT(6.0430)   FL_LIT(8.9853)  FL_LIT(-1.7594)) -- H8  
      )

rG03
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
      (G
      (Pt FL_LIT(10.9733)   FL_LIT(3.5117)  FL_LIT(-6.4286)) -- N2  
      (Pt  FL_LIT(6.4857)   FL_LIT(6.3816)  FL_LIT(-6.7035)) -- N7  
      (Pt  FL_LIT(6.9740)   FL_LIT(5.3703)  FL_LIT(-4.7760)) -- N9  
      (Pt  FL_LIT(6.1133)   FL_LIT(6.1613)  FL_LIT(-5.4808)) -- C8  
      (Pt  FL_LIT(8.4084)   FL_LIT(6.0747)  FL_LIT(-9.0933)) -- O6  
      (Pt FL_LIT(10.3759)   FL_LIT(4.5855)  FL_LIT(-8.3504)) -- H1  
      (Pt FL_LIT(11.6254)   FL_LIT(3.3761)  FL_LIT(-7.1879)) -- H21 
      (Pt FL_LIT(11.1917)   FL_LIT(3.0460)  FL_LIT(-5.5593)) -- H22 
      (Pt  FL_LIT(5.1705)   FL_LIT(6.6830)  FL_LIT(-5.3167)) -- H8  
      )

rG04
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
      (G
      (Pt  FL_LIT(5.1433)   FL_LIT(3.4373)   FL_LIT(3.4609)) -- N2  
      (Pt  FL_LIT(3.4605)   FL_LIT(3.1184)  FL_LIT(-1.5906)) -- N7  
      (Pt  FL_LIT(5.3247)   FL_LIT(4.2695)  FL_LIT(-1.1710)) -- N9  
      (Pt  FL_LIT(4.4244)   FL_LIT(3.8244)  FL_LIT(-2.0953)) -- C8  
      (Pt  FL_LIT(1.9600)   FL_LIT(1.7805)   FL_LIT(0.7462)) -- O6  
      (Pt  FL_LIT(3.2489)   FL_LIT(2.2879)   FL_LIT(2.9191)) -- H1  
      (Pt  FL_LIT(4.6785)   FL_LIT(3.0243)   FL_LIT(4.2568)) -- H21 
      (Pt  FL_LIT(5.9823)   FL_LIT(3.9654)   FL_LIT(3.6539)) -- H22 
      (Pt  FL_LIT(4.2675)   FL_LIT(3.8876)  FL_LIT(-3.1721)) -- H8  
      )

rG05
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
      (G
      (Pt FL_LIT(11.5110)  FL_LIT(10.1256)  FL_LIT(-2.7114)) -- N2  
      (Pt  FL_LIT(7.2891)   FL_LIT(8.9068)   FL_LIT(0.3121)) -- N7  
      (Pt  FL_LIT(7.7962)   FL_LIT(7.5519)  FL_LIT(-1.3859)) -- N9  
      (Pt  FL_LIT(6.9702)   FL_LIT(7.8292)  FL_LIT(-0.3353)) -- C8  
      (Pt  FL_LIT(9.0349)  FL_LIT(11.3951)   FL_LIT(0.8250)) -- O6  
      (Pt FL_LIT(10.9013)  FL_LIT(11.4422)  FL_LIT(-0.9512)) -- H1  
      (Pt FL_LIT(12.1031)  FL_LIT(10.9341)  FL_LIT(-2.5861)) -- H21 
      (Pt FL_LIT(11.7369)   FL_LIT(9.5180)  FL_LIT(-3.4859)) -- H22 
      (Pt  FL_LIT(6.0888)   FL_LIT(7.3990)   FL_LIT(0.1403)) -- H8  
      )

rG06
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
      (G
      (Pt  FL_LIT(6.2717)   FL_LIT(1.5402)  FL_LIT(-7.4250)) -- N2  
      (Pt  FL_LIT(7.2573)   FL_LIT(6.7070)  FL_LIT(-6.5394)) -- N7  
      (Pt  FL_LIT(6.9740)   FL_LIT(5.3703)  FL_LIT(-4.7760)) -- N9  
      (Pt  FL_LIT(7.2238)   FL_LIT(6.6275)  FL_LIT(-5.2453)) -- C8  
      (Pt  FL_LIT(7.0668)   FL_LIT(5.5163)  FL_LIT(-9.3763)) -- O6  
      (Pt  FL_LIT(6.5754)   FL_LIT(2.9964)  FL_LIT(-9.1545)) -- H1  
      (Pt  FL_LIT(6.1908)   FL_LIT(1.1105)  FL_LIT(-8.3354)) -- H21 
      (Pt  FL_LIT(6.1346)   FL_LIT(0.9352)  FL_LIT(-6.6280)) -- H22 
      (Pt  FL_LIT(7.4108)   FL_LIT(7.6227)  FL_LIT(-4.8418)) -- H8  
      )

rG07
  = Nuc
      (Tfo  FL_LIT(0.0894)  FL_LIT(-0.6059)   FL_LIT(0.7905)  -- dgf_base_tfo
           FL_LIT(-0.6810)   FL_LIT(0.5420)   FL_LIT(0.4924)
           FL_LIT(-0.7268)  FL_LIT(-0.5824)  FL_LIT(-0.3642)
           FL_LIT(34.1424)  FL_LIT(45.9610) FL_LIT(-11.8600))
      (Tfo FL_LIT(-0.8644)  FL_LIT(-0.4956)  FL_LIT(-0.0851)  -- p_o3'_275_tfo
           FL_LIT(-0.0427)   FL_LIT(0.2409)  FL_LIT(-0.9696)
            FL_LIT(0.5010)  FL_LIT(-0.8345)  FL_LIT(-0.2294)
            FL_LIT(4.0167)  FL_LIT(54.5377)  FL_LIT(12.4779))
      (Tfo  FL_LIT(0.3706)  FL_LIT(-0.6167)   FL_LIT(0.6945)  -- p_o3'_180_tfo
           FL_LIT(-0.2867)  FL_LIT(-0.7872)  FL_LIT(-0.5460)
            FL_LIT(0.8834)   FL_LIT(0.0032)  FL_LIT(-0.4686)
          FL_LIT(-52.9020)  FL_LIT(18.6313)  FL_LIT(-0.6709))
      (Tfo  FL_LIT(0.4155)   FL_LIT(0.9025)  FL_LIT(-0.1137)  -- p_o3'_60_tfo
            FL_LIT(0.9040)  FL_LIT(-0.4236)  FL_LIT(-0.0582)
           FL_LIT(-0.1007)  FL_LIT(-0.0786)  FL_LIT(-0.9918)
           FL_LIT(-7.6624) FL_LIT(-25.2080)  FL_LIT(49.5181))
      (Pt FL_LIT(31.3810)   FL_LIT(0.1400)  FL_LIT(47.5810)) -- P   
      (Pt FL_LIT(29.9860)   FL_LIT(0.6630)  FL_LIT(47.6290)) -- O1P 
      (Pt FL_LIT(31.7210)  FL_LIT(-0.6460)  FL_LIT(48.8090)) -- O2P 
      (Pt FL_LIT(32.4940)   FL_LIT(1.2540)  FL_LIT(47.2740)) -- O5' 
      (Pt FL_LIT(33.8709)   FL_LIT(0.7918)  FL_LIT(47.2113)) -- C5' 
      (Pt FL_LIT(34.1386)   FL_LIT(0.5870)  FL_LIT(46.1747)) -- H5' 
      (Pt FL_LIT(34.0186)  FL_LIT(-0.0095)  FL_LIT(47.9353)) -- H5''
      (Pt FL_LIT(34.7297)   FL_LIT(1.9687)  FL_LIT(47.6685)) -- C4' 
      (Pt FL_LIT(35.7723)   FL_LIT(1.6845)  FL_LIT(47.8113)) -- H4' 
      (Pt FL_LIT(34.6455)   FL_LIT(2.9768)  FL_LIT(46.6660)) -- O4' 
      (Pt FL_LIT(34.1690)   FL_LIT(4.1829)  FL_LIT(47.2627)) -- C1' 
      (Pt FL_LIT(35.0437)   FL_LIT(4.7633)  FL_LIT(47.5560)) -- H1' 
      (Pt FL_LIT(33.4145)   FL_LIT(3.7532)  FL_LIT(48.4954)) -- C2' 
      (Pt FL_LIT(32.4340)   FL_LIT(3.3797)  FL_LIT(48.2001)) -- H2''
      (Pt FL_LIT(33.3209)   FL_LIT(4.6953)  FL_LIT(49.5217)) -- O2' 
      (Pt FL_LIT(33.2374)   FL_LIT(5.6059)  FL_LIT(49.2295)) -- H2' 
      (Pt FL_LIT(34.2724)   FL_LIT(2.5970)  FL_LIT(48.9773)) -- C3' 
      (Pt FL_LIT(33.6373)   FL_LIT(1.8935)  FL_LIT(49.5157)) -- H3' 
      (Pt FL_LIT(35.3453)   FL_LIT(3.1884)  FL_LIT(49.7285)) -- O3' 
      (Pt FL_LIT(34.0511)   FL_LIT(7.8930)  FL_LIT(43.7791)) -- N1  
      (Pt FL_LIT(34.9937)   FL_LIT(6.3369)  FL_LIT(45.3199)) -- N3  
      (Pt FL_LIT(35.0882)   FL_LIT(7.3126)  FL_LIT(44.4200)) -- C2  
      (Pt FL_LIT(33.7190)   FL_LIT(5.9650)  FL_LIT(45.5374)) -- C4  
      (Pt FL_LIT(32.5845)   FL_LIT(6.4770)  FL_LIT(44.9458)) -- C5  
      (Pt FL_LIT(32.7430)   FL_LIT(7.5179)  FL_LIT(43.9914)) -- C6
      (G
      (Pt FL_LIT(36.3030)   FL_LIT(7.7827)  FL_LIT(44.1036)) -- N2  
      (Pt FL_LIT(31.4499)   FL_LIT(5.8335)  FL_LIT(45.4368)) -- N7  
      (Pt FL_LIT(33.2760)   FL_LIT(4.9817)  FL_LIT(46.4043)) -- N9  
      (Pt FL_LIT(31.9235)   FL_LIT(4.9639)  FL_LIT(46.2934)) -- C8  
      (Pt FL_LIT(31.8602)   FL_LIT(8.1000)  FL_LIT(43.3695)) -- O6  
      (Pt FL_LIT(34.2623)   FL_LIT(8.6223)  FL_LIT(43.1283)) -- H1  
      (Pt FL_LIT(36.5188)   FL_LIT(8.5081)  FL_LIT(43.4347)) -- H21 
      (Pt FL_LIT(37.0888)   FL_LIT(7.3524)  FL_LIT(44.5699)) -- H22 
      (Pt FL_LIT(31.0815)   FL_LIT(4.4201)  FL_LIT(46.7218)) -- H8  
      )

rG08
  = Nuc
      (Tfo  FL_LIT(0.2224)   FL_LIT(0.6335)   FL_LIT(0.7411)  -- dgf_base_tfo
           FL_LIT(-0.3644)  FL_LIT(-0.6510)   FL_LIT(0.6659)
            FL_LIT(0.9043)  FL_LIT(-0.4181)   FL_LIT(0.0861)
          FL_LIT(-47.6824)  FL_LIT(-0.5823) FL_LIT(-31.7554))
      (Tfo FL_LIT(-0.8644)  FL_LIT(-0.4956)  FL_LIT(-0.0851)  -- p_o3'_275_tfo
           FL_LIT(-0.0427)   FL_LIT(0.2409)  FL_LIT(-0.9696)
            FL_LIT(0.5010)  FL_LIT(-0.8345)  FL_LIT(-0.2294)
            FL_LIT(4.0167)  FL_LIT(54.5377)  FL_LIT(12.4779))
      (Tfo  FL_LIT(0.3706)  FL_LIT(-0.6167)   FL_LIT(0.6945)  -- p_o3'_180_tfo
           FL_LIT(-0.2867)  FL_LIT(-0.7872)  FL_LIT(-0.5460)
            FL_LIT(0.8834)   FL_LIT(0.0032)  FL_LIT(-0.4686)
          FL_LIT(-52.9020)  FL_LIT(18.6313)  FL_LIT(-0.6709))
      (Tfo  FL_LIT(0.4155)   FL_LIT(0.9025)  FL_LIT(-0.1137)  -- p_o3'_60_tfo
            FL_LIT(0.9040)  FL_LIT(-0.4236)  FL_LIT(-0.0582)
           FL_LIT(-0.1007)  FL_LIT(-0.0786)  FL_LIT(-0.9918)
           FL_LIT(-7.6624) FL_LIT(-25.2080)  FL_LIT(49.5181))
      (Pt FL_LIT(31.3810)   FL_LIT(0.1400)  FL_LIT(47.5810)) -- P   
      (Pt FL_LIT(29.9860)   FL_LIT(0.6630)  FL_LIT(47.6290)) -- O1P 
      (Pt FL_LIT(31.7210)  FL_LIT(-0.6460)  FL_LIT(48.8090)) -- O2P 
      (Pt FL_LIT(32.4940)   FL_LIT(1.2540)  FL_LIT(47.2740)) -- O5' 
      (Pt FL_LIT(32.5924)   FL_LIT(2.3488)  FL_LIT(48.2255)) -- C5' 
      (Pt FL_LIT(33.3674)   FL_LIT(2.1246)  FL_LIT(48.9584)) -- H5' 
      (Pt FL_LIT(31.5994)   FL_LIT(2.5917)  FL_LIT(48.6037)) -- H5''
      (Pt FL_LIT(33.0722)   FL_LIT(3.5577)  FL_LIT(47.4258)) -- C4' 
      (Pt FL_LIT(33.0310)   FL_LIT(4.4778)  FL_LIT(48.0089)) -- H4' 
      (Pt FL_LIT(34.4173)   FL_LIT(3.3055)  FL_LIT(47.0316)) -- O4' 
      (Pt FL_LIT(34.5056)   FL_LIT(3.3910)  FL_LIT(45.6094)) -- C1' 
      (Pt FL_LIT(34.7881)   FL_LIT(4.4152)  FL_LIT(45.3663)) -- H1' 
      (Pt FL_LIT(33.1122)   FL_LIT(3.1198)  FL_LIT(45.1010)) -- C2' 
      (Pt FL_LIT(32.9230)   FL_LIT(2.0469)  FL_LIT(45.1369)) -- H2''
      (Pt FL_LIT(32.7946)   FL_LIT(3.6590)  FL_LIT(43.8529)) -- O2' 
      (Pt FL_LIT(33.5170)   FL_LIT(3.6707)  FL_LIT(43.2207)) -- H2' 
      (Pt FL_LIT(32.2730)   FL_LIT(3.8173)  FL_LIT(46.1566)) -- C3' 
      (Pt FL_LIT(31.3094)   FL_LIT(3.3123)  FL_LIT(46.2244)) -- H3' 
      (Pt FL_LIT(32.2391)   FL_LIT(5.2039)  FL_LIT(45.7807)) -- O3' 
      (Pt FL_LIT(39.3337)   FL_LIT(2.7157)  FL_LIT(44.1441)) -- N1  
      (Pt FL_LIT(37.4430)   FL_LIT(3.8242)  FL_LIT(45.0824)) -- N3  
      (Pt FL_LIT(38.7276)   FL_LIT(3.7646)  FL_LIT(44.7403)) -- C2  
      (Pt FL_LIT(36.7791)   FL_LIT(2.6963)  FL_LIT(44.7704)) -- C4  
      (Pt FL_LIT(37.2860)   FL_LIT(1.5653)  FL_LIT(44.1678)) -- C5  
      (Pt FL_LIT(38.6647)   FL_LIT(1.5552)  FL_LIT(43.8235)) -- C6
      (G
      (Pt FL_LIT(39.5123)   FL_LIT(4.8216)  FL_LIT(44.9936)) -- N2  
      (Pt FL_LIT(36.2829)   FL_LIT(0.6110)  FL_LIT(44.0078)) -- N7  
      (Pt FL_LIT(35.4394)   FL_LIT(2.4314)  FL_LIT(44.9931)) -- N9  
      (Pt FL_LIT(35.2180)   FL_LIT(1.1815)  FL_LIT(44.5128)) -- C8  
      (Pt FL_LIT(39.2907)   FL_LIT(0.6514)  FL_LIT(43.2796)) -- O6  
      (Pt FL_LIT(40.3076)   FL_LIT(2.8048)  FL_LIT(43.9352)) -- H1  
      (Pt FL_LIT(40.4994)   FL_LIT(4.9066)  FL_LIT(44.7977)) -- H21 
      (Pt FL_LIT(39.0738)   FL_LIT(5.6108)  FL_LIT(45.4464)) -- H22 
      (Pt FL_LIT(34.3856)   FL_LIT(0.4842)  FL_LIT(44.4185)) -- H8  
      )

rG09
  = Nuc
      (Tfo FL_LIT(-0.9699)  FL_LIT(-0.1688)  FL_LIT(-0.1753)  -- dgf_base_tfo
           FL_LIT(-0.1050)  FL_LIT(-0.3598)   FL_LIT(0.9271)
           FL_LIT(-0.2196)   FL_LIT(0.9176)   FL_LIT(0.3312)
           FL_LIT(45.6217) FL_LIT(-38.9484) FL_LIT(-12.3208))
      (Tfo FL_LIT(-0.8644)  FL_LIT(-0.4956)  FL_LIT(-0.0851)  -- p_o3'_275_tfo
           FL_LIT(-0.0427)   FL_LIT(0.2409)  FL_LIT(-0.9696)
            FL_LIT(0.5010)  FL_LIT(-0.8345)  FL_LIT(-0.2294)
            FL_LIT(4.0167)  FL_LIT(54.5377)  FL_LIT(12.4779))
      (Tfo  FL_LIT(0.3706)  FL_LIT(-0.6167)   FL_LIT(0.6945)  -- p_o3'_180_tfo
           FL_LIT(-0.2867)  FL_LIT(-0.7872)  FL_LIT(-0.5460)
            FL_LIT(0.8834)   FL_LIT(0.0032)  FL_LIT(-0.4686)
          FL_LIT(-52.9020)  FL_LIT(18.6313)  FL_LIT(-0.6709))
      (Tfo  FL_LIT(0.4155)   FL_LIT(0.9025)  FL_LIT(-0.1137)  -- p_o3'_60_tfo
            FL_LIT(0.9040)  FL_LIT(-0.4236)  FL_LIT(-0.0582)
           FL_LIT(-0.1007)  FL_LIT(-0.0786)  FL_LIT(-0.9918)
           FL_LIT(-7.6624) FL_LIT(-25.2080)  FL_LIT(49.5181))
      (Pt FL_LIT(31.3810)   FL_LIT(0.1400)  FL_LIT(47.5810)) -- P   
      (Pt FL_LIT(29.9860)   FL_LIT(0.6630)  FL_LIT(47.6290)) -- O1P 
      (Pt FL_LIT(31.7210)  FL_LIT(-0.6460)  FL_LIT(48.8090)) -- O2P 
      (Pt FL_LIT(32.4940)   FL_LIT(1.2540)  FL_LIT(47.2740)) -- O5' 
      (Pt FL_LIT(33.8709)   FL_LIT(0.7918)  FL_LIT(47.2113)) -- C5' 
      (Pt FL_LIT(34.1386)   FL_LIT(0.5870)  FL_LIT(46.1747)) -- H5' 
      (Pt FL_LIT(34.0186)  FL_LIT(-0.0095)  FL_LIT(47.9353)) -- H5''
      (Pt FL_LIT(34.7297)   FL_LIT(1.9687)  FL_LIT(47.6685)) -- C4' 
      (Pt FL_LIT(34.5880)   FL_LIT(2.8482)  FL_LIT(47.0404)) -- H4' 
      (Pt FL_LIT(34.3575)   FL_LIT(2.2770)  FL_LIT(49.0081)) -- O4' 
      (Pt FL_LIT(35.5157)   FL_LIT(2.1993)  FL_LIT(49.8389)) -- C1' 
      (Pt FL_LIT(35.9424)   FL_LIT(3.2010)  FL_LIT(49.8893)) -- H1' 
      (Pt FL_LIT(36.4701)   FL_LIT(1.2820)  FL_LIT(49.1169)) -- C2' 
      (Pt FL_LIT(36.1545)   FL_LIT(0.2498)  FL_LIT(49.2683)) -- H2''
      (Pt FL_LIT(37.8262)   FL_LIT(1.4547)  FL_LIT(49.4008)) -- O2' 
      (Pt FL_LIT(38.0227)   FL_LIT(1.6945)  FL_LIT(50.3094)) -- H2' 
      (Pt FL_LIT(36.2242)   FL_LIT(1.6797)  FL_LIT(47.6725)) -- C3' 
      (Pt FL_LIT(36.4297)   FL_LIT(0.8197)  FL_LIT(47.0351)) -- H3' 
      (Pt FL_LIT(37.0289)   FL_LIT(2.8480)  FL_LIT(47.4426)) -- O3' 
      (Pt FL_LIT(34.3005)   FL_LIT(3.5042)  FL_LIT(54.6070)) -- N1  
      (Pt FL_LIT(34.7693)   FL_LIT(3.7936)  FL_LIT(52.2874)) -- N3  
      (Pt FL_LIT(34.4484)   FL_LIT(4.2541)  FL_LIT(53.4939)) -- C2  
      (Pt FL_LIT(34.9354)   FL_LIT(2.4584)  FL_LIT(52.2785)) -- C4  
      (Pt FL_LIT(34.8092)   FL_LIT(1.5915)  FL_LIT(53.3422)) -- C5  
      (Pt FL_LIT(34.4646)   FL_LIT(2.1367)  FL_LIT(54.6085)) -- C6
      (G
      (Pt FL_LIT(34.2514)   FL_LIT(5.5708)  FL_LIT(53.6503)) -- N2  
      (Pt FL_LIT(35.0641)   FL_LIT(0.2835)  FL_LIT(52.9337)) -- N7  
      (Pt FL_LIT(35.2669)   FL_LIT(1.6690)  FL_LIT(51.1915)) -- N9  
      (Pt FL_LIT(35.3288)   FL_LIT(0.3954)  FL_LIT(51.6563)) -- C8  
      (Pt FL_LIT(34.3151)   FL_LIT(1.5317)  FL_LIT(55.6650)) -- O6  
      (Pt FL_LIT(34.0623)   FL_LIT(3.9797)  FL_LIT(55.4539)) -- H1  
      (Pt FL_LIT(33.9950)   FL_LIT(6.0502)  FL_LIT(54.5016)) -- H21 
      (Pt FL_LIT(34.3512)   FL_LIT(6.1432)  FL_LIT(52.8242)) -- H22 
      (Pt FL_LIT(35.5414)  FL_LIT(-0.6006)  FL_LIT(51.2679)) -- H8  
      )

rG10
  = Nuc
      (Tfo FL_LIT(-0.0980)  FL_LIT(-0.9723)   FL_LIT(0.2122)  -- dgf_base_tfo
           FL_LIT(-0.9731)   FL_LIT(0.1383)   FL_LIT(0.1841)
           FL_LIT(-0.2083)  FL_LIT(-0.1885)  FL_LIT(-0.9597)
           FL_LIT(17.8469)  FL_LIT(38.8265)  FL_LIT(37.0475))
      (Tfo FL_LIT(-0.8644)  FL_LIT(-0.4956)  FL_LIT(-0.0851)  -- p_o3'_275_tfo
           FL_LIT(-0.0427)   FL_LIT(0.2409)  FL_LIT(-0.9696)
            FL_LIT(0.5010)  FL_LIT(-0.8345)  FL_LIT(-0.2294)
            FL_LIT(4.0167)  FL_LIT(54.5377)  FL_LIT(12.4779))
      (Tfo  FL_LIT(0.3706)  FL_LIT(-0.6167)   FL_LIT(0.6945)  -- p_o3'_180_tfo
           FL_LIT(-0.2867)  FL_LIT(-0.7872)  FL_LIT(-0.5460)
            FL_LIT(0.8834)   FL_LIT(0.0032)  FL_LIT(-0.4686)
          FL_LIT(-52.9020)  FL_LIT(18.6313)  FL_LIT(-0.6709))
      (Tfo  FL_LIT(0.4155)   FL_LIT(0.9025)  FL_LIT(-0.1137)  -- p_o3'_60_tfo
            FL_LIT(0.9040)  FL_LIT(-0.4236)  FL_LIT(-0.0582)
           FL_LIT(-0.1007)  FL_LIT(-0.0786)  FL_LIT(-0.9918)
           FL_LIT(-7.6624) FL_LIT(-25.2080)  FL_LIT(49.5181))
      (Pt FL_LIT(31.3810)   FL_LIT(0.1400)  FL_LIT(47.5810)) -- P   
      (Pt FL_LIT(29.9860)   FL_LIT(0.6630)  FL_LIT(47.6290)) -- O1P 
      (Pt FL_LIT(31.7210)  FL_LIT(-0.6460)  FL_LIT(48.8090)) -- O2P 
      (Pt FL_LIT(32.4940)   FL_LIT(1.2540)  FL_LIT(47.2740)) -- O5' 
      (Pt FL_LIT(32.5924)   FL_LIT(2.3488)  FL_LIT(48.2255)) -- C5' 
      (Pt FL_LIT(33.3674)   FL_LIT(2.1246)  FL_LIT(48.9584)) -- H5' 
      (Pt FL_LIT(31.5994)   FL_LIT(2.5917)  FL_LIT(48.6037)) -- H5''
      (Pt FL_LIT(33.0722)   FL_LIT(3.5577)  FL_LIT(47.4258)) -- C4' 
      (Pt FL_LIT(34.0333)   FL_LIT(3.3761)  FL_LIT(46.9447)) -- H4' 
      (Pt FL_LIT(32.0890)   FL_LIT(3.8338)  FL_LIT(46.4332)) -- O4' 
      (Pt FL_LIT(31.6377)   FL_LIT(5.1787)  FL_LIT(46.5914)) -- C1' 
      (Pt FL_LIT(32.2499)   FL_LIT(5.8016)  FL_LIT(45.9392)) -- H1' 
      (Pt FL_LIT(31.9167)   FL_LIT(5.5319)  FL_LIT(48.0305)) -- C2' 
      (Pt FL_LIT(31.1507)   FL_LIT(5.0820)  FL_LIT(48.6621)) -- H2''
      (Pt FL_LIT(32.0865)   FL_LIT(6.8890)  FL_LIT(48.3114)) -- O2' 
      (Pt FL_LIT(31.5363)   FL_LIT(7.4819)  FL_LIT(47.7942)) -- H2' 
      (Pt FL_LIT(33.2398)   FL_LIT(4.8224)  FL_LIT(48.2563)) -- C3' 
      (Pt FL_LIT(33.3166)   FL_LIT(4.5570)  FL_LIT(49.3108)) -- H3' 
      (Pt FL_LIT(34.2528)   FL_LIT(5.7056)  FL_LIT(47.7476)) -- O3' 
      (Pt FL_LIT(28.2782)   FL_LIT(6.3049)  FL_LIT(42.9364)) -- N1  
      (Pt FL_LIT(30.4001)   FL_LIT(5.8547)  FL_LIT(43.9258)) -- N3  
      (Pt FL_LIT(29.6195)   FL_LIT(6.1568)  FL_LIT(42.8913)) -- C2  
      (Pt FL_LIT(29.7005)   FL_LIT(5.7006)  FL_LIT(45.0649)) -- C4  
      (Pt FL_LIT(28.3383)   FL_LIT(5.8221)  FL_LIT(45.2343)) -- C5  
      (Pt FL_LIT(27.5519)   FL_LIT(6.1461)  FL_LIT(44.0958)) -- C6
      (G
      (Pt FL_LIT(30.1838)   FL_LIT(6.3385)  FL_LIT(41.6890)) -- N2  
      (Pt FL_LIT(27.9936)   FL_LIT(5.5926)  FL_LIT(46.5651)) -- N7  
      (Pt FL_LIT(30.2046)   FL_LIT(5.3825)  FL_LIT(46.3136)) -- N9  
      (Pt FL_LIT(29.1371)   FL_LIT(5.3398)  FL_LIT(47.1506)) -- C8  
      (Pt FL_LIT(26.3361)   FL_LIT(6.3024)  FL_LIT(44.0495)) -- O6  
      (Pt FL_LIT(27.8122)   FL_LIT(6.5394)  FL_LIT(42.0833)) -- H1  
      (Pt FL_LIT(29.7125)   FL_LIT(6.5595)  FL_LIT(40.8235)) -- H21 
      (Pt FL_LIT(31.1859)   FL_LIT(6.2231)  FL_LIT(41.6389)) -- H22 
      (Pt FL_LIT(28.9406)   FL_LIT(5.1504)  FL_LIT(48.2059)) -- H8  
      )

rGs = [rG01,rG02,rG03,rG04,rG05,rG06,rG07,rG08,rG09,rG10]

rG'
  = Nuc
      (Tfo FL_LIT(-0.2067)  FL_LIT(-0.0264)   FL_LIT(0.9780)  -- dgf_base_tfo
            FL_LIT(0.9770)  FL_LIT(-0.0586)   FL_LIT(0.2049)
            FL_LIT(0.0519)   FL_LIT(0.9979)   FL_LIT(0.0379)
            FL_LIT(1.0331) FL_LIT(-46.8078) FL_LIT(-36.4742))
      (Tfo FL_LIT(-0.8644)  FL_LIT(-0.4956)  FL_LIT(-0.0851)  -- p_o3'_275_tfo
           FL_LIT(-0.0427)   FL_LIT(0.2409)  FL_LIT(-0.9696)
            FL_LIT(0.5010)  FL_LIT(-0.8345)  FL_LIT(-0.2294)
            FL_LIT(4.0167)  FL_LIT(54.5377)  FL_LIT(12.4779))
      (Tfo  FL_LIT(0.3706)  FL_LIT(-0.6167)   FL_LIT(0.6945)  -- p_o3'_180_tfo
           FL_LIT(-0.2867)  FL_LIT(-0.7872)  FL_LIT(-0.5460)
            FL_LIT(0.8834)   FL_LIT(0.0032)  FL_LIT(-0.4686)
          FL_LIT(-52.9020)  FL_LIT(18.6313)  FL_LIT(-0.6709))
      (Tfo  FL_LIT(0.4155)   FL_LIT(0.9025)  FL_LIT(-0.1137)  -- p_o3'_60_tfo
            FL_LIT(0.9040)  FL_LIT(-0.4236)  FL_LIT(-0.0582)
           FL_LIT(-0.1007)  FL_LIT(-0.0786)  FL_LIT(-0.9918)
           FL_LIT(-7.6624) FL_LIT(-25.2080)  FL_LIT(49.5181))
      (Pt FL_LIT(31.3810)   FL_LIT(0.1400)  FL_LIT(47.5810)) -- P   
      (Pt FL_LIT(29.9860)   FL_LIT(0.6630)  FL_LIT(47.6290)) -- O1P 
      (Pt FL_LIT(31.7210)  FL_LIT(-0.6460)  FL_LIT(48.8090)) -- O2P 
      (Pt FL_LIT(32.4940)   FL_LIT(1.2540)  FL_LIT(47.2740)) -- O5' 
      (Pt FL_LIT(32.1610)   FL_LIT(2.2370)  FL_LIT(46.2560)) -- C5' 
      (Pt FL_LIT(31.2986)   FL_LIT(2.8190)  FL_LIT(46.5812)) -- H5' 
      (Pt FL_LIT(32.0980)   FL_LIT(1.7468)  FL_LIT(45.2845)) -- H5''
      (Pt FL_LIT(33.3476)   FL_LIT(3.1959)  FL_LIT(46.1947)) -- C4' 
      (Pt FL_LIT(33.2668)   FL_LIT(3.8958)  FL_LIT(45.3630)) -- H4' 
      (Pt FL_LIT(33.3799)   FL_LIT(3.9183)  FL_LIT(47.4216)) -- O4' 
      (Pt FL_LIT(34.6515)   FL_LIT(3.7222)  FL_LIT(48.0398)) -- C1' 
      (Pt FL_LIT(35.2947)   FL_LIT(4.5412)  FL_LIT(47.7180)) -- H1' 
      (Pt FL_LIT(35.1756)   FL_LIT(2.4228)  FL_LIT(47.4827)) -- C2' 
      (Pt FL_LIT(34.6778)   FL_LIT(1.5937)  FL_LIT(47.9856)) -- H2''
      (Pt FL_LIT(36.5631)   FL_LIT(2.2672)  FL_LIT(47.4798)) -- O2' 
      (Pt FL_LIT(37.0163)   FL_LIT(2.6579)  FL_LIT(48.2305)) -- H2' 
      (Pt FL_LIT(34.6953)   FL_LIT(2.5043)  FL_LIT(46.0448)) -- C3' 
      (Pt FL_LIT(34.5444)   FL_LIT(1.4917)  FL_LIT(45.6706)) -- H3' 
      (Pt FL_LIT(35.6679)   FL_LIT(3.3009)  FL_LIT(45.3487)) -- O3' 
      (Pt FL_LIT(37.4804)   FL_LIT(4.0914)  FL_LIT(52.2559)) -- N1  
      (Pt FL_LIT(36.9670)   FL_LIT(4.1312)  FL_LIT(49.9281)) -- N3  
      (Pt FL_LIT(37.8045)   FL_LIT(4.2519)  FL_LIT(50.9550)) -- C2  
      (Pt FL_LIT(35.7171)   FL_LIT(3.8264)  FL_LIT(50.3222)) -- C4  
      (Pt FL_LIT(35.2668)   FL_LIT(3.6420)  FL_LIT(51.6115)) -- C5  
      (Pt FL_LIT(36.2037)   FL_LIT(3.7829)  FL_LIT(52.6706)) -- C6
      (G
      (Pt FL_LIT(39.0869)   FL_LIT(4.5552)  FL_LIT(50.7092)) -- N2  
      (Pt FL_LIT(33.9075)   FL_LIT(3.3338)  FL_LIT(51.6102)) -- N7  
      (Pt FL_LIT(34.6126)   FL_LIT(3.6358)  FL_LIT(49.5108)) -- N9  
      (Pt FL_LIT(33.5805)   FL_LIT(3.3442)  FL_LIT(50.3425)) -- C8  
      (Pt FL_LIT(35.9958)   FL_LIT(3.6512)  FL_LIT(53.8724)) -- O6  
      (Pt FL_LIT(38.2106)   FL_LIT(4.2053)  FL_LIT(52.9295)) -- H1  
      (Pt FL_LIT(39.8218)   FL_LIT(4.6863)  FL_LIT(51.3896)) -- H21 
      (Pt FL_LIT(39.3420)   FL_LIT(4.6857)  FL_LIT(49.7407)) -- H22 
      (Pt FL_LIT(32.5194)   FL_LIT(3.1070)  FL_LIT(50.2664)) -- H8  
      )

