.ps 9
.sp 2
.nf
c     monte carlo
c
c
c
c **********************************************************************
c *                                                                    *
c *  copyright, 1986, the regents of the university of california.     *
c *  this software was produced under a u. s. government contract      *
c *  (w-7405-eng-36) by the los alamos national laboratory, which is   *
c *  operated by the university of california for the u. s. department *
c *  of energy. the u. s. government is licensed to use, reproduce,    *
c *  and distribute this software. permission is granted to the public *
c *  to copy and use this software without charge, provided that this  *
c *  notice and any statement of authorship are reproduced on all      *
c *  copies. neither the government nor the university makes any       *
c *  warranty, express or implied, or assumes any liability            *
c *  responsibility for the use of this software.                      *
c *                                                                    *
c **********************************************************************
c
c
c
c-----------------------------------------------------------------------
c
c      you must supply a time routine to return the wall clock
c      time, a date function to return the date, and a jobtim
c      function to return the elapsed cputime for the run in seconds.
c
c      it may be necessary to change the parameters defining the
c      checksums for machines with different word sizes, the values
c      supplied are for the cray xmp
c
c      the parameter -scale- determines the relative size of the
c      run. scale = 10 is a standard run.
c
c-----------------------------------------------------------------------
c
      program gamteb
c
c        scalar monte carlo code to transport .001 to 20.0 mev
c        gamma rays in a carbon cylinder of length cl, radius crad
c
c        gamteb was received from buck thompson,x-6,february,1982
c        and incorporated into benchmark set as lbmk21a.  last
c        revision was in march, 1986 when it was updated to fortran77
c        standard
c
      implicit real*8(a-h,o-z),integer*4(i-n)
c
c     folowwing controls the run time
c
      parameter (scale = 1)
c
c     following is the number of particles
c
      parameter(npp = 1*scale)
c
c     following is a small number
c
      parameter (small = 1.d-70)
c
       parameter (zero = 0.0d+00)
       parameter (one = 1.0d+00)
       parameter (two = 2.0d+00)
       parameter (three = 3.0d+00)
       parameter (four = 4.0d+00)
       parameter (five = 5.0d+00)
       parameter (six = 6.0d+00)
       parameter (ten = 10.0d+00)
       parameter (twenty = 20.0d+00)
       parameter (pt5 = .5d+00)
       parameter (pt25 = .25d+00)
       parameter (pt3 = .3d+00)
       parameter (pt001 = .001d+00)
       parameter (ptdm10 = 1.0d-10)
       parameter (ptdm6 = 1.0d-06)
       parameter (dp10 = 1.0d+10)
c
c
      common x,y,z,ps,u,v,w,erg,ia,wt,np,uold,vold,wold,cl,cl2,crad2,dls,ja
      common /stats/ e(35),el(35),xc(35),xpp(35),xpe(35),trans(35),
     1   bscat(35),escpe(35),trans2(35),bscat2(35),escpe2(35)
     2 ,rtrans(35),rbscat(35),rescpe(35)
      common /bnkk/ bank(100,11),fim(2)
      common /stat1/ transi(35),bscati(35),escpei(35),btoti,etoti,ttoti
      common /stat2/ nesc,ntrans,nbscat,neco,nwco,nroul,ncol,nocol
      common /con/ wcp1,wcp2,wco,wcp,ec,krn,crad,
     1  btot,etot,ttot,btot2,etot2,ttot2,wr1,wrl,wrg,nr,inbnk,nbank

      dimension sums1(3), sums2(3)
      character*6 isumh(3)
      equivalence (sums2(1),btot),(sums2(2),etot),(sums2(3),ttot)
c-----------------------------------------------------------------------
c         statement functions
       real*8 jobtim
c     jobtim(blfgzt) = cputim(blfgzt)
 
c-----------------------------------------------------------------------
      data isumh(1),isumh(2),isumh(3)/'check1','check2','check3'/
      data sums1(1),sums1(2),sums1(3)/.62993311975616528286d-02,
     *  .80674857764934415627d+00,.27479963747314961608d+00/
      data rho/2.22d+00/
 
c
c     open output file
c
      open(6,file='gamteb.out',status='unknown')
      rewind(6)
c
c        convert cross-section units to be per cm.
      do 1 i=1,35
      xc(i)=log( xc(i)*rho )
      if(xpp(i).eq.zero) xpp(i)=small
      if(xpe(i).eq.zero) xpe(i)=small
      xpp(i)=log( xpp(i)*rho )
      xpe(i)=log( xpe(i)*rho )
      el(i)=log(e(i))
    1 continue
c
c        initialize problem input
      ncol=0
      nocol=0
      nesc=0
      nbscat=0
      ntrans=0
      nwco=0
      neco=0
      wcp1=pt5
      wcp2=pt25
      wco=0
      wcp=0
      ec = pt001
      krn = 123454321
      cl=twenty
      cl2=cl+ten
      crad=one
      crad2=crad**2
      btot = zero
      btot2 = zero
      ttot = zero
      ttot2 = zero
      etot = zero
      etot2 = zero
      wrl = zero
      wrg = zero
      nr=0
      inbnk=0
      nbank=0
      fim(1)=one
      fim(2)=two
      inbnk=0
      do 5 i = 1,35
      trans2(i)= zero
      bscat2(i)= zero
      escpe2(i)= zero
      trans(i) = zero
      bscat(i) = zero
    5 escpe(i) = zero
c
c        start a history ****** main loop ********
c
      call tme(1)
      t0 = jobtim(dum0)
      nprs = npp
      seed = .5
      call sources(seed,nprs)
c
c        print output
  140 nps = npp
      t1 = jobtim(dum1)
      call tme(2)
c
c***** end main loop ******
c
      tend = t1 - t0
      do 220 i=1,35
      rnps= nps
      if(trans(i).ne.zero)go to 203
      rtrans(i)= zero
      go to 204
  203 rtrans(i)= sqrt((trans2(i)-trans(i)**2)/ rnps)
      rtrans(i)= rtrans(i)/trans(i)
  204 if(bscat(i).ne.zero)go to 205
      rbscat(i)= zero
      go to 206
  205 rbscat(i)= sqrt((bscat2(i)-bscat(i)**2)/ rnps)
      rbscat(i)= rbscat(i)/ bscat(i)
  206 if(escpe(i).ne.zero)go to 207
      rescpe(i)= zero
      go to 220
  207 rescpe(i)= sqrt((escpe2(i)-escpe(i)**2)/ rnps)
      rescpe(i)= rescpe(i)/ escpe(i)
  220 continue
      ttot = ttot/rnps
      ttot2 = ttot2/rnps
      btot = btot/rnps
      btot2 = btot2/rnps
      etot = etot/rnps
      etot2 = etot2/rnps
      if(ttot.ne.zero) go to 2000
      rttot = zero
      go to 2001
 2000 rttot = sqrt((ttot2 - ttot**2)/rnps)
      rttot = rttot/ttot
 2001 if(btot.ne.zero) go to 2002
      rbtot = zero
      go to 2003
 2002 rbtot = sqrt((btot2 - btot**2)/rnps)
      rbtot = rbtot/btot
 2003 if(etot.ne.zero) go to 2004
      retot = zero
      go to 2005
 2004 retot = sqrt((etot2 - etot**2)/rnps)
      retot = retot/etot
 2005 continue
      write(6,2020) btot
 2020 format(6x,'b total',9x, e12.5)
      do 225 i=1,35
  225 continue
      write(6,2030) etot
 2030 format(6x,'e total',9x, e12.5)
      do 230 i=1,35
  230 continue
      write(6,2040) ttot
 2040 format(6x,'t total',9x, e12.5)
      write(6,2100) nesc,ntrans,nbscat,absorb,neco,nwco,nroul,ncol,nocol
 2100 format(1x,'stats',3i5,e12.5,5i5)
      write(6,2110) escpe
      write(6,2110) trans
      write(6,2110) bscat
 2110 format(/1x,(5e10.3))
      wrg=wrg/npp
      wrl=wrl/npp
      wcp=wcp/npp
      wco=wco/npp
      stop
      end
 
      subroutine blkdat
      parameter (zero = 0.0d+00)
      implicit real*8(a-h,o-z),integer*4(i-n)
      common /stats/  e(35),el(35),xc(35),xpp(35),xpe(35),trans(35),
     1   bscat(35),escpe(35),trans2(35),bscat2(35),escpe2(35)
     2 ,rtrans(35),rbscat(35),rescpe(35)

       data (e(i),i=1,35)/  .001d+00, .0015d+00,
     1  .002d+00, .003d+00, .004d+00, .005d+00, .006d+00,
     2  .008d+00, .01d+00, .015d+00, .02d+00, .03d+00,
     3  .04d+00, .05d+00, .06d+00, .08d+00, .1d+00, .15d+00,
     4  .2d+00, .3d+00, .4d+00, .5d+00, .6d+00, .8d+00,
     5  1.d+00, 1.5d+00, 2.d+00, 3.d+00, 4.d+00, 5.d+00,
     6  6.d+00, 8.d+00, 10.d+00, 15.d+00, 20.d+00 /
      data (xc(i),i=1,35)/  .0150d+00, .0296d+00,
     1  .0451d+00, .0717d+00, .0913d+00, .105d+00, .115d+00,
     2  .128d+00, .137d+00, .152d+00, .160d+00, .165d+00,
     3  .165d+00, .163d+00, .160d+00, .153d+00, .146d+00,
     4  .133d+00, .122d+00, .106d+00, .0953d+00, .0867d+00,
     5  .0802d+00, .0707d+00, .0637d+00, .0516d+00, .044d+00,
     6  .0346d+00, .0289d+00, .0250d+00, .0221d+00,
     7  .0181d+00, .0154d+00, .0114d+00, .00913d+00 /
      data (xpp(i),i=1,35)/  zero, zero, zero, zero,
     1  zero, zero, zero, zero, zero, zero, zero, zero, zero,
     2  zero, zero, zero, zero, zero, zero, zero, zero, zero,
     3  zero, zero, zero, .0000792d+00, .000316d+00,
     4  .000923d+00, .00153d+00, .00208d+00, .00256d+00,
     5  .00343d+00, .00414d+00, .00547d+00, .00652d+00 /
      data (xpe(i),i=1,35)/  2010.d+00, 632.d+00,
     1  280.d+00, 87.7d+00, 37.3d+00, 18.9d+00, 10.4d+00,
     2  4.01d+00, 1.91d+00, .489d+00, .192d+00, .0491d+00,
     3  .0186d+00, .00887d+00, .00481d+00, .00179d+00,
     4  .000862d+00, .000234d+00, .0000918d+00, zero, zero,
     5  zero, zero, zero, zero, zero, zero, zero, zero, zero,
     6  zero, zero, zero, zero, zero /
      end

      subroutine tport
      implicit real*8(a-h,o-z),integer*4(i-n)
      logical cut,kill
c
c     following is a small number
c
      parameter (small = 1.d-70)
c
       parameter (zero = 0.0d+00)
       parameter (one = 1.0d+00)
       parameter (two = 2.0d+00)
       parameter (three = 3.0d+00)
       parameter (four = 4.0d+00)
       parameter (five = 5.0d+00)
       parameter (six = 6.0d+00)
       parameter (ten = 10.0d+00)
       parameter (twenty = 20.0d+00)
       parameter (pt5 = .5d+00)
       parameter (pt25 = .25d+00)
       parameter (pt3 = .3d+00)
       parameter (pt001 = .001d+00)
       parameter (ptdm10 = 1.0d-10)
       parameter (ptdm6 = 1.0d-06)
       parameter (dp10 = 1.0d+10)
c
c
      common x,y,z,ps,u,v,w,erg,ia,wt,np,uold,vold,wold,cl,cl2,crad2,dls,ja
      common /stats/  e(35),el(35),xc(35),xpp(35),xpe(35),trans(35),
     1   bscat(35),escpe(35),trans2(35),bscat2(35),escpe2(35)
     2 ,rtrans(35),rbscat(35),rescpe(35)
      common /bnkk/ bank(100,11),fim(2)
      common /stat1/ transi(35),bscati(35),escpei(35),btoti,etoti,ttoti
      common /stat2/ nesc,ntrans,nbscat,neco,nwco,nroul,ncol,nocol
      common /con/ wcp1,wcp2,wco,wcp,ec,krn,crad,
     1  btot,etot,ttot,btot2,etot2,ttot2,wr1,wrl,wrg,nr,inbnk,nbank

      dimension pbl(11), ibank(100,11)
      equivalence(pbl,x),(bank,ibank)

c
c        calculate distance dls to next surface intersection
c        for all three surfaces and also the number ja of the
c        next surface intersected

      btoti = zero
      ttoti = zero
      etoti = zero

   20 ja=0
      call track
c
c        find energy pointer for cross sections and tallys
      do 30 ie = 1,35
      if(erg.gt.e(ie)) go to 30
      i = ie
      go to 31
   30 continue
c
c        linear interpolation to get cross sections as f(erg)
   31 f=(log(erg)-el(i-1))/(el(i)-el(i-1))
      xsc=exp( xc(i-1)+f*(xc(i)-xc(i-1)) )
      xspp=exp( xpp(i-1)+f*(xpp(i)-xpp(i-1)) )
      xspe=exp( xpe(i-1)+f*(xpe(i)-xpe(i-1)) )
      xst  = xsc + xspp + xspe
c
c        calculate distance to next collision
      call grand(ps,r,rr1)
      s = -log(r)/xst
c
c        see if collision is still inside cylinder
c        if not, do tallys; if so, do collision physics
      if(s.lt.dls) go to 60
      nocol=nocol+1
      x=x+u*dls
      y=y+v*dls
      z=z+w*dls
      if(ja.ne.3) then
        call tally(i)
	goto 11
      endif
c  cross internal surface  split or roulette
   53 iap=ia
      ia=2-ia/2
      t1=fim(ia)/fim(iap)
      if(t1.gt.one) go to 57
c  russian roulette
      call roulet(kill,t1)
      if(kill) go to 11
      goto 20
c  splitting
   57 call split(t1)
      go to 20

c  check bank before starting new particle

   11 if(nbank.eq.0) go to 234
      do 521 ix=1,10
  521 pbl(ix)=bank(inbnk,ix)
      nbank=nbank-1
      ibank(inbnk,11)=ibank(inbnk,11)-1
      if(ibank(inbnk,11).eq.0) inbnk=inbnk-1
      go to 20
c
c        collisions
   60 ja = 0
      x=x+u*s
      y=y+v*s
      z=z+w*s
      ncol=ncol+1
c  photoelectric
      wtsav=wt
      wt=wt*(one-xspe/xst)
      absorb=absorb+(wtsav-wt)
      xstsb=xst-xspe
c weight cutoff
      call grand(ps,xseed,r2)
      call grand(r2,r1,r3)
      ps=xseed
      if(wt.gt.wcp2) go to 832
      if(wt*fim(ia).lt.r1*wcp1*fim(1)) go to 642
      wtsav=wt
      wt=wcp1*fim(1)/fim(ia)
      wcp=wcp+(wt-wtsav)
  832 continue
c compton scattering
      if(rr1.ge.xsc/xstsb) goto 100
      call compton(cut)
      if(cut) goto 11
      go to 20
c
c        pair production
  100 call pair(cut)
      if(cut) goto 11
      go to 20
c
c  terminate paryticle to weight cutoff
  642 wco=wco+wt
      nwco=nwco+1
      go to 11
  234 do 829 i=1,35
      bscat(i)=bscat(i)+bscati(i)
      bscat2(i)=bscat2(i)+bscati(i)**2
      trans(i)=trans(i)+transi(i)
      trans2(i)=trans2(i)+transi(i)**2
      escpe(i)=escpe(i)+escpei(i)
      escpe2(i)=escpe2(i)+escpei(i)**2
      bscati(i) = zero
      transi(i) = zero
      escpei(i) = zero
  829 continue
      btot=btot+btoti
      ttot=ttot+ttoti
      etot=etot+etoti
      btot2=btot2+btoti**2
      ttot2=ttot2+ttoti**2
      etot2=etot2+etoti**2
      return
      end

      subroutine track
      implicit real*8(a-h,o-z),integer*4(i-n)
c        calculate all intersections with all three surfaces
c
       parameter (zero = 0.0d+00)
       parameter (one = 1.0d+00)
       parameter (two = 2.0d+00)
       parameter (three = 3.0d+00)
       parameter (four = 4.0d+00)
       parameter (five = 5.0d+00)
       parameter (six = 6.0d+00)
       parameter (ten = 10.0d+00)
       parameter (twenty = 20.0d+00)
       parameter (pt5 = .5d+00)
       parameter (pt25 = .25d+00)
       parameter (pt3 = .3d+00)
       parameter (pt001 = .001d+00)
       parameter (ptdm10 = 1.0d-10)
       parameter (ptdm6 = 1.0d-06)
       parameter (dp10 = 1.0d+10)
c
      common x,y,z,ps,u,v,w,erg,ia,wt,np,uold,vold,wold,cl,cl2,crad2,dls,ja
      dlss = dp10
      if(ia.eq.2) go to 19
      do 300 j=1,3
      d1 = -one
      go to (55,160,50),j
   50 if(v.eq.zero) go to 300
      d1 = (cl-y)/v
      go to 280
   55 if(v.eq.zero) go to 300
      d1 = -y/v
      go to 280
  160 t1 = u**2 + w**2
      if(t1.eq.zero) go to 300
      a1 = (x*u + z*w)/t1
      b1 = (x**2 + z**2 - crad2)/t1
      t1 = a1**2 - b1
      if(t1.lt.zero) go to 300
      t2 = sqrt(t1)
      d1 = -a1 + t2
      d2 = -a1 - t2
      if(j.ne.ja) go to 290
      d2=-two*a1
      d1=d2
      go to 290
  280 d2 = -d1
  290 if(d1.le.zero) go to 300
      if(d2.gt.zero) d1=d2
      if(d1.ge.dlss) go to 300
      jas = j
      dlss = d1
  300 continue
      dls = dlss+ptdm10
      ja = jas
      return
   19 do 301 j=2,4
      d1 = -one
      go to (56,161,51,56),j
   51 if(v.eq.zero) go to 301
      d1 = (cl-y)/v
      go to 281
   56 if(v.eq.zero) go to 301
      d1 = (cl2-y)/v
      go to 281
  161 t1 = u**2 + w**2
      if(t1.eq.zero) go to 301
      a1 = (x*u + z*w)/t1
      b1 = (x**2 + z**2 - crad2)/t1
      t1 = a1**2 - b1
      if(t1.lt.zero) go to 301
      t2 = sqrt(t1)
      d1 = -a1 + t2
      d2 = -a1 - t2
      if(j.ne.ja) go to 291
      d2=-two*a1
      d1=d2
      go to 291
  281 d2 = -d1
  291 if(d1.le.zero) go to 301
      if(d2.gt.zero) d1=d2
      if(d1.ge.dlss) go to 301
      jas = j
      dlss = d1
  301 continue
      dls = dlss+ptdm10
      ja = jas
      return
      end
 
      subroutine klein(t1,t4,rn)
      implicit real*8(a-h,o-z),integer*4(i-n)
c        sample from klein-nishina using inverse fit.
c        t1=energy in, t4=energy out, in units of the rest mass
c        of an electron.
c
c
       parameter (zero = 0.0d+00)
       parameter (one = 1.0d+00)
       parameter (two = 2.0d+00)
       parameter (three = 3.0d+00)
       parameter (four = 4.0d+00)
       parameter (five = 5.0d+00)
       parameter (six = 6.0d+00)
       parameter (ten = 10.0d+00)
       parameter (twenty = 20.0d+00)
       parameter (pt5 = .5d+00)
       parameter (pt25 = .25d+00)
       parameter (pt3 = .3d+00)
       parameter (pt001 = .001d+00)
       parameter (ptdm10 = 1.0d-10)
       parameter (ptdm6 = 1.0d-06)
       parameter (dp10 = 1.0d+10)
c
      t2=one/t1
      t4=two*t1+one
      t5=one/t4
      t6=log(t4)
      t3=two*t1*(one+t1)*t5**2+four*t2+(one-two*t2*(one+t2))*t6
      if(t1.le.1.16666667d+00)go to 20
      t7=1.65898d+00+t2*(.62537d+00*t2-1.00796d+00)
      t3=t7/t3
      if(rn.le.t3)go to 10
      t4=(t6-1.20397d+00)/(one-t3)
      t7=pt3*exp(t4*(t3-rn))
      go to 40
   10 t4=t7/(3.63333d+00+t2*(5.44444d+00*t2-4.66667d+00))
      t7=pt5*t7
      t2=rn/t3
      t3=2.1d+00
      t5=1.4d+00
      go to 30
   20 t4=t3/(t4+t5)
      t7=pt5*t3
      t2=rn
      t5=one-t5
      t3=three*t5
      t5=two*t5
   30 t7=one+t2*(t2*(two*t7+t4-t3+t2*(t5-t7-t4))-t7)
   40 t4=t7*t1
      return
      end
 
      subroutine isos(rn)
      implicit real*8(a-h,o-z),integer*4(i-n)
c        sample a direction u,v,w isotropically.
c
c
       parameter (zero = 0.0d+00)
       parameter (one = 1.0d+00)
       parameter (two = 2.0d+00)
       parameter (three = 3.0d+00)
       parameter (four = 4.0d+00)
       parameter (five = 5.0d+00)
       parameter (six = 6.0d+00)
       parameter (ten = 10.0d+00)
       parameter (twenty = 20.0d+00)
       parameter (pt5 = .5d+00)
       parameter (pt25 = .25d+00)
       parameter (pt3 = .3d+00)
       parameter (pt001 = .001d+00)
       parameter (ptdm10 = 1.0d-10)
       parameter (ptdm6 = 1.0d-06)
       parameter (dp10 = 1.0d+10)
c
      common x,y,z,ps,u,v,w,erg,ia,wt,np,uold,vold,wold,cl,cl2,crad2,dls,ja
   10 call grand(rn,rn,r2)
      call grand(r2,r3,r4)
      t1=two*r4-one
      t2=two*r3-one
      rsq=t1**2+t2**2
      if(rsq.gt.one)go to 10
      u=two*rsq-one
      t3=sqrt((one-u**2)/rsq)
      v=t1*t3
      w=t2*t3
      return
      end
 
      subroutine rotas(c,rn)
      implicit real*8(a-h,o-z),integer*4(i-n)
c        rotate uold,vold,wold to u,v,w through a polar
c        angle whose cosine is c, and through an azimuthal
c        angle sampled uniformly.
c
c
       parameter (zero = 0.0d+00)
       parameter (one = 1.0d+00)
       parameter (two = 2.0d+00)
       parameter (three = 3.0d+00)
       parameter (four = 4.0d+00)
       parameter (five = 5.0d+00)
       parameter (six = 6.0d+00)
       parameter (ten = 10.0d+00)
       parameter (twenty = 20.0d+00)
       parameter (pt5 = .5d+00)
       parameter (pt25 = .25d+00)
       parameter (pt3 = .3d+00)
       parameter (pt001 = .001d+00)
       parameter (ptdm10 = 1.0d-10)
       parameter (ptdm6 = 1.0d-06)
       parameter (dp10 = 1.0d+10)
c
      common x,y,z,ps,u,v,w,erg,ia,wt,np,uold,vold,wold,cl,cl2,crad2,dls,ja
   10 call grand(rn,r1,r2)
      call grand(r2,rn,r3)
      t1=two*r1-one
      t2=two*r3-one
      r=t1**2+t2**2
      if(r.gt.one)go to 10
      r=sqrt((one-c**2)/r)
      t1=t1*r
      t2=t2*r
      if(abs(wold).gt..999999d+00)go to 30
      s=sqrt(one-wold**2)
      u=uold*c+(t1*uold*wold-t2*vold)/s
      v=vold*c+(t1*vold*wold+t2*uold)/s
      w=wold*c-t1*s
      return
   30 u=t1
      v=t2
      w=wold*c
      return
      end
      real*8 function rannum(m)
      implicit real*8(a-h,o-z),integer*4(i-n)
c
c uniformly random generator of numbers between 0. and 1.
c for any machine with at least 24 bits plus sign bit in integer mode.
c cycle is 1,048,576 random numbers.
c r e jones  div 5422
c
c i = 2**21 + 5 initially
      data i /2097157/
      i = mod(i*3,4194304)
      rannum = float(i)*2.384185791d-07
      return
      end
c
      subroutine tme(ncall)
      return
      end

      real*8 function jobtim(m)
      jobtim=0.
      return
      end

      subroutine tally(i)
      implicit real*8(a-h,o-z),integer*4(i-n)
      common x,y,z,ps,u,v,w,erg,ia,wt,np,uold,vold,wold,cl,cl2,crad2,dls,ja
      common /stat1/ transi(35),bscati(35),escpei(35),btoti,etoti,ttoti
      common /stat2/ nesc,ntrans,nbscat,neco,nwco,nroul,ncol,nocol
      go to(42,50,53,52) ja
   42 bscati(i) = bscati(i) + wt
      btoti = btoti + wt
      nbscat=nbscat+1
      return
   52 transi(i) = transi(i) + wt
      ttoti = ttoti + wt
      ntrans=ntrans+1
      return
   50 escpei(i) = escpei(i) + wt
      etoti = etoti + wt
      nesc=nesc+1
   53 return
      end

      subroutine roulet(kill,t1)
      implicit real*8(a-h,o-z),integer*4(i-n)
      common x,y,z,ps,u,v,w,erg,ia,wt,np,uold,vold,wold,cl,cl2,crad2,dls,ja
      common /con/ wcp1,wcp2,wco,wcp,ec,krn,crad,
     1  btot,etot,ttot,btot2,etot2,ttot2,wr1,wrl,wrg,nr,inbnk,nbank
      common /stat2/ nesc,ntrans,nbscat,neco,nwco,nroul,ncol,nocol
      logical kill

      nroul=nroul+1
      call grand(ps,r1,r2)
      call grand(r2,xseed,r3)
      if(t1.lt.r1) go to 58
      wtsav=wt
      wt=wt/t1
      wrg=wrg+(wt-wtsav)
      seed=xseed
      kill = .false.
      return
c  killed in russian roulette
   58 wrl=wrl+wt
      nr=nr+1
      kill = .true.
      return
      end

      subroutine split(t1)
      implicit real*8(a-h,o-z),integer*4(i-n)
      common x,y,z,ps,u,v,w,erg,ia,wt,np,uold,vold,wold,cl,cl2,crad2,dls,ja
      common /bnkk/ bank(100,11),fim(2)
      common /con/ wcp1,wcp2,wco,wcp,ec,krn,crad,
     1  btot,etot,ttot,btot2,etot2,ttot2,wr1,wrl,wrg,nr,inbnk,nbank
      dimension pbl(11)
      equivalence(pbl,x)

      np=t1-1.
      wt=wt/t1
      call grand(ps,ps1,ps2)
      ps=ps1
      nbank=nbank+np
      inbnk=inbnk+1
      do 59 ix=1,11
   59 bank(inbnk,ix)=pbl(ix)
      ps=ps2
      return
      end

      subroutine compton(cut)
      parameter (one=1.d0)
      implicit real*8(a-h,o-z),integer*4(i-n)
      common x,y,z,ps,u,v,w,erg,ia,wt,np,uold,vold,wold,cl,cl2,crad2,dls,ja
      common /con/ wcp1,wcp2,wco,wcp,ec,krn,crad,
     1  btot,etot,ttot,btot2,etot2,ttot2,wr1,wrl,wrg,nr,inbnk,nbank
      common /stat2/ nesc,ntrans,nbscat,neco,nwco,nroul,ncol,nocol
      logical cut

      call grand(ps,xseed,r2)
      call grand(r2,r3,r4)
      t1 = 1.956917d+00*erg
c        get new energy t4 and compton scattering angle
      call klein(t1,t4,r3)
      csa = one+one/t1-one/t4
      t5 = .511008d+00*t4
      if(abs(csa).gt.one) csa=dsign(one,csa)
      erg = t5
c
c        see if new energy is less than cutoff
      if(erg.gt.ec) go to 70
      neco=neco+1
      cut=.true.
      return
c        make compton angle relative to problem coordinate system
   70 uold = u
      vold = v
      wold = w
      call rotas(csa,r4)
      cut=.false.
      return
      end

      subroutine pair(cut)
      implicit real*8(a-h,o-z),integer*4(i-n)
      common x,y,z,ps,u,v,w,erg,ia,wt,np,uold,vold,wold,cl,cl2,crad2,dls,ja
      common /con/ wcp1,wcp2,wco,wcp,ec,krn,crad,
     1  btot,etot,ttot,btot2,etot2,ttot2,wr1,wrl,wrg,nr,inbnk,nbank
      common /stat2/ nesc,ntrans,nbscat,neco,nwco,nroul,ncol,nocol
      logical cut

      call grand(ps,xseed,r2)
      call grand(r2,r3,r4)
      ps=xseed
      erg = 0.511008d+00
      wt = 2.*wt
c
c        check energy cutoff
      if(erg.gt.ec) go to 110
      neco=neco+1
      cut=.true.
      return
c
c        isotropic emission in lab system
  110 call isos(r3)
      cut=.false.
      return
      end

      subroutine sources(seed,nprs)
      implicit real*8(a-h,o-z),integer*4(i-n)
      if(nprs.le.1) then
        call crpart(seed,rnd)
        call tport
      else
        n1=nprs/2
        n2=n1
        call grand(seed,r1,r2)
        call sources(r1,n1)
        call sources(r2,n2)
      endif
      return
      end

      subroutine crpart(seed,rnd)
      implicit real*8(a-h,o-z),integer*4(i-n)
       parameter (zero = 0.0d+00)
       parameter (one = 1.0d+00)
       parameter (two = 2.0d+00)
       parameter (three = 3.0d+00)
       parameter (four = 4.0d+00)
       parameter (five = 5.0d+00)
       parameter (six = 6.0d+00)
       parameter (ten = 10.0d+00)
       parameter (twenty = 20.0d+00)
       parameter (pt5 = .5d+00)
       parameter (pt25 = .25d+00)
       parameter (pt3 = .3d+00)
       parameter (pt001 = .001d+00)
       parameter (ptdm10 = 1.0d-10)
       parameter (ptdm6 = 1.0d-06)
       parameter (dp10 = 1.0d+10)
      common x,y,z,ps,u,v,w,erg,ia,wt,np,uold,vold,wold,cl,cl2,crad2,dls,ja
      call grand(seed,ps,rnd)
      erg = six
      wt = one
      u = zero
      v = one
      w = zero
      x = zero
      y = ptdm6
      z = zero
      ja = 0
      ia=1
      return
      end

      subroutine grand(seed,r1,r2)
      implicit real*8(a-h,o-z),integer*4(i-n)
      
      con1=65599.d0
      con2=71123.d0
      r1 = dmod((314557.d0*seed+2711.d0),con1)
      r2 = dmod((2711.d0*seed+314557.d0),con2)
      r1 = r1/con1
      r2 = r2/con2
      return
      end

