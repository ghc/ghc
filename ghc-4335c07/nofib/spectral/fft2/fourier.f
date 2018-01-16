! Fourier Transforms, Floating Point Benchmark - Fortran
!                                                nonANSI: recursion
!            Rex Page (rpage@trc.amoco.com)
!            Amoco Production Research, Sep 1992
 
!  timings  (Sun Microsystems Fortran 77, SparcStation 1+)
!          30Sep92
!   
!  Fortran timings (in seconds)  Sun Microsystems Fortran 77
!                                SparcStation 1+
!                                                    30Sep92
!      rampWave
!             n = 256         512          1024
!    tstfft        .2sec       .4sec         .9sec
!    tstdft        .5          1            2.2
!    tstsct        .25         .5           1.1
!
!       Unix   time a.out   used as timer

program main
  complex s(0:2047)
  real r(0:2047)
  external fft,fftinv
  external dft,dftinv
  print*, "enter n"
  read*, n
  print*, "print result ="
!  call rmwC(n, s)
!  print*, tst(fft,fftinv,s,n)
!  print*, tst(dft,dftinv,s,n)
  call rampWave(n, r)
  print*, tstsct(r,n)
end
 
subroutine fft(x,n, f)       ! Warning: works only for n=2**k<=2048
  complex x(0:n-1), f(0:n-1) ! time=O(n log(n)) algorithm 
  complex u(0:2047)
  call roots(n, u)
  call conj(n, u)
  call ffth(x,u,n, f)
  call fscale(n, f)
end
 
subroutine fftinv(x,n, f)    ! Warning: works only for n=2**k<=2048
  complex x(0:n-1), f(0:n-1) ! time=O(n log(n)) algorithm 
  complex u(0:2047)
  call roots(n, u)
  call ffth(x,u,n, f)
end
 
subroutine ffth(x,u,n, f)
  implicit automatic(a-z)
  complex x(0:n-1), u(0:n-1), f(0:n-1)
  complex xEvn(0:1023), xOdd(0:1023)
  complex uEvn(0:1023)
  complex fEvn(0:1023), fOdd(0:1023)
  if (n .eq. 1) then
    f(0) = x(0)
  else
    call evNth(2,u,   n, uEvn)
    call evNth(2,x,   n, xEvn)
    call evNth(2,x(1),n, xOdd)
    call ffth(xEvn, uEvn, n/2, fEvn)
    call ffth(xOdd, uEvn, n/2, fOdd)
    do i=0, n/2 - 1
      f(i)     = fEvn(i) + fOdd(i)*u(i)
      f(i+n/2) = fEvn(i) + fOdd(i)*u(i+n/2)
    end do
  end if
end
 
subroutine dft(x,n, f)       ! Warning: works only for n<=2048
  complex x(0:n-1), f(0:n-1) ! time=O(n*sum(squares of factors of n))
  complex u(0:2047)          !     =O(n*log(n)) when n is a product
  integer p(11), np          !                      of small primes
  call factor(n, p,np)
  call roots(n, u)
  call conj(n, u)
  call dfth(p,np,x,u,n, f)
  call fscale(n, f)
end
 
subroutine dftinv(x,n, f)    ! Warning: works only for n<=2048
  complex x(0:n-1), f(0:n-1) ! time=O(n*sum(squares of factors of n))
  complex u(0:2047)          !     =O(n*log(n)) when n is a product
  integer p(11), np          !                      of small primes
  call factor(n, p,np)
  call roots(n, u)
  call dfth(p,np,x,u,n, f)
end

subroutine dfth(p,np,x,u,n, f)
  implicit automatic(a-z)
  integer p(np)
  complex x(0:n-1), u(0:n-1), f(0:n-1)
  complex xPrt(0:2047), uPrt(0:1023), fPrt(0:1023)
  if (np .eq. 1) then
    call sfth(x,u,n, f)
  else
    call evNth(p(1),u,n, uPrt)
    nr = n/p(1)
    do i=0,n-1
      f(i) = 0
    end do
    do k=0,p(1)-1                    ! do partial dft's
      call evNth(p(1),x(k),n, xPrt)
      call dfth(p(2),np-1,xPrt,uPrt,nr, fPrt)
      do i=0,nr-1        ! mix-in partials
        do j=0,p(1)-1
          f(i+j*nr) = f(i+j*nr) + fPrt(i)*u(mod((i+j*nr)*k,n))
        end do
      end do             ! end mix-in
    end do                           ! end partial dft's
  end if
end

subroutine sft(x,n, f)        ! Warning: works only for n<=2048 
  complex x(0:n-1), f(0:n-1)  ! time=O(n**2) algorithm
  complex u(0:2047)
  complex fdot; external fdot
  call roots(n, u)
  call conj(n, u)
  call sfth(x,u,n, f)
  call fscale(n, f)
end
 
subroutine sftinv(x,n, f)     ! Warning: works only for n<=2048
  complex x(0:n-1), f(0:n-1)  ! time=O(n**2) algorithm
  complex u(0:2047)
  complex fdot; external fdot
  call roots(n, u)
  call sfth(x,u,n, f)
end

subroutine sfth(x,u,n, f)
  complex x(0:n-1), u(0:n-1), f(0:n-1)
  complex fdot; external fdot
  do i=0,n-1
    f(i) = fdot(x,u,n,i)
  end do
end

complex function fdot(x,u,n,k)
  complex x(0:n-1), u(0:n-1)
  integer n, k
  fdot = 0
  do i=0,n-1
    fdot = fdot + x(i)*u(mod(i*k,n))
  end do
end
 
subroutine sct(x,n, c)
  real x(0:n-1), c(0:n-1)
  real fdot; external fdot
  do i=0,n-1
    f(i) = cdot(x,n,i)
  end do
end

complex function cdot(x,n,k)
  complex x(0:n-1)
  integer n, k
  real theta
  theta = 8*atan(1.0)/n
  cdot = 0
  do i=0,n-1
    cdot = cdot + x(i)*cos(theta*i*k)
  end do
end
 
subroutine roots(n, u)
  complex u(0:n-1)
  twoPiOverN = 8*atan(1.0)/n
  do i=0,n-1
    u(i) = cmplx(cos(i*twoPiOverN), sin(i*twoPiOverN))
  end do
end

subroutine fscale(n, f)
  complex f(0:n-1)
  do i=0,n-1
    f(i) = f(i)/n
  end do
end

subroutine conj(n, u)
  complex u(0:n-1)
  do i=0,n-1
    u(i) = conjg(u(i))
  end do
end

real function dist(x,y,n)
  complex x(0:n-1), y(0:n-1)
  dist = 0
  do i=0,n-1
    dist = dist + abs(x(i)-y(i))**2
  end do
  dist = sqrt(dist)
end
 
subroutine evNth(p,x,n, y)
  integer p
  complex x(0:n-1), y(0: n/p - 1)
  do i=0,n/p - 1
    y(i) = x(i*p)
  end do
end

subroutine factor(n, f,nf)
  integer n, f(*), nf
  integer sieve(0:50),ns, nr, p, s
  nr = n
  nf = 0
  np = 0
  sieve(0) = 2
  ns = (int(sqrt(n+0.5))-1)/2
  do i=1,ns
    sieve(i) = 2*i+1
  end do
  s = 0
  p = sieve(0)
  do
   do  ! extract factors p from nr
      if (mod(nr,p) .ne. 0) exit
      nf = nf+1
      f(nf) = p
      nr = nr/p
    end do
    if (nr .eq. 1) exit
    do i=s+1,ns! find first non-composite remaining in sieve
      if (sieve(i) .ne. 0) exit
    end do
    s=i
    if (s .gt. ns) then  ! no remaining primes
      nf = nf+1          !   factorization completed
      f(nf) = nr
      exit
    end if
    p = sieve(s)
    do i=s+p,ns,p ! eliminate multiples of p from sieve
      sieve(i) = 0
    end do
  end do
end

subroutine rmwC(n, s)
  complex s(0:n-1)
  do i=0,n-1
    s(i) = cmplx(float(i), 0.0)
  end do
end

real function tst(ft,ftinv,s,n)
  external ft, ftinv
  complex s(0:n-1)
  complex fs(0:2047), fsinv(0:2047)
  call ft(s,n, fs)
  call ftinv(fs,n, fsinv)
  tst = dist(s,fsinv,n)
end

real function tstsct(s,n)
  real s(0:n-1)
  real cs(0:2047)
  call sct(s,n, cs)
  tstsct = 0
  do i=0,n-1
    tstsct = tstsct + cs(i)
  end do
end

real function validt(ft,s,n)
  external ft
  complex s(0:n-1)
  complex sdft(0:2047), ssft(0:2047)
  call  ft(s,n, sdft)
  call sft(s,n, ssft)
  validt = dist(sdft,ssft,n)
end
