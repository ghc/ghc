      program main
      n=5
      k = ifact(n)
      print *,k
      end
      function ifact(n)
      if(n.eq.1) then
        ifact=n
      else
        ifact = n*ifact(n-1)
      endif
      return
      end

