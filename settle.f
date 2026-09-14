      implicit none
      real*8 N,C,Z,t,meanC,meanZ,dZ,KZ,muC,muZ,mC,mZ,Gi,Ki,Pth
      real*8 dt,Pi,I,mumaxC,mumaxZ,KN,KC,Ntot,beta,Tem,Tlim
      integer j,k,l,q,R
      mumaxC=1.0
      mumaxZ=0.5  
      mC=0.1
      mZ=0.1 
      KN=0.0001         
      KC=0.001
      KZ=0.0001
      N=0.005    
      dZ=1
      dt=0.00001
      Pi=3.1415927
      t=0
      C=0.00009
      Z=0.000006
      Ntot=0.001
      Tem=15.3
      Gi=2.20
      Ki=1.20
      Pth=0.01    
      meanC=0.00000001*C
      meanZ=0.00000001*Z
      R=4.0  

      do l=2,100000000 
         N=Ntot-C-Z
         Tlim=1.7**((Tem-30)/10) 
         muC=mumaxC*N/(KN+N)
         muZ=Tlim*(Gi*(C-Pth/6600))/((Ki/6600)+(C-Pth/6600))       
 
         C=C+dt*(muC*C-muZ*Z-mC*C) 
         Z=Z+dt*(muZ*Z-mZ*Z-dZ*Z**2/(Z+KZ)) 
 
         t=t+dt
         meanC=meanC+0.00000001*C
         meanZ=meanZ+0.00000001*Z

         q=mod(l,10000)
         if (q == 0) then
           write(70,"(e13.5,e13.5)") t, C
           write(80,"(e13.5,e13.5)") t, Z
           write(90,"(e13.5,e13.5)") t, N
            if (t.gt.100) then 
              write(100,"(e13.5,e13.5)") C, Z
            end if
         end if
      end do
    
      end
