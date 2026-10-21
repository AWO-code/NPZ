!----------------------------------------------------------------------
!----------------------------------------------------------------------
!   enz :    A two-cell, one-substrate enzyme model 
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      SUBROUTINE FUNC(NDIM,U,ICP,PAR,IJAC,F,DFDU,DFDP)
!     ---------- ----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM, ICP(*), IJAC
      DOUBLE PRECISION, INTENT(IN) :: U(NDIM), PAR(*)
      DOUBLE PRECISION, INTENT(OUT) :: F(NDIM)
      DOUBLE PRECISION, INTENT(INOUT) :: DFDU(NDIM,NDIM), DFDP(NDIM,*)

      DOUBLE PRECISION N,C,Z,u_C,K_N,m_C,u_Z,K_C,m_Z,d_Z,K_Z,N_tot
      DOUBLE PRECISION muC,muZ,Ki,Gi,Pth,Tem

       C=U(1)
       Z=U(2)
       Gi=PAR(1)
       K_N=PAR(2)
       m_C=PAR(3)
       u_Z=PAR(4)
       K_C=PAR(5)
       m_Z=PAR(6)
       d_Z=PAR(7)
       K_Z=PAR(8)
       N_tot=PAR(9)
       Ki=1.20
       u_C=1.0
       Tem=15.3
       Pth=0.01

       F(1)=u_C*(N_tot-C-Z)/(K_N+N_tot-C-Z)*C-m_C*C-1.7**((Tem-30)/10)*(Gi*(C-Pth/6600))/((Ki/6600)+(C-Pth/6600))*Z
       F(2)=1.7**((Tem-30)/10)*(Gi*(C-Pth/6600))/((Ki/6600)+(C-Pth/6600))*Z-m_Z*Z-d_Z*Z**2/(Z+K_Z)

      END SUBROUTINE FUNC 

      SUBROUTINE STPNT(NDIM,U,PAR,T) 
!     ---------- -----

      IMPLICIT NONE
      INTEGER, INTENT(IN) :: NDIM
      DOUBLE PRECISION, INTENT(INOUT) :: U(NDIM),PAR(*)
      DOUBLE PRECISION, INTENT(IN) :: T

       PAR(1)=1.50
       PAR(2)=0.0001
       PAR(3)=0.1
       PAR(4)=0.5
       PAR(5)=0.001
       PAR(6)=0.1
       PAR(7)=0.91
       PAR(8)=0.0001
       PAR(9)=0.000101

       U(1)= 0.73333E-04
       U(2)= 0.11614E-04
      END SUBROUTINE STPNT

      SUBROUTINE BCND
      END SUBROUTINE BCND

      SUBROUTINE ICND
      END SUBROUTINE ICND

      SUBROUTINE FOPT
      END SUBROUTINE FOPT

      SUBROUTINE PVLS
      END SUBROUTINE PVLS
