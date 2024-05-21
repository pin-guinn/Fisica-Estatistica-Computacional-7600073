PROGRAM tarefa1
  
  CALL Wave(1d0)

  CONTAINS
  SUBROUTINE Wave(r)
    IMPLICIT NONE
    INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(p=15)
    REAL(dp), INTENT(IN) :: r
    REAL(dp), PARAMETER :: dx = 0.01_dp, L = 1_dp, c = 300_dp
    REAL(dp), PARAMETER :: x0 = L/3_dp, sig = L/30_dp
    REAL(dp), DIMENSION(:,:), ALLOCATABLE :: Yn
    INTEGER :: i, j, M = int(L/dx)

    OPEN(1,FILE="data.out",STATUS="UNKNOWN")

    ALLOCATE(Yn(1:3,0:M)) ! Yn(Tempo,Espaço) = Yn(1:3,0:M)
    Yn = 0_dp

    !! Pacote inicial Y(0,x)=Y0(x)=exp[-(x-x0)^2/sig^2]
    DO i=0, M
      Yn(1,i) = EXP(-((i*dx-x0)**2)/sig**2)
      WRITE(1,*) i*dx, Yn(1,i)
    END DO
    Yn(2,:) = Yn(1,:)

    !! FIxando todos os pontos x=0 e x=M
    Yn(1:3,0) = 0
    Yn(1:3,M) = 0


    WRITE(1,*) NEW_LINE('')
    !! Realizando o loop ao longo de toda a corda e sobre todos os instantes de tempo.
    !! Aqui os valores da coluna x=0 e x=M não são ataualizados, então naõ preciso me preocupar com eles
    !! já que estão todos fixados em zero.
    DO i=1, 200
      DO j=1, M-1
        Yn(3,j) = 2*(1-r*r)*Yn(2,j) - Yn(1,j) + r*r*(Yn(2,j+1) + Yn(2,j-1))
        WRITE(1,*) j*dx, Yn(3,j)
      END DO
      WRITE(1,*) NEW_LINE('')
      Yn(1,:) = Yn(2,:)
      Yn(2,:) = Yn(3,:)
    END DO

  END SUBROUTINE
END PROGRAM
