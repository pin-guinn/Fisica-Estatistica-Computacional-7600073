PROGRAM tarefa2
  
  CALL Wave(1d0)

  CONTAINS
  SUBROUTINE Wave(r)
    USE :: ogpf
    IMPLICIT NONE
    TYPE(gpf) :: gp
    INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(p=15)
    REAL(dp), INTENT(IN) :: r
    REAL(dp), PARAMETER :: dx = 0.01_dp, L = 1_dp, c = 300_dp
    REAL(dp), PARAMETER :: x0 = L/3_dp, sig = L/30_dp
    REAL(dp), DIMENSION(:,:), ALLOCATABLE :: Yn
    REAL(dp), DIMENSION(:), ALLOCATABLE :: xaxis
    INTEGER :: i, j, M = int(L/dx)

    ALLOCATE(Yn(1:3,0:M)) ! Yn(Tempo,Espaço) = Yn(1:3,0:M)
    Yn = 0_dp
    
    !! Alocando o eixo x para o gif
    ALLOCATE(xaxis(0:M))
    xaxis = linspace(0d0,L,M)

    !! Configurando o código para rodar o gif
    CALL gp%animation_start(0.1)
    CALL gp%axis([0d0, 1d0, -0.5d0, 0.5d0])
    CALL gp%options('set grid')
    CALL gp%xlabel('x (m)')
    CALL gp%ylabel('Amplitude')

    !! Pacote inicial Y(0,x)=Y0(x)
    DO i=0, M
      IF (i*dx .lt. L/4_dp) THEN
        Yn(1,i) = i*dx
      ELSE
        Yn(1,i) = -(i*dx)/3d0 + 1d0/3d0
      END IF
    END DO
    Yn(2,:) = Yn(1,:)

    CALL gp%plot(xaxis(0:M), Yn(2,0:M), 'w lp lc "blue" pt 7 ps 0.5 lw 2')

    !! Fixando todos os pontos x=0 e x=M
    Yn(1:3,0) = 0
    Yn(1:3,M) = 0

    !! Realizando o loop ao longo de toda a corda e sobre todos os instantes de tempo.
    !! Aqui os valores da coluna x=0 e x=M não são ataualizados, então naõ preciso me preocupar com eles
    !! já que estão todos fixados em zero.
    DO i=1, 200
      DO j=1, M-1
        Yn(3,j) = 2*(1-r*r)*Yn(2,j) - Yn(1,j) + r*r*(Yn(2,j+1) + Yn(2,j-1))
      END DO
      Yn(1,:) = Yn(2,:)
      Yn(2,:) = Yn(3,:)

      CALL gp%plot(xaxis(0:M), Yn(3,0:M), 'w l lc "blue" lw 2')
    END DO

    CALL gp%animation_show()
  END SUBROUTINE
END PROGRAM
