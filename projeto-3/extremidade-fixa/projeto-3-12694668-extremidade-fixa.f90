PROGRAM tarefa1
  ! USE :: ogpf
  IMPLICIT NONE
  ! TYPE(gpf) :: gp
  REAL(8)   , PARAMETER    :: r = 1, dx = 0.01d0, L = 1d0, c = 300d0, PI = 4d0*ATAN(1d0)
  REAL(8)   , PARAMETER    :: sig = L/30d0, dt = r*dx/c
  REAL(8)   , ALLOCATABLE  :: Yn(:,:), Psi(:)
  COMPLEX(8), ALLOCATABLE  :: P(:)
  COMPLEX(8)               :: i = (0d0,1d0)
  REAL(8)                  :: x0
  INTEGER                  :: j, k, M = int(L/dx), N=2000, xi = int(L/(4*dx))

  !!!!!!!!!!!!!!!!!!!!!!!!!! PARÂMETROS DO PROJETO !!!!!!!!!!!!!!!!!!!!!!!!!!
  !! ITEM a
  ! x0 = L/2d0
  ! OPEN(UNIT=1, FILE="item-a-espectro-de-potencia.out", STATUS="UNKNOWN")
  
  !! ITEM b
  ! x0 = L/4d0
  ! OPEN(UNIT=1, FILE="item-b-espectro-de-potencia.out", STATUS="UNKNOWN")
  
  !! ITEM c
  ! x0 = L/3d0
  ! OPEN(UNIT=1, FILE="item-c-espectro-de-potencia.out", STATUS="UNKNOWN")

  !! ITEM d
  x0 = L/20d0
  OPEN(UNIT=1, FILE="item-d-espectro-de-potencia.out", STATUS="UNKNOWN")


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!! EQUAÇÃO DE ONDA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ALLOCATE(Yn(1:3,0:M),Psi(0:N)) ! Yn(Tempo,Espaço) = Yn(1:3,0:M)
  Yn = 0d0

  DO j=0, M
    Yn(1,j) = EXP(-((j*dx-x0)**2)/sig**2)
  END DO
  Yn(2,:) = Yn(1,:)
  Psi(0) = Yn(2,xi)

  !! EXTREMIDADES FIXAS
  Yn(1:3,0) = 0
  Yn(1:3,M) = 0

  DO j=1, N
    DO k=1, M-1
      Yn(3,k) = 2d0*(1-r*r)*Yn(2,k) - Yn(1,k) + r*r*(Yn(2,k+1) + Yn(2,k-1))
    END DO
    Yn(1,:) = Yn(2,:)
    Yn(2,:) = Yn(3,:)
    Psi(j) = Yn(3,xi)
  END DO


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!! FOURIER E ESPECTRO DE POTÊNCIA !!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ALLOCATE(P(N/2-1))
  P = (0,0)
  
  DO j=1, N/2-1
    DO k=1, N
      P(j) = P(j) + psi(k)*ZEXP(2*PI*i*(j-1)*(k-1)/N)
    END DO
    !!        Ampl | It | Freq f=w/2pi | Parte Real    | Parte Imag    | P(f)
    WRITE(1,*) P(j),   j,  (j-1)/(N*Dt), REALPART(P(j)), IMAGPART(P(j)), REALPART(P(j))**2 + IMAGPART(P(j))**2
  END DO
  CLOSE(1)


  !!!!!!!!!!!!!!!!!!!!!!!!!!! MODOS NORMAIS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  OPEN(UNIT=1, FILE="modos-normais.out", STATUS="UNKNOWN")
  DO j=1, 20
    WRITE(1,*) j*c/(2*L), 1000
  END DO
  CLOSE(1)
END PROGRAM
