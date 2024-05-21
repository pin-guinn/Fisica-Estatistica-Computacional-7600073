MODULE utils
  INTEGER     , PARAMETER, PUBLIC   :: dp = SELECTED_REAL_KIND(p=15)
  REAL(dp)    , PARAMETER, PUBLIC   :: PI = 4d0*ATAN(1d0)
  COMPLEX(dp) , PARAMETER, PRIVATE  :: i  = (0d0,1d0)

  CONTAINS
  SUBROUTINE Fourier(N, Dt)
    INTEGER , INTENT(IN)  :: N
    REAL(dp), INTENT(IN)  :: Dt

    COMPLEX(dp) :: ym(N)
    REAL(dp)    :: yn(N)
    INTEGER     :: j, k

    ym = (0,0)
    
    OPEN(UNIT=1, FILE="data.in", STATUS="UNKNOWN")
    OPEN(UNIT=2, FILE="data.out", STATUS="UNKNOWN")
    
    DO j=1, N
      READ(1,*) yn(j)
    END DO

    DO j=1, N
      DO k=1, N
        ym(j) = ym(j) + yn(k)*ZEXP(2*PI*i*(j-1)*(k-1)/N)
      END DO
      !! Salvando: tupla de complexo | iteração | frequencia | parte real | parte imaginária
      WRITE(2,*) ym(j), j, (j-1)/(N*Dt), REALPART(ym(j)), IMAGPART(ym(j))
    END DO

    CLOSE(1)
    CLOSE(2)
  END SUBROUTINE

  SUBROUTINE InvFourier(N, Dt)
    INTEGER   , INTENT(IN)  :: N
    REAL(dp)  , INTENT(IN)  :: Dt
    COMPLEX(dp)             :: ym(N), yn(N)
    INTEGER                 :: j, k

    ym = (0,0)
    
    OPEN(UNIT=1, FILE="data.out", STATUS="UNKNOWN")
    OPEN(UNIT=2, FILE="datainv.out", STATUS="UNKNOWN")

    DO j=1, N
      READ(1,*) yn(j)
    END DO

    DO j=1, N
      DO k=1, N
        ym(j) = ym(j) + (1d0/N)*yn(k)*ZEXP(-2*PI*i*(j-1)*(k-1)/N)
      END DO
      !! Salvando: tupla de complexo | iteração | tempo | parte real | parte imaginária
      WRITE(2,*) ym(j), j, (j-1)*Dt, REALPART(ym(j)), IMAGPART(ym(j))
    END DO

    CLOSE(1)
    CLOSE(2)
  END SUBROUTINE

  SUBROUTINE GerarSerie(N, Dt, a1, a2, w1, w2)
    REAL(dp), INTENT(IN) :: Dt, a1, a2, w1, w2
    INTEGER , INTENT(IN) :: N

    OPEN(UNIT=1, FILE="data.in", STATUS="UNKNOWN")

    DO j=0, N-1
      WRITE(1,*) a1*DCOS(w1*Dt*j) + a2*DSIN(w2*Dt*j), j*Dt
    END DO

    CLOSE(1)
  END SUBROUTINE
END MODULE


PROGRAM tarefa1
  USE :: utils

  !! Teste de funcionamento
  ! CALL GerarSerie(200, 0.04d0, 2d0, 4d0, 4d0*PI, 2.5d0*PI)
  ! CALL Fourier(200, 0.04d0)
  ! CALL InvFourier(200, 0.04d0)
END PROGRAM
