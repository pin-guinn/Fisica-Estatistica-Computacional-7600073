PROGRAM tarefa2
  IMPLICIT NONE
  REAL(8), PARAMETER :: PI = 4d0*ATAN(1d0)
  INTEGER, PARAMETER :: It = 20000
  REAL(8) :: Ri, Rf, ang, d
  INTEGER :: i, j, dx, dy, cluster(It,2), rd(0:3,2), v(2), tmp, cnt, N = 0, Rmax = 0
  LOGICAL :: glue = .FALSE.

  OPEN(UNIT=1,FILE="data.out",STATUS="UNKNOWN")

  !! Random Walk Options
  rd(0, 1) = 1; rd(0, 2) = 0 !! (1, 0)
  rd(1, 1) =-1; rd(1, 2) = 0 !! (-1, 0)
  rd(2, 1) = 0; rd(2, 2) = 1 !! (0, 1)
  rd(3, 1) = 0; rd(3, 2) =-1 !! (0, -1)

  ! CALL SRAND(42)
  ! CALL SRAND(73)
  CALL SRAND(97)

  Ri = 5d0
  Rf = 1.5d0 * Ri

  cluster(1,:) = (/ 0, 0/)
  cnt = 1

  DO i=1, It
    ang = 2d0 * PI * rand()
    v = nint(Ri * (/ COS(ang), SIN(ang) /))
    glue = .FALSE.
    DO WHILE (.NOT.glue)
      glue = .FALSE.
      tmp = int(4*rand())
      dx = rd(tmp,1)
      dy = rd(tmp,2)
      DO j=1,cnt
        IF (v(1)+dx==cluster(j,1) .AND. v(2)+dy==cluster(j,2)) THEN
          !! Adicionar ao cluster a nova partícula
          glue = .TRUE.
          cnt = cnt + 1
          cluster(cnt,:) = v
          
          !! Calcular o novo tamanho do aglomerado
          d = SQRT((1d0*(v(1)**2 + v(2)**2)))
          IF (d .GT. Ri) THEN
            Ri = 5d0 + d 
            Rf = 1.5d0 * Ri
          END IF
          EXIT
        END IF
      END DO

      IF (.NOT.glue) THEN
        v = (/ v(1)+dx, v(2)+dy /)
        IF (SQRT((1d0*(v(1)**2 + v(2)**2))) .GT. Rf) EXIT
      END IF
    END DO
  END DO
    
  DO i=1,cnt
    write(1,*) cluster(i,:)
  END DO

  !! Determinando o raio do cluster para usar de referência quando calcular
  !! a dimensão fractal.
  DO i=1, cnt
    d = SQRT(((cluster(i,1)**2 + cluster(1,2)**2)*1d0))
    IF (d .GT. Rmax) Rmax = d
  END DO

  !! Finalmente, podemos iterar o cluster e obter a dimensão fractal do mesmo.
  OPEN(UNIT=2,FILE="fractal.out",STATUS="UNKNOWN")
  DO j=5, Rmax, 5 
    DO i=1, cnt
      d = SQRT(1d0*(cluster(i,1)**2+1d0*cluster(i,2)**2))
      IF (d .LE. j) N = N + 1
    END DO
    WRITE(2,*) LOG(1d0*j), LOG(1d0*N)
    N = 0
  END DO
END PROGRAM
