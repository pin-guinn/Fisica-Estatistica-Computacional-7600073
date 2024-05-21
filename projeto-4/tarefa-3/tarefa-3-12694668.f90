PROGRAM tarefa3
  IMPLICIT NONE
  REAL(8), PARAMETER :: PI = 4d0*ATAN(1d0)
  INTEGER, PARAMETER :: It = 50000
  REAL(8) :: Ri, Rf, theta, phi, d
  INTEGER :: i, j, dx, dy, dz, cluster(It,3), rd(0:5,3), v(3), tmp, cnt, N = 0, Rmax = 0
  LOGICAL :: glue

  OPEN(UNIT=1,FILE="data.out",STATUS="UNKNOWN")

  !! Random Walk Options
  rd(0, 1) = 1; rd(0, 2) = 0; rd(0,3) = 0 !! (1, 0, 0)
  rd(1, 1) =-1; rd(1, 2) = 0; rd(1,3) = 0 !! (-1, 0, 0)
  rd(2, 1) = 0; rd(2, 2) = 1; rd(2,3) = 0 !! (0, 1, 0)
  rd(3, 1) = 0; rd(3, 2) =-1; rd(3,3) = 0 !! (0,-1, 0)
  rd(4, 1) = 0; rd(4, 2) = 0; rd(4,3) = 1 !! (0, 0, 1)
  rd(5, 1) = 0; rd(5, 2) = 0; rd(5,3) =-1 !! (0, 0,-1)

  CALL SRAND(15)
  ! CALL SRAND(37)
  ! CALL SRAND(92)

  Ri = 5d0
  Rf = 1.5d0 * Ri

  cluster(1,:) = (/ 0, 0, 0 /)
  cnt = 1

  DO i=1, It
    theta = PI * rand()
    phi = 2d0 * PI * rand()
    v = nint(Ri * (/ SIN(theta)*COS(phi), SIN(theta)*SIN(phi), COS(theta) /))

    glue = .FALSE.
    DO WHILE (.NOT.glue)
      glue = .FALSE.
      tmp = int(6*rand())
      dx = rd(tmp,1)
      dy = rd(tmp,2)
      dz = rd(tmp,3)
      DO j=1,cnt
        IF ((v(1)+dx)==cluster(j,1) .AND. (v(2)+dy)==cluster(j,2) .AND. (v(3)+dz)==cluster(j,3)) THEN
          !! Adicionar ao cluster a nova partícula
          glue = .TRUE.
          cnt = cnt + 1
          cluster(cnt,:) = v
          
          !! Calcular o novo tamanho do aglomerado
          d = SQRT(1d0*(v(1)**2 + v(2)**2 + v(3)**2))
          IF (d .GT. Ri) THEN
            Ri = 5d0 + d 
            Rf = 1.5d0 * Ri
          END IF
          EXIT
        END IF
      END DO

      IF (.NOT.glue) THEN
        v = (/ v(1)+dx, v(2)+dy, v(3)+dz /)
        IF (SQRT(1d0*(v(1)**2 + v(2)**2 + v(3)**2)) .GT. Rf) EXIT
      END IF
    END DO
  END DO

  DO i=1,cnt
    write(1,*) cluster(i,:)
  END DO

  !! Determinando o raio do cluster para usar de referência quando calcular
  !! a dimensão fractal.
  DO i=1, cnt
    d = SQRT((cluster(i,1)**2 + cluster(i,2)**2 + cluster(i,3)**2)*1d0)
    IF (d .GT. Rmax) Rmax = d
  END DO

  !! Finalmente, podemos iterar o cluster e obter a dimensão fractal do mesmo.
  OPEN(UNIT=2,FILE="fractal.out",STATUS="UNKNOWN")
  DO j=5, Rmax, 2
    DO i=1, cnt
      d = SQRT(1d0*(cluster(i,1)**2+cluster(i,2)**2+cluster(i,3)**2))
      IF (d .LE. j) N = N + 1
    END DO
    WRITE(2,*) LOG(1d0*j), LOG(1d0*N)
    N = 0
  END DO
END PROGRAM
