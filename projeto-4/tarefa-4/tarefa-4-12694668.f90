PROGRAM tarefa4
  IMPLICIT NONE
  INTEGER, PARAMETER :: L = 400, It = 20000
  REAL(8) :: Ri, Rf, xi, d
  INTEGER :: i, j, dx, dy, cluster(It,2), rd(0:3,2), v(2), tmp, cnt = 0, N = 0, Rmax = 0
  LOGICAL :: glue = .FALSE.

  OPEN(UNIT=1,FILE="data.out",STATUS="UNKNOWN")

  !! Random Walk Options
  rd(0, 1) = 1; rd(0, 2) = 0 !! (1, 0)
  rd(1, 1) =-1; rd(1, 2) = 0 !! (-1, 0)
  rd(2, 1) = 0; rd(2, 2) = 1 !! (0, 1)
  rd(3, 1) = 0; rd(3, 2) =-1 !! (0, -1)

  ! CALL SRAND(15)
  ! CALL SRAND(55)
  CALL SRAND(61)

  Ri = 5d0
  Rf = 1.5d0 * Ri

  !! Fazendo a primeira fileira de partículas do cluster em y=0.
  DO i=0, L
    cluster(i,:) = (/ i-L/2, 0/)
    cnt = cnt + 1
  END DO

  DO i=1, It
    xi = -L/2 + L*RAND()
    v = INT((/ xi, Ri /))
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
          IF (v(2) .GT. Ri) THEN
            Ri = 5d0 + v(2)
            Rf = 1.5d0 * Ri
          END IF
          EXIT
        END IF
      END DO

      IF (.NOT.glue) THEN
        v = (/ v(1)+dx, v(2)+dy /)
        IF ( (v(2) .GT. Rf) .OR. (v(2) .LT. 0) ) EXIT
      END IF
    END DO
  END DO
    
  DO i=1,cnt
    write(1,*) cluster(i,:)
  END DO
END PROGRAM
