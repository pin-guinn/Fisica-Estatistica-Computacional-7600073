PROGRAM tarefa5
  IMPLICIT NONE
  INTEGER, PARAMETER :: L = 2000, It = 50000
  INTEGER :: cluster(L*L,2)=0, stat(L,L)=0, grid(L,L)=0, rd(0:3,2), tmp
  INTEGER :: i, j, x, y, dx, dy, cnt, N = 0, Rmax = 0
  LOGICAL :: glue = .FALSE.
  REAL(8) :: d

  OPEN(UNIT=1,FILE="data.out",STATUS="UNKNOWN")

  !! Random Walk Options
  rd(0, 1) = 1; rd(0, 2) = 0 !! (1, 0)
  rd(1, 1) =-1; rd(1, 2) = 0 !! (-1, 0)
  rd(2, 1) = 0; rd(2, 2) = 1 !! (0, 1)
  rd(3, 1) = 0; rd(3, 2) =-1 !! (0, -1)

  CALL SRAND(14)
  ! CALL SRAND(88)
  ! CALL SRAND(37)

  !! Populando o lattice 'stat' com p=10% não revolucionários.
  DO i=1,L
    DO j=1,L
      stat(i,j) = NINT(RAND()-(0.5d0-0.1d0))
    END DO
  END DO
  stat(L/2,L/2) = 0

  !! Definindo o primeiro revolucionário no meio do lattice.
  cluster(1,:) = (/ L/2, L/2 /)
  cnt = 1

  !! O algorítimo abaixo tem o seguinte objetivo: achar uma quantidade de aproximadamente It revolucionários
  !! realizando um random walk no cluster até achar pelo menos um novo.
  DO j=1, It
    glue = .FALSE.
    DO WHILE (.NOT.glue)
      glue = .FALSE.
      tmp = INT(4*RAND())
      dx = rd(tmp,1)
      dy = rd(tmp,2)
      DO i=1, cnt
        x = cluster(i,1)
        y = cluster(i,2)

        IF (stat(x+dx,y+dy) == 1) THEN
          glue = .TRUE.
          cnt = cnt + 1
          cluster(cnt,:) = (/ x+dx, y+dy /)
          stat(x+dx,y+dy) = 0
        END IF
      END DO

      IF (.NOT.glue) THEN
        DO i=1, cnt
          cluster(i,1) = cluster(i,1) + dx
          cluster(i,2) = cluster(i,2) + dy
        END DO
      END IF
    END DO
  END DO

  !! O script para gerar a imagem pinta de branco os 0s, vermelho os 1s e azul os 2s. 
  !! 1s sendo os não-revolucionários e 2 os revolucionários. As linhas seguintes apenas
  !! colocam toda essa informação na variável grid, que será exportada para o arquivo
  !! 'data.out'.
  DO i=1, cnt
    x = cluster(i,1)
    y = cluster(i,2)
    grid(x,y) = 2
  END DO

  grid = grid + stat
  DO i=1, L
    WRITE(1,*) grid(i,:)
  END DO

  !! Determinando o raio do cluster para usar de referência quando calcular
  !! a dimensão fractal.
  DO i=1, cnt
    d = SQRT(((cluster(i,1)-cluster(1,1))**2 + (cluster(i,2)-cluster(1,2))**2)*1d0)
    IF (d .GT. Rmax) Rmax = d
  END DO

  !! Finalmente, podemos iterar o cluster e obter a dimensão fractal do mesmo.
  OPEN(UNIT=2,FILE="fractal.out",STATUS="UNKNOWN")
  DO j=5, Rmax, 5
    DO i=1, cnt
      d = SQRT(1d0*(cluster(i,1)-cluster(1,1))**2+1d0*(cluster(i,2)-cluster(1,2))**2)
      IF (d .LE. j) N = N + 1
    END DO
    WRITE(2,*) LOG(1d0*j), LOG(1d0*N)
    N = 0
  END DO
END PROGRAM
