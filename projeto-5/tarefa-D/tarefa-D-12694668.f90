program tarefaD
  open(unit=1,file="D-Tempo.dat",status='unknown')
  open(unit=2,file="D-Magnetization.dat",status='unknown')
  Tempo = 0
  do i=4,10
    call ising(i,1d0/2d0,Tempo)
    write(1,*) i, Tempo, log(Tempo)
  end do
  close(1)
  close(2)

  contains
  subroutine heatbath(g, Beta, L, M)
    implicit none
    integer, intent(in) :: L
    real(8), intent(in) :: Beta
    real(8), intent(inout) :: M
    byte, intent(inout) :: g(L,L)
    
    integer :: x, y, i
    real(8) :: Pi, r(2), u
    byte :: H

    do i=1, L*L
      call RANDOM_NUMBER(r)
      x = 1 + floor(L*r(1))
      y = 1 + floor(L*r(2))

      H = ( g(mod(x-2+L,L)+1,y) + g(mod(x,L)+1,y) + g(x,mod(y-2+L,L)+1) + g(x,mod(y,L)+1) )
      Pi = exp( Beta*g(x,y)*H) / ( exp(-Beta*g(x,y)*H) + exp(Beta*g(x,y)*H) )

      call RANDOM_NUMBER(u)
      if (u .gt. Pi) then
        g(x,y) = -g(x,y)
        M = M + 2*g(x,y)
      end if
    end do
  end subroutine

  subroutine ising(L,Beta,Tempo)
    implicit none
    integer, intent(in) :: L
    real(8), intent(in) :: Beta
    real, intent(out) :: Tempo
    
    integer, parameter :: nterm = 10000, niter = 500000
    integer :: i, j, Ntroca
    real(8) :: Ma, Mb, init(L,L)
    byte :: g(L,L)

    Ntroca = 1
    Ma = 0
    Mb = 0

    call RANDOM_NUMBER(init)
    g = 2*nint(init)-1

    do i=1, L
      do j=1, L
        Ma = Ma + g(i,j)
      end do
    end do

    do i=1, nterm
      call heatbath(g,Beta,L,Ma)
    end do
    Mb = Ma

    do i=1, niter
      call heatbath(g,Beta,L,Mb)

      if (L .eq. 10) write(2,*) i, Mb/(L*L)

      if (Ma*Mb .lt. 0d0) then
        Ntroca = Ntroca + 1
        Ma = Mb
      end if
    end do

    Tempo = niter/Ntroca
  end subroutine
end program
