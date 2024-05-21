program tarefaA
  character(26) :: M_data(4)
  character(37) :: config_file(4)
  M_data = [character(len=26) :: "./tarefa-A1/A1-L-60-M.dat", "./tarefa-A1/A1-L-100-M.dat", &
                                &"./tarefa-A2/A2-L-60-M.dat", "./tarefa-A2/A2-L-100-M.dat"]
  config_file = [character(len=37) :: "./tarefa-A1/A1-L-60-final-config.out", "./tarefa-A1/A1-L-100-final-config.out",&
                                    &"./tarefa-A2/A2-L-60-final-config.out", "./tarefa-A2/A2-L-100-final-config.out" ]

  call ising(60, 3d0, M_data(1), config_file(1))
  call ising(100, 3d0, M_data(2), config_file(2))
  call ising(60, 0.1d0, M_data(3), config_file(3))
  call ising(100, 0.1d0, M_data(4), config_file(4))

  contains
  subroutine plot(g, L, config_file)
    integer, intent(in) :: L
    character(*), intent(in) :: config_file
    byte, intent(in) :: g(L,L)
    character(1) :: isimb(-1:1)
    isimb(1) = '1'
    isimb(-1) = '0'
    open(unit=2,file=config_file,status='unknown')
    do i=1, L
      write(2,'(200a2)') (isimb(g(i,j)), j=1, L)
    end do
    close(2)
  end subroutine

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

  subroutine ising(L,Beta,M_data,config_file)
    implicit none
    integer, intent(in) :: L
    real(8), intent(in) :: Beta
    character(*), intent(in) :: M_data, config_file
    
    integer :: i, nterm = 50000, niter = 10000
    real(8) :: M
    byte :: g(L,L)

    open(unit=1,file=M_data,status='unknown')
    
    g = 1
    M = 1d0*L*L

    do i=1, nterm + niter
      call heatbath(g, Beta, L, M)
      write(1,*) i, M / (L*L)
    end do
    call plot(g,L,config_file)
    close(1)
  end subroutine
end program
