program tarefaB1
  call annealing(60, 0d0, 3d0, 0.001d0, "B1-Annealing-L-60-Energy.dat", "B1-Annealing-L-60-Final-Config.out")

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

  subroutine heatbath(g, Beta, L, E)
    implicit none
    integer, intent(in) :: L
    real(8), intent(in) :: Beta
    real(8), intent(inout) :: E
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
        E = E - 2d0*g(x,y)*H
      end if
    end do
  end subroutine

  subroutine annealing(L,Beta_i,Beta_f,dB,E_data,config_file)
    implicit none
    integer, intent(in) :: L
    real(8), intent(in) :: Beta_i, Beta_f, dB
    character(*), intent(in) :: E_data, config_file
    
    integer :: i, j
    real(8) :: E , Beta, init(L,L)
    byte :: g(L,L)

    open(unit=1,file=E_data,status='unknown')

    call RANDOM_NUMBER(init)
    g = 2*nint(init)-1
    
    E = 0
    do i=1, L
      do j=1, L
        E = E - g(i,j)*( g(mod(i,L)+1,j) + g(i,mod(j,L)+1) )
      end do
    end do

    do i=int(Beta_i), int((Beta_f-Beta_i)/dB)
      Beta = i*dB
      call heatbath(g, Beta, L, E)
      write(1,*) i, E / (L*L)
    end do
    call plot(g,L,config_file)
    close(1)
  end subroutine
end program
