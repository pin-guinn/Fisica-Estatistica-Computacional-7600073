program tarefaC1
  integer :: L_list(3)
  character(35) :: E_data(6)
  E_data = [character(len=35) :: "./C1-L-60-E-Beta-1E3.dat","./C1-L-60-E-Beta-1E4.dat",&
                                &"./C1-L-80-E-Beta-1E3.dat","./C1-L-80-E-Beta-1E4.dat",&
                                &"./C1-L-100-E-Beta-1E3.dat","./C1-L-100-E-Beta-1E4.dat"]

  L_list = [60, 80, 100]
  do i=0, 2
    call termloop(L_list(i+1), 0d0, 1.75d0, 0.001d0, E_data(2*i+1))
    call termloop(L_list(i+1), 0d0, 1.75d0, 0.0001d0, E_data(2*i+2))
  end do

  contains
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

  subroutine termloop(L,Beta_min,Beta_max,dB,E_data)
    implicit none
    integer, intent(in) :: L
    real(8), intent(in) :: Beta_min, Beta_max, dB
    character(*), intent(in) :: E_data
    
    integer :: i, j
    real(8) :: E, Beta, init(L,L)
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

    !! Ida
    do i=int(Beta_min), int((Beta_max-Beta_min)/dB)
      Beta = i*dB
      call heatbath(g, Beta, L, E)
      write(1,*) Beta, E / (L*L)
    end do

    !! Volta
    do i=int((Beta_max-Beta_min)/dB)-1, int(Beta_min), -1
      Beta = i*dB
      call heatbath(g, Beta, L, E)
      write(1,*) Beta, E / (L*L)
    end do
    close(1)
  end subroutine
end program
