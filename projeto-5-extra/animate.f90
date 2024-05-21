program main
  implicit none
  integer       :: L = 500
  real(8)       :: Beta = 3d0

  open(unit=1,file='plot.plt',status='unknown')

  write(1,*) '#!/usr/local/bin/gnuplot -persist'
  write(1,*) 'reset'
  write(1,*) '## gnuplot global setting'
  write(1,*) 'set term gif animate delay 5 size 800,800 enhanced font "Courier Bold,16"'
  write(1,*) 'set output "ising.gif"'
  write(1,*) 'set cbrange [0:1]; set palette defined (0 "black", 1 "white")'

  call kissinit(1)

  ! call ising(L, Beta)
  ! call annealing(L, 0d0, 3d0, 0.01d0, .false.) ! Annealing
  ! call annealing(L, 0d0, 3d0, 0.01d0, .true.) ! Quenching (Péssima definição, mal otimizado, só queria salvar espaço)
  ! call termloop(L,0d0,1.75d0,0.01d0)

  close(1)

  contains
  subroutine make_frame(unit,data,L,Beta,mct)
    implicit none
    integer, intent(in) :: unit, L, mct
    real(8), intent(in) :: Beta
    byte, intent(in) :: data(L,L)
    integer :: i,j
    character(1), save :: isimb(-1:1)
    character(5) :: char_Beta
    character(10) :: char_L, char_L2, char_mct, fmt
    real :: altL
    isimb(1) = '1'
    isimb(-1) = '0'

    altL = L-0.5
    write(char_L,'(F5.1)') altL
    write(char_L2,'(I3.2)') L
    write(char_mct,'(I4.1)') mct
    write(char_Beta,'(F5.3)') Beta

    write(unit,*) 'set title "ising model, {/Symbol b} = '//trim(adjustl(char_Beta))//', MC Time = '//trim(adjustl(char_mct))//'"'
    write(unit,*) 'set xrange [-0.5:'//trim(char_L)//']; set xtics add ("'//trim(adjustl(char_L2))//'"'//trim(char_L)//')'
    write(unit,*) 'set yrange [-0.5:'//trim(char_L)//']; set ytics add ("'//trim(adjustl(char_L2))//'"'//trim(char_L)//')'
    write(unit,*) 'set tic scale 0'
    write(unit,*) 'unset grid; unset colorbox'
    write(unit,*) 'plot "-" matrix with image notitle'

    write(fmt,'(I4.2)') L
    fmt = "("//trim(adjustl(fmt))//"a2)"
    do i=1, L
      write(unit,trim(adjustl(fmt))) (isimb(data(i,j)), j=1, L)
    end do

    write(unit,*) 'e'
    write(unit,*) new_line('A')
  end subroutine make_frame

  subroutine heatbath(g, Beta, L)
    implicit none
    integer, intent(in) :: L
    real(8), intent(in) :: Beta
    byte, intent(inout) :: g(L,L)
    
    integer :: x, y, i
    real(8) :: Pi, r(2), u 
    real(8), save :: fb(-4:4)
    byte :: H

    do i=-4,4
      fb(i) = exp(Beta*i)
    end do

    do i=1, L*L
      x = 1 + floor(L*rkiss05())
      y = 1 + floor(L*rkiss05())

      H = ( g(mod(x-2+L,L)+1,y) + g(mod(x,L)+1,y) + g(x,mod(y-2+L,L)+1) + g(x,mod(y,L)+1) )
      Pi = fb(g(x,y)*H) / ( fb(-g(x,y)*H) + fb(g(x,y)*H) )

      u = rkiss05()
      if (u .gt. Pi) g(x,y) = -g(x,y)
    end do
  end subroutine

  subroutine ising(L,Beta)
    implicit none
    integer, intent(in) :: L
    real(8), intent(in) :: Beta
    
    integer :: i, j, k, nterm
    real(8) :: init(L,L)
    byte :: g(L,L)

    nterm = 1200

    do i=1,L
      do j=1,L
        g(i,j) = 2*nint(rkiss05())-1
      end do
    end do
    ! g = 1
    call make_frame(1, g, L, Beta, 0)

    do i=1, nterm
      call heatbath(g, Beta, L)
      call make_frame(1, g, L, Beta, i)
    end do
  end subroutine

  subroutine annealing(L,Beta_i,Beta_f,dB,switch)
    implicit none
    integer, intent(in) :: L
    real(8), intent(in) :: Beta_i, Beta_f, dB
    logical, intent(in) :: switch
    
    integer :: i, j, k
    real(8) :: Beta, init(L,L)
    byte :: g(L,L)

    do i=1,L
      do j=1,L
        g(i,j) = 2*nint(rkiss05())-1
      end do
    end do
    ! g = 1
    call make_frame(1, g, L, Beta, 0)

    if (switch) then ! Quenching
      Beta = 3d0
      do i=int(Beta_i), int((Beta_f-Beta_i)/dB)
        call heatbath(g, Beta, L)
        call make_frame(1,g,L,Beta,i)
      end do
    else ! Annealing
      do i=int(Beta_i), int((Beta_f-Beta_i)/dB)
        Beta = i*dB
        call heatbath(g, Beta, L)
        call make_frame(1,g,L,Beta,i)
      end do
    end if 
  end subroutine

  subroutine termloop(L,Beta_min,Beta_max,dB)
    implicit none
    integer, intent(in) :: L
    real(8), intent(in) :: Beta_min, Beta_max, dB
    
    integer :: i, j, k, turn
    real(8) :: Beta, init(L,L)
    byte :: g(L,L)

    do i=1,L
      do j=1,L
        g(i,j) = 2*nint(rkiss05())-1
      end do
    end do

    turn = int((Beta_max-Beta_min)/dB)
    !! Ida
    do i=int(Beta_min), turn
      Beta = i*dB
      call heatbath(g, Beta, L)
      call make_frame(1,g,L,Beta,i)
    end do

    !! Volta
    do i=turn-1, int(Beta_min), -1
      Beta = i*dB
      call heatbath(g, Beta, L)
      call make_frame(1,g,L,Beta,turn+i)
    end do
  end subroutine
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!|
  !!!!!!!!! rkiss05 code !!!!!!!!!|
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!v

  FUNCTION rkiss05()
  implicit none
 
  integer,parameter      :: r8b= SELECTED_REAL_KIND(P=14,R=99)   ! 8-byte reals
  integer,parameter      :: i4b= SELECTED_INT_KIND(8)            ! 4-byte integers 
  real(r8b),parameter    :: am=4.656612873077392578d-10       ! multiplier 1/2^31

  real(r8b)             :: rkiss05  
  integer(i4b)          :: kiss
  integer(i4b)          :: x,y,z,w              ! working variables for the four generators
  common /kisscom/x,y,z,w 
  
  x = 69069 * x + 1327217885
  y= ieor (y, ishft (y, 13)); y= ieor (y, ishft (y, -17)); y= ieor (y, ishft (y, 5))
  z = 18000 * iand (z, 65535) + ishft (z, - 16)
  w = 30903 * iand (w, 65535) + ishft (w, - 16)
  kiss = ishft(x + y + ishft (z, 16) + w , -1)
  rkiss05=kiss*am
  END FUNCTION rkiss05


  SUBROUTINE kissinit(iinit)
  implicit none
  integer,parameter      :: r8b= SELECTED_REAL_KIND(P=14,R=99)   ! 8-byte reals
  integer,parameter     :: i4b= SELECTED_INT_KIND(8)            ! 4-byte integers 

  integer(i4b) idum,ia,im,iq,ir,iinit
  integer(i4b) k,x,y,z,w,c1,c2,c3,c4
  real(r8b)   rdum
  parameter (ia=16807,im=2147483647,iq=127773,ir=2836)
  common /kisscom/x,y,z,w

  !!! Test integer representation !!!
  c1=-8
  c1=ishftc(c1,-3)
!     print *,c1
  if (c1.ne.536870911) then
     print *,'Nonstandard integer representation. Stoped.'
     stop
  endif

  idum=iinit
  idum= abs(1099087573 * idum)               ! 32-bit LCG to shuffle seeds
  if (idum.eq.0) idum=1
  if (idum.ge.IM) idum=IM-1

  k=(idum)/IQ
  idum=IA*(idum-k*IQ)-IR*k
  if (idum.lt.0) idum = idum + IM
  if (idum.lt.1) then
     x=idum+1 
  else 
     x=idum
  endif
  k=(idum)/IQ
  idum=IA*(idum-k*IQ)-IR*k
  if (idum.lt.0) idum = idum + IM
  if (idum.lt.1) then 
     y=idum+1 
  else 
     y=idum
  endif
  k=(idum)/IQ
  idum=IA*(idum-k*IQ)-IR*k
  if (idum.lt.0) idum = idum + IM
  if (idum.lt.1) then
     z=idum+1 
  else 
     z=idum
  endif
  k=(idum)/IQ
  idum=IA*(idum-k*IQ)-IR*k
  if (idum.lt.0) idum = idum + IM
  if (idum.lt.1) then
     w=idum+1 
  else 
     w=idum
  endif

  rdum=rkiss05()
  
  return
  end subroutine kissinit
end program
