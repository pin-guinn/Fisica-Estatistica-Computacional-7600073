program tarefaC2
  call ising(60, 123, 0.35d0, 0.55d0, 0.01d0, "./C2-L-60-E.dat", "./C2-L-60-Ct.dat")
  call ising(80, 456, 0.35d0, 0.55d0, 0.01d0, "./C2-L-80-E.dat", "./C2-L-80-Ct.dat")
  call ising(100, 789, 0.35d0, 0.55d0, 0.01d0, "./C2-L-100-E.dat", "./C2-L-100-Ct.dat")


  !! O passo de dB = 0.01 é muito grande para o gráfico de Variância versus Beta exibir uma curva
  !! mais precisa, então usando o valor esperado de T_c ≈ 2.27 (Beta_c ≈ 0.44), diminuímos o passo para 0.001 e rodamos
  !! novamente no intervalo (Beta_c-0.02,Beta_c+0.02) para somarmos aos dados anteriores.
  call ising(60, 123, 0.42d0, 0.46d0, 0.001d0, "./dummy.dat", "./C2-L-60-Ct-Extra.dat")
  call ising(80, 456, 0.42d0, 0.46d0, 0.001d0, "./dummy.dat", "./C2-L-80-Ct-Extra.dat")
  call ising(100, 789, 0.42d0, 0.46d0, 0.001d0, "./dummy.dat", "./C2-L-100-Ct-Extra.dat")

  contains
  subroutine ising(L,seed,Beta_i,Beta_f,dB,E_data,Ct_data)
    implicit none
    integer, intent(in) :: L, seed
    real(8), intent(in) :: Beta_i, Beta_f, dB
    character(*), intent(in) :: E_data, Ct_data
    
    integer, parameter :: nterm = 50000, niter = 200000
    integer :: i, j, k, m
    real(8) :: E, Beta, init(L,L), x, xx, Energies(nterm+niter,0:ceiling((Beta_f-Beta_i)/dB))
    real(8) :: fb(-4:4)
    byte :: g(L,L)

    open(unit=1,file=E_data,status='unknown')
    open(unit=2,file=Ct_data,status='unknown')

    call kissinit(seed)

    do i=1, L/2
      do j=1, L
        init(i,j) = 2*nint(rkiss05())-1
      end do
    end do
    init(L/2+1:L,1:L) = 1

    g = init

    do i=0, ceiling((Beta_f-Beta_i)/dB)
      g = init
      Beta = Beta_i + i*dB

      do m=-4,4
        fb(m) = exp(-Beta*m)
      end do

      x = 0
      xx = 0
      E = 0
      do j=1, L
        do k=1, L
          E = E - g(j,k)*( g(mod(j,L)+1,k) + g(j,mod(k,L)+1) )
        end do
      end do
      
      !! Termalização
      do m=1, nterm
        call heatbath(g, fb, L, E)
        Energies(m,i) = E / (L*L)
      end do

      do m=1, niter
        call heatbath(g, fb, L, E)
        Energies(nterm+m,i) = E / (L*L)
        x = x + E
        xx = xx + E*E
      end do
      x = x/niter
      xx = xx/niter
      write(2,*) 1/Beta, (xx - x*x) * (Beta**2) ! Beta, Calor Específico / N
    end do

    do i=1, nterm
      write(1,*) i, Energies(i,:)
    end do
    close(1)
    close(2)
  end subroutine

  subroutine heatbath(g, fb, L, E)
    implicit none
    integer, intent(in) :: L
    real(8), intent(in) :: fb(-4:4)
    real(8), intent(inout) :: E
    byte, intent(inout) :: g(L,L)
    
    integer :: x, y, i
    real(8) :: Pi, u
    byte :: H

    do i=1, L*L
      x = 1 + floor(L*rkiss05())
      y = 1 + floor(L*rkiss05())

      H = ( g(mod(x-2+L,L)+1,y) + g(mod(x,L)+1,y) + g(x,mod(y-2+L,L)+1) + g(x,mod(y,L)+1) )
      Pi = fb(g(x,y)*H) / ( fb(-g(x,y)*H) + fb(g(x,y)*H) )

      u = rkiss05()
      if (u .gt. Pi) then
        g(x,y) = -g(x,y)
        E = E - 2d0*g(x,y)*H
      end if
    end do
  end subroutine

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
  real(r8b)    rdum
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
