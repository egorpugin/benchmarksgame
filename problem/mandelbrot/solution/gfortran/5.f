! The Computer Language Benchmarks Game
! https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
!
! Simon Geard, 6/1/05
! Adapted  mandelbrot.c by Greg Buchholz
! Modified to use explicit kind parameters by Jason Blevins, 4/10/10.
!
!          ifort mandelbrot.f90 -O3 -static-libcxa -o mandelbrot

program mandelbrot
  implicit none
  integer, parameter :: dp = kind(1.0d0)
  integer w, h, x, y, bit_num
  integer(kind=1) byte_acc
  integer(kind=1), parameter :: K0 = 0
  integer(kind=1), parameter :: K1 = 1
  integer, parameter :: iter = 50
  real(kind=dp), parameter  :: limit2 = 4.0_dp
  integer  i
  character(len=8) argv
  complex(kind=dp) :: Z, C
  logical debug, in_mandelbrot

  call getarg(1,argv)
  read(argv,*) w
  h = w
  bit_num = 0
  byte_acc = K0
  ! Output pbm header
  write(*,'(a)') 'P4'
  write(*,'(i0,a,i0)') w,' ',h
  do y=0,h-1
     do x=0,w-1
        C = cmplx(2.0d0*x/w-1.5d0,2.0d0*y/h-1.0d0, dp)
        Z = (0.0d0,0.0d0)
        in_mandelbrot = .true.
        do i=0,iter-1
           Z = Z**2 + C
           if (real(Z*conjg(Z)) > limit2) then
              in_mandelbrot = .false.
              exit
           end if
        end do
        if (in_mandelbrot) then
           ! Inside the set so set this bit to 1
           byte_acc = ior(ishft(byte_acc,1),K1)
        else
           ! Outside the set so set this bit to 0
           byte_acc = ishft(byte_acc,1)
        end if

        bit_num = bit_num + 1
        if (bit_num == 8) then
           ! All bits set so output them
           write(*,'(a1)',advance='no') char(byte_acc)
           byte_acc = K0
           bit_num = 0

        elseif (x == w-1) then
           ! End of a row so left-justify the bits we have and output them
           byte_acc = ishft(byte_acc,8-mod(w,8))
           write(*,'(a1)',advance='no') char(byte_acc)
           byte_acc = K0
           bit_num = 0

        end if

     end do
  end do
end program mandelbrot

