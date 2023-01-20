! The Computer Language Benchmarks Game
! https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
!
! fasta implementation - translated from the lua program
! contributed by Simon Geard, 18/1/05
!
! Building info.
! ==============
!
! Linux  - using the Intel Fortran90 compiler:
!
!          ifort fasta.f90 -O3 -static-libcxa -o fasta
!
! Run
! ===
!          fasta 1000

program fasta

 implicit none
  integer num, m
  character(len=8) argv
  logical, dimension(:), allocatable :: flags
  integer, parameter :: IM = 139968
  integer, parameter :: IA = 3877
  integer, parameter :: IC = 29573
  character(len=*), parameter :: alu = &
ʼGGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGʼ // &
ʼGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGAʼ // &
ʼCCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATʼ // &
ʼACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAʼ // &
ʼGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGʼ // &
ʼAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCʼ // &
ʼAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAʼ

  type pair
     character(len=1) c
     real*8 p
  end type pair
  type(pair), dimension(15) :: iub
  type(pair), dimension(4)  :: homosapiens
  homosapiens = (/ pair(ʼaʼ, 0.3029549426680d0), &
                   pair(ʼcʼ, 0.1979883004921d0), &
                   pair(ʼgʼ, 0.1975473066391d0), &
                   pair(ʼtʼ, 0.3015094502008d0) /)
  call makeCumulative(homosapiens)

  iub = (/ pair(ʼaʼ, 0.27d0), &
           pair(ʼcʼ, 0.12d0), &
           pair(ʼgʼ, 0.12d0), &
           pair(ʼtʼ, 0.27d0), &
           pair(ʼBʼ, 0.02d0), &
           pair(ʼDʼ, 0.02d0), &
           pair(ʼHʼ, 0.02d0), &
           pair(ʼKʼ, 0.02d0), &
           pair(ʼMʼ, 0.02d0), &
           pair(ʼNʼ, 0.02d0), &
           pair(ʼRʼ, 0.02d0), &
           pair(ʼSʼ, 0.02d0), &
           pair(ʼVʼ, 0.02d0), &
           pair(ʼWʼ, 0.02d0), &
           pair(ʼYʼ, 0.02d0) /)
  call makeCumulative(iub)

  call getarg(1,argv)
  read(argv,*) num


  call makeRepeatFasta(ʼONEʼ,ʼHomo sapiens aluʼ,alu,num*2)

  call makeRandomFasta(ʼTWOʼ,ʼIUB ambiguity codesʼ,iub,num*3)

  call makeRandomFasta(ʼTHREEʼ,ʼHomo sapiens frequencyʼ,homosapiens,num*5)


contains

  real*8 function getRandom (maxval)
    real*8, intent(in) :: maxval
    integer, save :: last = 42

    last = mod(last * IA + IC, IM)
    getRandom = maxval * last / IM

  end function getRandom

  subroutine makeCumulative(a)
     type(pair), dimension(:), intent(inout) :: a
     integer i
     real*8 :: cp

     cp = 0.0d0
     do i=1,size(a)
        cp = cp + a(i)%p
        a(i)%p = cp
     end do
  end subroutine makeCumulative

  character(len=1) function selectRandom(a)
      type(pair), dimension(:), intent(inout) :: a
     integer i
     real*8 :: r

     r = getRandom(1.0d0)
     selectRandom = ʼJʼ
     do i=1,size(a)
        if (r < a(i)%p) then
           selectRandom = a(i)%c
           return
        end if
     end do

  end function selectRandom

  subroutine makeRandomFasta(id,desc,a,n)
     character(len=*), intent(in) :: id
     character(len=*), intent(in) :: desc
     type(pair), dimension(:), intent(inout) :: a
     integer, intent(in) :: n
     integer :: todo, i
     integer, parameter :: length = 60
     character(len=length) :: buff

     write(*,ʼ(4a)ʼ) ʼ>ʼ,id,ʼ ʼ,desc
     todo = n
     do
        if (todo <= 0) exit
        if (todo < length) then
           m = todo
        else
           m = length
        end if
        do i=1,m
           buff(i:i) = selectRandom(a)
        end do
        write(*,ʼ(a)ʼ) buff(1:m)
        todo = todo - length
     end do
  end subroutine makeRandomFasta

  subroutine makeRepeatFasta(id,desc,s,n)
     character(len=*), intent(in) :: id
     character(len=*), intent(in) :: desc
     character(len=*), intent(in) :: s
     integer, intent(in) :: n
     integer :: todo, i, j, k, kn
     integer, parameter :: length = 60
     character(len=length) :: buff
     intrinsic len

     write(*,ʼ(4a)ʼ) ʼ>ʼ,id,ʼ ʼ,desc
     todo = n; k = 1; kn = len(s)
     do
        if (todo <= 0) exit
        if (todo < length) then
           m = todo
        else
           m = length
        end if
        do j=1,m
           if (k > kn) then
              k = 1
           endif
           buff(j:j) = s(k:k)
           k = k + 1
        end do
        write(*,ʼ(a)ʼ) buff(1:m)
        todo = todo - length
     end do

  end subroutine makeRepeatFasta

end program fasta

