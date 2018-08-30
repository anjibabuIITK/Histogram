!A FORTRAN90 program to calculate Distribution of given data.
!
!Authour: ANJI BABU KAPAKAYALA
!         IIT KANPUR, INDIA.
!
!Writing Histogram 
PROGRAM histogram
  implicit none
  real*8              ::x,xmin,xmax,width,u
  real*8, allocatable ::prob(:)
  integer*8           ::i,j,N,bin,nbin  !N=md_steps
  character (len=125) :: input_filename,output_filename
 PRINT*, 'ENTER input_filename ', '   output_filename'
 READ(*,*)input_filename, output_filename
!  print *, 'N=',N, ' xmin=',xmin, ' xmax =', xmax, ' width=',width, ' input_filename = ', trim(input_filename),' output_filename = ', trim(ouput_filename)
  open(12,file=trim(input_filename),STATUS="OLD")
  open(13,file=trim(output_filename),STATUS="NEW")
  call get_steps(12,N)
  print*,"Total steps =",N
  call get_xmin_xmax(12,xmin,xmax,width)
  print*, "xmin= ",xmin,"xmax=",xmax,"Width=",width
  nbin=nint(((xmax-xmin)/width)) + 1
  print *, 'nbin =',nbin
  allocate(prob(nbin+1))
  prob = 0.0
     do i=1,N
        read(12,*)x
        bin =int((x-xmin)/width)+1
           if(bin < nbin+1)then
             prob(bin) = prob(bin) +1.00
           end if
     end do 
     do j=1,nbin
        write(13,*) real(j-1)*width+xmin, prob(j)/dfloat(N) !printing x vs P(x) vaules
     end do
  close(12)
  close(13)
  deallocate(prob)
  print*, "The Distribution of given data has stored in given outputfile"
!  print*,
!  print*, "Plot 'x vs P(x')"
END PROGRAM histogram
!===========================================================================!
SUBROUTINE get_steps(iunt,nsteps)
IMPLICIT NONE
INTEGER,INTENT(IN)  ::iunt
INTEGER*8,INTENT(OUT)  ::nsteps
INTEGER ::ios
nsteps=0
REWIND(iunt)
DO
READ(iunt,*,IOSTAT=ios)
IF(ios /= 0) EXIT
nsteps=nsteps+1
ENDDO
REWIND(iunt)
END SUBROUTINE get_steps
!============================================================================!
SUBROUTINE get_xmin_xmax(iunt,xmin,xmax,xdif)
 IMPLICIT NONE
 INTEGER,INTENT(IN)  :: iunt
 REAL*8,INTENT(INOUT)  :: xmin,xmax,xdif
 INTEGER :: ios
 REAL*8  :: x
 INTEGER, PARAMETER :: Def_No_bins=101
 REWIND(iunt)
 READ(iunt,*,IOSTAT=ios)x
 if(ios /= 0)stop 'ERROR reading INPUT'
 xmin=x
 xmax=x
 RLoop: DO
   READ(iunt,*,IOSTAT=ios)x
   if(ios /= 0)EXIT RLoop
   xmin=MIN(xmin,x)
   xmax=MAX(xmax,x)
 END DO RLoop
 xdif=(xmax-xmin)/DFLOAT(Def_No_bins)
 REWIND(iunt)
END SUBROUTINE get_xmin_xmax
!============================================================================!
!                        ANJI BABU KAPAKAYALA                                !
!============================================================================!










