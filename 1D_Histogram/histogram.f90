!A FORTRAN90 program to calculate Distribution of given data.
!
!Authour: ANJI BABU KAPAKAYALA
!         IIT KANPUR, INDIA.
!
!Writing Histogram 
PROGRAM histogram
 IMPLICIT NONE
 REAL*8              ::x,xmin,xmax,width,u
 REAL*8, ALLOCATABLE ::prob(:)
 INTEGER*8           ::i,j,N,bin,nbin  !N=md_steps
 CHARACTER (len=125) :: input_filename,output_filename
 PRINT*, 'ENTER input_filename '!, '   output_filename'
 READ(*,*)input_filename !, output_filename
!  print *, 'N=',N, ' xmin=',xmin, ' xmax =', xmax, ' width=',width, ' input_filename = ', trim(input_filename),' output_filename = ', trim(ouput_filename)
  OPEN(12,file=trim(input_filename),STATUS="OLD")
! OPEN(13,file=trim(output_filename),STATUS="NEW")
  OPEN(13,file="HISTOGRAM",STATUS= "NEW")
  CALL get_steps(12,N)
  print*,"Total steps =",N
  CALL get_xmin_xmax(12,xmin,xmax,width)
  PRINT*, "xmin= ",xmin,"xmax=",xmax,"Width=",width
  nbin=nint(((xmax-xmin)/width)) + 1
  PRINT*, 'nbin =',nbin
  ALLOCATE(prob(nbin+1))
  prob = 0.0
     DO i=1,N
        READ(12,*)x
        bin =int((x-xmin)/width)+1
           IF(bin < nbin+1)THEN
             prob(bin) = prob(bin) +1.00
           END IF
     END DO 
     DO j=1,nbin
        WRITE(13,*) real(j-1)*width+xmin, prob(j)/dfloat(N) !printing x vs P(x) vaules
     END DO
  CLOSE(12)
  CLOSE(13)
  DEALLOCATE(prob)
  PRINT*, "The Distribution of given data has stored in file named HISTOGRAM"
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










