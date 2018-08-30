! FORTRAN90 program to calculate 2D distribution of given data
!
! Authour  : ANJI BABU KAPKAYALA
!            IIT KANPUR
!            anjibabu480@gmail.com
!
! INPUT    :  COLAVR
! OUTPUT   :  HISTOGRAM.dat
!
!
PROGRAM histogram2D
  implicit none
  real*8                ::y,x,xmin,xmax,width,u,t,d1,d2,ymin,ymax,ydif,xdif,xvalue,yvalue
  real*8, allocatable   ::prob(:,:),fes(:,:),prob2(:,:)
  real*8, PARAMETER     :: kt=0.593 !(kcal/mol)
  integer*8             ::k,i,j,N,bin,nbin,xbin,ybin,binx,biny  !N=md_steps
  character (len=125)   :: input_filename,output_filename
!-----Opening Files
  open(12,file="COLVAR",STATUS="UNKNOWN")
  open(13,file="HISTOGRAM.dat",STATUS="UNKNOWN")
!-----Get No. of steps
  call get_steps(12,N)
!-----Get xmin,xmax,xdif,ymin,ymax,ydif
  call get_xmin_xmax(12,xmin,xmax,xdif,ymin,ymax,ydif)
!----- Caluculate Xbin and Ybin
  xbin=nint(((xmax-xmin)/xdif)) + 1
  ybin=nint(((ymax-ymin)/ydif)) + 1
!------Writing Given Data 
  WRITE(*,15) N
  WRITE(*,16) xmin,xmax,xdif
  WRITE(*,17) ymin,ymax,ydif
  WRITE(*,18) xbin,ybin
  allocate(prob(xbin,ybin))
  allocate(prob2(xbin,ybin))
!  allocate(fes(xbin,ybin))
  prob = 0.0
     do i=1,N
        read(12,*)t,x,y
        binx =int((x-xmin)/xdif)+1
        biny =int((y-ymin)/ydif)+1
          if(binx < xbin+1 .AND. biny < ybin+1)  then
            prob(binx,biny) = prob(binx,biny) + 1.00
        end if
     end do 
   do j=1,xbin
      xvalue = real(j-1)*xdif+xmin
        do k=1,ybin     
           yvalue=real(k-1)*ydif+ymin
           prob2(j,k)=prob(j,k)/dfloat(N)
!           fes(j,k)=-kt*log(prob(j,k))
           write(13,19) xvalue,yvalue,prob2(j,k)!printing x :y: P(x,y)
!           write(*,*) xvalue,yvalue,prob2(j,k)!printing x :y: P(x,y)
        end do
           write(13,*) 
   end do
  close(12)
  close(13)
  deallocate(prob)
  deallocate(prob2)
  WRITE(*,20)
 15 FORMAT (//3X,"Total Steps = ",1X,I10)
 16 FORMAT (3X,"Xmin = ",1X,F10.4,3X,"XMax = ",1X,F10.4,3X,"Xwidth = ",1X,F10.4)
 17 FORMAT (3X,"Ymin = ",1X,F10.4,3X,"YMax = ",1X,F10.4,3X,"Ywidth = ",1X,F10.4)
 18 FORMAT (3X,"Xbin = ",1X,I10,3X,"Ybin = ",1X,I10//)
 19 FORMAT (2X,F15.5,3X,F15.5,3X,F15.5)
 20 FORMAT (3X,"The 2D distribution of given data has been stored in file 'HISTOHRAM.dat'"//)
END PROGRAM histogram2D
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
SUBROUTINE get_xmin_xmax(iunt,xmin,xmax,xdif,ymin,ymax,ydif)
 IMPLICIT NONE
 INTEGER,INTENT(IN)    :: iunt
 REAL*8,INTENT(INOUT)  :: xmin,xmax,xdif,ymin,ymax,ydif
! local variables
 INTEGER :: ios
 REAL*8  :: x,t,y,d1
 INTEGER, PARAMETER    :: bins=101
 REWIND(iunt)
 READ(iunt,*,IOSTAT=ios)t,x,y,d1
 if(ios /= 0)stop 'ERROR reading INPUT'
 xmin=x
 xmax=x
 ymin=y
 ymax=y
 DO
   READ(iunt,*,IOSTAT=ios)t,x,y,d1
   if(ios /= 0)EXIT 
   xmin=MIN(xmin,x)
   ymin=MIN(ymin,y)
   xmax=MAX(xmax,x)
   ymax=MAX(ymax,y)
 END DO
 xdif=(xmax-xmin)/DFLOAT(bins)
 ydif=(ymax-ymin)/DFLOAT(bins)
 REWIND(iunt)
END SUBROUTINE get_xmin_xmax
!============================================================================!
!                        ANJI BABU KAPAKAYALA                                !
!============================================================================!
