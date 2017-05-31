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
  integer*8           ::i,j,N,bin,nbin
  character (len=125) :: input_filename,output_filename
  
  print *, 'enter: N ',' xmin',' xmax ', ' width', ' input_filename','  output_filename'
  read(*,*)N,xmin,xmax,width, input_filename, output_filename
  print *, 'N=',N, ' xmin=',xmin, ' xmax =', xmax, ' width=',width, ' input_filename = ', trim(input_filename),' output_filename = ', trim(ouput_filename)
 
  open(12,file=trim(input_filename),STATUS="OLD")
  open(13,file=trim(output_filename),STATUS="NEW")

  nbin=nint(((xmax-xmin)/width)) + 1
  print *, 'nbin =',nbin
  allocate(prob(nbin+1))
  prob = 0.0

     do i=1,N
        read(12,*)x
        bin =int((x-xmin)/width)+1
           if(bin.le.nbin+1)then
             prob(bin) = prob(bin) +1.00
           end if
     end do 

     do j=1,nbin
        write(13,*) real(j-1)*width+xmin, prob(j)/dfloat(N) !printing x vs P(x) vaules
     end do

  close(12)
  close(13)
  deallocate(prob)

!  print*, "The Distribution of given data has stored in file"
!  print*,
!  print*, "Plot 'x vs P(x')"

END PROGRAM histogram
