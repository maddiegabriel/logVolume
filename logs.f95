! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
! file:   logs.f95
! author: Maddie Gabriel
! course: CIS*3190 assignment #1
! date:   january 20th 2019
! goal:   allows user to calculate the volume of a given log in both
!         cubic metres and board feet using jclark() subroutine.
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

program main
  print*, ''
  print*, '------------------------------------'
  print*, '   WELCOME TO MY LOG CALCULATOR!'
  print*, '   Built with love for CIS*3190'
  print*, '   By: Maddie Gabriel (0927580)'
  print*, '------------------------------------'
  print*, ''
  call getLOGdata()
end program main

! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
! func:  getLOGdata()
! goal:  get user input of data needed to calculate log volume
! input: none
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
subroutine getLOGdata ()
  real :: DIBsmall = 0.0, DIBlarge = 0.0, totLength = 0.0, volumeCubic = 0.0
  integer :: KERF = 0
  character (len=2) :: again = 'Y'

  do while (again == 'Y' .or. again == 'y')
    ! GET USER INPUT
    print*, ''
    print*, '------ NEW LOG VOLUME ESTIMATION ------'
    print*, 'Enter Log Diameter at SMALL end (inches): '
    read (*,*) DIBsmall
    print*, 'Enter Log Diameter at LARGE end (inches): '
    read (*,*) DIBlarge
    print*, 'Enter Total log length (feet): '
    read (*,*) totLength
    print*, 'Enter KERF: '
    read (*,*) KERF
    
    ! CALCULATE VOLUMES
    call calcLOGjclark(DIBsmall, DIBlarge, totLength, KERF, volumeBF)
    call calcLOGvolume(DIBsmall, DIBlarge, totLength, volumeCubic)
      
    ! PRINT RESULT/ERROR
    print*, ''
    print*, '---------------- RESULT ----------------'
    
    if (volumeBF == 0) then
      print*, 'ERROR: Sorry! Unable to calculate the log volume.'
    else
      write(*,'("Volume = ", f12.2, " board feet")') volumeBF
      write(*,'("Volume = ", f12.2, " cubic metres")') volumeCubic
    end if
    
    print*, ''
    print*, 'Want to calculate again? (Y/N)'
    read (*,'(A)') again
  end do

  ! EXIT MESSAGE
  print*, ''
  print*, '-------------- THANKS AND GOODBYE! --------------'
  print*, ''
  return
end subroutine

! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
! func:   calcLOGjclark()
! goal:   calculate board foot volume of sawlogs by the intl. rule
! input:
!   DIBsmall = log's scaling diameter (inches)
!   DIBlarge = DIB at log's large end (inches) (0.0 if 1/2 inch taper)
!   totLength = total log length (feet)
!   KERF > 0 if KERF assumption is 1/4 inch
!   KERF <=0 if KERF assumption is 1/8 inch
!   volumeBF = log volume returned to calling program (board feet)
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
subroutine calcLOGjclark (DIBsmall, DIBlarge, totLength, KERF, volumeBF)
  real, intent(in) :: DIBsmall, DIBlarge, totLength
  integer, intent(in) :: KERF
  real, intent(out) :: volumeBF
  
  volumeBF = 0.0
    
  ! if total log length is less that 4 feet, no board foot volume will be computed
  if ((totLength - 4.0) < 0) then
    return
  end if

  ! if the log's large end diameter is provided to jclark, a taper rate will be computed
  ! else if DIBlarge = 0, the standard assumption of 1/2 inch per 4 feet of log length will be used
  if (DIBlarge > 0) then
    taper = 4.0 * (DIBlarge - DIBsmall) / totLength
  else
    taper = 0.5
  end if

  ! this loop finds out how many full 4 foot segments the log contains
  do i = 1,20
    if ( (totLength - float(4*i)) < 0) then
      exit
    end if
  end do
  
  length = i - 1
  segLength = float(4*length)
  
  ! this statement moves the scaling diameter down to the end of the 4 foot segments
  ! and increases it according to taper
  diameter = DIBsmall + (taper/4.0) * (totLength - segLength)

  ! this loop finds out how many full feet of length are in the segment less than 4 feet long
  do i = 1,4
    fullFeet = float(i)
    if ((segLength - totLength + fullFeet) > 0) then
      exit
    end if
  end do

  ! the next 3 statements calculate log volume in the 1/2/3 foot segment at the small end of the log
  feetNum = fullFeet - 1.0
  diameterExtra = DIBsmall + (taper/4.0) * (totLength - segLength - feetNum)
  volumeAdd = 0.055 * feetNum * diameterExtra * diameterExtra - 0.1775 * feetNum * diameterExtra

  ! this loop calculates volume in the portion of the log containing whole 4 foot segments
  do i = 1,length
    diameterFull = diameter + taper * float(i-1)
    volumeBF = volumeBF + 0.22 * diameterFull * diameterFull - 0.71 * diameterFull
  end do

  volumeBF = volumeBF + volumeAdd
  
  ! if KERF is greater than zero, international 1/8 inch volume (as computed above) will be converted to international 1/4 inch volume
  if (KERF > 0) then
    volumeBF = 0.905 * volumeBF
  end if

  return
end subroutine

! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
! func: calcLOGvolume()
! goal: calculate sawlog volume in cubic metres
! input:
!   DIBsmall = log's scaling diameter (inches)
!   DIBlarge = DIB at log's large end (inches) (0.0 if 1/2 inch taper)
!   totLength = total log length (feet)
!   volumeCubic = log volume returned to calling program (cubic metres)
! *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
subroutine calcLOGvolume(DIBsmall, DIBlarge, totLength, volumeCubic)
  real, intent(in) :: DIBsmall, totLength
  real, intent(inout) :: DIBlarge
  real, intent(out) :: volumeCubic
  real :: smallDIBradius=0.0, largeDIBradius=0.0, metresLength=0.0
  real :: areaSmall=0.0, areaLarge=0.0, div=0.0
  real :: PI = 3.14159265359
  
  div = totLength / 4.0
  
  ! if DIBlarge = 0, the standard assumption of 1/2 inch per 4 feet of log length will be used
  ! this assumption is converted from imperial to metric
  if (DIBlarge <= 0) then
    DIBlarge = DIBsmall + (div * 0.5)
  end if
  
  ! convert imperial values (inches/feet) to metric (metres)
  smallDIBradius = (DIBsmall / 39.37) / 2.0
  largeDIBradius = (DIBlarge / 39.37) / 2.0
  metresLength = totLength / 3.2808
  
  ! the next 2 statements calculate the area at small/large log end
  areaSmall = PI * (smallDIBradius * smallDIBradius)
  areaLarge = PI * (largeDIBradius * largeDIBradius)
  
  volumeCubic = ( ( areaSmall + areaLarge ) / 2.0 ) * metresLength
  return
end subroutine
