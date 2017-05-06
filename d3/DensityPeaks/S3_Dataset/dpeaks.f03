PROGRAM DensityPeaks
  USE Tools
  IMPLICIT NONE

  INTEGER :: i
  REAL :: x, y


  type(obs) :: dat(dimdat)


  do i = 1, dimdat
    READ(5,*) x, y, lbl
    dat(i) % x = x
    dat(i) % y = y
    !dat(i) % label = lbl
    dat(i) % density = - 1
    dat(i) % delta = - 1.0
  end do


  !------------------ Print Dataset ---------------------

  !  do i = 1, 10
  !    print*, dat(i)
  !  end do

!------------------ Compute Density ---------------------

  do i = 1, dimdat
    call density_exp(dat(i), dat, dc)
  end do

  do i = 1, dimdat
    call delta(dat(i), dat)
  end do

  !------------------ Print Dataset ---------------------

  !  do i = 1, 10
  !    print*, dat(i)
  !  end do

!----------------------- Printing Data -----------------

  OPEN(UNIT=104, FILE="data/decisiongraph_aggregation.txt", status="REPLACE")
  !OPEN(UNIT=104, FILE="data/decisiongraph_s3.txt", status="REPLACE")
  do i = 1, dimdat
    WRITE(104,*) dat(i)
  end do



END PROGRAM DensityPeaks
