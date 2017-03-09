PROGRAM Kmeans
  USE Tools
  IMPLICIT NONE

  INTEGER :: i, k, iter, niters
  !INTEGER, PARAMETER :: nk = 15
  REAL :: x, y


  type(center) :: center_of_cl(nk)
  type(obs) ::  dat(5000)

  niters = 100
  !nk = 15

  call SRAND(getpid())

  do i = 1, 5000
    READ(5,*) x, y
    dat(i) % x = x
    dat(i) % y = y
    dat(i) % cluster = - 1
  end do

  call rand_center(dat, center_of_cl)

  OPEN(UNIT = 101, FILE = "rand_centers.txt")
    do k = 1, nk
      WRITE(101,*) center_of_cl(k)
    end do

  OPEN(UNIT = 102, FILE = "initial_observations.txt")
    do i = 1, 5000
      WRITE(102,*) dat(i)
    end do

!------------------ Iterative ---------------------
  do iter = 1, niters

    do i = 1, 5000
      call  assign_a_cluster(dat(i), center_of_cl)
    end do

    call find_new_center(dat,center_of_cl)
  end do
!-------------------------------------------------

  OPEN(UNIT=103, FILE="final_observations.txt")
  do i = 1, 5000
    WRITE(103,*) dat(i)
  end do


  OPEN(UNIT=104, FILE="new_centers.txt")
  do k = 1, nk
    WRITE(104,*) center_of_cl(k)
  end do

END PROGRAM Kmeans
