PROGRAM Kmeans
  USE Tools
  IMPLICIT NONE

  INTEGER :: i, k, time, num_conv
  REAL :: x, y, old_c, new_c, convergence


  type(center) :: center_of_cl(nk)
  type(obs) ::  dat(5000)
  integer :: nconv(times)
  integer :: procid(times)
  real :: obj(times)

  do i = 1, 5000
    READ(5,*) x, y
    dat(i) % x = x
    dat(i) % y = y
    dat(i) % cluster = - 1
  end do

!------------------  ---------------------

do time = 1, times

  call SRAND(getpid() + time)
  call rand_center(dat, center_of_cl)

  !------------------ Print on File ---------------------

  ! OPEN(UNIT = 101, FILE = "rand_centers.txt")
  !   do k = 1, nk
  !     WRITE(101,*) center_of_cl(k)
  !   end do
  !
  ! OPEN(UNIT = 102, FILE = "initial_observations.txt")
  !   do i = 1, 5000
  !     WRITE(102,*) dat(i)
  !   end do

!------------------ Convergence ---------------------

  convergence = 1
  num_conv = 0
  do while(convergence .ge. 0.001)
    do i = 1, 5000
      call  assign_a_cluster(dat(i), center_of_cl)
    end do

    old_c = MAXVAL(center_of_cl % x)
    call find_new_center(dat,center_of_cl)
    new_c = MAXVAL(center_of_cl % x)

    convergence = ABS(new_c - old_c)
    num_conv = num_conv + 1
  end do

  procid(time) = getpid() + time
  nconv(time) = num_conv
  obj(time) = comp_obj_func(dat, center_of_cl)
end do



  OPEN(UNIT=103, FILE="info.txt")
  do i = 1, times
    WRITE(103,*) procid(i), nconv(i), obj(i)
  end do

  OPEN(UNIT=104, FILE="final_observations.txt")
  do i = 1, 5000
    WRITE(104,*) dat(i)
  end do

  OPEN(UNIT=105, FILE="new_centers.txt")
  do k = 1, nk
    WRITE(105,*) center_of_cl(k)
  end do

END PROGRAM Kmeans
