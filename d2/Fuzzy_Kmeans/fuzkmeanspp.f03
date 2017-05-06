PROGRAM FuzzyKmeanspp
  USE Tools
  IMPLICIT NONE

  INTEGER :: i, k, time, num_conv
  REAL :: x, y, old_c, new_c, convergence
  !integer :: STATUS = 0


  type(center) :: center_of_cl(nk)
  type(obs) ::  dat(5000)
  integer :: nconv(times)
  integer :: procid(times)
  real :: obj(times), obj_min, obj_mean

  do i = 1, 5000
    READ(5,*) x, y
    dat(i) % x = x
    dat(i) % y = y
    dat(i) % cluster_array =  0.0
    !dat(i) % nearest_cluster = -1
  end do


!do num_clusters = 2, 20
!------------------  ---------------------
do time = 1, times

  call SRAND(getpid() + time)
  call kmeans_pp(dat, center_of_cl)
  !call rand_center(dat, center_of_cl)


  !------------------ Print on File ---------------------

if (time == times) then
  OPEN(UNIT = 101, FILE = "data/rand_centers.txt")
    do k = 1, nk
      WRITE(101,*) center_of_cl(k)
    end do

  OPEN(UNIT = 102, FILE = "data/initial_observations.txt")
    do i = 1, 5000
      WRITE(102,*) dat(i)
    end do
end if

!------------------ Fuzzy Check ---------------------

! do i = 1, nk
! print*, "------", dat(10) % cluster_array(i)
! end do
! call  fuzzy_assignation_of_cluster(dat(10), center_of_cl)
! do i = 1, nk
! print*, "......", dat(10) % cluster_array(i)
! end do
!
! CALL EXIT(STATUS)

!------------------ Convergence ---------------------

  convergence = 1
  num_conv = 0

  do while(convergence .ge. 0.001)
    do i = 1, 5000
      call  fuzzy_assignation_of_cluster(dat(i), center_of_cl)
    end do


    old_c = center_of_cl(1) % x
    call find_new_center(dat,center_of_cl)
    new_c = center_of_cl(1) % x


    convergence = ABS((new_c - old_c)/old_c)


    num_conv = num_conv + 1

  end do






!------------------- Some Data to Store -----------------------

  procid(time) = getpid() + time
  nconv(time) = num_conv
  obj(time) = fuzzy_comp_obj_func(dat, center_of_cl)


  if (nk == 15) then
    OPEN(UNIT=103, FILE="data/info_nk_15.txt")
    do i = 1, times
      WRITE(103,*) i, procid(i), nconv(i), obj(i)
    end do

  if (time == times/2) then
    OPEN(UNIT=104, FILE="data/final_observations.txt")
    do i = 1, 5000
      WRITE(104,*) dat(i)
    end do

    OPEN(UNIT=105, FILE="data/new_centers.txt")
    do k = 1, nk
      WRITE(105,*) center_of_cl(k)
    end do
  end if
end if


end do

  obj_min = MINVAL(obj)
  obj_mean = SUM(obj)/times

  OPEN(UNIT=106,FILE="data/data_fuzzykmeans.txt",action='write',position='append')
  !do i = 2, 20
  WRITE(106,*) nk, obj_min, obj_mean
  !end do


END PROGRAM FuzzyKmeanspp
