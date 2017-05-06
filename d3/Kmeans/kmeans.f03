PROGRAM Kmeans
  USE Tools
  IMPLICIT NONE

  INTEGER :: i, k, time, num_conv, ground_truth
  REAL :: x, y, convergence
  REAL :: old_c(nk), new_c(nk), diff(nk)


  type(center) :: center_of_cl(nk)
  type(obs) ::  dat(788)
  integer :: nconv(times)
  integer :: procid(times)
  real :: obj(times), fratio(times), NMI(times), obj_min, obj_mean, fratio_mean, NMI_mean

  do i = 1, 788
    READ(5,*) x, y, ground_truth
    dat(i) % x = x
    dat(i) % y = y
    dat(i) % ground_truth = ground_truth
    dat(i) % cluster = - 1
  end do


!do num_clusters = 2, 20
!------------------  ---------------------

do time = 1, times

  call SRAND(getpid() + time)
  call kmeans_pp(dat, center_of_cl)
  !call rand_center(dat, center_of_cl)


  !------------------ Print on File ---------------------

if (time == times/2) then
  OPEN(UNIT = 101, FILE = "data/rand_centers.txt")
    do k = 1, nk
      WRITE(101,*) center_of_cl(k)
    end do

  OPEN(UNIT = 102, FILE = "data/initial_observations.txt")
    do i = 1, 788
      WRITE(102,*) dat(i)
    end do
end if

!------------------ Convergence ---------------------

  convergence = 1
  num_conv = 0
  do while(convergence .ge. 0.0000001)
  !do j = 1,30
    do i = 1, 788
      call  assign_a_cluster(dat(i), center_of_cl)
    end do

    do k = 1 ,nk
    old_c(k) = center_of_cl(k) % x
    end do

    call find_new_center(dat, center_of_cl)

    do k = 1 ,nk
    new_c(k) = center_of_cl(k) % x
    end do
    !print*, old_c(5), new_c(5)

    do k = 1 ,nk
    diff(k) = ABS(new_c(k) - old_c(k))
    end do
    !print*, MAXVAL(diff(:))
    convergence = MINVAL(diff(:))
    !print*, "-------", diff(3)
    num_conv = num_conv + 1

    do i = 1, 788
      call  assign_a_cluster(dat(i), center_of_cl)
    end do
  end do

!------------------- Some Data to Store -----------------------

  procid(time) = getpid() + time
  nconv(time) = num_conv
  obj(time) = comp_obj_func(dat, center_of_cl)
  fratio(time) = f_ratio(dat, center_of_cl)
  NMI(time) = NormMutualInfo( dat(:) % cluster, dat(:) % ground_truth)


  if (nk == 7) then
    OPEN(UNIT=103, FILE="data/info_nk_7.txt")
    do i = 1, times
      WRITE(103,*) i, procid(i), nconv(i), obj(i)
    end do

  if (time == times/2) then
    OPEN(UNIT=104, FILE="data/final_observations.txt")
    do i = 1, 788
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
  fratio_mean = SUM(fratio)/times
  NMI_mean = SUM(NMI)/times

  OPEN(UNIT=106,FILE="data/data_kmeans.txt",action='write',position='append')
  !do i = 2, 20
  WRITE(106,*) nk, obj_min, obj_mean, fratio_mean, NMI_mean
  !end do


END PROGRAM Kmeans
