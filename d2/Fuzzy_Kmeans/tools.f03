MODULE Tools


INTEGER, PARAMETER :: nk = 2
INTEGER, PARAMETER :: times = 100

type obs
  real :: x
  real :: y
  real, dimension(nk) :: cluster_array
  integer :: nearest_cluster(1)
end type obs

type center
  real :: x
  real :: y
  integer :: cluster
  !integer :: num_obs
end type center


CONTAINS

  subroutine kmeans_pp(myobs, randcenters)
    type(obs), dimension(5000) :: myobs
    type(center) :: randcenters(:)
    real, dimension(5000,nk) :: dist2
    real, dimension(5000,nk) :: prob
    integer :: k, r, ri, counter
    real :: rp
    logical :: res

    dist2 = 1000000000

! ---------------- Initialize Center 1 -------------

    r = MOD(IRAND(), 5000)
    randcenters(1) % x = myobs(r) % x + 1
    randcenters(1) % y = myobs(r) % y + 1
    randcenters(1) % cluster = 1
    !randcenters(1) % num_obs = 0

! --------------------------------------------------

    do k = 1, nk - 1

      do i = 1, 5000
        dist2(i, k) = ABS(myobs(i) % x - randcenters(k) % x)**1 + &
                      ABS(myobs(i) % y - randcenters(k) % y)**1
      end do

      tot_dist2 = SUM(dist2(:, k))

      do i = 1, 5000
        if (k == 1) then
          prob(i, k) = dist2(i, k)/tot_dist2
        else
          prob(i, k) = MINVAL(dist2(i, : k )/tot_dist2)
        end if
      end do
      !print*, "Max prob for", k, "is:", MAXVAL(prob(:, k)*100)

      res = .TRUE.
      counter = 0
      do while(res .eqv. .TRUE.)
        rp = RAND()
        ri = MOD(IRAND(), 5000)
        counter = counter + 1
        if (prob(ri, k)*100 > rp) then
          randcenters(k + 1) % x = myobs(ri) % x + 1
          randcenters(k + 1) % y = myobs(ri) % y + 1
          randcenters(k + 1) % cluster = k + 1
          !randcenters(k + 1) % num_obs = 0
          res = .FALSE.
        end if
      end do
      !print*, "The", k + 1, "center is assign after", counter, " iterations!"
    end do
  end subroutine kmeans_pp


! subroutine rand_center(randobs, randcenters)
!   type(obs) :: randobs(:)
!   type(center) :: randcenters(:)
!   integer :: r, k
!
!   do k = 1, nk
!     r = MOD(IRAND(), 5000)
!     randcenters(k) % x = randobs(r) % x
!     randcenters(k) % y = randobs(r) % y
!     randcenters(k) % cluster = k
!     !randcenters(k) % num_obs = 0
!   end do
! end subroutine rand_center


subroutine fuzzy_assignation_of_cluster(myobs, centers)
  type(obs) :: myobs
  type(center) :: centers(:)
  real :: inv_sq_sum
  real, dimension(nk) :: sq_dist_array
  real, dimension(nk) :: u_factor
  integer :: k

  u_factor = 0
  inv_sq_sum = 0
  do k = 1, nk
    !sq_dist_array(k) = 0
    sq_dist_array(k) = (myobs % x - centers(k) % x)**2 + &
               (myobs % y - centers(k) % y)**2
    inv_sq_sum = inv_sq_sum + 1.0 / sq_dist_array(k)
  end do

  do k = 1, nk
    !u_factor(k) = 0
    u_factor(k) = 1.0 / (sq_dist_array(k) * inv_sq_sum)
  end do

  !print*, "--- check = 1", SUM(u_factor(:))

  do k = 1, nk
  myobs % cluster_array(k) = 0.0
  myobs % cluster_array(k) = u_factor(k)
  end do
  myobs % nearest_cluster = MAXLOC(myobs % cluster_array)

end subroutine fuzzy_assignation_of_cluster

subroutine find_new_center(myobs, centers)
  type(obs) :: myobs(:)
  type(center) :: centers(:)
  integer :: k
  real :: xx, yy, sum_uu



  do k = 1, nk
    sum_uu = 0.0
    xx = 0.0
    yy = 0.0
    centers(k) % x = 0.0
    centers(k) % y = 0.0
    do i=1,5000
      xx = xx + (myobs(i) % cluster_array(k))**2 * myobs(i) % x
      yy = yy + (myobs(i) % cluster_array(k))**2 * myobs(i) % y
      sum_uu = sum_uu +  (myobs(i) % cluster_array(k))**2
    end do

    centers(k) % x = xx / sum_uu
    centers(k) % y = yy / sum_uu
  end do
end subroutine find_new_center

real function fuzzy_comp_obj_func(myobs, centers) result(objf)
  type(obs) :: myobs(:)
  type(center) :: centers(:)
  !real :: obj(:)

  objf = 0
  do i = 1, 5000
    do k = 1, nk
      objf = objf + ((myobs(i) % cluster_array(k))**2) * SQRT((myobs(i) % x - centers(k) % x)**2 + &
             (myobs(i) % y - centers(k) % y)**2)
    end do
  end do

end function fuzzy_comp_obj_func

END MODULE Tools
