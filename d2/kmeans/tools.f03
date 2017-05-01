MODULE Tools


INTEGER, PARAMETER :: nk = 20
INTEGER, PARAMETER :: times = 100

type obs
  real :: x
  real :: y
  integer :: cluster
end type obs

type center
  real :: x
  real :: y
  integer :: cluster
  integer :: num_obs
end type center


CONTAINS

  subroutine kmeans_pp(myobs, randcenters)
    type(obs), dimension(5000) :: myobs
    type(center) :: randcenters(:)
    real, dimension(5000,17) :: dist2
    real, dimension(5000,17) :: prob
    integer :: k, r, ri, counter
    real :: rp
    logical :: res

    dist2 = 1000000000

! ---------------- Initialize Center 1 -------------

    r = MOD(IRAND(), 5000)
    randcenters(1) % x = myobs(r) % x
    randcenters(1) % y = myobs(r) % y
    randcenters(1) % cluster = 1
    randcenters(1) % num_obs = 0

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
          randcenters(k + 1) % x = myobs(ri) % x
          randcenters(k + 1) % y = myobs(ri) % y
          randcenters(k + 1) % cluster = k + 1
          randcenters(k + 1) % num_obs = 0
          res = .FALSE.
        end if
      end do
      !print*, "The", k + 1, "center is assign after", counter, " iterations!"
    end do
  end subroutine kmeans_pp


subroutine rand_center(randobs, randcenters)
  type(obs) :: randobs(:)
  type(center) :: randcenters(:)
  integer :: r, k

  do k = 1, nk
    r = MOD(IRAND(), 5000)
    randcenters(k) % x = randobs(r) % x
    randcenters(k) % y = randobs(r) % y
    randcenters(k) % cluster = k
    randcenters(k) % num_obs = 0
  end do
end subroutine rand_center


subroutine assign_a_cluster(myobs, centers)
  type(obs) :: myobs
  type(center) :: centers(:)
  real :: dist

  dist = 942327.0 * 942327.0 + 947322.0 * 947322.0

  do i = 1, nk
    dist_new = (myobs % x - centers(i) % x)**2 + &
               (myobs % y - centers(i) % y)**2

    if (dist_new < dist) then
     dist = dist_new
     myobs % cluster = i
    end if
  end do

end subroutine assign_a_cluster

subroutine find_new_center(myobs, centers)
  type(obs) :: myobs(:)
  type(center) :: centers(:)
  integer :: k, nobs
  real :: x, y

  do k = 1, nk
    x = 0
    y = 0
    nobs = 0
    do i = 1, 5000
      if (myobs(i) % cluster == k) then
        x = x + myobs(i) % x
        y = y + myobs(i) % y
        nobs = nobs + 1
      end if
    end do
    centers(k) % num_obs = nobs
    centers(k) % x = x / nobs
    centers(k) % y = y / nobs
  end do
end subroutine find_new_center

real function comp_obj_func(myobs, centers) result(objf)
  type(obs) :: myobs(:)
  type(center) :: centers(:)
  !real :: obj(:)

  objf = 0
  do k = 1, nk
    do i = 1, 5000
      if (myobs(i) % cluster == k) then
        objf = objf + SQRT((myobs(i) % x - centers(k) % x)**2 + &
              (myobs(i) % y - centers(k) % y)**2)
        end if
      end do
    end do

end function comp_obj_func

END MODULE Tools
