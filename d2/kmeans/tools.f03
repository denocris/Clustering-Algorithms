MODULE Tools


INTEGER, PARAMETER :: nk = 15
INTEGER, PARAMETER :: times = 5

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
