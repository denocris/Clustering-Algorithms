MODULE Tools

!integer :: nk

!nk = 15

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

  do k = 1, 15
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

  do i = 1, 15
    dist_new = (myobs % x - centers(i) % x)**2 + &
               (myobs % y - centers(i) % y)**2

    if (dist_new < dist) then
     dist = dist_new
     myobs % cluster = i
    end if
  end do

end subroutine assign_a_cluster


! integer function obs_in_cluster(myobs, cen, k) result(ret)
!   integer :: k
!   type(obs) :: myobs
!   type(center) :: cen
!   ret = cen % num_obs
!   if (myobs % cluster == k) then
!     ret = ret + 1
!   end if
! end function obs_in_cluster



subroutine find_new_center(myobs, centers)
  type(obs) :: myobs(:)
  type(center) :: centers(:)
  integer :: k, nobs
  real :: x, y

  do k = 1, 15
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







! type(obs) function new_center(x, y) result(new_c)
!   type (obs), intent(out) :: new_c
!
! end function new_center

END MODULE Tools


! type(center) function rand_center() result(ret)
!   ret % x = rand() * (942327 - 32710) + 32710
!   ret % y = rand() * (947322 - 70003) + 70003
!   ret % cluster = 1
!   ret % num_obs = 0
! end function rand_center
