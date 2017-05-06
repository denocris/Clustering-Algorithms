MODULE Tools


INTEGER, PARAMETER :: dimdat = 788 !5000 ! number of points in the dataset: Aggregation.txt
REAL, PARAMETER :: dc = 2.5 !57500

type obs
  real :: x
  real :: y
  integer :: label
  real :: density
  real :: delta
end type obs

! type center
!   real :: x
!   real :: y
!   integer :: cluster
!   integer :: num_obs
! end type center


CONTAINS

! -------------- Test Function --------------

  subroutine density(point, other_point, dist_c)
    type(obs) :: point
    type(obs) :: other_point(:)
    real :: dist_c, dist_tmp
    integer :: count

    count = 0
    do j = 1, dimdat
      dist_tmp = (point % x - other_point(j) % x)**2 + &
                (point % y - other_point(j) % y)**2
      if (dist_tmp > 0.00001 .AND. dist_tmp < dist_c) then
          count = count + 1
      end if
    end do

    point % density = count

  end subroutine density

  subroutine density_exp(point, other_point, dist_c)
    type(obs) :: point
    type(obs) :: other_point(:)
    real :: dist_c, dist_tmp
    real :: sum

    sum = 0
    do j = 1, dimdat
      dist_tmp = SQRT((point % x - other_point(j) % x)**2 + &
                (point % y - other_point(j) % y)**2)
      if (dist_tmp > 0.00001) then
          sum = sum + EXP( -1.0 * (dist_tmp / dist_c)**2 )
      end if
    end do

    point % density = sum

  end subroutine density_exp



  subroutine delta(point, other_point)
    type(obs) :: point
    type(obs) :: other_point(:)
    real :: dist_tmp, min_dist, max_dist

    max_dist = 0.1

    do j = 1, dimdat
      dist_tmp = SQRT((point % x - other_point(j) % x)**2 + &
                (point % y - other_point(j) % y)**2)
      if (dist_tmp > max_dist) then
        max_dist = dist_tmp
      end if
    end do

    min_dist = max_dist

    do j = 1, dimdat
      dist_tmp = SQRT((point % x - other_point(j) % x)**2 + &
                (point % y - other_point(j) % y)**2)
      if (other_point(j) % density > point % density .AND. dist_tmp > 0.0001 &
      .AND. dist_tmp < min_dist) then
            min_dist = dist_tmp
      end if
    end do

    point % delta = min_dist

  end subroutine delta

END MODULE Tools
