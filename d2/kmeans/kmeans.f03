PROGRAM Kmeans
  USE Tools
  IMPLICIT NONE

  INTEGER :: i, k !, counter
  REAL :: x, y

  type(center) :: center_of_cl(15)
  type(obs) ::  dat(5000)

  do i = 1, 5000
    READ(5,*) x,y
    dat(i)%x=x
    dat(i)%y=y
    dat(i)%cluster=-1
  end do


  OPEN(UNIT=101, FILE="rand_centers.txt")
  do k = 1, 15
    center_of_cl(k) = rand_center()
    center_of_cl(k) % cluster = k
    !print*, center(i) % x, center(i) % y
    WRITE(101,*) center_of_cl(k)
  end do

  OPEN(UNIT=102, FILE="obs_with_cluster.txt")
  do i = 1, 5000
   call  assign_a_cluster(dat(i), center_of_cl)
   WRITE(102,*) dat(i)
  end do

  call find_new_center(dat,center_of_cl)

  OPEN(UNIT=103, FILE="new_centers.txt")
  do k = 1, 15
    WRITE(103,*) center_of_cl(k)
  end do


END PROGRAM Kmeans


! do k = 1, 15
!   counter = 0
!   do i = 1, 5000
!     if (dat(i) % cluster == k) then
!       counter = counter + 1
!     end if
!   end do
!   center_of_cl(k) % num_obs = counter
! end do
