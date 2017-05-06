MODULE Tools


INTEGER, PARAMETER :: nk = 2
INTEGER, PARAMETER :: times = 50

type obs
  real :: x
  real :: y
  integer :: ground_truth
  integer :: cluster
end type obs

type center
  real :: x
  real :: y
  integer :: cluster
  integer :: num_obs
end type center


CONTAINS


! STANDARD K-Means
  subroutine rand_center(randobs, randcenters)
    type(obs) :: randobs(:)
    type(center) :: randcenters(:)
    integer :: r, k

    do k = 1, nk
      r = MOD(IRAND(), 788)
      randcenters(k) % x = randobs(r) % x
      randcenters(k) % y = randobs(r) % y
      randcenters(k) % cluster = k
      randcenters(k) % num_obs = 0
    end do
  end subroutine rand_center


!K-Meanspp
  subroutine kmeans_pp(myobs, randcenters)
    type(obs), dimension(788) :: myobs
    type(center) :: randcenters(:)
    real, dimension(788,nk) :: dist2
    real, dimension(788,nk) :: prob
    integer :: k, r, ri, counter
    real :: rp
    logical :: res

    dist2 = 1000000000

! ---------------- Initialize Center 1 -------------

    r = MOD(IRAND(), 788)
    randcenters(1) % x = myobs(r) % x
    randcenters(1) % y = myobs(r) % y
    randcenters(1) % cluster = 1
    randcenters(1) % num_obs = 0

! --------------------------------------------------

    do k = 1, nk - 1

      do i = 1, 788
        dist2(i, k) = ABS(myobs(i) % x - randcenters(k) % x)**1 + &
                      ABS(myobs(i) % y - randcenters(k) % y)**1
      end do

      tot_dist2 = SUM(dist2(:, k))

      do i = 1, 788
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
        ri = MOD(IRAND(), 788)
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




! -----------------------------------------------

subroutine assign_a_cluster(myobs, centers)
  type(obs) :: myobs
  type(center) :: centers(:)
  real :: dist

  dist = (942327.0 * 942327.0 + 947322.0 * 947322.0)

  do i = 1, nk
    dist_new = (myobs % x - centers(i) % x)**2 + &
               (myobs % y - centers(i) % y)**2

    if (dist_new < dist) then
     dist = dist_new
     myobs % cluster = i
    end if
  end do

end subroutine assign_a_cluster


! -----------------------------------------------

subroutine find_new_center(myobs, centers)
  type(obs) :: myobs(:)
  type(center) :: centers(:)
  integer :: k, nobs
  real :: x, y

  !print*, "iiii", centers(3)

  do k = 1, nk
    x = 0
    y = 0
    nobs = 0
    do i = 1, 788
      if (myobs(i) % cluster == k) then
        x = x + myobs(i) % x
        y = y + myobs(i) % y
        nobs = nobs + 1
      end if
    end do
    centers(k) % num_obs = nobs
    centers(k) % x = x / REAL(nobs)
    centers(k) % y = y / REAL(nobs)
  end do

  !print*, "fff", centers(3)
end subroutine find_new_center

! -----------------------------------------------

real function comp_obj_func(myobs, centers) result(objf)
  type(obs) :: myobs(:)
  type(center) :: centers(:)
  !real :: obj(:)

  objf = 0
  do k = 1, nk
    do i = 1, 788
      if (myobs(i) % cluster == k) then
        objf = objf + SQRT((myobs(i) % x - centers(k) % x)**2 + &
              (myobs(i) % y - centers(k) % y)**2)
        end if
      end do
    end do

end function comp_obj_func

! -----------------------------------------------

real function f_ratio(myobs, centers) result(fratio)
  type(obs) :: myobs(:)
  type(center) :: centers(:)
  real :: mass_center_x, mass_center_y, SSW, SSB


  SSW = 0.0
  DO i = 1, nk
    SSW = SSW + &
    SUM( (myobs(:) % x - centers(i) % x)**2 +  (myobs(:) % y - centers(i) % y)**2, 1, myobs(:) % cluster == i)
  END DO

  mass_center_x = SUM(myobs(:) % x, 1)/SIZE(myobs(:) % x)
  mass_center_y = SUM(myobs(:) % y, 1)/SIZE(myobs(:) % y)

  SSB = SUM( centers(:) % num_obs * (centers(:) % x - mass_center_x)**2 ) &
  + SUM( centers(:) % num_obs * (centers(:) % y - mass_center_y)**2 )

  fratio = nk * SSW / SSB

end function f_ratio

real function NormMutualInfo(final_color,color) result(NMI)
  INTEGER,INTENT(IN) :: final_color(:),color(:)

  REAL,ALLOCATABLE :: p_color(:), p_final_color(:), p_joint(:,:)
  REAL  MI, H_color, H_final_color
  INTEGER :: n_final_color, n_color

  INTEGER ::N1, i, j

  N1 =SIZE(color)
  n_final_color=MAXVAL(final_color)-MINVAL(final_color)+1
  n_color=MAXVAL(color)-MINVAL(color)+1


       ALLOCATE( p_final_color(n_final_color))
       ALLOCATE( p_color(n_color) )
       ALLOCATE( p_joint(n_final_color,n_color))

  p_final_color=0
  p_color=0
  p_joint=0


 !      ! Calculate the join probability
    DO i = 1, n_final_color
      DO j = 1, n_color
        p_joint(i,j) = COUNT(final_color(:) == i .and. color(:)== j)/REAL(N1)
      END DO
    END DO
   !  WRITE(*,*) "N1" ,N1
   !  WRITE(*,*) "p_joint", p_joint

    DO i = 1, n_final_color
      p_final_color(i) = COUNT(final_color(:) == i)/REAL(N1)
    END DO

   ! WRITE(*,*) "p_final_color", p_final_color

    DO j = 1,  n_color
      p_color(j) = COUNT(color(:) == j)/REAL(N1)
    END DO
   ! WRITE(*,*) "p_color", p_color

    MI=0
    DO i = 1, n_final_color
      DO j = 1, n_color
        IF( p_joint(i,j) > 1e-16 ) MI = MI + p_joint(i,j)*log(p_joint(i,j)/(p_final_color(i)*p_color(j)))
      END DO
    END DO

    H_final_color = -SUM(p_final_color(:)*log(p_final_color(:)), 1, p_final_color(:) > 1e-16 )

    H_color = -SUM(p_color(:)*log(p_color(:)), 1, p_color(:) > 1e-16 )

    NMI = 2*MI/(H_final_color + H_color)

    OPEN(UNIT=30,FILE="nmi.txt",STATUS="REPLACE",ACTION="WRITE")
      WRITE(30,*) "# mutual information: ", MI
      WRITE(30,*) "# entropy of clusterization: ", H_final_color
      WRITE(30,*) "# entropy of ground truth: ", H_color
      WRITE(30,*) "# normalized mutual information: ", NMI
    CLOSE(UNIT=30)


    DEALLOCATE( p_final_color)
       DEALLOCATE( p_color )
       DEALLOCATE( p_joint)


end function NormMutualInfo


END MODULE Tools
