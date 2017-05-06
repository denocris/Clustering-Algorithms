PROGRAM Dimensional_Reduction
  IMPLICIT NONE

  INTEGER :: i, j, l, m, k
  INTEGER :: len_dat, reduced_dim, info
  REAL ::  dat(351,34), mu(34), diff(34)
  DOUBLE PRECISION :: covmat(34,34), eig(34), work(34*(3+34/2)),  y(351,24)

  reduced_dim = 24

  len_dat = 351

  y = 0.0
  diff = 0.0
  covmat = 0.0

  do i = 1,351
    READ(5,*) (dat(i,j), j = 1, 34)
  end do

  do j = 1, 34
    mu(j) = 0
    do i = 1, len_dat
      mu(j) = mu(j) + dat(i,j)
    end do
    !mu(j) = 1./len_dat * mu(j)
  end do

  mu = mu/351

  do i = 1, len_dat
    diff(j)= 0
    do j = 1, 34
      diff(j) = dat(i,j) - mu(j)
    end do

    do l = 1, 34
      do m = l, 34
        covmat(l,m) = covmat(l,m) + diff(l) * diff(m)
        covmat(m,l) = covmat(l,m)
      end do
    end do
  end do

  covmat = covmat/351

  print*, (covmat(1,j), j = 1, 34)


  call dsyev('V','U' ,34 ,covmat ,34 ,eig ,work ,34*(3+34/2), info)

  !print*, (eig(i), i=1,34)


  do i = 1, len_dat
    do j = 1, reduced_dim
        do k = 1, 34
          y(i,j) = y(i,j) + dat(i,k) * covmat(35 - j,k)
        end do
    end do
  end do

  print*, (y(1,j), j=1,24)


END PROGRAM Dimensional_Reduction
