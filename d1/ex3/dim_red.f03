PROGRAM Dimensional_Reduction
  IMPLICIT NONE

  INTEGER :: len_dat, i, j
  REAL ::  dat(351,34)

  len_dat = 351

  do i=1,351
    READ(5,*) (dat(i,j), j = 1, 34)
  end do
  print*,(dat(1,j),j=1,34)

  !DEALLOCATE(dat)

END PROGRAM Dimensional_Reduction
