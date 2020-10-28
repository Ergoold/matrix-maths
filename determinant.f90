program determinant
  use gauss
  use mats
  implicit none

  real, allocatable :: mat(:, :)
  integer :: n
  integer :: det_sign

  print *, 'Enter matrix side length:'
  read (*, *) n
  allocate(mat(n, n))

  print *, 'Enter matrix values:'
  read (*, *) mat

  call echelon(mat, det_sign)
  print *, 'Determinant:', det_sign
end program determinant
