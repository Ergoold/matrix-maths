program invert
  use gauss
  use mats
  implicit none
  real, allocatable :: mat(:, :), M(:, :)
  integer :: n

  print *, 'Enter matrix side length:'
  read (*, *) n
  allocate(mat(n, n))

  print *, 'Enter matrix values:'
  read (*, *) mat
  allocate(M(2 * n, n))
  M(:n, :) = mat
  M(n + 1:, :) = identity(n)

  call reduce(M)
  print *, 'Inverted matrix:'
  call print_mat(M(n + 1:, :))
end program invert
