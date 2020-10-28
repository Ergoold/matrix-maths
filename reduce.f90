program reduce
  use gauss
  use mats
  implicit none
  real, allocatable :: mat(:, :)
  integer :: n, m

  print *, 'Enter matrix dimensions (columns, rows):'
  read (*, *) n, m
  allocate(mat(n, m))

  print *, 'Enter matrix values:'
  read (*, *) mat

  print *, 'Reduced matrix:'
  call echelon(mat)
  call print_mat(mat)
end program reduce
