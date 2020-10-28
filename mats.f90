module mats
  implicit none

  public

  contains

  function identity(n) result(I)
    integer, intent(in) :: n
    real :: I(n, n)
    
    integer :: j, k
    do j = 1, n
      do k = 1, n
        if (j == k) then
          I(j, k) = 1
        else
          I(j, k) = 0
        end if
      end do
    end do
  end function identity

  subroutine print_mat(M)
    real, intent(in) :: M(:, :)

    integer :: i
    do i = 1, size(M, 2)
      print *, M(:, i)
    end do
  end subroutine print_mat
end module mats
