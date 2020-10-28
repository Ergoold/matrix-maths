module gauss
  implicit none

  private
  public echelon, reduce

  contains

  ! only call this after calling echelon
  subroutine reduce(M)
    real, intent(inout) :: M(:, :)

    integer :: i
    real :: mii

    do i = 1, max(size(M, 1), size(M, 2))
      mii = M(i, i)
      if (mii /= 1 .and. mii /= 0) then
        call mul_row(M, i, 1 / mii)
      end if
    end do
  end subroutine reduce

  subroutine echelon(M)
    real, intent(inout) :: M(:, :)
    call echelon_sub(M, 1, 1)
  end subroutine echelon

  recursive subroutine echelon_sub(A, n, m)
    real, intent(inout) :: A(:, :)
    integer, intent(in) :: n, m

    integer :: i
    integer :: non_zero_row

    if (n <= size(A, 1) .and. m <= size(A, 2)) then
      non_zero_row = 0
      do i = size(A, 2), m, -1
        if (A(n, i) /= 0) then
          non_zero_row = i
        end if
      end do
      if (non_zero_row /= 0) then
        if (non_zero_row /= m) then
          call swap_rows(A, non_zero_row, m)
        end if
        do i = 1, size(A, 2)
          if (i /= m .and. A(n, i) /= 0) then
            call add_row(A, i, m, -A(n, i) / A(n, m))
          end if
        end do
      end if
      call echelon_sub(A, n + 1, m + 1)
    end if
  end subroutine echelon_sub

  subroutine swap_rows(M, row1, row2)
    real, intent(inout) :: M(:, :)
    integer, intent(in) :: row1, row2

    real, dimension(size(M, 1)) :: tmp
    call copy_vec(tmp, M(:, row1))
    call copy_vec(M(:, row1), M(:, row2))
    call copy_vec(M(:, row2), tmp)
  end subroutine swap_rows

  subroutine mul_row(M, rowi, n)
    real, intent(inout) :: M(:, :)
    integer, intent(in) :: rowi
    real, intent(in) :: n

    integer :: i
    do i = 1, size(M, 1)
      M(i, rowi) = M(i, rowi) * n
    end do
  end subroutine mul_row

  subroutine add_row(M, row1, row2, n)
    real, intent(inout) :: M(:, :)
    integer, intent(in) :: row1, row2
    real, intent(in) :: n

    integer :: i
    do i = 1, size(M, 1)
      M(i, row1) = M(i, row1) + M(i, row2) * n
    end do
  end subroutine add_row

  subroutine copy_vec(to, from)
    real, intent(out) :: to(:)
    real, intent(in) :: from(size(to))

    integer :: i
    do i = 1, size(to)
      to(i) = from(i)
    end do
  end subroutine copy_vec
end module gauss
