module grid_mod
   implicit none
contains
   subroutine print_grid(g, row, col)
      use kinds_mod
      implicit none

      integer(rk), intent(in) :: row, col
      integer(rk), intent(in) :: g(row, col)
      integer(rk) :: r, c

      do r = 1, row
         do c = 1, col
            if (g(r, c) == 1) then
               write(*, '(A)', advance='no') '██'
            else
               write(*, '(A)', advance='no') '░░'
            end if
         end do
         print *, ""
      end do
   end subroutine print_grid
end module grid_mod
