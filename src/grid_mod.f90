module grid_mod
   implicit none
   private

   public :: print_grid, read_from_file
contains
   subroutine read_from_file(path, file_grid, rows, cols)
      use kinds_mod
      implicit none
      character(len=*), intent(in) :: path
      integer(rk), intent(out) :: file_grid(100, 100)
      integer(rk), intent(out) :: rows, cols

      integer(rk) :: c
      integer(rk) :: unit = 10, ios
      character(len=256) :: line
      open(unit, file=path, status='old', action='read', iostat=ios)

      if (ios /= 0) then
         print '(A)', 'Error opening file: '//trim(path)
         stop 1
      end if

      rows = 0
      cols = 0
      do
         read(unit, '(A)', iostat=ios) line
         if (ios /= 0) exit

         rows = rows + 1
         cols = max(cols, len_trim(line))

         if (rows > size(file_grid, 1) .or. cols > size(file_grid, 2)) then
            print '(A)', 'Grid size exceeds maximum allowed (100x100).'
            stop 1
         end if

         do c = 1, len_trim(line)
            if (line(c:c) == '#') then
               file_grid(rows, c) = 1
            else
               file_grid(rows, c) = 0
            end if
         end do
      end do

      close(unit)
   end subroutine

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
